package edu.aeracode.scala.s99p

// TODO: 2017-01-26 Add documentation
package object list {

  import List._

  /** =[[http://aperiodic.net/phil/scala/s-99/ S-99: Ninety-Nine Scala Problems]]=
    *
    * == Lists ==
    *
    * [[https://en.wikipedia.org/wiki/List_(abstract_data_type)]]
    *
    * 1. Making List -> List[E] -> List[+E]
    * 2. Construct Operator's type parameter are covariant: A >: E
    * 3. foldr / foldl
    * 4. map (P10 encode)
    * 5. (P12, decode) flatten + implicit (identity) (for ensure element type)
    *    - flatMap for
    * 6. To0 much List[E]? - type alias helps!
    * {{{
    * Here         00-09
    * Companion    10-12
    * }}}
    */
  sealed trait List[+E] {

    /** shortened type alias */
    private type L[+T] = List[T]

    def head: E
    def tail: List[E]
    def isEmpty: Boolean
    def :+[A >: E](e: A): L[A]
    def +:[A >: E](e: A): L[A]
    def :++[A >: E](l: L[A]): L[A]
    def :-[A >: E](e: A): List[A]
    def :--[A >: E](es: L[A]): L[A] = (es fold[L[A]] this) { (r, e) => r :- e }
    def ~++:[A >: E](es: L[A]): L[A] = (es fold[L[A]] this) { (r, e) => e +: r }

    /** =Fold Right=
      * x,,1,,:x,,2,,:x,,3,,:x,,4,,:x,,5,, => f x,,1,, (f x,,2,, (f x,,3,, (f x,,4,, (f x,,5,, z))))
      */
    def foldr[T](z: T)(f: (E, T) => T): T = List.foldr(f, z, this)

    /** =Fold Left=
      * x,,1,,:x,,2,,:x,,3,,:x,,4,,:x,,5,, => f (f (f (f (f z x,,1,,) x,,2,,) x,,3,,) x,,4,,) x,,5,,
      */
    def fold[T](z: T)(f: (T, E) => T): T = List.foldl(f, z, this)

    /** Is equivalent of `foldr1` */
    def reduceRight[A >: E](f: (E, A) => A): A =
      if (isEmpty) notDefined("reduceRight")
      else if (tail.isEmpty) head
      else f(head, tail reduceRight f)

    /** Is equivalent of `foldl1` */
    def reduce[T >: E](f: (T, E) => T): T =
      if (isEmpty) notDefined("reduce")
      else List.foldl(f, head, tail)


    /** [[https://en.wikipedia.org/wiki/Map_(higher-order_function)]]
      *
      * @note Build upon a fold and reverse functions
      */
    def map[T](f: E => T): L[T] = mapReverse(f).reverse

    def mapReverse[T](f: E => T): L[T] = fold(nil[T]) { (r, e) => f(e) +: r }

    def flatten[B](implicit toList: E => L[B]): L[B] = reverse.flattenReverse(toList)

    def flattenReverse[B](implicit toList: E => L[B]): L[B] =
      fold(nil[B]) { (r, e) => toList(e) :++ r }

    def flatMap[T](f: E => L[T]): L[T] = fold(nil[T]) { (r, e) => r :++ f(e) }

    def filterReverse(p: (E) => Boolean): L[E] = fold(nil[E]) { (r, e) =>
      if (p(e)) e +: r else r
    }

    def filter(p: (E) => Boolean): L[E] = filterReverse(p).reverse

    def foreach(proc: (E) => Any): Unit = map(proc)

    override def toString: String =
      if (isEmpty) "Nil"
      else tail.fold(s"($head") { (s, e) => s + s",$e" } + ")"

    /*------------------------------------------------------------------------------------------*/

    /** P01 (*) Find the last element of a list. */
    def last: E = if (isEmpty) notDefined("last") else reduce((_, e) => e)

    /** P02 (*) Find the last but one element of a list (generalized to n). */
    def lastN(n: Int): E =
      if (isEmpty) notDefined("lastN")
      else if (n < 1) throw new IndexOutOfBoundsException
      else fold((n, this)) { case ((count, list), _) =>
        if (count > 0) (count - 1, list)
        else (0, list.tail)
      } match {
        case (0, list) => list.head
        case _         => throw new IndexOutOfBoundsException
      }

    /** P03 (*) Find the Kth element of a list. */
    def nth(n: Int): E =
      if (isEmpty) notDefined("apply")
      else if (n == 0) head
      else tail.nth(n - 1)

    /** P04 (*) Find the number of elements of a list. */
    def length: Int = fold(0) { (n, _) => n + 1 }

    /** P05 (*) Reverse a list. */
    def reverse: List[E] = fold(nil[E]) { (r, e) => e +: r }

    /** P06 (*) Find out whether a list is a palindrome. */
    def isPalindrome: Boolean = this == reverse

    /** =P07 (**) Flatten a nested list structure=
      *
      * @example
      * {{{
      * scala> List(List(1, 1), 2, List(3, List(5, 8))).flattenDeep
      * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
      * }}}
      * @note ranamed to flattenRecursive.
      * @see [[edu.aeracode.scala.s99p.List.flatten]]
      */
    def flattenRecursive: List[Any] = (reverse fold nil[Any]) {
      case (r, e: List[_]) => e.flattenRecursive :++ r
      case (r, e)          => e +: r
    }

    /** =P08 (**) Eliminate consecutive duplicates of list elements=
      * If a list contains repeated elements they should be replaced with a single copy of the
      * element. The order of the elements should not be changed.
      *
      * @example
      * {{{
      * scala> List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).compress
      * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
      * }}}
      */
    def compress: List[E] =
      fold(nil[E]) { (l, e) => if (l.isEmpty || l.head != e) e +: l else l }.reverse

    /** =P09 (**) Pack consecutive duplicates of list elements into sublists=
      * If a list contains repeated elements they should be placed in separate sublists.
      *
      * @example
      * {{{
      * scala> List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).pack
      * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a),
      *       List('d), List('e, 'e, 'e, 'e))
      * }}}
      */
    def pack: List[List[E]] = fold(nil[List[E]]) { (l, e) =>
      if (l.isEmpty || l.head.head != e) List(e) +: l
      else (e +: l.head) +: l.tail
    }.reverse

    /** =P10 (*) Run-length encoding of a list=
      * Use the result of problem P09 to implement the so-called run-length encoding data
      * compression
      * method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the
      * number
      * of duplicates of the element E.
      *
      * @example
      * {{{
      * scala> List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).encode
      * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
      * }}}
      */
    def encode: List[(Int, E)] = pack map { e => (e.length, e.head) }


    /** =P11 (*) Modified run-length encoding=
      *
      * Modify the result of problem P10 in such a way that if an element has no duplicates it is
      * simply copied into the result list. Only elements with duplicates are transferred
      * as (N, E) terms.
      *
      * @example
      * {{{
      * scala> List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).encodeModified
      * res0: List[Any] = List(Right(4, 'a), Left('b), Right(2, 'c), Right(2, 'a), Left('d),
      *   Right(4, 'e)))
      * }}}
      */
    def encodeModified: List[Either[E, (Int, E)]] =
      encode map { e => if (e._1 == 1) Left(e._2) else Right(e) }

    /** =P12 (**) Decode a run-length encoded list=
      *
      * Given a run-length code list generated as specified in problem P10, construct its
      * uncompressed
      * version.
      *
      * @example
      * {{{
      * scala> List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)).decode
      * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      * }}}
      */
    def decode[B](implicit asTuple: E => (Int, B)): List[B] = {
      // TODO: Move to List companion object
      def fill(n: Int, a: B, l: List[B] = Nil): List[B] = if (n == 0) l else fill(n - 1, a, a +: l)
      flatMap { e =>
        val t = asTuple(e)
        fill(t._1, t._2)
      }
    }

    /** =P13 (**) Run-length encoding of a list (direct solution)=
      *
      * Implement the so-called run-length encoding data compression method directly.
      * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
      *
      * @example
      * {{{
      * scala> List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).encodeDirect
      * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
      * }}}
      */
    def encodeDirect: List[(Int, E)] = fold(nil[(Int, E)]) { (r, e) =>
      if (r.isEmpty || r.head._2 != e) (1, e) +: r
      else (r.head._1 + 1, e) +: r.tail
    }.reverse

    /** =P14 (*) Duplicate the elements of a list=
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'c, 'd).duplicate
      * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
      * }}}
      */
    def duplicate: List[E] = (reverse fold nil[E]) { (r, e) => e +: e +: r }

    /** =P15 (**) Duplicate the elements of a list a given number of times=
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'c, 'd).duplicateN(3)
      * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
      * }}}
      */
    def duplicateN(n: Int): List[E] =
      if (n < 0) throw new IllegalArgumentException("n < 0")
      else (reverse fold nil[E]) { (r, e) =>
        def loop(i: Int, acc: List[E]): List[E] = if (i == 0) acc else loop(i - 1, e +: acc)
        loop(n, r)
      }


    /** =P16 (**) Drop every Nth element from a list=
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).drop(3)
      * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
      * }}}
      */
    def drop(n: Int): L[E] = fold((1, nil[E])) { case ((i, r), e) =>
      if (i % n == 0) (i + 1, r) else (i + 1, e +: r)
    }._2.reverse


    /** =P17 (*) Split a list into two parts=
      * The length of the first part is given. Use a Tuple for your result.
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).split(3)
      * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      * }}}
      */
    def split(n: Int): (L[E], L[E]) =
      if (n < 0) throw new IllegalArgumentException("n < 0")
      else {
        def loop(i: Int, acc: L[E], l: L[E]): (L[E], L[E]) =
          if (i == 0 || l.isEmpty) (acc, l)
          else loop(i - 1, l.head +: acc, l.tail)
        val (l, r) = loop(n, Nil, this)
        (l.reverse, r)
      }

    /** =P18 (**) Extract a slice from a list=
      *
      * Given two indices, I and K, the slice is the list containing the elements from and including
      * the Ith element up to but not including the Kth element of the original list. Start counting
      * the elements with 0.
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).slice(3, 7)
      * res0: List[Symbol] = List('d, 'e, 'f, 'g)
      * }}}
      */
    def slice(from: Int, to: Int): L[E] =
      if (from > to) throw new IllegalArgumentException("from > to")
      else if (from < 0) throw new IllegalArgumentException("from negative")
      else if (to < 0) throw new IllegalArgumentException("from negative")
      else {

        // TODO: Separate method?
        def drop(i: Int, l: L[E]): L[E] =
          if (i == 0) l
          else if (l.isEmpty) throw new IndexOutOfBoundsException
          else drop(i - 1, l.tail)

        // TODO: Separate method? Reversed
        def take(i: Int, l: L[E], acc: L[E]): L[E] =
          if (i == 0) acc
          else if (l.isEmpty) throw new IndexOutOfBoundsException
          else take(i - 1, l.tail, l.head +: acc)

        take(to - from, drop(from, this), Nil).reverse
      }


    /** =P19 (**) Rotate a list N places to the left=
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).rotate(3)
      * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      *
      * scala> List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).rotate(-2)
      * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
      * }}}
      */
    def rotate(n: Int): L[E] =
      if (isEmpty) Nil else {
        /*  did mistake did not test values bigger than length  */
        /*  did mistake did not test values bigger than length  */
        val size = length
        val norm = n % size
        val point = if (norm >= 0) norm else size + norm
        val (first, second) = split(point)
        second :++ first
      }

    /** =P20 (*) Remove the Kth element from a list=
      * Return the list and the removed element in a Tuple. Elements are numbered from 0.
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd).removeAt(1)
      * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
      * }}}
      */
    def removeAt(index: Int): (L[E], E) = {
      val (l1, l2) = split(index)
      if (l2.isEmpty) throw new IndexOutOfBoundsException
      (l1 :++ l2.tail, l2.head)
    }

    /** =P21 (*) Insert an element at a given position into a list=
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd).insertAt(1, 'new)
      * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
      * }}}
      */
    // TODO: Check bounds doesn't work
    def insertAt[A >: E](index: Int, e: A): List[A] =
    if (isEmpty && index > 0) throw new IndexOutOfBoundsException
    else {
      val (l1, l2) = split(index)
      l1 :++ (e +: l2)
    }

    /* P22 List.obj*/


    /** =P23 (**) Extract a given number of randomly selected elements from a list=
      *
      * @example
      * {{{
      * scala> List('a, 'b, 'c, 'd, 'f, 'g, 'h).randomSelect(3)
      * res0: List[Symbol] = List('e, 'd, 'a)
      * }}}
      * @note Hint: Use the solution to problem P20
      * @todo Randomly?
      */
    def randomSelect(n: Int): L[E] =
      if (n < 0) throw new IllegalArgumentException
      else {
        def loop(n: Int, list: L[E], listLength: Int, acc: L[E]): L[E] =
          if (n == 0) acc
          else {
            val (nextL, e) = list.removeAt((math.random * listLength).toInt)
            loop(n - 1, nextL, listLength - 1, e +: acc)
          }
        loop(n, this, length, Nil)
      }

    /* P24 List.obj */


    /** =P25 (*) Generate a random permutation of the elements of a list=
      *
      * @note Use the solution of problem P23.
      * @example
      * {{{
      * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
      * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
      * }}}
      * @todo see [[http://aperiodic.net/phil/scala/s-99/p25.scala proposed solution]]
      */
    def randomPermute: L[E] = randomSelect(length)

    /** =P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a
      * list=
      * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that
      * there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
      * For pure mathematicians, this result may be great. But we want to really generate all the
      * possibilities.
      *
      * @example
      * {{{
      * scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
      * res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
      * }}}
      * @todo see [[http://aperiodic.net/phil/scala/s-99/p26.scala]]
      */
    def combinations(n: Int): L[L[E]] =
      if (n == 0) List(List())
      else {
        def loop(l: L[E], acc: L[L[E]]): L[L[E]] =
          if (l.isEmpty) acc
          else loop(l.tail, acc :++ (l.tail.combinations(n - 1) map (l.head +: _)))
        loop(this, Nil)
      }


    /** =P27 (**) Group the elements of a set into disjoint subsets=
      * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4
      * persons? Write a function that generates all the possibilities.
      *
      * @example
      * {{{
      * scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
      * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List
      * (Flip, Gary, Hugo, Ida)), ...
      * }}}
      *
      * b) Generalize the above predicate in a way that we can specify a list of group sizes and
      * the predicate will return a list of groups.
      * @example
      * {{{
      * scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary",
      * "Hugo", "Ida"))
      * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi,
      * Flip, Gary, Hugo, Ida)), ...
      * }}}
      *
      * Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the
      * same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat),
      * (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
      * <br>
      * You may find more about this combinatorial problem in a good book on discrete mathematics
      * under the term "multinomial coefficients".
      */
    def group(ks: L[Int]): L[L[L[E]]] = if (ks.isEmpty) List(nil) else
      for {
        cl <- combinations(ks.head)
        rest <- this :-- cl group ks.tail
      } yield {
        cl +: rest
      }


    /** =P28 (**) Sorting a list of lists according to length of sublists=
      *
      * a) We suppose that a list contains elements that are lists themselves. The objective is to
      * sort the elements of the list according to their length. E.g. short lists first, longer
      * lists later, or vice versa.
      *
      * @example
      * {{{
      * scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i,
      * 'j, 'k, 'l), List('m, 'n), List('o)))
      * res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List
      * ('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
      * }}}
      *
      * b) Again, we suppose that a list contains elements that are lists themselves. But this time
      * the objective is to sort the elements according to their length frequency; i.e. in the
      * default, sorting is done ascendingly, lists with rare lengths are placed, others with a
      * more frequent length come later.
      * @example
      * {{{
      * scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List
      * ('i, 'j, 'k, 'l), List('m, 'n), List('o)))
      * res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f,
      * 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
      * }}}
      *
      * Note that in the above example, the first two lists in the result have length 4 and 1 and
      * both lengths appear just once. The third and fourth lists have length 3 and there are two
      * list of this length. Finally, the last three lists have length 2. This is the most frequent
      * length.
      */
    def lsort[T](implicit asList: E => L[T]): L[E] = sort {asList(_).length < asList(_).length}
    def lsortFreq[T](implicit asList: E => L[T]): L[E] = {
      val ll = map(e => (asList(e).length, e))
      ll.map { case (s, e) => (ll.count(_._1 == s), e) }
        .sort({ case ((l1, _), (l2, _)) => l1 < l2 })
        .map(_._2)


    }


    def count(p: E => Boolean): Int = fold(0) { (r, e) => if (p(e)) r + 1 else r }

    def min(isLess: (E, E) => Boolean): E = if (isEmpty) notDefined("min") else
      reduce((a, b) => if (isLess(a, b)) a else b)

    // TODO: sorting whole themes
    def sort(isLess: (E, E) => Boolean): L[E] = if (isEmpty) Nil else {
      val m = min(isLess)
      m +: (this :- m).sort(isLess)
    }

  }


  object List {
    def apply[E](xs: E*): List[E] = if (xs.isEmpty) Nil else Cons(xs.head, List(xs.tail: _*))
    def empty[E]: List[E] = List[E]()

    /** [[edu.aeracode.scala.s99p.List#foldr]] */
    def foldr[T, E](f: (E, T) => T, z: T, xs: List[E]): T =
      if (xs.isEmpty) z else f(xs.head, foldr(f, z, xs.tail))

    /** [[edu.aeracode.scala.s99p.List#fold]] */
    def foldl[T, E](f: (T, E) => T, z: T, xs: List[E]): T =
      if (xs.isEmpty) z else foldl(f, f(z, xs.head), xs.tail)

    private[list] def notDefined(s: String): Nothing =
      throw new NoSuchElementException(s + " is not defined for Nil")


    /** =P22 (*) Create a list containing all integers within a given range=
      *
      * @example
      * {{{
      * scala> range(4, 9)
      * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
      * }}}
      */
    // TODO: Create object?
    def range(from: Int, to: Int): List[Int] = {
      def loop(n: Int, acc: List[Int]): List[Int] =
        if (n < from) acc
        else loop(n - 1, n +: acc)
      loop(to, Nil)
    }

    /** =P24 (*) Lotto: Draw N different random numbers from the set 1..M=
      *
      * @example
      * {{{
      * scala> lotto(6, 49)
      * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
      * }}}
      */
    def lotto(n: Int, m: Int): List[Int] =
      if (n < 0) throw new IllegalArgumentException
      else {
        def loop(i: Int, acc: List[Int]): List[Int] =
          if (i == 0) acc
          else loop(i - 1, (1 + (math.random * m).toInt) +: acc)
        loop(n, Nil)
      }


  }


  case object Nil extends List[Nothing] {
    override def head: Nothing = notDefined("first")
    override def tail: Nothing = notDefined("tail")
    override def isEmpty = true
    override def :+[A >: Nothing](e: A): List[A] = Cons(e, Nil)
    override def +:[A >: Nothing](e: A): List[A] = Cons(e, Nil)
    override def :++[A >: Nothing](l: List[A]): List[A] = l
    override def :-[A >: Nothing](e: A): List[A] = Nil
  }


  case class Cons[E](head: E, tail: List[E]) extends List[E] {
    override def isEmpty = false
    // TODO: Recursive
    override def :+[A >: E](x: A): List[A] = Cons(head, tail :+ x)
    override def +:[A >: E](x: A): List[A] = Cons(x, this)
    override def :++[A >: E](l: List[A]): List[A] = Cons(head, tail :++ l)
    // TODO: Recursive
    override def :-[A >: E](e: A): List[A] = if (e == head) tail else Cons(head, tail :- e)
  }

  def nil[E]: List[E] = Nil

}
