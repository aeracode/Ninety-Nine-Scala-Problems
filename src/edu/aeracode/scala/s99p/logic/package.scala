package edu.aeracode.scala.s99p

import list._

import scala.collection.mutable

package object logic {


  implicit class S99Boolean(val value: Boolean) extends AnyVal {
    def or(x: => Boolean): Boolean = logic.or(value, x)
    def and(x: => Boolean): Boolean = logic.and(value, x)
    def nand(x: => Boolean): Boolean = logic.nand(value, x)
    def xor(x: => Boolean): Boolean = logic.xor(value, x)
    def equ(x: => Boolean): Boolean = logic.equ(value, x)
  }

  /** =P46 (**) Truth tables for logical expressions=
    *
    * Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which
    * return true or false according to the result of their respective operations; e.g. and(A, B)
    * is true if and only if both A and B are true.
    *
    * @example
    * {{{
    * scala> and(true, true)
    * res0: Boolean = true
    *
    * scala> xor(true. true)
    * res1: Boolean = false
    * }}}
    *
    * A logical expression in two variables can then be written as an function of two variables,
    * e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
    *
    * Now, write a function called table2 which prints the truth table of a given logical
    * expression in two variables.
    * @example
    * {{{
    * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    * A     B     result
    * true  true  true
    * true  false true
    * false true  false
    * false false false
    * }}}
    */
  type BoolFunc = (=> Boolean, => Boolean) => Boolean
  val and : BoolFunc = (a, b) => if (a) b else false
  val or  : BoolFunc = (a, b) => if (a) true else b
  val nand: BoolFunc = (a, b) => if (a) !b else true
  val xor : BoolFunc = (a, b) => a != b
  val equ : BoolFunc = (a, b) => a == b
  //val impl: BoolFunc =

  val boolValues = List(true, false)

  def table2(f: BoolFunc): Unit = {
    def row(a: Boolean, b: Boolean): Unit = println(f"$a%-6s $b%-6s ${f(a, b)}%s")
    println("A      B      result")
    for (a <- boolValues; b <- boolValues) row(a, b)
  }

  /** =P47 (*) Truth tables for logical expressions (2)=
    * Continue problem  P46 by redefining and, or, etc as operators= (i.e. make them methods of a
    * new class with an implicit conversion from Boolean.) not will have to be left as a object
    * method.
    *
    * @example
    * {{{
    * scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
    * A     B     result
    * true  true  true
    * true  false true
    * false true  false
    * false false false
    * }}}
    */
  def table2infix(f: BoolFunc): Unit = {
    table2(f)
  }

  /* =P48 (**) Truth tables for logical expressions (3)=     Omitted for now.  */

  /** =P49 (**) Gray code=
    * An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.
    * For example,
    * {{{
    * n = 1: C(1) = ("0", "1").
    * n = 2: C(2) = ("00", "01", "11", "10").
    * n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
    * }}}
    * Find out the construction rules and write a function to generate Gray codes.
    *
    * @example
    * {{{
    * scala> gray(3)
    * res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
    * }}}
    *
    * See if you can use [[https://en.wikipedia.org/wiki/Memoization memoization]] to make the
    * function more efficient.
    */
  private val grayMemo = mutable.Map(0 -> List(""))
  def gray(n: Int): List[String] = grayMemo.getOrElseUpdate(n,
    (gray(n - 1) mapReverse ("0" + _)) ~++: (gray(n - 1) mapReverse ("1" + _)))


  def grayR(n: Int): List[String] = {
    if (n == 0) List("")
    else {
      val g = grayR(n - 1)
      val l1 = g mapReverse ("0" + _)
      val l2 = g mapReverse ("1" + _)
      l1 ~++: l2
    }
  }

  /** =P50 (***) Huffman code=
    * First of all, consult a good book on discrete mathematics or algorithms for a detailed
    * description of Huffman codes(
    * [[https://en.wikipedia.org/wiki/Huffman_coding or wikipedia]], but
    * [[https://en.wikipedia.org/wiki/Canonical_Huffman_code this]]
    * wiki article is better for lesson.
    *
    * We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples.
    *
    * @example
    * {{{
    * (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)).
    * }}}
    * Our objective is to
    * construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
    * @example
    * {{{
    * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e, 1101), (f, 1100))
    * }}}
    */
  //  compute huffman code:
  //    input:   message ensemble (set of (message, probability)).
  //    base D.
  //  output:  code ensemble (set of (message, code)).
  //  algorithm:
  //    1- sort the message ensemble by decreasing probability.
  //    2- N is the cardinal of the message ensemble (number of different   messages).
  //    3- compute the integer n_0 such as 2<=n_0<=D and (N-n_0)/(D-1) is integer.
  //    4- select the n_0 least probable messages, and assign them each a digit code.
  //    5- substitute the selected messages by a composite message summing their probability, and
  //         re-order it.
  //    6- while there remains more than one message, do steps thru 8.
  //        7- select D least probable messages, and assign them each a digit code.
  //        8- substitute the selected messages by a composite message summing their probability,
  //           and re-order it.
  //    9- the code of each message is given by the concatenation of the code digits of the
  //         aggregate they've been put in.
  def huffman(input: List[(String, Int)]): List[(String, String)] = {
    type M = String
    type N = Int


    sealed trait C {def f: N}
    case class G(c0: C, c1: C, f: N) extends C
    case class S(m: M, f: N) extends C

    println(input)
    val ms = input mapReverse { e => S(e._1, e._2) }

    def buildComposition(l: List[C]): C =
      if (l.tail.isEmpty) l.head
      else {
        val f: (C, C) => Boolean = _.f < _.f
        val a = l.min(f)
        val l1 = l :- a
        val b = l1.min(f)
        val l2 = l1 :- b
        buildComposition(G(a, b, a.f + b.f) +: l2)
      }

    val composition = buildComposition(ms)
    println(composition)

    def find(c: C, m: M, path: String): Option[String] = c match {
      case S(v, _)      => if (v == m) Some(path) else None
      case G(c0, c1, _) => find(c0, m, path + "0") orElse find(c1, m, path + "1")
    }


    ms mapReverse (e => (e.m, find(composition, e.m, "").get))
  }

}
