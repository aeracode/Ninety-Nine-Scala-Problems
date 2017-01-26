package edu.aeracode.scala.s99p.list

import org.scalatest.FunSuite

class ListSuite extends FunSuite {

  test("equality of lists created by either Cons or List") {
    assert(Cons(1, Cons(2, Cons(3, Nil))) == List(1, 2, 3))
    assert(Cons(1, Cons(2, Cons(3, Nil))) != List(1, 3, 2))
  }

  // TODO: Replace with property generator driven test
  test("Abstract definition axiom ```first (cons (e, l)) = e```") {
    val e = 5
    val l = List(2, 3, 4, 5)
    assert(Cons(e, l).head == e)
  }

  // TODO: Replace with property generator driven test
  test("Abstract definition axiom ```rest (cons (e, l)) = l```") {
    val e = 5
    val l = List(2, 3, 4, 5)
    assert(Cons(e, l).tail == l)
  }

  test("append") {
    assert(Nil :+ 1 == List(1))
    assert(Cons(1, Nil) :+ 1 == List(1, 1))
  }

  /** [[http://zvon.org/other/haskell/Outputprelude/foldr_f.html]] */
  test("foldr") {
    import List.foldr
    assert(foldr[Int, Int](_ + _, 5, List(1, 2, 3, 4)) == 15)
    assert(foldr[Int, Int](_ / _, 2, List(8, 12, 24, 4)) == 8)
    assert(foldr[Int, Int](_ / _, 3, List()) == 3)
    assert(foldr[Boolean, Boolean](_ && _, true, List(1 > 2, 3 > 2, 5 == 5)) == false)
    assert(foldr(math.max, 18, List(3, 6, 12, 4, 55, 11)) == 55)
    assert(foldr(math.max, 111, List(3, 6, 12, 4, 55, 11)) == 111)
    assert(foldr[Int, Int]((x, y) => (x + y) / 2, 54, List(12, 4, 10, 6)) == 12)
  }


  /** [[http://zvon.org/other/haskell/Outputprelude/foldl_f.html]] */
  test("foldl") {
    assert((List(4, 2, 4) fold 64) (_ / _) == 2)
    assert((List[Int]() fold 3) (_ / _) == 3)
    assert((List(1, 2, 3, 4) fold 5) (math.max) == 5)
    assert((List(1, 2, 3, 4, 5, 6, 7) fold 5) (math.max) == 7)
    assert((List(1, 2, 3) fold 4) ((x, y) => 2 * x + y) == 43)
  }


  /** [[http://zvon.org/other/haskell/Outputprelude/foldl_f.html]] */
  test("foldr1") {
    assert((List(1, 2, 3, 4) reduceRight (_ + _)) == 10)
    assert((List(8, 12, 24, 4) reduceRight (_ / _)) == 4)
    assert((List(12) reduceRight (_ / _)) == 12)
    assert((List(1 > 2, 3 > 2, 5 == 5) reduceRight (_ && _)) == false)
    assert((List(3, 6, 12, 4, 55, 11) reduceRight math.max) == 55)
    assert((List(12, 4, 10, 6) reduceRight { (x, y) => (x + y) / 2 }) == 9)
  }

  test("flatten") {
    assert(List(List(1, 2), List(3, 4)).flatten == List(1, 2, 3, 4))
  }

  test("substraction") {
    println(List(1, 2, 3) :- 3)
    println(List(1, 2, 3, 3) :-- List(3))
  }

  test("min") {
    println(List(1, 2, 3).min(_ < _))
  }

  test("sort") {
    println(List(22, 12, 413).sort(_ < _))
  }


  test("P01. last") {
    assert(List(1, 1, 2, 3, 5, 8).last == 8)
  }

  test("P05. reverse") {
    assert(List(1, 1, 2, 3, 5, 8).reverse == List(8, 5, 3, 2, 1, 1))
    assert(Nil.reverse == Nil)
  }

  test("P06. isPalindrome") {
    assert(List(1, 1, 2, 2, 1, 1).isPalindrome == true)
    assert(List(2, 1, 2, 2, 1, 1).isPalindrome == false)
  }

  test("P07. flattenRecursive") {
    assert(List(List(1, 1), 2, List(3, List(5, 8))).flattenRecursive
      == List(1, 1, 2, 3, 5, 8))
  }

  test("P08. compress") {
    assert(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).compress
      == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(Nil.compress == Nil)
  }

  test("P09. pack") {
    assert(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).pack
      == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e,
      'e, 'e)))
    assert(Nil.pack == Nil)
  }

  test("P10. encode") {
    assert(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).encode
      == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(Nil.encode == Nil)
  }

  test("P11. encodeModified") {
    assert(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).encodeModified
      == List(Right(4, 'a), Left('b), Right(2, 'c), Right(2, 'a), Left('d), Right(4, 'e)))

    assert(Nil.encodeModified == Nil)
  }

  test("P12. decode") {
    assert(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)).decode
      == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    val l = List(1, 1, 2, 3, 5, 5, 8)
    assert(l.encode.decode == l)
    assert(Nil.encode == Nil)
  }

  test("P13. encodeDirect") {
    assert(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).encodeDirect
      == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(Nil.encodeDirect == Nil)
  }

  test("P14. duplicate") {
    assert(List('a, 'b, 'c, 'c, 'd).duplicate == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    assert(Nil.duplicate == Nil)
  }

  test("P15. duplicateN") {
    assert(List('a, 'b, 'c, 'c, 'd).duplicateN(3)
      == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    assert(Nil.duplicateN(3) == Nil)
  }

  test("P16. drop") {
    assert(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).drop(3)
      == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    assert(Nil.drop(10) == Nil)
  }

  test("P17. split") {
    assert(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).split(3)
      == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    assert(Nil.split(10) == (Nil, Nil))
    assertThrows[IllegalArgumentException](Nil.split(-1))
  }

  test("P18. slice") {
    assert(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).slice(3, 7) == List('d, 'e, 'f, 'g))
    assert(List('a, 'b, 'c, 'd).slice(1, 1) == Nil)
    assertThrows[IndexOutOfBoundsException](Nil.slice(1, 2))
    assertThrows[IndexOutOfBoundsException](List(1, 2).slice(1, 3))
    assertThrows[IllegalArgumentException](Nil.slice(-1, 2))
    assertThrows[IllegalArgumentException](Nil.slice(1, -2))
    assertThrows[IllegalArgumentException](Nil.slice(2, 1))
  }

  test("P19. rotate") {
    assert(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).rotate(3)
      == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).rotate(-2)
      == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    assert(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k).rotate(0)
      == List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    assert(Nil.rotate(24) == Nil)
  }

  test("P20. removeAt") {
    assert(List('a, 'b, 'c, 'd).removeAt(1)
      == (List('a, 'c, 'd), 'b))
    assertThrows[IndexOutOfBoundsException](Nil.removeAt(24))
  }

  test("P21. intertAt") {
    assert(List('a, 'b, 'c, 'd).insertAt(1, 'new)
      == List('a, 'new, 'b, 'c, 'd))
    assert(Nil.insertAt(0, 1) == List(1))
    assertThrows[IndexOutOfBoundsException](Nil.insertAt(1, 1))
    // TODO: Doesn't work
    // assertThrows[IndexOutOfBoundsException](List('a, 'b, 'c, 'd).insertAt(6, 'new))
  }

  test("P22. range") {
    assert(List.range(4, 9) == List(4, 5, 6, 7, 8, 9))
    assert(List.range(9, 0) == Nil)
    assert(List.range(4, 4) == List(4))
  }

  test("P23. randomSelect") {
    assert(List('a, 'b, 'c, 'd, 'f, 'g, 'h).randomSelect(3).length == 3)
    assert(List('a, 'a, 'a, 'a, 'a, 'a, 'a).randomSelect(3) == List('a, 'a, 'a))
  }

  test("P23. lotto") {
    assert(List.lotto(6, 10).length == 6)
    assert(List.lotto(6, 1) == List(1, 1, 1, 1, 1, 1))
    assert(List.lotto(0, 1) == Nil)
  }

  test("P24. combination") {
    val list = List('a, 'b, 'c, 'd)
    val n = 3
    println(s"Combinations of $n from $list:")
    println(list.combinations(n))
  }

  test("P24. group") {
    var list = List('a, 'b, 'c)
    var k = List(2, 1)
    println(s"Groups $k from $list:")
    println(list.group(k))

    k = List(1, 1, 1)
    println(s"Groups $k from $list:")
    println(list.group(k))
  }


  test("P24. lsort") {
    println(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e),
      List('i, 'j, 'k, 'l), List('m, 'n), List('o)).lsort)


    println(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e),
      List('i, 'j, 'k, 'l), List('m, 'n), List('o)).lsortFreq)
  }





}

