package edu.aeracode.scala.s99p

import org.scalatest._

//noinspection SimplifyBoolean
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

}