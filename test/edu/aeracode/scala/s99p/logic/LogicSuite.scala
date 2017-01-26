package edu.aeracode.scala.s99p.logic

import edu.aeracode.scala.s99p.list.{Cons, List, Nil}
import org.scalatest.FunSuite

// TODO: 2017-01-26 Add documentation
class LogicSuite extends FunSuite {

  test("P46") {
    table2((a, b) => and(a, or(a, b)))
  }

  test("P47") {
    table2((a, b) => a and (a or b))
  }

  test("P49. gray") {
    assert(gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }


  test("P50. huffman") {
    val h = huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
    println(h)
    assert(h
      == List(("a", "0"), ("b", "101"), ("c", "100"), ("d", "111"), ("e", "1101"), ("f", "1100")))
  }

}
