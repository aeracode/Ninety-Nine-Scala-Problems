package edu.aeracode.scala.s99p.arithmetic

import org.scalatest.FunSuite
import edu.aeracode.scala.s99p.List

class S99IntSuite extends FunSuite {

  test("P31. isPrime") {
    assert(1.isPrime)
    assert(3.isPrime)
    assert(7.isPrime)
    assert(!8.isPrime)
    assert(!9.isPrime)
    assert(!10.isPrime)
  }


  test("P32. gcd") {
    assert(gcd(36, 63) == 9)
    assert(gcd(1071, 462) == 21)
    assert(gcd(462, 1071) == 21)
  }


  test("P33. isCoprimeTo") {
    assert(35.isCoprimeTo(64))
    assert(!4.isCoprimeTo(6))
  }


  test("P34. totient") {
    assert(10.totient == 4)
  }


  test("P35. primeFactors") {
    assert(315.primeFactors == List(3, 3, 5, 7))
    assert(4.primeFactors == List(2, 2))
  }


  test("P36. primeFactorMultiplicity") {
    assert(315.primeFactorMultiplicity == List((3, 2), (5, 1), (7, 1)))
    assert(4.primeFactorMultiplicity == List((2, 2)))
  }
}