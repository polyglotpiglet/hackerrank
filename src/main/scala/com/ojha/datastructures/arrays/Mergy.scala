package com.ojha.datastructures.arrays

/**
 * Two sorted arrays, A and B
 * A has enough empty space to fit B
 * merge b into A
 */
object Mergy extends App {
  val a = Array[Int](1,3,5,0,0)
  val b = Array[Int](2,4)

  merge(a,b)
  a.foreach(println)

  def merge(a: Array[Int], b: Array[Int]) = {
    var lastInA = a.length - b.length - 1
    var last = a.length - 1
    b.reverse.foreach { x =>
      if (x > a(lastInA)) {
        a(last) = x
        last -= 1
      }
      else {
        while (a(lastInA) > x ) {
          a(last) = a(lastInA)
          lastInA -= 1
          last -= 1
        }
        a(last) = x
        last -= 1
      }
    }

  }


}
