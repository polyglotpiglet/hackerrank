package com.ojha.datastructures.arrays

/**
 * O(log(n)) to find index of number in sorted, rotated array
 */
object RotatedArray extends App {

  val arr = Array[Int](15,16,19,20,25,1,3,4,5,7,10,14)
  val toFind = 5

  println(find(5, 0, arr.length - 1))

  def find(n: Int, start: Int, end: Int): Int = {
    val mid = ((end - start) / 2) + start
    if (arr(mid) == n) mid
    else if (arr(start) < arr(mid)) {
      if (arr(mid) < n) {
        find(n, mid, end)
      }
      else {
        find(n, start, mid)
      }
    }
    else {
      if (arr(mid) < n) {
        find(n, mid, end)
      }
      else {
        find (n, start, mid)
      }

    }
  }

}
