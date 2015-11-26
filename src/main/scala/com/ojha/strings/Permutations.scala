package com.ojha.strings

/**
 * Created by alexandra on 26/11/15.
 */
object Permutations extends App {
  val str = "abc"
  permute(str)

  def permute(str: String): Unit = {
    for (i <- 0 until str.length) {
      for (j <- i until str.length) {
        println(swap(str, i, j))
      }
    }
  }

  def swap(str: String, i: Int, j: Int): String = {
    val a = str.toCharArray
    val temp = a(i)
    a(i) = a(j)
    a(j) = temp
    a.mkString
  }

}
