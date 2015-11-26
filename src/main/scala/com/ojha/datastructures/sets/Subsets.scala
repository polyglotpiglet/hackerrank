package com.ojha.datastructures.sets

/**
 * Created by alexandra on 26/11/15.
 */
object Subsets extends App {

  subs(Set(1,2,3)).foreach(println)

  def subs(s: Set[Int]): IndexedSeq[Set[Int]] = {
    for {
      i <- 0 until math.pow(2, s.size).toInt
      str = pad(Integer.toBinaryString(i), s.size)
      cat = s.zip(str.toCharArray).filter(x => x._2 == '1').map(p => p._1)
    } yield cat
  }

  def pad(s: String, n: Int): String = {
    if (s.length == n) s
    else pad("0" + s, n)
  }
}
