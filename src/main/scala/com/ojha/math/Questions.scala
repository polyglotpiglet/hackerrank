package com.ojha.math

/**
 * Created by alexandra on 23/11/15.
 */
object Questions {

  def main(args: Array[String]): Unit = {
    summingTheNSeries()
  }

  def summingTheNSeries(): Unit = {
    read().map{ n => (n * n) % 1000000007 } foreach println
  }

  def read(): IndexedSeq[BigInt] = {
    val noLines = scala.io.StdIn.readLine().toInt
    for (i <- 0 until noLines) yield BigInt(scala.io.StdIn.readLine().trim)
  }


}
