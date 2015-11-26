package com.ojha.bits

/**
 * Created by alexandra on 26/11/15.
 */
object DecBin extends App {

  val x = 0.625

  toBin(x)

  def toBin(x: Double): Double = {

    val rawr = x * 2
    val first = rawr.toInt
    val rest = rawr - first
    println(s"first = $first rest = $rest" )


    x

  }

}
