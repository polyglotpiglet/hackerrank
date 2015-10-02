package com.ojha.algorithms

/**
 * Created by alexandra on 01/10/15.
 */
object Meow extends App {

    def parseInput(): (Int, Int, Int) = {
        val input = io.Source.stdin.getLines().take(1).flatMap(_.split(' ').map(_.toInt)).toArray
        if (input.length != 3) throw new RuntimeException("Grr should pass in M (# rows in matrix), N (# columns) and R (# rotations)")

        val M = input(0)
        val N = input(1)
        val R = input(2)

        (M,N,R)
    }

    def readMatrix(rows: Int, columns: Int) = {
       val read = io.Source.stdin.getLines().take(rows).toArray
      read.map(_.split(' ').map(_.toInt))
    }

    val (rows, columns, rotations) = parseInput()
    val matrix = readMatrix(rows, columns)
    //val rotated = rotateMatrix(rows, columns, matrix)
    val rotated = rotateLoop(0, rows, columns, matrix)


  def rotateMatrix(rows: Int, columns: Int, matrix: Array[Array[Int]]) = {
      println("----------------")
      for (i <- 0 until columns/2) {
        println(i)
      }
    }

  def isInLoop(i: Int, j: Int, index: Int, rows: Int, columns: Int): Boolean = {
    return 
  }

  def rotateLoop(index: Int, rows: Int, columns: Int, matrix: Array[Array[Int]]): Unit = {
    for (i <- rows; j <- rows) {
      if (isInLoop(i, j, index)) // move backwards
      else matrix(i)(j)

    }
  }









    def cats() {
        println(io.Source.stdin.getLines().take(2).map(_.toInt).sum)
    }

}
