package com.ojha.algorithms.sorting

import scala.collection.immutable

/**
 * Created by alexandra on 25/11/15.
 */
object QuickSort extends App {

  val a = Array(3,6,2,4,8,5,7,1)
  quickSort2(a)
  println(a.mkString)

  /*
  (3,6,2,4,8,5,7,1)
  (3,1,2,4,8,5,7,6)
   */



  def quickSort(a: Array[Int]) = {

    def swap(i: Int, j: Int) = { val t = a(i); a(i) = a(j); a(j) = t}

    def sortAux(l: Int, r: Int): Unit = {
      val pivot = a((l + r) / 2)
      var i = l; var j = r
      while (i < j) {
        while (a(i) < pivot) i += 1
        while (a(j) > pivot) j -= 1

        if (i <= j) {
          swap(i,j)
          i += 1
          j -= 1
        }
        if (l < j) sortAux(l, j)
        if (r > i) sortAux(i, r)

      }

    }

    sortAux(0, a.length - 1)
  }

  def quickSort2(a: Array[Int]): Unit =  {

    def swap(i: Int, j: Int) { val t = a(i); a(i) = a(j); a(j) = t }

    def sortAux(l: Int, r: Int): Unit = {
      val pivot = a((l+r)/2)
      var i = l
      var j = r
      while (i < j) {
        while (a(i) < pivot) i += 1
        while (a(j) > pivot) j -= 1

        if (i <= j) {
          swap(i,j)
          i += 1
          j -= 1
        }
        if (l < j) sortAux(l, j)
        if (r > i) sortAux(i, r)
      }

    }
    sortAux(0, a.length - 1)
  }



}
