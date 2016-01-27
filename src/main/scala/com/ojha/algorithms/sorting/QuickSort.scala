package com.ojha.algorithms.sorting

/**
 * Created by alexandra on 25/11/15.
 */
object QuickSort extends App {

  val a = Array(5,2,3,1,4)//Array(3,6,2,4,8,5,7,1)
  hoare()
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

  def quickSort3(a: Array[Int]): Unit = {

    def swap(i: Int, j: Int) { val t = a(i); a(i) = a(j); a(j) = t}

    def sortAux(l: Int, r: Int): Unit = {
      val pivot = a((l + r)/2)
      var i = l
      var j = r

      while (i < j) {
        while (a(i) < pivot) i += 1
        while (a(j) > pivot) j -= 1

        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }

        if (l < j) sortAux(l, j)
        if (r > i) sortAux(i, r)

      }

    }

    sortAux(0, a.length - 1)

  }

  def quickSort4(a: Array[Int]): Unit = {

    def swap(i: Int, j: Int) = { val t = a(i); a(i) = a(j); a(j) = t }

    def sortAux(l: Int, r: Int): Unit = {
      val pivot = a((l + r) / 2)
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

  def lomutoPartitionQuicksort(a: Array[Int]): Unit = {

    def swap(i: Int, j: Int): Unit = { val t = a(i); a(i) = a(j); a(j) = t }

    def partition(l: Int, r: Int): Int = {
      val pivot = a(r)
      var i = l
      for (j <- l until r) {
        if (a(j) < pivot) { swap(i,j); i += 1 }
      }

      swap(i, r)
      i
    }

    def aux(l: Int, r: Int): Unit = {
      if (l < r) {
        val p = partition(l, r)
        aux(l, p - 1)
        aux(p, r)
      }
    }

    aux(0, a.length - 1)
  }


  def l2() = {

    def swap(i: Int, j: Int) = { val t = a(i); a(i) = a(j); a(j) = t }

    def go(l: Int, r: Int): Unit = {
      if (l < r) {
        val p = partition(l, r)
        go(l, p - 1)
        go(p + 1, r)
      }

    }

    def partition(l: Int, r: Int): Int = {
      val p = a(r)
      var i = l
      for (j <- l to r) {
        if (a(j) < p) {
          swap(i, j)
          i += 1
        }
      }
      swap(i,r)
      i
    }

    go(0, a.length - 1)

  }

  def hoare(): Unit = {

    def swap(i: Int, j: Int) = { val t = a(i); a(i) = a(j); a(j) = t }

    def aux(l: Int, r: Int): Unit = {
      val p = a((l + r)/2)
      var i = l
      var j = r

      while (i < j) {
        while (a(i) < p) i += 1
        while (a(j) > p) j -= 1

        if (i <= j) {
          swap(i,j)
          i += 1
          j -= 1
        }

        if (l < j) aux(l, j)
        if (r > i) aux(i, r)
      }
    }

    aux(0, a.length - 1)
  }













}
