package com.ojha.algorithms.sorting

/**
 * Created by alexandra on 24/11/15.
 */
object MergeSort extends App {

    val a = Array(3,4,7,2,1,5,8)
    mergeSort(a).foreach(println)


  def mergeSort(a: Array[Int]): Array[Int] = {

    def meow(aa: IndexedSeq[Array[Int]]): Array[Int] = {
      val merged = for (i <- aa.indices by 2) 
        yield merge(aa(i), if (i+1 == aa.length) Array[Int]() else aa(i+1), Array[Int]())
      if (merged.head.length == a.length) merged.head
      else meow(merged)
    }

    meow(a.map { x =>  Array[Int](x) })

  }

  def merge(a1: Array[Int], a2: Array[Int], a3: Array[Int]): Array[Int] = {

    if (a1.nonEmpty && a2.nonEmpty) {
      if (a1.head < a2.head) {
        merge(a1.tail, a2, a3 :+ a1.head)
      }
      else {
        merge(a1, a2.tail, a3 :+ a2.head)
      }
    }
    else if (a1.nonEmpty) {
      a3 ++ a1
    }
    else if (a2.nonEmpty) {
      a3 ++ a2
    }
    else {
      a3
    }

  }


}
