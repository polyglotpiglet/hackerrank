package com.ojha.datastructures.heaps

import scala.collection.mutable

/**
 * Created by alexandra on 02/12/15.
 */
object MedianUsingTwoHeaps extends App {

  val minHeap = mutable.PriorityQueue.empty(MinOrder)
  val maxHeap = mutable.PriorityQueue[Int]()

  def go() = {
    import scala.io._
    val n = StdIn.readInt()
    for (i <- 0 until n) {
      val no = StdIn.readInt()
      store(no)
      println(getMedian)
    }
  }

  go()


  var count = 0
  var first: Int = 0

  def getMedian: Double = {
    if (count == 1) return first
    (maxHeap.size, minHeap.size) match {
      case (i,j) if i == j => (maxHeap.head + minHeap.head) / 2.0
      case (i,j) if i > j => maxHeap.head
      case (i,j) if j > i => minHeap.head
    }
  }

  def store(value: Int): Unit = {
    if (count == 0) first = value
    else if (count == 1) {
      if (first < value) {
        maxHeap.enqueue(first)
        minHeap.enqueue(value)

      }
      else {
        maxHeap.enqueue(value)
        minHeap.enqueue(first)
      }
    }
    else {

      if (value < maxHeap.head) {
        maxHeap.enqueue(value)
        balance()
      }
      else {
        minHeap.enqueue(value)
        balance()
      }

    }
    count += 1
  }

  def balance() = {
    maxHeap.size - minHeap.size match {
      case i if i > 1 =>
        // means that max is bigger than min
        minHeap.enqueue(maxHeap.dequeue())

      case i if i < -1 =>
        // minheap size is bigger than maxheap size
        maxHeap.enqueue(minHeap.dequeue())

      case _ => // balanced
    }
  }

  object MinOrder extends Ordering[Int] {
    def compare(x:Int, y:Int) = y compare x
  }

}


