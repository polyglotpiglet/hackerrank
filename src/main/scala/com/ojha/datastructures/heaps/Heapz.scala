package com.ojha.datastructures.heaps

/**
 * Created by alexandra on 26/11/15.
 */
object Heapz extends App {

  val a = Array.fill[Int](20)(0)
  val ordering: (Int, Int) => Boolean = { (a,b) => a > b } // max heap
  var currentIndex = 0


  insert(20)
  insert(10)
  insert(5)
  insert(15)
  insert(15)
  insert(15)
  println()

  /*
   * Always insert at the end of the array and then call heapify
   */
  def insert(v: Int): Unit = {
    a(currentIndex) = v
    heapify(currentIndex)
    currentIndex += 1
  }

  def heapify(index: Int): Unit = {
    if (index != 0) {
      val newVal = a(index)
      val pIndex = parentIndex(index).get
      val parent = a(pIndex) // horrible but safe because nonzero
      if (!ordering(parent, newVal)) {
        swap(index, pIndex)
        heapify(pIndex)
      }
    }
  }

  /*
   * Heapify method is going to have to swap stuff
   */
  def swap(i: Int, j: Int): Unit = { val t = a(i); a(i) = a(j); a(j) = t }

  /*
   * Because we are storing in an array we need to know what the parent index is for a given index
   *
   * (0) (1,2) (3,4,5,6) (7,8,9,10,11,12,13,14)
   *
   * Eg parentIndex of 1 is 0
   * Eg parentIndex of 14 is 6 (it is the right subchild of 6)
   *
   */
  def parentIndex(i: Int): Option[Int] = {
    if (i == 0) None
    else {
      def twoPower(v: Int, pow: Int): Int = {
        if (v == 0) pow
        else twoPower(v >> 1, pow + 1)
      }
      val level = twoPower(i + 1, 0) - 1
      val parentLevel = level - 1
      val levelStart = math.pow(2, level) - 1
      val lengthAlongLevel = i - levelStart
      val lengthAlongParentLevel = lengthAlongLevel / 2
      val parentIndex = (math.pow(2, parentLevel) - 1) + lengthAlongParentLevel
      Some(parentIndex.toInt)
    }

  }




}
