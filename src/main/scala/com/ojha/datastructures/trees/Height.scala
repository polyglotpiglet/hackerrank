package com.ojha.datastructures.trees

import com.ojha.datastructures.trees.Traversal.Node

import scala.collection._

/**
 * Created by alexandra on 19/11/15.
 */
object Height extends App {


  /*
          3
      5       2
    1   4    6
           7  8


   */

  val node1 = Node(1, null, null)
  val node4 = Node(4, null, null)
  val node5 = Node(5, node1, node4)

//  val node7 = Node(7, null, null)
//  val node8 = Node(8, null, null)

  val node6 = Node(6, null, null)
  val node2 = Node(2, node6, null)

  val node3 = Node(3, node5, node2)

  val root = node3

  height(node1)

  def height(root: Node) = {
    val stack = new mutable.Stack[(Node, Int)]()

    var node = root
    var currentHeight = 0
    var max = currentHeight
    while (stack.nonEmpty || node != null) {

      if (node != null) {
        currentHeight += 1
        if (currentHeight > max) max = currentHeight

        if (node.right != null) {
          stack.push((node.right, currentHeight))
        }
        node = node.left
      }
      else {
        val popped = stack.pop()
        node = popped._1
        currentHeight = popped._2
      }

    }

    println(max)

  }

}
