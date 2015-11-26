package com.ojha.datastructures.trees

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by alexandra on 19/11/15.
 */
object Balanced extends App {

  case class Node(value: Int, left: Node, right: Node)

  /*
        7
    3       9
  1   4    8

  3
 1 4

   4
 3  6
1
 */

  val node1 = Node(1, null, null)
  val node4 = Node(4, null, null)
  val node3 = Node(3, node1, node4)
  val node8 = Node(8, null, null)
  val node9 = Node(9, node8, null)
  val node7 = Node(7, node3, node9)
  val root = node7

  printTree(root)

  @tailrec
  def search(root: Node, value: Int): Option[Node] = {
    if (root == null) None
    else if (root.value == value) Some(root)
    else if (root.value > value) search(root.left,value)
    else search(root.right, value)

  }


  def printTree(tree: Node): Unit = {
    val h = height(tree)
    val numberOfNodes = math.pow(2, h - 1)

    val queue = new mutable.Queue[(Node, Int)]()

    queue.enqueue((tree, 0))
    
    var prevLevel = 0

    while (queue.nonEmpty) {

      val (node: Node, level: Int) = queue.dequeue()
      if (level > prevLevel) {
        prevLevel = level
        println()
      }

      val space = (math.pow(2, h - level) / 2).toInt
      for (i <- 0 to space) print(' ')
      print(node.value)
      if (node.left != null) queue.enqueue((node.left, level+1))
      if (node.right != null) queue.enqueue((node.right, level+1))

    }
  }

  def height(tree: Node): Int = {
    if (tree == null) 0
    else math.max(height(tree.left), height(tree.right)) + 1
  }


  def insert(tree: Node, toInsert: Node): Node = {

    if (tree == null) return toInsert





    null

  }

}
