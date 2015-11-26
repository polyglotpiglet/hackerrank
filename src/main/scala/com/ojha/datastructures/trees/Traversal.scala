package com.ojha.datastructures.trees

import scala.collection.mutable

/**
 * Created by alexandra on 17/11/15.
 */
object Traversal extends App {

  case class Node(value: Int, left: Node, right: Node)

  /*
          3
      5       2
    1   4    6


   */

  val node1 = Node(1, null, null)
  val node4 = Node(4, null, null)
  val node5 = Node(5, node1, node4)

  val node6 = Node(6, null, null)
  val node2 = Node(2, node6, null)

  val node3 = Node(3, node5, node2)

  val root = node3

  recursivePreOrder(root)
  println()
  iterativePreOrder(root)
  println()
  println("---------")
  recursiveInOrder(root)
  println()
  iterativeInOrder(root)
  println()
  println("---------")
  recursivePostOrder(root)
  println()
  iterativePostOrder(root)

  def recursivePreOrder(node: Node): Unit = {
    if (node != null) {
      print(node.value + " ")
      recursivePreOrder(node.left)
      recursivePreOrder(node.right)
    }
  }

  def iterativePreOrder(root: Node): Unit = {
    val stack = new mutable.Stack[Node]()
    var node = root
    while (stack.nonEmpty || node != null) {
      if (node != null) {
        print(node.value + " ")
        if (node.right != null) stack.push(node.right)
        node = node.left
      }
      else {
        node = stack.pop()
      }
    }

  }

  def recursiveInOrder(node: Node): Unit = {
    if (node != null) {
      recursiveInOrder(node.left)
      print(node.value + " ")
      recursiveInOrder(node.right)
    }
  }

  def iterativeInOrder(root: Node): Unit = {
    val stack = new mutable.Stack[Node]()
    var node = root
    while (stack.nonEmpty || node != null) {
      if (node != null) {
        stack.push(node)
        node = node.left
      }
      else {
        node = stack.pop()
        print(node.value + " ")
        node = node.right
      }
    }
  }

  def recursivePostOrder(node: Node): Unit = {
    if (node != null) {
      recursivePostOrder(node.left)
      recursivePostOrder(node.right)
      print(node.value + " ")
    }
  }

  def iterativePostOrder(root: Node): Unit = {
    val stack = new mutable.Stack[Node]()
    var node = root

    var prev: Node = null
    while (stack.nonEmpty || node != null) {
      if (node != null) {
        stack.push(node)
        node = node.left
      }
      else {
        node = stack.pop()
        if (node.right != null && prev != node.right) {
          stack.push(node)
          node = node.right
        }
        else {
          print(node.value + " ")
          prev = node
          node = null
        }
      }
    }


  }






}
