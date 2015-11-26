package com.ojha.datastructures.heaps

/**
 * Created by alexandra on 26/11/15.
 */
object Heapz {

  case class Node(value: Int, left: Node, right: Node)

  def add(node: Node, heap: Node): Node = {
    if (heap == null) node
    else null
  }

}
