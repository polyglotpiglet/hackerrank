package com.ojha.datastructures.graphs

import scala.collection.mutable

/**
 * Created by alexandra on 24/01/16.
 */
object Dijkstra extends App {

  val graph = buildExampleGraph()
  shortestPath(Node[String]("car"), Node[String]("tat"), graph)
  shortestPath(Node[String]("cat"), Node[String]("tar"), graph)

  case class Node[T](value: T)
  class Graph[T] {
    private val edges = new mutable.HashMap[Node[T], Seq[Node[T]]]()
    private var nodes = Seq[Node[T]]()

    def addNode(node: Node[T]) = nodes = node +: nodes

    def addNodes(newNodes: Seq[Node[T]]) = nodes = newNodes ++ nodes

    def addEdge(n1: Node[T], n2: Node[T]) = {
      edges(n1) = n2 +: edges.getOrElse(n1, Seq[Node[T]]())
      edges(n2) = n1 +: edges.getOrElse(n2, Seq[Node[T]]())
    }

    def getNodesWithEdgeFrom(node: Node[T]) = edges.getOrElse(node, Seq[Node[T]]())

    def getAllNodes = nodes
  }

  def buildExampleGraph(): Graph[String] = {
    val graph = new Graph[String]()
    val cat = Node[String]("cat")
    val cab = Node[String]("cab")
    val car = Node[String]("car")
    val bar = Node[String]("bar")
    val tar = Node[String]("tar")
    val tat = Node[String]("tat")

    graph.addNodes(Seq(cat, cab, car, bar, tar, tat))

    graph.addEdge(cat, cab)
    graph.addEdge(cat, car)
    graph.addEdge(cab, car)
    graph.addEdge(car, bar)
    graph.addEdge(bar, tar)
    graph.addEdge(tar, tat)

    graph
  }

  def shortestPath[T](start: Node[T], end: Node[T], graph: Graph[T]): Unit = {
    val allNodes = graph.getAllNodes

    // initially all distances are infinity, except start node where distance = 0
    val distances: mutable.Map[Node[T], (Seq[Node[T]], Int)]
      = collection.mutable.Map(allNodes.map {
        case n: Node[T] if n == start => n -> (Seq(n), 0)
        case n: Node[T] => n -> (Seq[Node[T]](), Integer.MAX_VALUE)
    }: _*)

    var unvisited = allNodes

    def aux(node: Node[T]): (Seq[Node[T]],Int) = {
      if (!unvisited.contains(end)) distances(end)
      else {
        unvisited = unvisited.filterNot(_ == node) // mark current node as visited

        val linkedNodes = graph.getNodesWithEdgeFrom(node).filter(unvisited.contains(_))

        linkedNodes.foreach { n =>
          distances(n) = distances(n) match {
            case (s, Integer.MAX_VALUE) => ( distances(node)._1 :+ n, distances(node)._2 + 1)
            case (s, i) => if (i < distances(node)._2 + 1) (s,i) else ( distances(node)._1 :+ n, distances(node)._2 + 1)
          }
        }

        if (!unvisited.contains(end)) distances(end)
        else {
          val nextNode = unvisited.min(Ordering.by[Node[T], Int](distances(_)._2))
          aux(nextNode)
        }
      }
    }

    println(aux(start))
  }


}


