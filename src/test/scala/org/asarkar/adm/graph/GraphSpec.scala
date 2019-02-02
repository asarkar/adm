package org.asarkar.adm.graph

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class GraphSpec extends FlatSpec {
  "graph" should "return the ith node of a BST in sorted order" in {
    val builder = DefaultDirectedGraph.createBuilder[Int, DefaultEdge](classOf[DefaultEdge])
    // src/test/resources/bst.svg
    val root = 8
    val g = builder
      .addEdge(root, 3)
      .addEdge(3, 1)
      .addEdge(3, 6)
      .addEdge(6, 4)
      .addEdge(6, 7)
      .addEdge(root, 10)
      .addEdge(10, 14)
      .addEdge(14, 13)
      .build()

    inorder(g, root, 5) should contain theSameElementsInOrderAs Seq(1, 3, 4, 6, 7)
    inorder(g, root, 4) should contain theSameElementsInOrderAs Seq(1, 3, 4, 6)
    inorder(g, root, 9) should contain theSameElementsInOrderAs Seq(1, 3, 4, 6, 7, 8, 10, 13, 14)
  }
}
