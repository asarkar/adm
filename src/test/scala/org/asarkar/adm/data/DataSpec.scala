package org.asarkar.adm.data

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DataSpec extends FlatSpec {
  "data" should "find the middle node of a singly-linked list" in {
    mid(LinkedList(1, 2, 3)) shouldBe 2
    mid(LinkedList(1, 2, 3, 4)) shouldBe 3
  }

  it should "convert a binary search tree into a linked list" in {
    val builder = DefaultDirectedGraph.createBuilder[Int, DefaultEdge](classOf[DefaultEdge])
    // src/test/resources/bst.png
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

    flatten(g, root) should contain theSameElementsInOrderAs Seq(8, 3, 1, 6, 4, 7, 10, 14, 13)
  }

  it should "reverse a linked list" in {
    reverseList(LinkedList(1, 2, 3, 4)).toSeq should contain theSameElementsInOrderAs Seq(4, 3, 2, 1)
  }

  it should "compute the product of all other integers in given array" in {
    product(IndexedSeq(1, 2, 3, 4, 5)) should contain theSameElementsInOrderAs Seq(120, 60, 40, 30, 24)
  }

  it should "reverse the words in a sentence" in {
    reverseWords("My name is Chris") shouldBe "Chris is name My"
  }

  it should "find the most common ordered word pair" in {
    val (pair, n) = mostCommonPair("A new puppy in New York is happy with it's New York life")
    pair shouldBe "New York"
    n shouldBe 2
  }
}
