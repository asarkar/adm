package org.asarkar.adm

import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge

package object graph {

  /*
   * 5-31: Which data structures are used in depth-first and breath-first search?
   *
   * ANSWER: Stack and Queue, respectively. For recursive BFS, there's an implicit function stack as well.
   */

  /*
   * 5-32: Write a function to traverse binary search tree and return the ith node in sorted order.
   *
   * ANSWER: Do an inorder traversal keeping track of the number of nodes visited, and stop when the ith node
   * is visited.
   */

  import org.asarkar.adm.data.BSTOps

  /*
   * 5-32: Write a function to traverse binary search tree and return the ith node in sorted order.
   */
  def inorder(g: Graph[Int, DefaultEdge], root: Int, i: Int): Seq[Int] = {
    def visit(v: Int, numVisited: Int): Seq[Int] = {
      val (l, r) = g.children(v)

      val leftSubtree = Option.option2Iterable(l.map(visit(_, numVisited))).flatten.toSeq

      leftSubtree ++
        (if (leftSubtree.size + numVisited == i) Seq.empty[Int]
        else if (leftSubtree.size + numVisited == i - 1) Seq(v)
        else
          Seq(v) ++
            Option.option2Iterable(r.map(visit(_, numVisited + 1 + leftSubtree.size))).flatten.toSeq
          )
    }

    visit(root, 0)
  }
}
