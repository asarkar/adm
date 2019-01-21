package org.asarkar.adm

import org.jgrapht.GraphTests
import org.jgrapht.alg.matching.HopcroftKarpMaximumCardinalityBipartiteMatching
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.collection.JavaConverters._
import scala.collection.mutable

package object dp {
  /*
   * 8-24: Given a set of coin denominators, find the minimum number of coins to make a certain amount of change.
   *
   * ANSWER: This problems exhibits Optimal Substructure. See "Coin Changing Problem.pdf" for proof. There's no
   * restriction on the number of times a denomination is used.
   *
   * Let C[p] be the minimum number of coins of denominations d₁,d₂,...,dₖ needed to make change for p cents.
   * In the optimal solution to making change for p cents, there must exist some first coin dᵢ, where dᵢ ≤ p.
   * Furthermore, the remaining coins in the optimal solution must themselves be the optimal solution to
   * making change for p - dᵢ cents, since coin changing exhibits optimal substructure Thus, if dᵢ is the
   * first coin in the optimal solution to making change for p cents, then C[p] = 1 + C[p - dᵢ];
   * i.e., one dᵢ coin plus C[p - dᵢ] coins to optimally make change for p - dᵢ cents.
   *
   * But, we don’t know which coin dᵢ is the first coin in the optimal solution to making change for p cents;
   * however, we may check all k such possibilities (subject to the constraint that dᵢ ≤ p), and take the minimum.
   * Furthermore, when making change for 0 cents, the value of the optimal solution is clearly 0 coins.
   * We thus have the following recurrence:
   * C[p] = min{ 1 + C[p - dᵢ] } when p > 0 and dᵢ ≤ p
   *      = 0 if p = 0
   */
  def minCoins(denom: Seq[Int], change: Int): Seq[Int] = {
    // num coins
    val c = Array.ofDim[Int](change + 1)
    // denomination
    val s = Array.ofDim[Int](change + 1)
    val xs = denom.sorted

    for (p <- 1 to change) {
      val y = xs
        .filter(_ <= p)
        .foldLeft((Int.MaxValue, -1)) { case ((min, coin), d) =>
          val x = 1 + c(p - d)
          if (x < min) (x, d) else (min, coin)
        }
      c(p) = y._1
      s(p) = y._2
    }

    Iterator.iterate((change, s(change))) { case (remaining, d) =>
      val x = remaining - d
      (x, s(x))
    }
      .takeWhile(_._1 > 0)
      .map(_._2)
      .toList
  }

  /*
   * Variation of the above: You are working at the cash counter at a fun-fair, and you have different types of coins
   * available to you in infinite quantities. Can you determine the number of ways of making change for a particular
   * amount using the given types of coins?
   * For example, if you have 4 types of coins, and the value of each type is given as 8, 3, 1, 2 respectively,
   * you can make change for 3 units in three ways: {1, 1, 1}, {1, 2}, and {3}.
   *
   * ANSWER: Let dp[i][j] denote the solution to the [i,j]-th subproblem; that is, dp[i][j] is the number of ways to
   * make change for the amount i, using coins j through n.
   */
  def numWays(denom: Seq[Int], change: Int): Int = {
    val dp = Array.tabulate[Int](change + 1, denom.size + 1) { (i, j) =>
      // if amount = 0, only one way to make the change, by returning no change
      if (i == 0) 1
      else 0
    }
    val xs = denom.sorted

    for (i <- 1 to change) {
      for (j <- 1 to denom.size) {
        dp(i)(j) =
          // can't use coin j
          if (xs(j - 1) > i) dp(i)(j - 1)
          // use coin j, and since there is no constraint on the denominations, solve the smaller subproblem using the
          // same denomination.
          else dp(i - xs(j - 1))(j) +
            // don't use coin j (same as above)
            dp(i)(j - 1)
      }
    }

    println(s"Change: $change, denominations: $denom")
    dp.foreach(a => println(a.mkString))

    dp(change)(denom.size)
  }

  /*
   * Another variation of the above: you're allowed to use a denomination only once. If change = 3, and denominations
   * For example, if you have 4 types of coins, and the value of each type is given as 8, 3, 1, 2 respectively,
   * you can make change for 3 units in two ways: {1, 2}, and {3}. {1, 1, 1} isn't allowed.
   */
  def numWays2(denom: Seq[Int], change: Int): Int = {
    val dp = Array.tabulate[Int](change + 1, denom.size + 1) { (i, j) =>
      if (i == 0) 1
      else 0
    }
    val xs = denom.sorted

    for (i <- 1 to change) {
      for (j <- 1 to denom.size) {
        dp(i)(j) =
          if (xs(j - 1) > i) dp(i)(j - 1)
          // same as above, except since we used coin j for this subproblem, we can't use it for any other
          else dp(i - xs(j - 1))(j - 1) + dp(i)(j - 1)
      }
    }

    println(s"Change: $change, denominations: $denom")
    dp.foreach(a => println(a.mkString))

    dp(change)(denom.size)
  }

  /*
   * 8-25: You are given an array of n numbers, each of which may be positive, negative, or zero.
   * Give an efficient algorithm to identify the index positions i and j to the maximum sum of the
   * ith through jth numbers.
   *
   * ANSWER: See https://github.com/asarkar/epi/src/main/scala/org/asarkar/epi/dp/package.scala
   */

  /*
   * 8-26: Observe that when you cut a character out of a magazine, the character on the reverse side of the page
   * is also removed. Give an algorithm to determine whether you can generate a given string by pasting cutouts
   * from a given magazine. Assume that you are given a function that will identify the character and its
   * position on the reverse side of the page for any given character position.
   *
   * ANSWER: Basically, we have a bunch of pieces of paper with a single letter on each side.
   * In the original question they came from a magazine, but they could come from anywhere.
   * We then have to determine whether we can arrange those pieces of paper to form some target word.
   * Ww can only use each piece once, but we you can choose what order we want them in and which side
   * we want to use from each piece.
   *
   * We need to be careful which pair of letters we choose when there are multiple matches. For example,
   * given {(F,O), (F,C), (O,K), (Z,Z)} and string FOO, if we choose (F,O) first, we won't get a match.
   * If, however, we choose (F,C) first, we will get a match.
   *
   * We formulate this problem as a maximum cardinality matching in a bipartite graph.
   *
   * The bipartite graph is formed by adding two edges from every character of the given string to a cutout, one edge
   * for each side. We then use JGraphT library to run Hopcroft–Karp algorithm.
   * In the end, we simply check whether the cardinality of the matching is equal to the length of the string.
   *
   * This problem can also be formulated as a Maxflow problem, although general-purpose Maxflow algorithms like Dinic's
   * are slower than special case algorithms like Hopcroft-Karp. See 8-25.jpg for a solution from Stack Overflow.
   */
  def canGenerate(s: Seq[(Char, Char)], x: String): Boolean = {
    val cutouts: Map[Char, Set[Int]] = s
      .zipWithIndex
      .flatMap(x => x._1.productIterator.map(_.asInstanceOf[Char]).map((_, x._2)))
      .groupBy(_._1)
      // conversion to set eliminates the cases where both sides of the cutout have the same letter.
      // allowing those edges would mean having parallel edges in the graph that don't matter for the solution.
      .mapValues(_.map(_._2).toSet)

    val builder = DefaultUndirectedGraph.createBuilder[Int, DefaultEdge](classOf[DefaultEdge])
    val l = x.length

    x
      .zipWithIndex
      .foreach { case (c, i) =>
        // add the vertex explicitly in case none of the cutouts have this char
        builder.addVertex(i)
        cutouts.get(c).foreach(_.foreach(j => builder.addEdge(i, j + l)))
      }

    val g = builder.buildAsUnmodifiable()

    if (g.edgeSet.size < l) false
    else {
      val y = g.vertexSet
        .asScala
        .partition(_ < l)
        .productIterator
        .map(x => x.asInstanceOf[mutable.Set[Int]].asJava)
        .toList

      assert(GraphTests.isBipartitePartition(g, y.head, y.last), "Graph must be Bipartite")

      val m = new HopcroftKarpMaximumCardinalityBipartiteMatching(g, y.head, y.last).getMatching

      m.getWeight.intValue == l
    }
  }
}
