package org.asarkar.adm

import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, Map => MutableMap}

package object data {

  implicit class BSTOps(g: Graph[Int, DefaultEdge]) {
    def children(v: Int): (Option[Int], Option[Int]) = {
      assert(g.outDegreeOf(v) <= 2, s"outdegree($v) = ${g.outDegreeOf(v)} > 2")
      val xs = g.outgoingEdgesOf(v)
        .asScala
        .map(g.getEdgeTarget)

      xs.size match {
        case 0 => (None, None)
        case 1 => if (xs.head < v) (xs.headOption, None) else (None, xs.headOption)
        case 2 =>
          assert(xs.head < v && xs.last > v || xs.head > v && xs.last < v, s"$xs violates BST property at node $v")
          if (xs.head < v) (xs.headOption, xs.lastOption)
          else (xs.lastOption, xs.headOption)
      }
    }
  }

  /*
   * 3-18: What method would you use to look up a word in a dictionary?
   *
   * ANSWER: Binary search.
   */

  /*
   * 3-19: Imagine you have a closet full of shirts. What can you do to organize your shirts for easy retrieval?
   *
   * ANSWER: Sort by color. If the colors are unique, and can be assigned a total ordering, we could then do a
   * binary search. However, that'll only give us a range of similarly colored shirts, at which point, it's up to
   * us what we feel like wearing that day.
   */

  /*
   * 3-20: Write a function to find the middle node of a singly-linked list.
   *
   * ANSWER: The idea is to run a slow and a fast pointer, such the slow the pointer is incremented by 1 at each step,
   * and the fast pointer by 2. When the fast pointer reaches the end, or falls off the end, the slow pointer is
   * pointing to the middle element. We consider the middle element to be at the ⌊n/2⌋ index (zero-based).
   */
  def mid(xs: LinkedList[Int]): Int = {
    Iterator.iterate((xs, xs)) { case (slow, fast) =>
      if (fast.next.isEmpty) (slow, LinkedList.empty[Int]) else (slow.next, fast.next.next)
    }
      .dropWhile(!_._2.isEmpty)
      .take(1)
      .map(_._1.datum)
      .next()
  }

  /*
   * 3-21: Write a function to compare whether two binary trees are identical. Identical trees have the same key value
   * at each position and the same structure.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/bintree/package.scala
   */

  /*
   * 3-22: Write a program to convert a binary search tree into a linked list.
   */
  def flatten(g: Graph[Int, DefaultEdge], root: Int): Seq[Int] = {
    val (l, r) = g.children(root)

    Seq(root) ++
      Option.option2Iterable(l)
        .flatMap(flatten(g, _)) ++
      Option.option2Iterable(r)
        .flatMap(flatten(g, _))
  }

  /*
   * 3-23: Implement an algorithm to reverse a linked list. Now do it without recursion.
   */
  def reverseList[T](xs: LinkedList[T]): LinkedList[T] = {
    xs.foldLeft(LinkedList.empty[T])((acc, x) => LinkedList(x, acc))
  }

  /*
   * 3-24: What is the best data structure for maintaining URLs that have been visited by a Web crawler?
   * Give an algorithm to test whether a given URL has already been visited, optimizing both space and time.
   *
   * ANSWER: Bloom Filter is a good choice. See http://llimllib.github.io/bloomfilter-tutorial/
   */

  /*
   * 3-25: You are given a search string and a magazine. You seek to generate all the characters in search string
   * by cutting them out from the magazine. Give an algorithm to efficiently determine whether the magazine contains
   * all the letters in the search string.
   *
   * ANSWER: Create a frequency hash table using the characters of the search string. For each character in the
   * magazine, check if it's present in the hash table. If yes, decrement the value by 1; if the value becomes zero,
   * remove the entry. Keep going until the hash table is empty (solution possible) or all the characters in the
   * magazine have been looked at (not possible).
   */

  /*
   * 3-26: Reverse the words in a sentence-i.e., "My name is Chris" becomes "Chris is name My". Optimize for time
   * and space.
   *
   * ANSWER: Since strings are immutable, we can't alter the input string (in C, you can). Thus, the minimum amount
   * of additional space we need is O(n). We create an empty array of that size, use regex to extract the words one
   * at a time, and copy them to the appropriate positions in the array. We also insert a blank space before each word.
   * Note that a StringBuilder can't be used, because it doesn't allow insertion at arbitrary points even if created
   * with enough capacity.
   */
  def reverseWords(s: String): String = {
    val n = s.length
    val buffer = Array.ofDim[Char](n)

    raw"(\S+)".r.findAllIn(s)
      .foldLeft(n - 1) { (i, x) =>
        val indexOfSpace = i - x.length
        if (indexOfSpace >= 0) buffer(indexOfSpace) = ' '
        Array.copy(x.toCharArray, 0, buffer, indexOfSpace + 1, x.length)

        indexOfSpace - 1
      }

    buffer.mkString
  }

  /*
   * 3-27: Determine whether a linked list contains a loop as quickly as possible without using any extra storage.
   * Also, identify the location of the loop.
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/lists/package.scala
   */

  /*
   * 3-28: You have an unordered array X of n integers. Find the array M containing n elements where Mᵢ is the product
   * of all integers in X except for Xᵢ. You may not use division. You can use extra memory.
   * (Hint: There are solutions faster than O(n²).)
   *
   * ANSWER: We can solve this in O(n) time (2 passes over the input array). Observe that for the required product
   * m[i] is the product of the left portion until i (p[i]), and the product of the right portion from i + 1 (q[i]).
   * In the first pass, we calculate the p[i] and q[i], with some clever (?) index arithmetic. By definition,
   * p(0) = q(n) = 1. In the second pass, we simply compute m[i] = p[i] * q[i].
   */

  def product(xs: IndexedSeq[Int]): IndexedSeq[Int] = {
    val n = xs.size
    val p = Array.fill[Int](n)(1)
    val q = Array.fill[Int](n)(1)

    for ((i, j) <- (1 until n).zip(n - 2 to 0 by -1)) {
      p(i) = xs(i - 1) * p(i - 1)
      q(j) = xs(j + 1) * q(j + 1)
    }

    (0 until n)
      .map(i => p(i) * q(i))
  }

  /*
   * 3-29: Give an algorithm for finding an ordered word pair (e.g., "New York") occurring with the greatest frequency
   * in a given webpage. Which data structures would you use? Optimize both time and space.
   *
   * ANSWER: We need to look at all the words, so clearly we can't do better than O(n) time. One solution is inserting
   * each ordered pair in a priority queue (O(n) space), and then returning the top item. However, if
   * the text has many repeated pairs of words, we could use a Trie to reduce the memory usage.
   * See src/test/resources/3-29.png
   */
  class TrieNode(val parent: Option[TrieNode] = None, val children: MutableMap[Char, TrieNode] = MutableMap.empty, var n: Int = 0) {
    def add(c: Char): TrieNode = {
      val child = children.getOrElseUpdate(c, new TrieNode(parent = Some(this)))
      child.n += 1
      child
    }

    def letter(node: TrieNode): Char = {
      node.parent
        .flatMap(_.children.find(_._2 eq node))
        .map(_._1)
        .getOrElse('\u0000')
    }

    override def toString: String = {
      Iterator.iterate((ListBuffer.empty[Char], Option(this))) { case (buffer, node) =>
        node
          .filter(_.parent.isDefined)
          .map(letter)
          .foreach(buffer.prepend(_))

        (buffer, node.flatMap(_.parent))
      }
        .dropWhile(_._2.isDefined)
        .take(1)
        .map(_._1.mkString)
        .next()
    }
  }

  def mostCommonPair(text: String): (String, Int) = {
    val root = new TrieNode()

    @tailrec
    def loop(s: String, mostCommon: TrieNode, count: Int, parent: TrieNode): (String, Int) = {
      s.split("\\s+", 2) match {
        case Array(head, tail@_*) if head.nonEmpty =>
          val word = head.foldLeft(parent)((tn, c) => tn.add(c))
          val (common, n, p) = if (parent eq root) (mostCommon, count, word.add(' '))
          else if (word.n > count) (word, word.n, root)
          else (mostCommon, count, root)

          loop(tail.headOption.getOrElse(""), common, n, p)
        case _ => (mostCommon.toString, count)
      }
    }

    loop(text, new TrieNode(), -1, root)
  }
}
