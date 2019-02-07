package org.asarkar.adm

package object sortingandsearching {
  /*
   * 4-40: If you are given a million integers to sort, what algorithm would you use to sort them?
   * How much time and memory would that consume?
   *
   * ANSWER: We could use a non-comparison based sort like Radix sort. Radix sort typically uses Counting sort as a
   * subroutine, which runs in O(n), and requires O(n + k) space (k = range of input).
   * Radix sort itself runs in O(d(n + k)) time, where d is the base of the numbers (in this case 10).
   * Since it uses Counting sort as a subroutine, the space complexity is the same as Counting sort, which is O(n + k).
   *
   * See CLRS chapter 8 for further details on both of these sorts.
   *
   * On a JVM, an integer is of size 4 bytes. Thus, 1 million (2^6) integers take up 4 million bytes, which is 4 MB.
   * The size of an array on the JVM is bounded by the value of max int, which is 2^32 - 1. Since most modern machines
   * have GBs of RAM available, we could load the 1 million ints in memory and sort by Radix sort. If the constant
   * factors hidden in the O(n + k) for k ≅ n is not acceptable, we could run an in place comparison sort like
   * Quick Sort, trading speed for space, although not by much (log₂ 1000000 ≅ 20).
   */

  /*
   * 4-41: Describe advantages and disadvantages of the most popular sorting algorithms.
   *
   * ANSWER: This is why Wikipedia exists: https://en.wikipedia.org/wiki/Sorting_algorithm
   */

  /*
   * 4-42: Implement an algorithm that takes an input array and returns only the unique elements in it.
   *
   * ANSWER: Do we know anything about the distribution of the elements? Is the input array sorted? Is there a
   * restriction on space? Answers to these questions will determine the strategy we choose.
   *
   * If space is not a premium, we could simply create a frequency table, and make a second pass to output the elements
   * that have value > 1. Space O(n), time O(n).
   *
   * If space is tight, we could sort the array in place using Quick sort, and then make one pass as follows:
   * See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/arrays/package.scala
   */

  /*
   * 4-43: You have a computer with only 2Mb of main memory. How do you use it to sort a large file of 500 Mb that is
   * on disk?
   *
   * ANSWER: Assuming a typical block size of 4 KB, 2 MB of main memory yields 512 blocks. If we use replacement
   * selection, followed by a 512-way merge sort, we can sort 4 MB (twice the size of RAM) x 512 in one pass. That's
   * about 2 GB.
   *
   * See https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/ExternalSort.html#replacement-selection
   */

  /*
   * 4-44: Design a stack that supports push, pop, and retrieving the minimum element in constant time. Can you do this?
   *
   * ANSWER: See https://github.com/asarkar/epi/tree/master/src/main/scala/org/asarkar/epi/stacks/package.scala
   */

  /*
   * 4-45: Given a search string of three words, find the smallest snippet of the document that contains all three of
   * the search words-i.e. , the snippet with smallest number of words in it. You are given the index positions where
   * these words in occur search strings, such as word1: (1, 4, 5), word2: (4, 9, 10), and word3: (5, 6, 15).
   * Each of the lists are in sorted order, as above.
   *
   * ANSWER: Let's rephrase the question. Given three sets of integers (call them A, B, and C), find the minimum
   * contiguous range that contains one element from each set.
   * There is some confusion about what the three sets are. The 2nd edition of the book states them as {1, 4, 5},
   * {4, 9, 10}, and {5, 6, 15}. However, another version is {1, 4, 5}, {3, 9, 10}, and {2, 6, 15}.
   * If one word is not a suffix/prefix of another, version 1 isn't possible, so let's go with the second one.
   *
   * Simply inspecting 4-45.png visually, we can see that there are two answers to this question: [1,3] and [2,4],
   * both of size 3 (three points in each range).
   *
   * The algorithm is given below. The idea is to start with the smallest valid range, and incrementally try to shrink
   * it by moving the left boundary inwards. At each step, one of the three indices is incremented, so the algorithm is
   * guaranteed to eventually terminate. In the worst case, i, j, and k are incremented in that order, and the
   * algorithm runs in O(n^2) (9 in this case) time. For the given example, it terminates after 5 iterations.
   */

  def snippet(a: IndexedSeq[Int], b: IndexedSeq[Int], c: IndexedSeq[Int]): Int = {
    Iterator.iterate((0, 0, 0, Int.MaxValue)) { case (i, j, k, min) =>
      if (a.isDefinedAt(i) && b.isDefinedAt(j) && c.isDefinedAt(k)) {
        val x = a(i)
        val y = b(j)
        val z = c(k)

        val m1 = Seq(x, y, z).min
        val m2 = Seq(x, y, z).max
        val m3 = math.min(min, m2 - m1 + 1)

        if (m1 == x) (i + 1, j, k, m3)
        else if (m1 == y) (i, j + 1, k, m3)
        else (i, j, k + 1, m3)
      } else (-1, -1, -1, min)
    }
      .dropWhile(_._1 >= 0)
      .take(1)
      .map(_._4)
      .next()
  }

  /*
   * 4-46: You are given 12 coins. One of them is heavier or lighter than the rest. Identify this coin in just
   * three weighings.
   *
   * ANSWER: http://www.algorist.com/algowiki/index.php/TADM2E_4.46
   * https://en.wikipedia.org/wiki/Balance_puzzle
   */
}
