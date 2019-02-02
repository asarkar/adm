package org.asarkar.adm

import scala.collection.mutable.{Set => MutableSet}
import scala.util.Random

package object combinatorial {

  implicit class StringBuilderOps(sb: StringBuilder) {
    def swap(i: Int, j: Int): Unit = {
      val x = sb(i).toString
      val y = sb(j).toString
      sb
        .replace(i, i + 1, y)
        .replace(j, j + 1, x)
    }
  }

  /*
   * 7-14: Write a function to find all permutations of the letters in a particular string.
   *
   * ANSWER: We use Heap's algorithm (not related to the heap data structure). The basic idea is for each element in
   * the last position, generate permutations for the rest of n - 1 elements, and append the last element to each one
   * of those permutations. Then swap the last element (with ?), and repeat the process. The swap is where the magic is:
   * it puts a unique element in the last position for a given sequence length.
   *
   * https://en.wikipedia.org/wiki/Heap%27s_algorithm
   *
   * For a much more detailed, albeit somewhat convoluted, explanation, see
   * http://ruslanledesma.com/2016/06/17/why-does-heap-work.html
   */
  def permutations1(s: String): scala.collection.Set[String] = {
    def generate(sb: StringBuilder, n: Int): scala.collection.Set[String] = {
      if (n == 1) collection.immutable.Set(sb.toString)
      else {
        val xs = (0 until n - 1)
          .foldLeft(MutableSet.empty[String]) { (perm, i) =>
            perm ++= generate(sb, n - 1)

            if (n % 2 == 0) sb.swap(i, n - 1)
            else sb.swap(0, n - 1)

            perm
          }
        xs ++= generate(sb, n - 1)
      }
    }

    generate(new StringBuilder(s), s.length)
  }

  /*
   * This is an alternative implementation of 7-14 using Steinhaus–Johnson–Trotter algorithm.
   *
   * The algorithm is based on the idea that sequence of permutations for a given number n can be formed from the
   * sequence of permutations for n - 1 by placing the number n into each possible position in each of the shorter
   * permutations. When the permutation on n - 1 items is an even permutation (even number of inversion), then the
   * number n is placed in all possible positions in descending order, from n down to 1; when the permutation on
   * n - 1 items is odd, the number n is placed in all the possible positions in ascending order.
   *
   * Above idea is implemented by associating a direction with each element. An element may "look" to its left (-1),
   * or to its right (+1). A "mobile" element is one that's looking at another element smaller than itself.
   * The algorithm is as follows:
   *
   * 1. Find the highest mobile element; if none exists, stop.
   * 2. Swap the mobile element with the one it sees.
   * 3. Reverse the direction of any elements larger than the mobile.
   * 4. Repeat.
   *    *
   * https://en.wikipedia.org/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm
   * http://argskwargs.io/blog/python/johnson-trotter-algorithm/
   */
  def permutations2(s: String): scala.collection.Set[String] = {
    val xs = s.toCharArray
      .map((_, -1))

    // step 1
    def mobile(): (Int, Int) = {
      xs
        .zipWithIndex
        .foldLeft((-1, -1)) { case (mob, ((ch, dir), i)) =>
          val sees = i + dir
          val prevMob = xs.lift(mob._1).map(_._1).getOrElse('\u0000')
          if (ch > prevMob && xs.isDefinedAt(sees) && ch > xs(sees)._1) (i, dir)
          else mob
        }
    }

    // step 2
    def swap(i: Int, j: Int): Unit = {
      val tmp = xs(j)

      xs(j) = xs(i)
      xs(i) = tmp
    }

    // step 3
    def flipDirection(mob: Int): Unit = {
      xs.indices
        .filter(i => xs(i)._1 > xs(mob)._1)
        .foreach(i => xs(i) = (xs(i)._1, -xs(i)._2))
    }

    def loop(mob: (Int, Int), perm: MutableSet[String]): scala.collection.Set[String] = {
      if (xs.isDefinedAt(mob._1 + mob._2)) {
        swap(mob._1, mob._1 + mob._2)
        flipDirection(mob._1 + mob._2)

        perm += xs.map(_._1).mkString
        loop(mobile(), perm)
      }
      perm += xs.map(_._1).mkString
    }

    loop(mobile(), MutableSet(s))
  }

  /*
   * 7-15: Implement an efficient algorithm for listing all k-element subsets of n items.
   *
   * ANSWER: See https://github.com/asarkar/epi/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * 7-16: An anagram is a rearrangement of the letters in a given string into a sequence of dictionary words,
   * like Steven Skiena into Vainest Knees. Propose an algorithm to construct all the anagrams of a given string.
   *
   * ANSWER: How's this different from 7-14, except may be eliminating words that are not present in a dictionary?
   */

  /*
   * 7-17: Telephone keypads have letters on each numerical key. Write a program that generates all possible words
   * resulting from translating a given digit sequence (e.g., 145345) into letters.
   *
   * ANSWER: See https://github.com/asarkar/epi/src/main/scala/org/asarkar/epi/strings/package.scala
   */

  /*
   * 7-18: You start with an empty room and a group of n people waiting outside. At each step, you may either admit
   * one person into the room, or let one out. Can you arrange a sequence of 2^n steps, so that every possible
   * combination of people is achieved exactly once?
   *
   * ANSWER: See https://github.com/asarkar/epi/src/main/scala/org/asarkar/epi/recursion/package.scala
   */

  /*
   * 7-19: Use a random number generator (rng04) that generates numbers from {0, 1, 2, 3, 4} with equal probability
   * to write a random number generator that generates numbers from 0 to 7 (rng07) with equal probability.
   * What are expected number of calls to rng04 per call of rng07?
   *
   * ANSWER: For each call to rng04, the probability of getting any number between 0 and 4 is 1/5.
   * Let E be the expected number of calls before getting a number that's *not* 4.
   * It could be the first call; if not, we keep calling. That gives the following recurrence for E.
   *
   * E = 1 . 4/5 + (E + 1) . 1/5
   * => 5E = E + 5
   * => E = 5/4
   *
   * We are making 2 filtered calls to rng04; by Linearity of Expectation (expected value of a sum is the sum of the
   * expected values), the expected number of calls to rng04 per call of rng07 is 2 . 5/4 = 2.5.
   */
  private def rng04: Int = Random.nextInt(5)

  // [0, 3]
  private def rng: Int = Iterator.continually(rng04)
    .dropWhile(_ > 3)
    .take(1)
    .next()

  def rng07: Int = {
    // [0, 3]
    val i = rng
    // {0, 1}
    val j = rng % 2

    (i << 1) + j
  }
}
