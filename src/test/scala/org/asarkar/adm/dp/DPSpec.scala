package org.asarkar.adm.dp

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DPSpec extends FlatSpec {
  "minCoins" should "return the fewest denominations needed to make the given change" in {
    minCoins(Seq(5, 6, 1, 8), 11) should contain theSameElementsAs Seq(5, 6)
  }

  "numWays" should "return the number of ways to make the given change" in {
    numWays(Seq(8, 3, 1, 2), 3) shouldBe 3
    numWays(Seq(3, 1, 2), 4) shouldBe 4
  }

  "numWays2" should "return the number of ways to make the given change" in {
    numWays2(Seq(8, 3, 1, 2), 3) shouldBe 2
    numWays2(Seq(3, 1, 2), 4) shouldBe 1
    numWays2(Seq(1, 2), 4) shouldBe 0
  }

  "canGenerate" should "determine if a string can be generated from the given cutouts" in {
    canGenerate(Seq(('F', 'O'), ('F', 'C'), ('O', 'K'), ('Z', 'Z')), "FOO") shouldBe true
    canGenerate(Seq(('F', 'O'), ('F', 'C'), ('O', 'K')), "FOZ") shouldBe false
  }
}
