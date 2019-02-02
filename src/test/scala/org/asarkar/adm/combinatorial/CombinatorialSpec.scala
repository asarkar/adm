package org.asarkar.adm.combinatorial

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class CombinatorialSpec extends FlatSpec {
  "combinatorial" should "generate all permutations of the letters" in {
    permutations1("123") should contain theSameElementsAs Vector("123", "213", "132", "312", "231", "321")
    permutations1("AABC") should have size 12

    permutations2("123") should contain theSameElementsAs Vector("123", "213", "132", "312", "231", "321")
    permutations2("AABC") should have size 12
  }
}
