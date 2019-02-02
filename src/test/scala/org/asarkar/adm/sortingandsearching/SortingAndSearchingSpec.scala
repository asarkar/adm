package org.asarkar.adm.sortingandsearching

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class SortingAndSearchingSpec extends FlatSpec {
  "soringandsearching" should "find the smallest snippet containing all search words" in {
    snippet(IndexedSeq(1, 4, 5), IndexedSeq(3, 9, 10), IndexedSeq(5, 6, 15)) shouldBe 3
  }
}
