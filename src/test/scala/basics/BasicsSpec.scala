package basics

import basics.Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BasicsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "gcd" should "be equals to" in {
    gcd(19, 18) shouldEqual 1
    gcd(12, 16) shouldEqual 4
    gcd(27, 36) shouldEqual 9
    gcd(20, 0) shouldEqual 20
  }

  "lcm" should "be equals to" in {
    lcm(4, 5) shouldEqual 20
    lcm(18, 36) shouldEqual 36
    lcm(1, 1) shouldEqual 1
  }
}
