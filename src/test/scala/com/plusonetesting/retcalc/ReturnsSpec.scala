package com.plusonetesting.retcalc

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{Matchers, WordSpec}

class ReturnsSpec extends WordSpec with Matchers{

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.0001)

  "ReturnsMonthlyRate" should {

    "return a fixed rate for FixedReturns" in {
      Returns.monthlyRate(FixedReturns(0.04), 0) should
      === (0.04 / 12)
      Returns.monthlyRate(FixedReturns(0.02), 10) should
      === (0.02 / 12)
    }

    val variableReturn = VariableReturns(
      Vector(
        VariableReturn("2000.01", 0.1),
        VariableReturn("2000.02", 0.2)
      )
    )
    "return the nth rate for VariableReturns" in {
      Returns.monthlyRate(variableReturn, 0) should === (0.1)
      Returns.monthlyRate(variableReturn, 1) should === (0.2)
    }

    "roll over from the first rate if n > length of VariableReturns" in {
      Returns.monthlyRate(variableReturn, 2) should === (0.1)
      Returns.monthlyRate(variableReturn, 3) should === (0.2)
      Returns.monthlyRate(variableReturn, 4) should === (0.1)
    }

    "return the n+offset th rate for OffsetReturns" in {
      val returns = OffsetReturns(variableReturn,1)
      Returns.monthlyRate(returns, 0) should === (0.2)
    }

  }

}
