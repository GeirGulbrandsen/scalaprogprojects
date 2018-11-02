package com.plusonetesting.retcalc

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int,
                         currentExpenses: Int, initialCapital: Int)
object RetCalc {

  def futureCapital(returns: Returns, nbOfMonths: Int, netIncome: Int,
                    currentExpenses: Int, initialCapital: Double): Double = {

    val monthlySavings = netIncome - currentExpenses

    //    def nextCapital(accumulated: Double, month: Int): Double =
    //      accumulated * (1 + interestRate) + monthlySavings

    (0 until nbOfMonths).foldLeft(initialCapital) {
      case (accumulated, month) =>
        accumulated * (1 + Returns.monthlyRate(returns, month)) + monthlySavings
    }
  }


  def simulatePlan(returns: Returns, params: RetCalcParams, nbOfMonthsSaving: Int,
                   ): (Double, Double) = {
    import params._

    val capitalAtRetirement = futureCapital(returns, nbOfMonthsSaving,
      netIncome, currentExpenses, initialCapital)

    val capitalAfterDeath = futureCapital(
      OffsetReturns(returns, nbOfMonthsSaving),
      nbOfMonthsInRetirement,
      0, currentExpenses, capitalAtRetirement)

    (capitalAtRetirement, capitalAfterDeath)
  }


  def nbOfMonthsSaving(returns: Returns, params: RetCalcParams): Int = {

    @tailrec
    def loop(months: Int): Int = {
      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(returns, params, months)

      if (capitalAfterDeath > 0.0)
        months
      else
        loop(months + 1)
    }

    loop(0)
  }

  def nbOfMonthsSavingV2(returns: Returns, params: RetCalcParams): Int = {
    import params._

    @tailrec
    def loop(months: Int): Int = {
      val (_, capitalAfterDeath) = simulatePlan(returns, params, months)

      if (capitalAfterDeath > 0.0)
        months
      else
        loop(0)
    }

    if (netIncome > currentExpenses)
      loop(0)
    else
      Int.MaxValue
  }
}
