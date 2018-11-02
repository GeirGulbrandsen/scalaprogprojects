package com.plusonetesting.retcalc

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int,
                         currentExpenses: Int, initialCapital: Int)

object SimulatePlanApp extends App {
  println(strMain(args))

  def strMain(args: Array[String]): String = {
    val (from +: until +: Nil) = args(0).split(",").toList
    val nbOfYearsSaving = args(1).toInt
    val nbOfYearsInRetirement = args(2).toInt

    val allReturns = Returns.fromEquityAndInflationData(
      equities = EquityData.fromResource("sp500.tsv"),
      inflations = InflationData.fromResource("cpi.tsv"))
    val (capitalAtRetirement, capitalAfterDeath) =
      RetCalc.simulatePlan(
        returns = allReturns.fromUntil(from, until),
        params = RetCalcParams (
          nbOfMonthsInRetirement = nbOfYearsInRetirement * 12,
          netIncome = args(3).toInt,
          currentExpenses = args(4).toInt,
          initialCapital = args(5).toInt),
        nbOfMonthsSaving = nbOfYearsSaving * 12)

    s"""
       |Capital after $nbOfYearsSaving years of savings:    ${capitalAtRetirement.round}
       |Capital after $nbOfYearsInRetirement years in retirement: ${capitalAfterDeath.round}
       |""".stripMargin
  }
}

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
