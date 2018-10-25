package com.plusonetesting.retcalc

object RetCalc {

  def futureCapital(interestRate: Double, nbOfMonths: Int, netIncome: Int,
                    currentExpenses: Int, initialCapital: Double): Double = {

    val monthlySavings = netIncome - currentExpenses

    //    def nextCapital(accumulated: Double, month: Int): Double =
    //      accumulated * (1 + interestRate) + monthlySavings

    (0 until nbOfMonths).foldLeft(initialCapital)((accumulated, _) =>
      accumulated * (1 + interestRate) + monthlySavings)
  }


  def simulatePlan(interestRate: Double, nbOfMonthsSaving: Int, nbOfMonthsInRetirement: Int,
                   netIncome: Int, currentExpenses: Int, initialCapital: Int): (Double, Double) = {

    val monthlySavings = netIncome - currentExpenses

    val capitalAtRetirement = futureCapital(interestRate, nbOfMonthsSaving,
      netIncome, currentExpenses, initialCapital)

    val capitalAfterDeath = futureCapital(interestRate, nbOfMonthsInRetirement,
      0, currentExpenses, capitalAtRetirement)

    (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(interestRate: Double, nbOfMonthsInRetirement: Int, netIncome: Int,
                       currentExpenses: Int, initialCapital: Int): Int = {

    def loop(months: Int): Int = {

      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
        interestRate, months, nbOfMonthsInRetirement, netIncome, currentExpenses, initialCapital)

      val returnValue =
        if (capitalAfterDeath > 0.0)
          months
        else
          loop(months + 1)

      returnValue
    }

    loop(0)
  }
}
