package com.plusonetesting.examples

case class Person(firstName: String, lastName: String, age: Int) {
  def description = s"$firstName $lastName is $age ${if (age > 1) "years" else "year"} old."

}
