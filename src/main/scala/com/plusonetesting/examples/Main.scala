package com.plusonetesting.examples

object Main extends App {

  val persons = List(
    Person("Akira", "Sakura", 12),
    Person("Peter", "Muller", 34),
    Person("Nick", "Tagart", 52)
  )

  val adults = Person.filterAdult(persons)

  val descriptions = adults.map(p => p.description).mkString("\n\t")

  println(s"The adults are \n\t$descriptions")

}
