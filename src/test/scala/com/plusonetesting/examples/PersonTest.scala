package com.plusonetesting.examples

import org.scalatest.{Matchers, WordSpec}

class PersonTest extends WordSpec with Matchers {

  "A person" should {

    "be instantiated with a name and an age" in {
      val john = Person(firstName = "John", lastName = "Smith", age = 42)
      john.firstName should be("John")
      john.lastName should be("Smith")
      john.age should be(42)
    }

    "get a human readable description from the companion object" in {
      val paul = Person("Paul", "Smith", 24)
      paul.description should be("Paul Smith is 24 years old.")
    }

    "get a correct description when of age 1" in {
      val paul = Person("Paul", "Smith", 1)
      paul.description should be("Paul Smith is 1 year old.")
    }

  }

}
