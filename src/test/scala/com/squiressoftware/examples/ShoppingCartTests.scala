package com.squiressoftware.examples

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Failure;

class ShoppingCartTests extends FlatSpec with Matchers {

  behavior of "A Shopping Cart"

  it should "return £0.6 for a single apple" in {
    val basket = List.apply("Apple")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(0.6)
  }

  it should "return £0.25 for a single orange" in {
    val basket = List.apply("Orange")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(0.25)
  }

  it should "return £0.85 for a single orange and a single orange" in {
    val basket = List.apply("Orange", "Apple")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(0.85)
  }

  it should "return failure incorrect value" in {
    val basket = List.apply("Fish")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket)
    totalCost shouldBe a[Failure[NoSuchElementException]]
  }

  it should "return correct value for mixed list with one apple offer" in {
    val basket = List.apply("Apple", "Apple", "Orange", "Apple")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(1.45)
  }

  it should "return failure for mixed list with invalid value" in {
    val basket = List.apply("Apple", "Apple", "Orange", "Apple", "Fish")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket)
    totalCost shouldBe a[Failure[NoSuchElementException]]
  }

  it should "return correct value for mixed list which has oranges offer" in {
    val basket = List.apply("Orange", "Apple", "Orange", "Orange")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(1.10)
  }

  it should "return correct value for mixed list which has oranges offer and apples offer" in {
    val basket = List.apply("Orange", "Apple", "Orange", "Orange", "Apple")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(1.10)
  }

  it should "return correct value for mixed list which has oranges offer twice and apples offer twice" in {
    val basket = List.apply("Orange", "Apple", "Orange", "Orange", "Apple", "Orange", "Apple", "Orange", "Orange", "Apple")
    val totalCost = ShoppingCart.getTotalShoppingCartCost(basket).get
    totalCost should equal(2.20)
  }

}
