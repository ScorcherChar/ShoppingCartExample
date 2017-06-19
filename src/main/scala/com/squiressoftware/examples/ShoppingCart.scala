package com.squiressoftware.examples

import scala.util.{Failure, Success, Try}

object ShoppingCart {

  //Probably want to externalise this for a real application (database or something) but for a simple example this is fine
  object ShoppingItem extends Enumeration {
    type ShoppingItem = Value
    val Apple, Orange = Value

    def getItemValue(shoppingItem: ShoppingItem.ShoppingItem): BigDecimal = shoppingItem match {
      case ShoppingItem.Apple => 0.6
      case ShoppingItem.Orange => 0.25
    }
  }

  def getTotalShoppingCartCost(inputList: List[String]): Try[BigDecimal] = {
    try {
      val parsedList = inputList.map(i => ShoppingItem.withName(i))
      Success(getBasketItemsCosts(parsedList))
    } catch {
      case e: NoSuchElementException => return Failure(e)
    }
  }

  private def getBasketItemsCosts(basket: List[ShoppingItem.ShoppingItem]): BigDecimal = {
    val apples = basket.filter(x => x.equals(ShoppingItem.Apple))
    val oranges = basket.filter(x => x.equals(ShoppingItem.Orange))

    if (isEleigleForApplesOffer(basket)) {
      val remainingBasket = apples.take(apples.size - 2) ++ oranges
      return ShoppingItem.getItemValue(ShoppingItem.Apple) + getBasketItemsCosts(remainingBasket)
    }

    if (isEleigleForOrangesOffer(basket)) {
      val remainingBasket = apples ++ oranges.take(oranges.size - 3)
      return (2 * ShoppingItem.getItemValue(ShoppingItem.Orange)) + getBasketItemsCosts(remainingBasket)
    }

    return basket.map(i => ShoppingItem.getItemValue(i)).sum
  }

  private def isEleigleForApplesOffer(basket: List[ShoppingItem.ShoppingItem]): Boolean = {
    basket.count(i => i.equals(ShoppingItem.Apple)) >= 2
  }

  private def isEleigleForOrangesOffer(basket: List[ShoppingItem.ShoppingItem]): Boolean = {
    basket.count(i => i.equals(ShoppingItem.Orange)) >= 3
  }

}
