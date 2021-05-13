package main.scala

import scala.collection.mutable.ListBuffer

class Functions {
  def add = (vals: List[BigDecimal]) => {vals.sum}

  val divide: List[BigDecimal] => BigDecimal = vals => {
    var result = vals(0)
    for (i <- 1 to vals.size-1) {
      if (vals(i) == 0) {
        result = 0
      }
      else {
        result = result / vals(i)
      }
    }
    result.toInt
  }

  val subtract: List[BigDecimal] => BigDecimal = vals => {
    var result = vals(0)
    for (i <- 1 to vals.size-1) {
      result = result - vals(i)
    }
    result
  }

  def multiply = (vals: List[BigDecimal]) => {vals.product}


  def square(vals: List[BigDecimal]): Unit = {
    return vals.map(x => x*x)
  }

}
