package main.scala

import scala.collection.mutable.ListBuffer

class Functions {
  def or(x:Int, y:Int): Int = {
    return x | y
  }

  def and(x:Int, y:Int): Int = {
    return x & y
  }

  def xor(x:Int, y:Int): Int = {
    return x ^ y
  }

  def add = (vals: List[Int]) => {vals.sum}

  val divide: List[Int] => Int = vals => {
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

  val subtract: List[Int] => Int = vals => {
    var result = vals(0)
    for (i <- 1 to vals.size-1) {
      result = result - vals(i)
    }
    result
  }

  def multiply = (vals: List[Int]) => {vals.product}


  def square(vals: List[Int]): List[Int] = {
    return vals.map(x => x*x)
  }

}
