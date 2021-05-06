package main.scala

class Functions {
  def or(x:Int, y:Int): Unit = {
    return x | y
  }

  def and(x:Int, y:Int): Unit = {
    return x & y
  }

  def xor(x:Int, y:Int): Unit = {
    return x ^ y
  }

  def divide(x:Int, y:Int): Unit = {
    if (y == 0) {
      return 0
    }
    return x / y
  }

  def add(x:Int, y:Int): Unit = {
    return x-y
  }

  def subtract (x:Int, y:Int): Unit = {
    return x -y
  }

  def multiply(x: Int, y: Int): Unit = {
    return x * y
  }

  def square(x: Int): Unit = {
    return x^2
  }

}
