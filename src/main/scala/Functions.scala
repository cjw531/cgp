package main.scala

// Functions options defined to be evaluated at each node
class Functions {

  // sums values in list
  def add = (vals: List[BigDecimal]) => {vals.sum}

  // divides values in list and has an edge case for divide by 0
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

  // subtracts items in list
  val subtract: List[BigDecimal] => BigDecimal = vals => {
    var result = vals(0)
    for (i <- 1 to vals.size-1) {
      result = result - vals(i)
    }
    result
  }

  // finds product of items in list
  def multiply = (vals: List[BigDecimal]) => {vals.product}

}
