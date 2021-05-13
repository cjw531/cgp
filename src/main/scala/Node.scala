package main.scala

import scala.collection.mutable.ListBuffer

class Node (idx: Int, num: Int, col_index: Int, node_weight: Double) {
  var func_idx = idx
  def number = num
  var incoming = ListBuffer[Node]()
  var outgoing = ListBuffer[Node]()
  def col_where = col_index
  var weight: Double = node_weight

  def set_weight (new_weight: Double): Unit = {
    this.weight = new_weight
  }

  def add_in (new_node: Node): Unit = {
    this.incoming += new_node
  }

  def remove_in (removal: Node): Unit = {
    this.incoming -= removal
  }

  def add_out (new_node: Node): Unit = {
    this.outgoing += new_node
  }

}
