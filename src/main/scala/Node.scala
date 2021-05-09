package main.scala

import scala.collection.mutable.ListBuffer

class Node (idx: Int, num: Int, col_index: Int) {
  var func_idx = idx
  def number = num
  var incoming = ListBuffer[Node]()
  var outgoing = ListBuffer[Node]()
  def col_where = col_index

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
