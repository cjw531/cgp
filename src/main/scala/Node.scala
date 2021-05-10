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

  def deep_copy (): Node = {
    val copied_idx = this.func_idx
    val copied_num = this.number
    val copied_col_idx = this.col_where
    val copied_node = new Node(copied_idx, copied_num, copied_col_idx)
    var copied_incoming = ListBuffer[Node]()
    var copied_outgoing = ListBuffer[Node]()
    for (n <- this.incoming) {
      val n_copied_node = new Node(n.func_idx, n.number, n.col_where)
      copied_incoming += n_copied_node
    }
    for (n <- this.outgoing) {
      val n_copied_node = new Node(n.func_idx, n.number, n.col_where)
      copied_outgoing += n_copied_node
    }
    copied_node.incoming = copied_incoming
    copied_node.outgoing = copied_outgoing
    return copied_node
  }

}
