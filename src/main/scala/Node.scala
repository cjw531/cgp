package main.scala

import scala.collection.mutable.ListBuffer

// Represents the nodes in the CGP including the input and output nodes
class Node (idx: Int, num: Int, col_index: Int, node_weight: Double) {
  var func_idx = idx // Index indicating which function is used
  def number = num // Node number (starts from 0 and goes to total number of nodes in CGP)
  var incoming = ListBuffer[Node]() // Nodes that have edges connecting to the current node
  var outgoing = ListBuffer[Node]() // Nodes that are connected to the current node
  def col_where = col_index // The column in which the node is a part of
  var weight: Double = node_weight // The weight associated with the node

  // Methods for altering the variables stored within the object instance
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
