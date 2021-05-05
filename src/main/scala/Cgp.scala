package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Cgp (input: Int, output: Int, ar: Int, level: Int, row: Int, col: Int) {
  def num_input = input
  def num_output = output
  def arity = ar
  def lv_back = level
  def num_row = row
  def num_col = col
  var node_list = ListBuffer[Node]()
  var evaluation_score = 0

  def set_evaluation_score(scoreToSet: Int): Unit = {
    this.evaluation_score = scoreToSet
  }

  def create_cgp(): Unit = {
    // add input nodes to the node list
    for (i <- 0 to (input - 1)) {
      var input_node = new Node("", i, 0)
      this.node_list += input_node
    }

    // for node grid (inner nodes, without input nodes)
    var col_start = 0 // n-th col #
    for (i <- num_input to (num_input - 1) + (this.num_row * this.num_col)) {
      if ((i - input) % num_col == 0) {
        col_start += 1
      }
      var inner_node = new Node(random_function(), i, col_start)
      this.node_list += inner_node // add node to the list
    }

    // randomly connect nodes
    for (i <- num_input to (num_input - 1) + (this.num_row * this.num_col)) {
      var node = this.node_list(i)
      var col_from = node.col_where - this.arity
      if (col_from < 0) col_from = 0
      var col_to = node.col_where - 1
      var subset = get_node_subset(col_from, col_to)
      for (n <- subset) {
        node.add_in(n)
        this.node_list(n.number).add_out(node)
      }
    }

    // connect edge to outputs
    for (i <- this.num_input + (this.num_row*this.num_col) to this.num_input + (this.num_row*this.num_col) + this.num_output) {
      var output_node = new Node("", i, num_col+1)
      this.node_list += output_node
      var col_from = this.num_col - this.lv_back
      if (col_from < 0) col_from = 0
      var col_to = this.num_col
      var subset = ListBuffer[Node]()
      for (node <- this.node_list) {
        if (node.col_where >= col_from && node.col_where <= col_to) {
          subset += node
        }
      }
      var connection = subset.apply(Random.nextInt(subset.size))
      output_node.add_in(connection)
      this.node_list(connection.number).add_out(output_node)
    }
  }

  def get_node_subset(from: Int, to: Int): ListBuffer[Node] = {
    var subset = ListBuffer[Node]()
    for (node <- this.node_list) {
      if (node.col_where >= from && node.col_where <= to) {
        subset += node
      }
      // TODO: think of an optimization here
      // TODO: e.g. early exit from the loop?
    }

    var adjacent = ListBuffer[Node]()
    for (i <- 1 to this.arity) { // pick arity # of random nodes
      adjacent += subset.apply(Random.nextInt(subset.size))
    }
    return adjacent // return the node that has a connection
  }

  /* returns random operations for each node */
  def random_function(): String = {
    val x = List("+", "-", "*", "/")
    x.apply(Random.nextInt(x.size))
  }

  def determine_nodes_to_process() = {
    // Initialize Boolean list with all falses
    var NU = ListBuffer[Boolean]()
    for (i <- 0 to this.num_input + (this.num_row * this.num_col) - 1) {
      NU += false
    }

    // Set output nodes to be true
    for (i <- (this.input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output) - 1) {
      NU += true
    }

    // Find active nodes (go from output node and track the path to input)
    for (i <- (this.input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output) - 1) {
      var output_node = node_list(i)
      var path = ListBuffer[Node]()
      path += output_node
      path = find_path(output_node, path) // recursion call
      
    }
  }

  def find_path(cgp_node: Node, path: ListBuffer[Node]): ListBuffer[Node] = {
    // base cases
    if (cgp_node.col_where == 0) { // currently reached the input node
      return path
    } else if (cgp_node.incoming.isEmpty) { // no incoming edges (the start point of traversal)
      return path
    } else {
      for (incoming_node <- cgp_node.incoming) {
        path += incoming_node
        return find_path(incoming_node, path)
      }
    }
    throw new IllegalStateException  // unreachable
    /*
    Exception done to resolve the return error (it becomes void/Unit type)
    Reference: https://stackoverflow.com/questions/28884227/infinite-loop-seems-to-confuse-scalas-type-system
     */
  }

  def decode_cgp(): Unit = {
    // Create boolean list of path from output to input
    determine_nodes_to_process()
  }

}
