package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.Random
// Any type? or generic
class Cgp (input: Int, output: Int, ar: Int, level: Int, row: Int, col: Int) {
  def num_input = input
  def num_output = output
  def arity = ar
  def lv_back = level
  def num_row = row
  def num_col = col
  var node_list = ListBuffer[Node]()
  var active_node = ListBuffer[Boolean]()
  var evaluation_score = 0
  var sample_points = ListBuffer[Int]()
  var true_values = ListBuffer[Int]()

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
      var subset = get_node_subset(col_from, col_to, this.arity)
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
      var subset = get_node_subset(col_from, col_to, 1) // output node has only 1 edge
      output_node.add_in(subset(0)) // 0th element is the only element stored here
      this.node_list(subset(0).number).add_out(output_node)
    }
  }

  /* based on levels_back and arity, get the adjacent nodes */
  def get_node_subset(from: Int, to: Int, arity: Int): ListBuffer[Node] = {
    var subset = ListBuffer[Node]()
    for (node <- this.node_list) {
      if (node.col_where >= from && node.col_where <= to) {
        subset += node
      }
      // TODO: think of an optimization here
      // TODO: e.g. early exit from the loop?
    }

    var adjacent = ListBuffer[Node]()
    for (i <- 1 to arity) { // pick arity # of random nodes
      adjacent += subset.apply(Random.nextInt(subset.size))
    }
    return adjacent // return the node that has a connection
  }

  /* returns random operations for each node */
  def random_function(): String = {
    val x = List("+", "-", "*", "/")
    x.apply(Random.nextInt(x.size))
  }

  def generate_sample_of_points(): Unit ={
    var number_of_points = 100
    val r = scala.util.Random
    for (i <- 0 to number_of_points) {
      this.sample_points += r.nextInt(100)
    }
  }

  def func(x: Int): Int = {
    return x^2 + 2*x + 1
  }

  def evaluate_true_values(): Unit = {
    for (point <- this.sample_points) {
      this.true_values += func(point)
    }
  }

  // squared difference
  def compute_fitness(computed_values: ListBuffer[Int]): Unit = {
    var diff = ListBuffer[Int]()
    for (i <- 0 to computed_values.length) {
     diff += (computed_values(i) - this.true_values(i))
    }
    diff.map(x => x*x)
    this.evaluation_score = diff.sum
  }

  def evaluate_cgp(NU: ListBuffer[Boolean]): Unit = {
    // Find true values to compare
    generate_sample_of_points()
    evaluate_true_values()

    // compute values for active nodes
    var NP = ListBuffer[Int]()

    for (i <- 0 to NU.length) {
      NP += 0
    }

    var predictions = ListBuffer[Int]()

    for (point <- sample_points) {
      for (cgp_node_idx <- 0 to NU.length) {
        if (NU(cgp_node_idx) == true) { // if it's active
          var node = this.node_list(cgp_node_idx + num_input)
          // apply function to all incoming
          for (incoming_node <- node.incoming) {
            // node.operator
          }
          // NP(cgp_node_idx) +=
        }
      }
    }

    // compare to true values to get a fitness metric
    compute_fitness(predictions)
  }

  def decode_cgp(): Unit = {
    var NU = find_active_nodes()
    evaluate_cgp(NU)
  }

  def find_active_nodes(): ListBuffer[Boolean] = {
    // Initialize Boolean list with all falses
    var NU = ListBuffer[Boolean]()
    for (i <- 0 to this.num_input + (this.num_row * this.num_col) - 1) {
      NU += false
    }

    // commenting out because we really only need the true false values for the middle nodes
    // since those are the nodes which evaluate the functions

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
      this.active_node = boolean_nodes(path, NU)
    }

    return NU
  }

  /* Find node path from the input to the output node */
  def find_path(cgp_node: Node, path: ListBuffer[Node]): ListBuffer[Node] = {
    // base cases
    if (cgp_node.incoming.isEmpty) { // no incoming edges anymore
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

  /* Based on path, mark nodes into boolean values, resulted in their activate status */
  def boolean_nodes(path: ListBuffer[Node], NU: ListBuffer[Boolean]): ListBuffer[Boolean] = {
    for (p <- path) {
      NU(p.number) = true
    }
    return NU
  }

  def mutate_cgp(probability: Double, is_node: Boolean, is_edge: Boolean): Unit = {
    val r = new scala.util.Random
    for (n <- node_list) {
//      var rand = scala.util.Random.nextDouble
      var rand_prob = r.nextDouble
      if (rand_prob <= probability) {
        if (is_node && is_edge) mutate_node(n)
        else if (is_node) mutate_node(n)
        else if (is_edge) mutate_incoming_edges(n)
      }
    }
  }

  def mutate_incoming_edges(node: Node): Unit = {
    node.incoming.clear()
    var col_from = node.col_where - this.arity
    if (col_from < 0) col_from = 0
    var col_to = node.col_where - 1
    var subset = get_node_subset(col_from, col_to, this.arity)
    for (n <- subset) {
      node.add_in(n)
    }
  }

  def mutate_operator(node: Node): Unit = {
    var prev_op = node.operator
    var new_op = random_function()
    while (prev_op == new_op) new_op = random_function()
    node.operator = new_op
  }

  def mutate_node(node: Node): Unit = {
    mutate_operator(node)
    mutate_incoming_edges(node)
  }

}
