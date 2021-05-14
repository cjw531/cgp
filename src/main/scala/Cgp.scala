package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.Random
// Any type? or generic
class Cgp (input: Int, output: Int, level: Int, row: Int, col: Int, funcs: List[List[BigDecimal] => BigDecimal]) {
  def num_input = input
  def num_output = output
  def arity = funcs.size
  def lv_back = level
  def num_row = row
  def num_col = col
  val functions_options = funcs
  var node_list = ListBuffer[Node]()
  var NU = ListBuffer[Boolean]()
  var evaluation_score: BigDecimal = 0

  def set_evaluation_score(scoreToSet: BigDecimal): Unit = {
    this.evaluation_score = scoreToSet
  }

  def add_to_node_list(node: Node): Unit = {
    this.node_list += node
  }

  def set_node_list(node_list: ListBuffer[Node]): Unit = {
    this.node_list = node_list
  }

  def set_NU(nu_arr: ListBuffer[Boolean]): Unit = {
    this.NU = nu_arr
  }

  def create_cgp(): Unit = {
    // add input nodes to the node list
    for (i <- 0 to (this.num_input - 1)) {
      var input_node = new Node(-1, i, 0, 1.0)
      this.node_list += input_node
    }

    // for node grid (inner nodes, without input nodes)
    var col_start = 0 // n-th col #
    for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
      if ((i - this.num_input) % num_col == 0) {
        col_start += 1
      }
      val r = scala.util.Random
      var inner_node = new Node(random_function(), i, col_start, r.nextDouble * 2)
      this.node_list += inner_node // add node to the list
    }

    // randomly connect nodes
    for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
      var node = this.node_list(i)
      var col_from = node.col_where - this.lv_back
      if (col_from < 0) col_from = 0
      var col_to = node.col_where - 1
      var subset = get_node_subset(col_from, col_to, this.arity)
      for (n <- subset) {
        node.add_in(n)
        this.node_list(n.number).add_out(node)
      }
    }

    // connect edge to outputs
    for (i <- this.num_input + (this.num_row*this.num_col) to this.num_input + (this.num_row*this.num_col) + this.num_output - 1) {
      var output_node = new Node(-1, i, this.num_col+1, 1.0)
      this.node_list += output_node
      var col_from = this.num_col - this.lv_back + 1
      if (col_from < 0) col_from = 0
      var subset = get_node_subset(col_from, this.num_col, 1) // output node has only 1 edge
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
  def random_function(): Int = {
    val r = scala.util.Random
    r.nextInt(this.functions_options.size)
  }

  def evaluate_cgp(points: ListBuffer[BigDecimal]): ListBuffer[BigDecimal] = {
    // if no active nodes, then output maps to input and just return the point then
    if (!this.NU.contains(true)) {
      var predictions = ListBuffer[BigDecimal]()
      for (point <- points) {
        predictions += point
      }
      return predictions
    }

    // compute values for active nodes
    var NP = ListBuffer[BigDecimal]()

    // Set all to 0 to begin with
    for (i <- 0 to this.NU.length-1) {
      NP += 0.0
    }

    var predictions = ListBuffer[BigDecimal]()

    for (point <- points) {
      for (cgp_node_idx <- 0 to this.NU.length-1) {
        if (NU(cgp_node_idx) == true) { // if it's active
          var node = this.node_list(cgp_node_idx + this.num_input) // offset so we can idx node in node_list
          // apply function to all incoming
          var temp_vals = ListBuffer[BigDecimal]()
          for (incoming_node <- node.incoming) {
            if (incoming_node.incoming.isEmpty) {
              temp_vals += point
            }
            else {
              temp_vals += (NP(incoming_node.number - num_input)) * incoming_node.weight
            }
          }
          NP(cgp_node_idx) = this.functions_options(node.func_idx)(temp_vals.toList)
        }
      }
      // hard-coded to one output rn
      var cgp_eval_val = NP(node_list(this.num_input + (this.num_row*this.num_col)).incoming(0).number - num_input)// output_node
      predictions += cgp_eval_val
    }
    return predictions
  }

  def decode_cgp(points: ListBuffer[BigDecimal]): ListBuffer[BigDecimal] = {
    find_active_nodes()
//    println(this.NU)
    return evaluate_cgp(points)
  }

  def find_active_nodes(): Unit = {
    // Initialize Boolean list with all falses
    this.NU.clear()
    for (i <- this.num_input to this.num_input + (this.num_row * this.num_col) - 1) {
      this.NU += false
    }

    // Find active nodes (go from output node and track the path to input)
    for (i <- (this.num_input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output) - 1) {
      var output_node = this.node_list(i)
      new_find_path(output_node)
    }
  }

  def new_find_path(node: Node): Unit = {
    if (node.incoming.isEmpty) {
      return
    }
    for (n <- node.incoming) {
      if (n.incoming.isEmpty == false) {
        NU(n.number - this.num_input) = true
        new_find_path(n)
      }
    }
  }

//  def mutate_cgp(probability: Double, is_node: Boolean, is_edge: Boolean): Unit = {
//    val r = new scala.util.Random
//    for (node_idx <- this.num_input to node_list.size-1) {
//      var rand_prob = r.nextDouble
//      if (rand_prob <= probability) {
//        if (is_node && is_edge) mutate_node(this.node_list(node_idx))
//        else if (is_node) mutate_node(this.node_list(node_idx))
//        else if (is_edge) mutate_incoming_edges(this.node_list(node_idx))
//      }
//    }
//  }

  def mutate_cgp(prob_mutate_edge: Double, prob_mutate_op: Double, prob_mutate_weight: Double): Unit = {
    val r = new scala.util.Random
    for (node_idx <- this.num_input to node_list.size-1) {
      var rand_prob = r.nextDouble
      if (rand_prob <= prob_mutate_edge) {
        mutate_incoming_edges(this.node_list(node_idx))
        mutate_operator(this.node_list(node_idx))
        mutate_node_weight(this.node_list(node_idx))
      }
//      rand_prob = r.nextDouble
//      if (rand_prob <= prob_mutate_op) {
//        mutate_operator(this.node_list(node_idx))
//      }
//      rand_prob = r.nextDouble
//      if (rand_prob <= prob_mutate_weight) {
//        mutate_operator(this.node_list(node_idx))
//      }
    }
  }

  def mutate_incoming_edges(node: Node): Unit = {
    if (node.incoming.size == 1) {
      // Output node
      node.incoming.clear()
      var col_from = this.num_col - this.lv_back + 1
      if (col_from < 0) col_from = 0
      var col_to = this.num_col
      var subset = get_node_subset(0, this.num_col, 1)
      for (subst_node <- subset) {
        node.add_in(subst_node)
      }
    }
    else {
      // Inner node
      node.incoming.clear()
      var col_from = node.col_where - this.lv_back
      if (col_from < 0) col_from = 0
      var col_to = node.col_where - 1
      var subset = get_node_subset(col_from, col_to, this.arity)
      for (subst_node <- subset) {
        node.add_in(subst_node)
      }
    }
  }

  def mutate_operator(node: Node): Unit = {
    var prev_func_idx = node.func_idx
    var new_func_idx = random_function()
    while (prev_func_idx == new_func_idx) new_func_idx = random_function()
    node.func_idx = new_func_idx
  }

  def mutate_node(node: Node): Unit = {
    mutate_operator(node)
    mutate_incoming_edges(node)
  }

  def mutate_node_weight(node: Node): Unit = {
    val r = scala.util.Random
    node.set_weight(r.nextDouble * 2)
  }

}
