package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.Random
// Any type? or generic
class Cgp (input: Int, output: Int, level: Int, row: Int, col: Int, funcs: List[List[Int] => Int]) {
  def num_input = input // number of input
  def num_output = output // number of output
  def arity = funcs.size // number of incoming edges
  def lv_back = level // max. number of column that can be connected
  def num_row = row // number of row
  def num_col = col // number of column
  val functions_options = funcs // different types of functions
  var node_list = ListBuffer[Node]() // list of all nodes for the cgp
  var NU = ListBuffer[Boolean]() // T/F active node list
  var evaluation_score: BigInt = 0 // eval score

  /* TODO: @Nilesh Do we need this following methods? */
//  def set_evaluation_score(scoreToSet: BigInt): Unit = {
//    this.evaluation_score = scoreToSet
//  }

//  def set_node_list(node_list: ListBuffer[Node]): Unit = {
//    this.node_list = node_list
//  }

//  def set_NU(nu_arr: ListBuffer[Boolean]): Unit = {
//    this.NU = nu_arr
//  }

  def create_cgp(): Unit = {
    // add input nodes to the node list
    for (i <- 0 to (this.num_input - 1)) {
      var input_node = new Node(-1, i, 0) // input node does not have function (-1)
      this.node_list += input_node
    }

    // for node grid (inner nodes, without input nodes, row by col)
    var col_start = 0 // n-th col #
    for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
      if ((i - this.num_input) % num_col == 0) { // col # increment technique
        col_start += 1
      }
      var inner_node = new Node(random_function(), i, col_start) // create node
      this.node_list += inner_node // add node to the list
    }

    // randomly connect nodes
    for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
      var node = this.node_list(i) // iterate thru each node to make a connection
      var col_from = node.col_where - this.lv_back
      if (col_from < 0) col_from = 0 // set minimum so that it starts from 0th col no matter how big the arity is
      var col_to = node.col_where - 1
      var subset = get_node_subset(col_from, col_to, this.arity) // randomly draw nodes
      for (n <- subset) { // for the selected nodes, connect them
        node.add_in(n)
        this.node_list(n.number).add_out(node)
      }
    }

    // create output node, connect edge to outputs
    for (i <- this.num_input + (this.num_row*this.num_col) to this.num_input + (this.num_row * this.num_col) + this.num_output - 1) {
      var output_node = new Node(-1, i, this.num_col + 1) // output node does not have function (-1)
      this.node_list += output_node // append the created output node to the node list
      var col_from = this.num_col - this.lv_back + 1
      if (col_from < 0) col_from = 0 // set minimum so that it starts from 0th col no matter how big the arity is
      var subset = get_node_subset(col_from, this.num_col, 1) // output node has only 1 edge
      output_node.add_in(subset(0)) // 0th element is the only element stored here
      this.node_list(subset(0).number).add_out(output_node)
    }
  }

  /* based on levels_back and arity, get the adjacent nodes */
  private def get_node_subset(from: Int, to: Int, arity: Int): ListBuffer[Node] = {
    var subset = ListBuffer[Node]()
    for (node <- this.node_list) { // get the subset of the node between the col # range
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
    return adjacent
  }

  /* returns the random operation's index */
  private def random_function(): Int = {
    val r = scala.util.Random
    r.nextInt(this.functions_options.size) // return random index
  }

  /* evaluation */
  def evaluate_cgp(points: ListBuffer[Int]): ListBuffer[Int] = {
    // compute values for active nodes
    var NP = ListBuffer[Int]()
    for (i <- 0 to this.NU.length - 1) { // for nodes
      NP += 0 // initialize into 0
    }

    var predictions = ListBuffer[Int]()
    for (point <- points) { // for all points
      for (cgp_node_idx <- 0 to this.NU.length - 1) { // iterate thru node list
        if (this.NU(cgp_node_idx) == true) { // if it's active node
          var node = this.node_list(cgp_node_idx + this.num_input) // offset so we can idx node in node_list
          var temp_vals = ListBuffer[Int]()
          for (incoming_node <- node.incoming) { // apply function to all incoming
            if (incoming_node.incoming.isEmpty) temp_vals += point // input node, just append point
            else temp_vals += NP(incoming_node.number - this.num_input) // inner node calculation
          }
          NP(cgp_node_idx) = this.functions_options(node.func_idx)(temp_vals.toList)
        }
      }
      // TODO: hard-coded to one output rn
      var cgp_eval_val = NP(node_list(this.num_input + (this.num_row*this.num_col)).incoming(0).number - num_input)// output_node
      predictions += cgp_eval_val
    }
    return predictions
  }

  /* cgp decode wrapper function */
  def decode_cgp(points: ListBuffer[Int]): ListBuffer[Int] = {
    find_active_nodes() // find T/F active nodes
    return evaluate_cgp(points) // return the evaluation score
  }

  /* T/F values to determine node's active status */
  private def find_active_nodes(): Unit = {
    // Initialize Boolean list with all falses
    this.NU.clear()
    for (i <- this.num_input to this.num_input + (this.num_row * this.num_col) - 1) {
      this.NU += false
    }

    // Find active nodes (go from output node and track the path to input)
    for (i <- (this.num_input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output) - 1) {
      var output_node = this.node_list(i) // iterate through all the output node(s)
      find_path(output_node) // each output node goes thru recursive path-finding method
    }
  }

  /* recursive function for decoding the path from output -> input */
  private def find_path(node: Node): Unit = {
    if (node.incoming.isEmpty) { // base case
      return
    }
    for (n <- node.incoming) {
      if (!n.incoming.isEmpty) {
        NU(n.number - this.num_input) = true // find active node, set it as true
        find_path(n) // recursive call
      }
    }
  }

  /* mutation wrapper function with the mutation probability rate,
   * and flag which determines which one to mutate */
  def mutate_cgp(probability: Double, is_node: Boolean, is_edge: Boolean): Unit = {
    val r = new scala.util.Random
    for (node_idx <- this.num_input to this.node_list.size - 1) { // for all node except for input
      var rand_prob = r.nextDouble
      if (rand_prob <= probability) { // coin flip on each node
        if (is_node && is_edge) mutate_node(this.node_list(node_idx))
        else if (is_node) mutate_node(this.node_list(node_idx))
        else if (is_edge) mutate_incoming_edges(this.node_list(node_idx))
      }
    }
  }

  /* mutate its edges */
  private def mutate_incoming_edges(node: Node): Unit = {
    if (node.incoming.size == 1) { // Output node
      node.incoming.clear()
      var col_from = this.num_col - this.lv_back + 1
      if (col_from < 0) col_from = 0
      var subset = get_node_subset(0, this.num_col, 1)
      for (subst_node <- subset) {
        node.add_in(subst_node)
      }
    } else { // Inner node
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

  /* mutate the operation; NOT allowing the duplicate draw of function */
  private def mutate_operator(node: Node): Unit = {
    var prev_func_idx = node.func_idx
    var new_func_idx = random_function()
    while (prev_func_idx == new_func_idx) new_func_idx = random_function()
    node.func_idx = new_func_idx
  }

  /* mutate the node (generate new node) */
  private def mutate_node(node: Node): Unit = {
    mutate_operator(node)
    mutate_incoming_edges(node)
  }

}
