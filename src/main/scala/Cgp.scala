package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Cgp (input: Int, output: Int, level: Int, row: Int, col: Int, funcs: List[List[BigDecimal] => BigDecimal]) {
  def num_input = input // number of inputs into CGP
  def num_output = output // number of outputs from CGP
  def arity = funcs.size // defines arity to be the total number of functions possible
  def lv_back = level // number of levels back to search
  def num_row = row // number of rows in inner nodes of CGP
  def num_col = col // number of columns in inner nodes of CGP
  val functions_options = funcs // possible functions to apply at node
  var node_list = ListBuffer[Node]() // list of nodes in CGP
  var NU = ListBuffer[Boolean]() // boolean array of active nodes
  var evaluation_score: BigDecimal = 0 // performance metric of CGP

  /******** Start of Setter methods for CGP object vars   *************/
  // Setter method for setting performance metric of CGP
  def set_evaluation_score(scoreToSet: BigDecimal): Unit = {
    this.evaluation_score = scoreToSet
  }

  // Setter method for adding node to the node list
  def add_to_node_list(node: Node): Unit = {
    this.node_list += node
  }

  def set_node_list(node_list: ListBuffer[Node]): Unit = {
    this.node_list = node_list
  }

  def set_NU(nu_arr: ListBuffer[Boolean]): Unit = {
    this.NU = nu_arr
  }
  /******** End of Setter methods for CGP object vars   *************/

  // Creates CGP with all nodes and random edges connecting nodes
  def create_cgp(): Unit = {
    // create and add input nodes to the node list
    for (i <- 0 to (this.num_input - 1)) {
      var input_node = new Node(-1, i, 0, 1.0)
      this.node_list += input_node
    }

    // Create and add inner nodes to the input list
    var col_start = 0 // n-th col #
    for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
      if ((i - this.num_input) % num_col == 0) {
        col_start += 1
      }
      val r = scala.util.Random
      var inner_node = new Node(random_function(), i, col_start, -2 + (r.nextDouble * (2 - -2)))
      this.node_list += inner_node // add node to the list
    }

    // randomly connect nodes
    for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
      var node = this.node_list(i)
      var col_from = node.col_where - this.lv_back // can choose to connect from level back
      if (col_from < 0) col_from = 0
      var col_to = node.col_where - 1 // choose to 1 column before
      var subset = get_node_subset(col_from, col_to, this.arity) // gets random nodes in those columns
      for (n <- subset) {
        node.add_in(n)
        this.node_list(n.number).add_out(node)
      }
    }

    // create output nodes and connect edge to outputs
    for (i <- this.num_input + (this.num_row*this.num_col) to this.num_input + (this.num_row*this.num_col) + this.num_output - 1) {
      var output_node = new Node(-1, i, this.num_col+1, 1.0)
      this.node_list += output_node
      // output node can choose any node in CGP
      var subset = get_node_subset(0, this.num_col, 1) // output node has only 1 edge
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

  /**** This part below is for decoding/solving the CGP *******/

  def decode_cgp(points: ListBuffer[BigDecimal]): ListBuffer[BigDecimal] = {
    find_active_nodes() // Compute the boolean array of active nodes
    return evaluate_cgp(points) // Go through active nodes and compute values to produce output
  }
  def find_active_nodes(): Unit = {
    // Initialize Boolean list with all falses
    this.NU.clear()
    for (i <- this.num_input to this.num_input + (this.num_row * this.num_col) - 1) {
      this.NU += false
    }
    // FOR EACH OUTPUT, CALLS RECURSIVE HELPER TO FIND ACTIVE NODES
    // Find active nodes (go from output node and track the path to input)
    for (i <- (this.num_input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output) - 1) {
      var output_node = this.node_list(i)
      new_find_path(output_node)
    }
  }

  // Recursive function to find active nodes
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

  // Compute function at each active node and find output
  /*********
  For leif: So what happens here is that we have the boolean array of active nodes,
  and we iterate through this array from left to right. We create an array of values called NP and set them all to 0
  to begin with. So, we iterate through the boolean array, and for very true node, we
  go through its "incoming" edges and put them into a list. We then apply the function at that node to the list
  and update it's value within the NP array.
    *****************/

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
          for (incoming_node <- node.incoming) { // go through incoming nodes of the node
            if (incoming_node.incoming.isEmpty) {  //  if it's an input node then just add the point
              temp_vals += (point * incoming_node.weight)
            }
            else {
              // inner node, so take value at that node and then multiply it by the weight
              temp_vals += (NP(incoming_node.number - num_input)) * incoming_node.weight
            }
          }
          // apply function to the list of val
          NP(cgp_node_idx) = this.functions_options(node.func_idx)(temp_vals.toList)
        }
      }
      // hard-coded to one output rn
      // gets value at the node of the NP array which the output node connects to
      var cgp_eval_val = NP(node_list(this.num_input + (this.num_row*this.num_col)).incoming(0).number - num_input)// output_node
      predictions += cgp_eval_val
    }
    return predictions
  }


  /*************
   *
   * This part handles the mutation
   * @param prob_mutate_edge = probability of mutation and creating the whole node again
   */
  def mutate_cgp(prob_mutate_edge: Double): Unit = {
    val r = new scala.util.Random
    for (node_idx <- this.num_input to node_list.size-1) {
      var rand_prob = r.nextDouble
      if (rand_prob <= prob_mutate_edge) {
        mutate_incoming_edges(this.node_list(node_idx)) // mutate edge
        mutate_operator(this.node_list(node_idx)) // mutate function
        mutate_node_weight(this.node_list(node_idx)) // mutate the weight
      }
    }
  }

  def mutate_incoming_edges(node: Node): Unit = {
    if (node.incoming.size == 1) {
      // Output node
      node.incoming.clear()
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

  // sets a new random operator
  def mutate_operator(node: Node): Unit = {
    var prev_func_idx = node.func_idx
    var new_func_idx = random_function()
    while (prev_func_idx == new_func_idx) new_func_idx = random_function()
    node.func_idx = new_func_idx
  }

  // sets a new weight in range of -2 -> 2
  def mutate_node_weight(node: Node): Unit = {
    val r = scala.util.Random
    node.set_weight(-2 + (r.nextDouble * (2 - -2)))
  }

}
