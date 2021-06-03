import org.nlogo.{agent, api, core, nvm}
import core.Syntax
import api.ScalaConversions._
import api.{Argument, Context, ExtensionManager, ScalaConversions}
import org.nlogo.core.AgentKind

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.double2bigDecimal
import scala.util.Random

class cgp extends api.DefaultClassManager {
  /* Node Class */
  class Node(idx: Int, num: Int, col_index: Int) {
    var func_idx = idx
    def number = num
    var incoming = ListBuffer[Int]()
    var outgoing = ListBuffer[Node]()
    val col_where = col_index
    def add_in(new_node: Node): Unit = {
      this.incoming += new_node.number
    }
    def remove_in(removal: Node): Unit = {
      this.incoming -= removal.number
    }
    def add_out(new_node: Node): Unit = {
      this.outgoing += new_node
    }
  }

  /* Cgp Class */
  class Cgp(input: Int, output: Int, node_arity: Int, level: Int, row: Int, col: Int, funcs: List[List[Double] => Double]) {
    val num_input = input
    val num_output = output
    val arity = node_arity
    val lv_back = level
    val num_row = row
    val num_col = col

    val functions_options = funcs
    var node_list = ListBuffer[Node]()
    var NU = ListBuffer[Boolean]()
    var evaluation_score: Double = 0

    def set_evaluation_score(scoreToSet: Double): Unit = {
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
        var input_node = new Node(-1, i, 0)
        this.node_list += input_node
      }

      // for node grid (inner nodes, without input nodes)
      var curr_num_nodes_in_row = -1
      var col_start = 1 // n-th col #
      for (i <- this.num_input to ((this.num_input - 1) + (this.num_row * this.num_col))) {
        curr_num_nodes_in_row += 1
        if (curr_num_nodes_in_row == this.num_row) {
          col_start += 1
          curr_num_nodes_in_row = 0
        }
        //        if ((i - this.num_input) % this.num_col == 0) {
        //          col_start += 1
        //        }
        var inner_node = new Node(random_function(), i, col_start)
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
      for (i <- this.num_input + (this.num_row * this.num_col) to this.num_input + (this.num_row * this.num_col) + this.num_output - 1) {
        var output_node = new Node(-1, i, this.num_col + 1)
        this.node_list += output_node
        var col_from = 0 // can reach back to any level
        var subset = get_node_subset(col_from, this.num_col, 1) // output node has only 1 edge
        output_node.add_in(subset(0)) // 0th element is the only element stored here
        this.node_list(subset(0).number).add_out(output_node)
      }


//      println("Creating CGP")
//      for (le_node <- this.node_list) {
//        print("Node ")
//        print(le_node.number)
//        print(" is in column ")
//        println(le_node.col_where)
//      }

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

    //    // Step 1
    //    var ToEvaluate = ListBuffer[Boolean]() // length = MaxGraph.Length
    //    for (i <- 0 to (this.num_row * this.num_col)) {
    //      ToEvaluate += false
    //    }
    //    var NodeOutput = ListBuffer[Double]() // length = MaxGraph.Length + Number of program inputs
    //    for (i <- 0 to this.num_input + (this.num_row * this.num_col)) {
    //      NodeOutput += 0.0
    //    }
    //    var NodesUsed = ListBuffer[Int]() // length = M ?
    //
    //    // Step 2
    //    //// Identify initial nodes that need to be evaluated
    //    for (i <- this.num_input + (this.num_row * this.num_col) to this.num_input + (this.num_row * this.num_col) + this.num_output - 1) {
    //      ToEvaluate(this.node_list(i).incoming(0).number - this.num_input) = true
    //    }
    //
    //    var p = ToEvaluate.length - 1
    //    do {
    //      if (ToEvaluate(p)) {
    //        // set incoming edges to be true
    //        var x = this.node_list(this.num_input + p).incoming(0)
    //        var y = this.node_list(this.num_input + p).incoming(1)
    //        if (!x.incoming.isEmpty) {
    //          ToEvaluate(x.number - this.num_input) = true
    //        }
    //        if (!y.incoming.isEmpty) {
    //          ToEvaluate(y.number - this.num_input) = true
    //        }
    //        NodesUsed += p
    //      }
    //      p = p - 1
    //    } while (p >= 0)

    // Step 3
    //    //// load input data values
    //    var inputs_list = List(5.0, 2.0)
    //    p = 0
    //    do {
    //      NodeOutput += inputs_list(p)
    //    } while (p < this.num_input)

    // Step 4
    //// Execute graph



    /////////////////////////
    def solve_cgp(inputs_list: List[Double]): ListBuffer[Double] = {
      //      println("Solving CGP")
      //      for (le_node <- this.n ode_list) {
      //        print("Node ")
      //        print(le_node.number)
      //        print(" is in column ")
      //        println(le_node.col_where)
      //      }
      var predictions = ListBuffer[Double]()

      // if no active nodes, then output maps to input and just return the point then
      // output becomes their input b/c their active node array is all false
      if (!this.NU.contains(true)) {
        for (output_node <- (this.num_input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output - 1)) {
          for (incoming_node <- this.node_list(output_node).incoming) {
            predictions += inputs_list(incoming_node)
          }
        }
        return predictions
      }

      var NP = ListBuffer[Double]() // values arr
      // Set all to 0 to begin with
      for (i <- 0 to this.NU.length - 1) {
        NP += 0
      }

      //      println("Set all NP to 0 to begin with")

      //      for (le_node <- this.node_list) {
      //        print("Node ")
      //        print(le_node.number)
      //        print(" is in column ")
      //        println(le_node.col_where)
      //      }

      for (cgp_node_idx <- 0 to this.NU.length - 1) {
        if (NU(cgp_node_idx) == true) {
          var node = this.node_list(cgp_node_idx + this.num_input) // offset so we can idx node in node_list
          var temp_vals = ListBuffer[Double]()
          for (incoming_node <- node.incoming) {
            if (this.node_list(incoming_node).incoming.isEmpty) { // use input value
              temp_vals += inputs_list(incoming_node)
            }
            else { // get the list of incoming values
              temp_vals += NP(incoming_node - this.num_input)
            }
          }
          NP(cgp_node_idx) = this.functions_options(node.func_idx)(temp_vals.toList) // applies function
        }
      }

      //      println("Went from front to back of active nodes to compute vals")

      //      for (le_node <- this.node_list) {
      //        print("Node ")
      //        print(le_node.number)
      //        print(" is in column ")
      //        println(le_node.col_where)
      //      }

      // go thru all the outputs (each output has 1 incoming)
      for (output_node <- (this.num_input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output - 1)) {
        var incoming_node = this.node_list(output_node).incoming(0)
        if (this.node_list(incoming_node).incoming.isEmpty) { // is this an input node?
          predictions += inputs_list(incoming_node)
        }
        else {
          predictions += NP(incoming_node - this.num_input)
        }
      }
      import scala.math.abs

      //      for (le_node <- this.node_list) {
      //        print("Node ")
      //        print(le_node.number)
      //        print(" is in column ")
      //        println(le_node.col_where)
      //      }

      //      print("Node List: ")
      //      println(this.node_list)
      //      for (node <- this.node_list) {
      //        print("Node number: ")
      //        print(node.number)
      //        print ( " with function idx of ")
      //        print(" with column number ")
      //        print(node.col_where)
      //        print(node.func_idx)
      //        print(" has incoming edges ")
      //        for (incoming_node <- node.incoming) {
      //          print(incoming_node.number)
      //          print(" ")
      //        }
      //        println("")
      //      }
      //      println("")
      //
      //      print("Inputs: ")
      //      println(inputs_list)
      //      print("Predictions: ")
      //      println(predictions)

      // convert predictions so that they are probabilities that sum to 1
      predictions = predictions.map(x => scala.math.abs(x))
      var total = predictions.sum

      if (total > 0) {
        predictions = predictions.map(x => x / total)
      }
      else {
        predictions = predictions.map(x => 0.0 / 1.0)
      }
      /// Print statements to ensure working correctly

      return predictions

    }

    def find_active_nodes(): Unit = {
      // Initialize Boolean list with all falses
      this.NU.clear()
      for (i <- this.num_input to (this.num_input + (this.num_row * this.num_col) - 1)) {
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
        if ((node_list(n)).incoming.isEmpty == false) {
          this.NU(n - this.num_input) = true
          new_find_path(node_list(n))
        }
      }
    }

    def mutate_cgp(probability: Double, is_node: Boolean, is_edge: Boolean): Unit = {
      val r = new scala.util.Random
      for (node_idx <- this.num_input to node_list.size - 1) {
        var rand_prob = r.nextDouble
        if (rand_prob <= probability) {
          if (is_node && is_edge) mutate_node(this.node_list(node_idx))
          else if (is_node) mutate_node(this.node_list(node_idx))
          else if (is_edge) mutate_incoming_edges(this.node_list(node_idx))
        }
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

  }

  /* Functions Class */
  class Functions {
    val add = (vals: List[Double]) => {
      vals.sum
    }

    val divide: List[Double] => Double = vals => {
      var result = vals(0)
      for (i <- 1 to vals.size - 1) {
        if (vals(i) == 0) {
          result = 0
        }
        else {
          result = result / vals(i)
        }
      }
      result
    }

    val subtract: List[Double] => Double = vals => {
      var result = vals(0)
      for (i <- 1 to vals.size - 1) {
        result = result - vals(i)
      }
      result
    }

    val multiply = (vals: List[Double]) => {
      vals.product
    }

    val constant: List[Double] => Double = vals => {
      val result = 1
      result
    }

    val compare_1: List[Double] => Double = vals => {
      var r = scala.util.Random
      var a = vals(r.nextInt(vals.size))
      var b = vals(r.nextInt(vals.size))
      var result = 1
      if (a < b) result = 1
      else result = 0
      result
    }

    val compare_2: List[Double] => Double = vals => {
      var all_positive = true
      for (v <- vals) {
        if (v <= 0) all_positive = false
      }

      var result = 1
      if (all_positive) result = 1
      else result = 0
      result
    }

    val compare_3: List[Double] => Double = vals => {
      var any_positive = false
      for (v <- vals) {
        if (v > 0) any_positive = true
      }

      var result = 1
      if (any_positive) result = 1
      else result = 0
      result
    }

  }

  var num_input = 1
  var num_output = 1
  var arity = 2
  var lv_back = 2
  var num_row = 3
  var num_col = 3

  var num_cgps_to_evaluate = 10
  var cgps = ListBuffer[Cgp]()
  var best_cgp_idx = -1
  var lowest_cgp_metric_val: Double = 0

  var funcs = new Functions()
  var function_options = List(funcs.add, funcs.subtract, funcs.multiply, funcs.divide,
    funcs.constant, funcs.compare_1, funcs.compare_2, funcs.compare_3)

  // initialize because scala doesn't allow variable declaration without initialization
  var CGP_to_mutate = new Cgp(-1, num_output, arity, lv_back, num_row, num_col, function_options)

  var turtlesToCgps: mutable.Map[api.Turtle, Cgp] = mutable.LinkedHashMap[api.Turtle, Cgp]()

  /* Load primitives for NetLogo */
  def load(manager: api.PrimitiveManager) {
    manager.addPrimitive("add-cgps", addCgp)
    manager.addPrimitive("get-action", getAction)
    manager.addPrimitive("mutate-reproduce", mutate_reproduce)
    manager.addPrimitive(name = "clear-cgp", clearCgp)
  }


  object mutate_reproduce extends api.Command {
    override def getSyntax: Syntax =
      Syntax.commandSyntax(right = List(Syntax.AgentType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType), agentClassString = "-T--")

    override def perform(args: Array[Argument], context: Context): Unit = {
      CGP_to_mutate = turtlesToCgps(args(0).getAgent.asInstanceOf[api.Turtle]) // get cgp from parameter
      var mutation_rate = args(1).getDoubleValue
      var numb_inps = args(2).getIntValue
      var numb_outs = args(3).getIntValue
      var numb_arts = args(4).getIntValue
      var numb_lvls = args(5).getIntValue
      var numb_rows = args(6).getIntValue
      var numb_cols = args(7).getIntValue
      // Make new CGP with same properties as parent
      var mutated_cgp = new Cgp(numb_inps, numb_outs, numb_arts, numb_lvls, numb_rows, numb_cols, function_options)
      // Set nodes in node list
      for (node <- CGP_to_mutate.node_list) {
        var copied_node = new Node(node.func_idx, node.number, node.col_where)
        mutated_cgp.add_to_node_list(copied_node)
      }
      // Set incoming and outgoing of nodes
      for (node <- CGP_to_mutate.node_list) {
        for (incoming_node <- node.incoming) {
          mutated_cgp.node_list(node.number).add_in(mutated_cgp.node_list(incoming_node))
        }
        for (outgoing_node <- node.outgoing) {
          mutated_cgp.node_list(node.number).add_out(mutated_cgp.node_list(outgoing_node.number))
        }
      }
      // Mutate
      mutated_cgp.mutate_cgp(mutation_rate, true, true)
      mutated_cgp.find_active_nodes()

      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, mutated_cgp)
      }
    }
  }

  object getAction extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(right = List(Syntax.ListType), ret = Syntax.ListType, agentClassString = "-T--")

    def report(args: Array[Argument], context: Context): AnyRef = {
      var input_points_nlogo = args(0).getList.map(x => x.toString.toDouble).toList
      var action_probs = turtlesToCgps(context.getAgent.asInstanceOf[api.Turtle]).solve_cgp(input_points_nlogo)
      action_probs.toLogoList // stay, left, right
    }
  }

  object addCgp extends api.Command {
    override def getSyntax: Syntax = Syntax.commandSyntax(right = List(Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType), agentClassString = "-T--")

    override def perform(args: Array[Argument], context: Context): Unit = {
      var numb_inps = args(0).getIntValue
      var numb_outs = args(1).getIntValue
      var numb_arts = args(2).getIntValue
      var numb_lvls = args(3).getIntValue
      var numb_rows = args(4).getIntValue
      var numb_cols = args(5).getIntValue
      var net = new Cgp(numb_inps, numb_outs, numb_arts, numb_lvls, numb_rows, numb_cols, function_options)
      net.create_cgp()
      net.find_active_nodes()
      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, net)
      }
    }
  }

  object clearCgp extends api.Command {
    override def getSyntax =
      Syntax.commandSyntax(right = List(), agentClassString = "-T--")

    def perform(args: Array[Argument], context: Context): Unit = {
      turtlesToCgps.remove(context.getAgent.asInstanceOf[api.Turtle])
    }
  }
}