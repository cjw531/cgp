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

  /* Cgp Class */
  class Cgp (input: Int, output: Int, level: Int, row: Int, col: Int, funcs: List[List[Double] => Double]) {
    def num_input = input
    def num_output = output
    def arity = funcs.size
    def lv_back = level
    def num_row = row
    def num_col = col
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
      var col_start = 0 // n-th col #
      for (i <- this.num_input to (this.num_input - 1) + (this.num_row * this.num_col)) {
        if ((i - this.num_input) % num_col == 0) {
          col_start += 1
        }
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
      for (i <- this.num_input + (this.num_row*this.num_col) to this.num_input + (this.num_row*this.num_col) + this.num_output - 1) {
        var output_node = new Node(-1, i, this.num_col+1)
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

    def evaluate_cgp(points: ListBuffer[Double]): ListBuffer[Double] = {
      // if no active nodes, then output maps to input and just return the point then
      if (!this.NU.contains(true)) {
        var predictions = ListBuffer[Double]()
        for (point <- points) {
          predictions += point
        }
        return predictions
      }

      // compute values for active nodes
      var NP = ListBuffer[Double]()

      // Set all to 0 to begin with
      for (i <- 0 to this.NU.length-1) {
        NP += 0
      }

      var predictions = ListBuffer[Double]()

      for (point <- points) {
        for (cgp_node_idx <- 0 to this.NU.length-1) {
          if (NU(cgp_node_idx) == true) { // if it's active
            var node = this.node_list(cgp_node_idx + this.num_input) // offset so we can idx node in node_list
            // apply function to all incoming
            var temp_vals = ListBuffer[Double]()
            for (incoming_node <- node.incoming) {
              if (incoming_node.incoming.isEmpty) {
                temp_vals += point
              }
              else {
                temp_vals += NP(incoming_node.number - num_input)
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

    def decode_cgp(points: ListBuffer[Double]): ListBuffer[Double] = {
      find_active_nodes()
      //    println(this.NU)
      return evaluate_cgp(points)
    }

    def solve_cgp(inputs_list: List[Double]): ListBuffer[Double] = {
      var predictions = ListBuffer[Double]()

      // if no active nodes, then output maps to input and just return the point then
      if (!this.NU.contains(true)) {
        for (output_node <- (this.num_input + (this.num_row + this.num_col)) to (this.num_input + (this.num_row + this.num_col) + this.num_output - 1)) {
          for (incoming_node <- this.node_list(output_node).incoming) {
            predictions += inputs_list(incoming_node.number)
          }
        }
        return predictions
      }

      var NP = ListBuffer[Double]()
      // Set all to 0 to begin with
      for (i <- 0 to this.NU.length-1) {
        NP += 0
      }

      for (cgp_node_idx <- 0 to this.NU.length-1) {
        if (NU(cgp_node_idx) == true) {
          var node = this.node_list(cgp_node_idx + this.num_input) // offset so we can idx node in node_list
          var temp_vals = ListBuffer[Double]()
          for (incoming_node <- node.incoming) {
            if (incoming_node.incoming.isEmpty) {
              temp_vals += inputs_list(incoming_node.number)
            }
            else {
              temp_vals += NP(incoming_node.number - this.num_input)
            }
          }
          NP(cgp_node_idx) = this.functions_options(node.func_idx)(temp_vals.toList)
        }
      }

      for (output_node <- (this.num_input + (this.num_row * this.num_col)) to (this.num_input + (this.num_row * this.num_col) + this.num_output - 1)) {
        var incoming_node = this.node_list(output_node).incoming(0)
        if (incoming_node.incoming.isEmpty) {
          predictions += inputs_list(incoming_node.number)
        }
        else {
          predictions += NP(incoming_node.number - this.num_input)
        }
      }
      import scala.math.abs

      // convert predictions so that they are probabilities that sum to 1
      predictions = predictions.map(x => scala.math.abs(x))

//      return predictions

      var total = predictions.sum

      var g_index = 0
      if (total > 0) {
        predictions = predictions.map(x => x / total)
        // weighted choice: https://softwareengineering.stackexchange.com/questions/150616/get-weighted-random-item

        // Calculate the cumulative sums of the weights
        var cum_sum = ListBuffer[Double]()
        var ongoing_sum = 0.0
        for (pred <- predictions) {
          ongoing_sum = ongoing_sum + pred
          cum_sum += ongoing_sum
        }

        // Generate a random number n in the range of 0 to sum(weights)
        val r = scala.util.Random
        var rand_num = r.nextDouble * predictions.sum

        // Find the last item whose cumulative sum is above n
        var idx = 0
        for (i <- cum_sum) {
          if (i > rand_num) {
            g_index = idx
          }
          idx += 1
        }
      }
      else {
        predictions = ListBuffer(0.0,0.0,0.0)
        return predictions
      }

//      val g_index= predictions.indexOf(predictions.max)
      if (g_index == 0) {
        predictions = ListBuffer(1.0, 0.0, 0.0)
      }
      else if (g_index == 1) {
        predictions = ListBuffer(0.0, 1.0, 0.0)
      }
      else {
        predictions = ListBuffer(0.0, 0.0, 1.0)
      }

      predictions
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
          this.NU(n.number - this.num_input) = true
          new_find_path(n)
        }
      }
    }

    def mutate_cgp(probability: Double, is_node: Boolean, is_edge: Boolean): Unit = {
      val r = new scala.util.Random
      for (node_idx <- this.num_input to node_list.size-1) {
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
    val add = (vals: List[Double]) => {vals.sum}

    val divide: List[Double] => Double = vals => {
      var result = vals(0)
      for (i <- 1 to vals.size-1) {
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
      for (i <- 1 to vals.size-1) {
        result = result - vals(i)
      }
      result
    }

    val multiply = (vals: List[Double]) => {vals.product}

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

  var sample_points = ListBuffer[Double]()
  var true_values = ListBuffer[Double]()
  // initialize because scala doesn't allow variable declaration without initialization
  var CGP_to_mutate = new Cgp(-1, num_output, lv_back, num_row, num_col, function_options)

  var turtlesToCgps: mutable.Map[api.Turtle, Cgp] = mutable.LinkedHashMap[api.Turtle, Cgp]()


  // TODO: define true function to evaluate
  // TODO: hardcoded for now
  def func(x: Double): Double = {
    return (x * x) + 2*x + 1
  }

  /* Load primitives for NetLogo */
  def load(manager: api.PrimitiveManager) {
    manager.addPrimitive("rand-point", generate_random_point)
    manager.addPrimitive("true-value", get_true_values)
    manager.addPrimitive("init_cgp", init_cgp_eval)
    manager.addPrimitive("mutate_breed", mutate_breed)
    manager.addPrimitive("add-cgps", addCgp)
    manager.addPrimitive("get-cgp-list", getCgpList)
    manager.addPrimitive("get-action", getAction)
    manager.addPrimitive("mutate-reproduce", mutate_reproduce)
  }


  object mutate_reproduce extends api.Command {
    override def getSyntax: Syntax =
      Syntax.commandSyntax(right = List(Syntax.AgentType, Syntax.NumberType), agentClassString = "-T--")
    override def perform(args: Array[Argument], context: Context): Unit = {
      CGP_to_mutate = turtlesToCgps(args(0).getAgent.asInstanceOf[api.Turtle]) // get cgp from parameter
      var mutation_rate = args(1).getDoubleValue
      // Make new CGP with same properties as parent
      var mutated_cgp = new Cgp(9, 3, 2, 4, 4, function_options)
      // Set nodes in node list
      for (node <- CGP_to_mutate.node_list) {
        var copied_node = new Node(node.func_idx, node.number, node.col_where)
        mutated_cgp.add_to_node_list(copied_node)
      }
      // Set incoming and outgoing of nodes
      for (node <- CGP_to_mutate.node_list) {
        for (incoming_node <- node.incoming) {
          mutated_cgp.node_list(node.number).add_in(mutated_cgp.node_list(incoming_node.number))
        }
        for (outgoing_node <- node.outgoing) {
          mutated_cgp.node_list(node.number).add_out(mutated_cgp.node_list(outgoing_node.number))
        }
      }
      // Mutate
      mutated_cgp.mutate_cgp(mutation_rate, true, true)
      mutated_cgp.find_active_nodes()
//      mutated_cgp.sol

      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, mutated_cgp)
      }
    }
  }

  object getAction extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(right = List(Syntax.ListType), ret = Syntax.ListType, agentClassString = "-T--")
    def report(args: Array[Argument], context: Context): AnyRef = {
      var input_points_nlogo = args(0).getList.map( x => x.toString.toDouble).toList
      var action_probs = turtlesToCgps(context.getAgent.asInstanceOf[api.Turtle]).solve_cgp(input_points_nlogo)
      action_probs.toLogoList // stay, left, right
    }
  }

  object addCgp extends api.Command {
    override def getSyntax: Syntax = Syntax.commandSyntax(right = List(), agentClassString = "-T--")
    override def perform(args: Array[Argument], context: Context): Unit = {
      var net = new Cgp(9, 3, 2, 4, 4, function_options)
      net.create_cgp()
      net.find_active_nodes()
      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, net)
      }
    }
  }

  object getCgpList extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(ret = Syntax.ListType, agentClassString = "-T--")
    def report(args: Array[Argument], context: Context): AnyRef = {
      var cgp_layers = ListBuffer[ListBuffer[Int]]()
      for (node <- turtlesToCgps(context.getAgent.asInstanceOf[api.Turtle]).node_list) {
        var node_numbers = ListBuffer[Int]()
        for (incoming_nodes <- node.incoming) {
          node_numbers += incoming_nodes.number
        }
        cgp_layers += node_numbers
      }
      // [[], [0,0], []]

      cgp_layers.toLogoList
    }
  }

  // [ 1 [], 2 [1, 2], 3 [], []


  override def clearAll(): Unit = {
    super.clearAll()
    sample_points = ListBuffer[Double]()
    true_values = ListBuffer[Double]()

    turtlesToCgps = mutable.LinkedHashMap[api.Turtle, Cgp]()
  }

  /* Overrides */
  // Generate random points
  object generate_random_point extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(right = List(Syntax.NumberType), ret = Syntax.ListType, agentClassString = "O---")
    def report(args: Array[Argument], context: Context): AnyRef = {
      var number_of_points = args(0).getIntValue
      val r = scala.util.Random
      for (i <- 1 to number_of_points) {
        sample_points += (-10 + (10 - -10) * r.nextDouble())
      }
      (sample_points).toLogoList
    }
  }


  // Evaluate true values
  object get_true_values extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(ret = Syntax.ListType, agentClassString = "O---")
    def report(args: Array[Argument], context: Context): AnyRef = {
      for (point <- sample_points) {
        true_values += func(point)
      }
      (true_values).toLogoList
    }
  }

  // initially create CGPs and find the best one
  object init_cgp_eval extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(ret = Syntax.ListType, agentClassString = "O---")
    def report(args: Array[Argument], context: Context): AnyRef = {
      for (i <- 0 to num_cgps_to_evaluate) {
        var CGP = new Cgp(1, num_output, lv_back, num_row, num_col, function_options)
        CGP.create_cgp()

        // squared difference
        var preds = CGP.decode_cgp(sample_points)
        var diff = ListBuffer[Double]()
        for (i <- 0 to preds.length-1) diff += (true_values(i) - preds(i))
        var diff_squared = ListBuffer[Double]()
        for (i <- 0 to diff.length-1) diff_squared += (diff(i) * diff(i))
        CGP.evaluation_score = diff_squared.sum
        if (i == 0) {
          best_cgp_idx = i
          lowest_cgp_metric_val = CGP.evaluation_score
        }
        else if (CGP.evaluation_score < lowest_cgp_metric_val) {
          best_cgp_idx = i
          lowest_cgp_metric_val = CGP.evaluation_score
        }
        cgps += CGP
      }

      CGP_to_mutate = cgps(best_cgp_idx)
      val best_idx_value = ListBuffer[Int]()
      best_idx_value += best_cgp_idx
      best_idx_value += (CGP_to_mutate.evaluation_score).toInt
      cgps.clear()
      cgps += CGP_to_mutate
      (best_idx_value).toLogoList
    }
  }

  // Evaluate true values
  object mutate_breed extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(right = List(Syntax.NumberType), ret = Syntax.NumberType, agentClassString = "O---")
    def report(args: Array[Argument], context: Context): AnyRef = {
      var mutation_rate = args(0).getDoubleValue
      var best_cgp = CGP_to_mutate
      var lowest_MSE = CGP_to_mutate.evaluation_score
      for (i <- 0 to num_cgps_to_evaluate - 1) {
        // Make new CGP with same properties as parent
        var mutated_cgp = new Cgp(1, num_output, lv_back, num_row, num_col, function_options)
        // Set nodes in node list
        for (node <- CGP_to_mutate.node_list) {
          var copied_node = new Node(node.func_idx, node.number, node.col_where)
          mutated_cgp.add_to_node_list(copied_node)
        }
        // Set incoming and outgoing of nodes
        for (node <- CGP_to_mutate.node_list) {
          for (incoming_node <- node.incoming) {
            mutated_cgp.node_list(node.number).add_in(mutated_cgp.node_list(incoming_node.number))
          }
          for (outgoing_node <- node.outgoing) {
            mutated_cgp.node_list(node.number).add_out(mutated_cgp.node_list(outgoing_node.number))
          }
        }
        // Mutate
        mutated_cgp.mutate_cgp(mutation_rate, true, true)
        var preds = mutated_cgp.decode_cgp(sample_points)
        // Evaluate predictions
        var diff = ListBuffer[Double]()
        for (i <- 0 to preds.length - 1) {
          diff += (true_values(i) - preds(i))
        }
        var diff_squared = ListBuffer[Double]()
        for (i <- 0 to diff.length - 1) {
          diff_squared += (diff(i) * diff(i))
        }
        mutated_cgp.evaluation_score = diff_squared.sum
        if (mutated_cgp.evaluation_score < lowest_MSE) {
          lowest_MSE = mutated_cgp.evaluation_score
          best_cgp = mutated_cgp
        }
      } // end of while loop for creating mutations

      // assign new parent for generation
      CGP_to_mutate = best_cgp
      Double.box((CGP_to_mutate.evaluation_score).toInt)
    }
  }
}

