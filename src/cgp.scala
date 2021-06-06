import org.nlogo.{agent, api, core, nvm}
import core.Syntax
import api.ScalaConversions._
import api.{Argument, Context, ExtensionManager, ScalaConversions}
import org.nlogo.core.AgentKind

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.double2bigDecimal
import scala.math.abs
import scala.util.Random

class cgp extends api.DefaultClassManager {

  /*
  Defines possible functions for operators to contain
   */
    class Functions() {
      val total_funcs = 7

      def add (x: Double, y: Double): Double = {
        x + y
      }
      def subtract (x: Double, y: Double): Double = {
        x - y
      }
      def multiply (x: Double, y: Double): Double = {
        x * y
      }
      def divide (x: Double, y: Double): Double = {
        if (y == 0) {
          return 0
        }
        return (x / y)
      }
      def constant(x: Double, y:Double): Double = {
        return 1.0
      }
      def both_positive(x: Double, y: Double): Double = {
        if (x > 0 && y > 0) {
          return 1.0
        }
        else {
          return 0.0
        }
      }

      def greater_than(x: Double, y:Double): Double = {
        if (x >= y) {
          return 1.0
        }
        else {
          return 0.0
        }
      }
    }

  /*
  Defines node class which define network
   */
    class Node(number_i: Int, func_idx_i: Int, incoming1: Int, incoming2: Int) {
      val number = number_i
      var func_idx = func_idx_i
      var Incoming1 = incoming1
      var Incoming2 = incoming2

      def set_incoming_node_1(new_incoming_node_1: Int): Unit = {
        this.Incoming1 = new_incoming_node_1
      }

      def set_incoming_node_2(new_incoming_node_2: Int): Unit = {
        this.Incoming2 = new_incoming_node_2
      }

      def set_func_idx(new_func_idx: Int): Unit = {
        this.func_idx = new_func_idx
      }

    }

  /*
  Defines CGP object that holds entire network
   */
  class Cgp(num_inputs_i: Int, num_outputs_i: Int, lvls_back_i: Int, num_rows_i: Int, num_cols_i: Int) {
    val arity = 2
    val num_inputs = num_inputs_i
    val num_outputs = num_outputs_i
    val lvls_back = lvls_back_i
    val num_rows = num_rows_i
    val num_cols = num_cols_i
    val graph = ListBuffer[Node]()
    val funcs = new Functions()
    var node_list = ListBuffer[Node]()
    var OutputConnections = ListBuffer[Int]()
    val r = scala.util.Random
    var ActiveNodes = ListBuffer[Boolean]()
    def set_node_list(new_node_list: ListBuffer[Node]): Unit = {
      this.node_list = new_node_list
    }
    def set_output_connection(new_output_connection: Int, idx_to_change: Int): Unit = {
      this.OutputConnections(idx_to_change) = new_output_connection
    }
    def add_to_output_connections(node_num_to_add: Int): Unit = {
      OutputConnections += node_num_to_add
    }

    /*
    Creates random CGP
     */
    def create_cgp(): Unit = {
      // make graph
      /////// create nodes with linkage back to any of the nodes
      var last_node_number_in_column = this.num_inputs - 1
      // Creates new inner node and choose a random incoming edge on it which can be any node in a column before it
      var row_counter = 1
      for (i <- this.num_inputs to (this.num_inputs + (this.num_rows * this.num_cols) - 1)) {
        var incoming1 = 0
        var incoming2 = 0
        if (last_node_number_in_column > 0) {
          incoming1 = r.nextInt(last_node_number_in_column) // pick random node before it
          incoming2 = r.nextInt(last_node_number_in_column)
        }
        var new_node = new Node(i, r.nextInt(funcs.total_funcs), incoming1, incoming2) // create new node
        this.node_list += new_node // add to node list
        // Set last node in column
        if (row_counter % this.num_rows == 0) {
          last_node_number_in_column = i
          row_counter = 0
        }
        row_counter += 1
      }
      // connect outputs to any previous nodes
      for (i <- (this.num_inputs + (this.num_rows * this.num_cols)) to (this.num_inputs + (this.num_rows * this.num_cols) + this.num_outputs - 1)) {
        this.OutputConnections += r.nextInt(this.num_inputs + (this.num_rows*this.num_cols) - 1)
      }
    }

    /*
    Generate a boolean array of all the nodes that are active in the network
     */
    def find_active_nodes(): Unit = {

      // Instantiate active nodes array with all false to start with
      for (i <- 0 to (this.num_rows*this.num_cols)-1) {
        this.ActiveNodes += false
      }
      // Set first linkages from output nodes to graph to be true
      for (output_node <- this.OutputConnections) {
        if (output_node >= this.num_inputs) {
          this.ActiveNodes(output_node - this.num_inputs) = true
        }
      }

      // Follow path to set all active nodes to be true
      for (output_node <- this.OutputConnections) {
        if (output_node >= this.num_inputs) {
          find_active_nodes_helper(output_node)
        }
      }
    } // end of Find Active Nodes

    def find_active_nodes_helper(node_number: Int): Unit = {
      // Base case: If an input node, then has ended path
      if (node_number < this.num_inputs) {
        return
      }

      // Recurse on incoming edges and set to true
      if (this.node_list(node_number - this.num_inputs).Incoming1 >= this.num_inputs) {
        this.ActiveNodes(this.node_list(node_number - this.num_inputs).Incoming1 - this.num_inputs) = true
      }
      if (this.node_list(node_number - this.num_inputs).Incoming2 >= this.num_inputs) {
        this.ActiveNodes(this.node_list(node_number - this.num_inputs).Incoming2 - this.num_inputs) = true
      }
      find_active_nodes_helper(this.node_list(node_number - this.num_inputs).Incoming1)
      find_active_nodes_helper(this.node_list(node_number - this.num_inputs).Incoming2)
    } // end of recursive helper function

    /*
    Solve the network to get output values
     */
    def decode_cgp(input_values: List[Double]): List[Double] = {
      // if all active nodes are false, then just return the input value
      if (!this.ActiveNodes.contains(true)) {
        var result = ListBuffer[Double]()
        for (output_node <- this.OutputConnections) {
          result += input_values(output_node)
        }
        return result.toList
      }

      var NodeOutput = ListBuffer[Double]()

      // Set all 0's to start with
      for (i <- 0 to (this.num_inputs + (this.num_rows*this.num_cols) - 1)) {
        NodeOutput += 0.0
      }

      // Set input values to begin with
      for (input_idx <- 0 to input_values.length-1) {
        NodeOutput(input_idx) = input_values(input_idx)
      }
      for (active_node_idx <- 0 to ActiveNodes.length-1) {
        if (ActiveNodes(active_node_idx) == true) {
          // If active node is true, then compute the function at the node with its incoming edges
          var function_to_apply = this.node_list(active_node_idx).func_idx
          var computed_val = 0.0
          // apply functions to incoming edges at nodes
          if (function_to_apply == 0) {
            computed_val = funcs.add(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          else if (function_to_apply == 1) {
            computed_val = funcs.subtract(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          else if (function_to_apply == 2) {
            computed_val = funcs.multiply(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          else if (function_to_apply == 3) {
            computed_val = funcs.divide(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          else if (function_to_apply == 4) {
            computed_val = funcs.constant(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          else if (function_to_apply == 5) {
            computed_val = funcs.both_positive(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          else {
            computed_val = funcs.greater_than(NodeOutput(this.node_list(active_node_idx).Incoming1), NodeOutput(this.node_list(active_node_idx).Incoming2))
          }
          // set value in array to use in later iteration
          NodeOutput(active_node_idx + this.num_inputs) = computed_val
        }
      }
      // Get output values by extracting output from its incoming edge
      var result = ListBuffer[Double]()
      for (output_node <- this.OutputConnections) {
        result += NodeOutput(output_node)
      }
      return result.toList
    }
  } // end of CGP class

  /*
  Mutate CGP by re-making nodes by a probability
   */
  def mutate_cgp(parent_cgp: Cgp, mutation_rate: Double): Cgp = {
    // deep copy CGP
    var mutated_cgp = new Cgp(parent_cgp.num_inputs, parent_cgp.num_outputs, parent_cgp.lvls_back, parent_cgp.num_rows, parent_cgp.num_cols)
    var node_list_copied = ListBuffer[Node]()
    for (node <- parent_cgp.node_list) {
      var copied_node = new Node(node.number, node.func_idx, node.Incoming1, node.Incoming2)
      node_list_copied += copied_node
    }
    mutated_cgp.set_node_list(node_list_copied)
    for (output_node <- parent_cgp.OutputConnections) {
      mutated_cgp.add_to_output_connections(output_node)
    }

    val r = scala.util.Random

    // remake nodes by certain probability
    var flag_changed = false
    while (flag_changed == false) {
      var last_node_number_in_column = mutated_cgp.num_inputs - 1
      var row_counter = 1
      for (node_idx <- 0 to (mutated_cgp.node_list.length - 1) + mutated_cgp.num_outputs) {
        //        println(node_idx)
        if (node_idx > mutated_cgp.node_list.length - 1) {
          var output_idx = node_idx - mutated_cgp.node_list.length
          val prob = r.nextDouble * 100
          if (prob <= mutation_rate) {
            flag_changed = true
            mutated_cgp.set_output_connection(r.nextInt(mutated_cgp.num_inputs + (mutated_cgp.num_rows*mutated_cgp.num_cols) - 1), output_idx)
          }
        }
        else {
          var node = mutated_cgp.node_list(node_idx)
          val prob = r.nextDouble * 100
          if (prob <= mutation_rate) {
            flag_changed = true // mark that a change was made
            // alter an entire node (alter incoming edges and function)
            node.set_func_idx(r.nextInt(3))
            if (last_node_number_in_column > 0) {
              node.set_incoming_node_1(r.nextInt(last_node_number_in_column))
              node.set_incoming_node_2(r.nextInt(last_node_number_in_column))
            }
          }
        }
        if (row_counter % mutated_cgp.num_rows == 0) {
          last_node_number_in_column = node_idx + 1
          row_counter = 0
        }
        row_counter += 1
      }
    }

    // compute active nodes
    mutated_cgp.find_active_nodes()
    return mutated_cgp
  }

  /*******************************************************************************
  **************************NETLOGO PORTION***************************************
   *******************************************************************************/
    // define initial map to contain turtle -> Cgp relationship
  var turtlesToCgps: mutable.Map[api.Turtle, Cgp] = mutable.LinkedHashMap[api.Turtle, Cgp]()

  /* Load primitives for NetLogo */
  def load(manager: api.PrimitiveManager) {
    manager.addPrimitive("add-cgps", addCgp)
    manager.addPrimitive("get-action", getAction)
    manager.addPrimitive("mutate-reproduce", mutate_reproduce)
    manager.addPrimitive(name = "clear-cgp", clearCgp)
  }

  /*
  Adds CGP to agent
   */
  object addCgp extends api.Command {
    override def getSyntax: Syntax = Syntax.commandSyntax(right = List(Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType), agentClassString = "-T--")

    override def perform(args: Array[Argument], context: Context): Unit = {
      // get inputs
      var numb_inps = args(0).getIntValue
      var numb_outs = args(1).getIntValue
      var numb_lvls = args(2).getIntValue
      var numb_rows = args(3).getIntValue
      var numb_cols = args(4).getIntValue
      // create cgp instance
      var net = new Cgp(numb_inps, numb_outs, numb_lvls, numb_rows, numb_cols)
      net.create_cgp() // creates random network
      net.find_active_nodes() // finds a boolean array of all the active nodes
      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, net)
      }
    }
  }

  /*
  Mutates the CGP from the parent when creating an offspring
   */
  object mutate_reproduce extends api.Command {
    override def getSyntax: Syntax =
          Syntax.commandSyntax(right = List(Syntax.AgentType, Syntax.NumberType), agentClassString = "-T--")
    override def perform(args: Array[Argument], context: Context): Unit = {
      // get inputs
      var CGP_to_mutate = turtlesToCgps(args(0).getAgent.asInstanceOf[api.Turtle])
      var mutation_rate = args(1).getDoubleValue
      // mutate the parent CGP
      var offspring_cgp = mutate_cgp(CGP_to_mutate, mutation_rate)
      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, offspring_cgp)
      }
    }
  }

  /*
  Decodes the network to get the action vector
   */
  object getAction extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(right = List(Syntax.ListType), ret = Syntax.ListType, agentClassString = "-T--")

    def report(args: Array[Argument], context: Context): AnyRef = {
      // get inputs
      var input_points_nlogo = args(0).getList.toList.map(_.toString.toDouble)
      // solve the network based on the active nodes
      var action_probs = turtlesToCgps(context.getAgent.asInstanceOf[api.Turtle]).decode_cgp(input_points_nlogo)
      return action_probs.toLogoList
    }
  }

  /*
  Removes CGP from hashmap to clear memory
   */
  object clearCgp extends api.Command {
    override def getSyntax =
      Syntax.commandSyntax(right = List(), agentClassString = "-T--")

    def perform(args: Array[Argument], context: Context): Unit = {
      turtlesToCgps.remove(context.getAgent.asInstanceOf[api.Turtle])
    }
  }
}