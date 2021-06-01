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

    class Functions() {
      val total_funcs = 4

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
    }

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
      var OutputConnection = -1
      val r = scala.util.Random
      var ActiveNodes = ListBuffer[Boolean]()

      def set_node_list(new_node_list: ListBuffer[Node]): Unit = {
        this.node_list = new_node_list
      }

      def set_output_connection(new_output_connection: Int): Unit = {
        this.OutputConnection = new_output_connection
      }

      /////////////////////////
      ///// Creates CGP ///////
      /////////////////////////
      def create_cgp(): Unit = {
        // make graph
        /////// create nodes with linkeage back to any of the nodes
        var last_node_number_in_column = 0
        for (i <- 1 to (this.num_rows * this.num_cols)) {
          var incoming1 = 0
          var incoming2 = 0
          if ((i - 1) > 0) {
            incoming1 = r.nextInt(i - 1) // pick random node before it
            incoming2 = r.nextInt(i - 1)
          }
          var new_node = new Node(i, r.nextInt(funcs.total_funcs), incoming1, incoming2)
          this.node_list += new_node
        }

        // connect output to any
        this.OutputConnection = r.nextInt(this.num_inputs + (this.num_rows*this.num_cols) - 1)
      }

      /////////////////////////
      ///// Finds Active Nodes ///////
      /////////////////////////
      def find_active_nodes(): Unit = {

        // Instantiate active nodes array with all false to start with
        for (i <- 0 to (this.num_rows*this.num_cols)-1) {
          this.ActiveNodes += false
        }

        // If the output node connects straight to an input then they are all false
        if (this.OutputConnection < this.num_inputs) {
          return
        }

        // Set first linkage from output node to graph to be true
        this.ActiveNodes(this.OutputConnection - this.num_inputs) = true

        // Follow path to set all active nodes to be true
        find_active_nodes_helper(this.OutputConnection)
      } // end of Find Active Nodes

      def find_active_nodes_helper(node_number: Int): Unit = {
        // Base case: If an input node, then has ended path
        if (node_number < this.num_inputs) {
          return
        }
        if (this.node_list(node_number - this.num_inputs).Incoming1 >= this.num_inputs) {
          this.ActiveNodes(this.node_list(node_number - this.num_inputs).Incoming1 - this.num_inputs) = true
        }
        if (this.node_list(node_number - this.num_inputs).Incoming2 >= this.num_inputs) {
          this.ActiveNodes(this.node_list(node_number - this.num_inputs).Incoming2 - this.num_inputs) = true
        }
        find_active_nodes_helper(this.node_list(node_number - this.num_inputs).Incoming1)
        find_active_nodes_helper(this.node_list(node_number - this.num_inputs).Incoming2)
      } // end of recursive helper function

      /////////////////////////
      ///// Solves CGP ///////
      /////////////////////////
      def decode_cgp(input_value: Double): Double = {
        // if all active nodes are false, then just return the input value
        if (!this.ActiveNodes.contains(true)) {
          return input_value
        }

        var NodeOutput = ListBuffer[Double]()

        // Set all 0's to start with
        for (i <- 0 to (this.num_inputs + (this.num_rows*this.num_cols) - 1)) {
          NodeOutput += 0.0
        }

        // Set input values
        NodeOutput(0) = input_value

        for (active_node_idx <- 0 to ActiveNodes.length-1) {
          if (ActiveNodes(active_node_idx) == true) {
            // If active node is true, then compute the function at the node with its incoming edges
            var function_to_apply = this.node_list(active_node_idx + this.num_inputs).func_idx
            var computed_val = 0.0
            if (function_to_apply == 0) {
              computed_val = funcs.add(NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming1), NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming2))
            }
            else if (function_to_apply == 1) {
              computed_val = funcs.subtract(NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming1), NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming2))
            }
            else if (function_to_apply == 2) {
              computed_val = funcs.multiply(NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming1), NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming2))
            }
            else {
              computed_val = funcs.divide(NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming1), NodeOutput(this.node_list(active_node_idx + this.num_inputs).Incoming2))
            }
            NodeOutput(active_node_idx + this.num_inputs) = computed_val
          }
        }
        return NodeOutput(this.OutputConnection)
      }
    } // end of CGP class

    /////////////////////////
    ///// mutates CGP ///////
    /////////////////////////
    def mutate_cgp(parent_cgp: Cgp, mutation_rate: Double): Cgp = {
      // deep copy CGP
      var mutated_cgp = new Cgp(1, 1, 5, 1, 5)
      var node_list_copied = ListBuffer[Node]()
      for (node <- parent_cgp.node_list) {
        var copied_node = new Node(node.number, node.func_idx, node.Incoming1, node.Incoming2)
        node_list_copied += copied_node
      }
      mutated_cgp.set_node_list(node_list_copied)
      mutated_cgp.set_output_connection(parent_cgp.OutputConnection)

      val r = scala.util.Random

      // remake nodes by certain probability
      var flag_changed = false
      while (flag_changed == false) {
        for (node_idx <- 0 to (mutated_cgp.node_list.length - 1) + mutated_cgp.num_outputs) {
          // change: make it possible for edge of output to change
          if (node_idx > mutated_cgp.node_list.length - 1) {
            val prob = r.nextDouble * 100
            if (prob <= mutation_rate) {
              flag_changed = true
              mutated_cgp.OutputConnection = r.nextInt(mutated_cgp.num_inputs + (mutated_cgp.num_rows*mutated_cgp.num_cols) - 1)
            }
          }
          else {
            var node = mutated_cgp.node_list(node_idx)
            val prob = r.nextDouble * 100
            if (prob <= mutation_rate) {
              flag_changed = true // mark that a change was made
              // alter an entire node (alter incoming edges and function)
              node.set_func_idx(r.nextInt(3))
              if (node.number - 1 > 0) {
                node.set_incoming_node_1(r.nextInt(node.number - 1))
                node.set_incoming_node_2(r.nextInt(node.number - 1))
              }
            }
          }
        }
      }

      // compute active nodes
      mutated_cgp.find_active_nodes()
      return mutated_cgp
    }

    //  println("Hi")
//    var cgp = new Cgp(1, 1, 5, 1, 5)
//    cgp.create_cgp()
//    cgp.find_active_nodes()
//
//    var mutatedCgp = mutate_cgp(cgp, 0.05)

    def func_to_eval(point: Double): Double = {
      (point * point * point) + point
    }

//    val r = scala.util.Random
//
//    // create initial CGPs
//    val initial_num_cgps = 15
//    var lowest_mse = Double.MaxValue
//    var parent_cgp: Cgp = _
//    for (i <- 0 to initial_num_cgps) {
//      var new_cgp = new Cgp(1, 1, 5, 1, 5)
//      new_cgp.create_cgp()
//      new_cgp.find_active_nodes()
//      var error_val = evaluate_CGP_against_points(new_cgp)
//      print("Error: ")
//      println(error_val)
//      if (error_val < lowest_mse) {
//        lowest_mse = error_val
//        parent_cgp = new_cgp
//      }
//    }

//    // mutate best ones
//    var generation = 0
//    val num_offspring = 5
//    lowest_mse = Double.MaxValue
//    var curr_best_offspring: Cgp = _
//    while (lowest_mse > 0) {
//      lowest_mse = Double.MaxValue
//      var parent_error = evaluate_CGP_against_points(parent_cgp)
//      if (parent_error < lowest_mse) {
//        lowest_mse = parent_error
//        curr_best_offspring = parent_cgp
//      }
//      for (i <- 0 to num_offspring) {
//        var offspring_cgp = mutate_cgp(parent_cgp, 0.05)
//        var error_val = evaluate_CGP_against_points(offspring_cgp)
//        if (error_val < lowest_mse) {
//          lowest_mse = error_val
//          curr_best_offspring = offspring_cgp
//        }
//      }
//      print("Error: ")
//      print(lowest_mse)
//      print(" in generation ")
//      println(generation)
//
//      generation += 1
//      parent_cgp = curr_best_offspring
//    }

    def evaluate_CGP_against_points(cgp_to_eval: Cgp): Double = {
      val r = scala.util.Random
      var ongoing_error = 0.0

      for (i <- 1 to 500) {
        // half the time negative and positive
        if (r.nextDouble <= 50) {
          var point = r.nextDouble * 10
          var evaluation = cgp_to_eval.decode_cgp(point)
          ongoing_error += (func_to_eval(point) - evaluation) * (func_to_eval(point) - evaluation)
        }
        else {
          var point = r.nextDouble * -10
          var evaluation = cgp_to_eval.decode_cgp(point)
          ongoing_error += (func_to_eval(point) - evaluation) * (func_to_eval(point) - evaluation)
        }
      }
      return ongoing_error / 500
    }


  /*******************************************************************************
  **************************NETLOGO PORTION***************************************
   *******************************************************************************/
  var turtlesToCgps: mutable.Map[api.Turtle, Cgp] = mutable.LinkedHashMap[api.Turtle, Cgp]()

  /* Load primitives for NetLogo */
  def load(manager: api.PrimitiveManager) {
    manager.addPrimitive("add-cgps", addCgp)
    manager.addPrimitive("get-action", getAction)
    manager.addPrimitive("mutate-reproduce", mutate_reproduce)
    manager.addPrimitive(name = "clear-cgp", clearCgp)
  }

  object addCgp extends api.Command {
    override def getSyntax: Syntax = Syntax.commandSyntax(right = List(Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType, Syntax.NumberType), agentClassString = "-T--")

    override def perform(args: Array[Argument], context: Context): Unit = {
      var numb_inps = args(0).getIntValue
      var numb_outs = args(1).getIntValue
      var numb_lvls = args(2).getIntValue
      var numb_rows = args(3).getIntValue
      var numb_cols = args(4).getIntValue
      var net = new Cgp(numb_inps, numb_outs, numb_lvls, numb_rows, numb_cols)
      net.create_cgp()
      net.find_active_nodes()
      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, net)
      }
    }
  }

  object mutate_reproduce extends api.Command {
    override def getSyntax: Syntax =
          Syntax.commandSyntax(right = List(Syntax.AgentType, Syntax.NumberType), agentClassString = "-T--")
    override def perform(args: Array[Argument], context: Context): Unit = {
      var CGP_to_mutate = turtlesToCgps(args(0).getAgent.asInstanceOf[api.Turtle])
      var mutation_rate = args(1).getDoubleValue
      var offspring_cgp = mutate_cgp(CGP_to_mutate, mutation_rate)
      context.getAgent match {
        case turtle: api.Turtle => turtlesToCgps.update(turtle, offspring_cgp)
      }
    }
  }

  object getAction extends api.Reporter {
    override def getSyntax =
      Syntax.reporterSyntax(right = List(Syntax.ListType), ret = Syntax.ListType, agentClassString = "-T--")

    def report(args: Array[Argument], context: Context): AnyRef = {
      var input_points_nlogo = args(0).getDoubleValue
      var action_probs = turtlesToCgps(context.getAgent.asInstanceOf[api.Turtle]).decode_cgp(input_points_nlogo)
      return action_probs.asInstanceOf[AnyRef]
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