package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break


object Runner extends App {
  var num_input = 1
  var num_output = 1
  var arity = 2
  var lv_back = 2
  var num_row = 3
  var num_col = 3

  var num_cgps_to_evaluate = 10
  var cgps = ListBuffer[Cgp]()
  var best_cgp_idx = -1
  var lowest_cgp_metric_val: BigInt = 0


  var funcs = new Functions()
  var function_options = List(funcs.add, funcs.subtract, funcs.multiply, funcs.divide)

  // Generate sample of points
  var number_of_points = 100
  val r = scala.util.Random
  var sample_points = ListBuffer[Int]()
  for (i <- 1 to number_of_points) {
      sample_points += r.nextInt(50)
  }
  print("Points: ")
  println(sample_points)

  // define true functin to evaluate
  def func(x: Int): Int = {
    return x^2 + 2*x + 1
  }

  // Evaluate sample of points on true function
  val true_values = ListBuffer[Int]()
  for (point <- this.sample_points) {
    true_values += func(point)
  }

  print("True values: ")
  println(true_values)

  // initially create CGPs and find the best one
  for (i <- 0 to num_cgps_to_evaluate) {
    var CGP = new Cgp(1, num_output, lv_back, num_row, num_col, function_options)
    CGP.create_cgp()

    var preds = CGP.decode_cgp(sample_points)
    print("Predictions: ")
    println(preds)
    // squared difference

    // how are preds
    var diff = ListBuffer[BigInt]()
    for (i <- 0 to preds.length-1) {
      diff += (true_values(i) - preds(i))
    }
    var diff_squared = ListBuffer[BigInt]()
    for (i <- 0 to diff.length-1) {
      diff_squared += diff(i).pow(2)
    }
    CGP.evaluation_score = diff_squared.sum
    print("Evaluation Score: ")
    println(CGP.evaluation_score)
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

  print("Best CGP is CGP#")
  println(best_cgp_idx)
  print("Best evaluation score: ")
  var CGP_to_mutate = cgps(best_cgp_idx)
  println(CGP_to_mutate.evaluation_score)

  cgps.clear()
  cgps += CGP_to_mutate

  var threshold = 100

  var num_generations = 0
  while (CGP_to_mutate.evaluation_score > threshold) {
    if (num_generations == 10000) {
      break
    }
    num_generations += 1
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
      mutated_cgp.mutate_cgp(0.4, true, true)
      var preds = mutated_cgp.decode_cgp(sample_points)
      // Evaluate predictions
      var diff = ListBuffer[BigInt]()
      for (i <- 0 to preds.length - 1) {
        diff += (true_values(i) - preds(i))
      }
      var diff_squared = ListBuffer[BigInt]()
      for (i <- 0 to diff.length - 1) {
        diff_squared += diff(i).pow(2)
      }
      mutated_cgp.evaluation_score = diff_squared.sum
      if (mutated_cgp.evaluation_score < lowest_MSE) {
        lowest_MSE = mutated_cgp.evaluation_score
        best_cgp = mutated_cgp
      }
    } // end of while loop for creating mutations

    // assign new parent for generation
    CGP_to_mutate = best_cgp
    println(CGP_to_mutate.evaluation_score)
  } // end of while loop for generations
} // end of class
