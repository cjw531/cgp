package main.scala

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break
import java.io.FileWriter
import java.io.{BufferedWriter, FileWriter}
import java.util.concurrent.{Callable, FutureTask}
import scala.math.BigDecimal.double2bigDecimal

object Runner extends App {

//  val outputFile = new BufferedWriter(new FileWriter("EvalsOverGenInt.csv"))


//  val csvWriter = new FileWriter("EvalsOverGenDouble_newpoints_try.csv")
//  csvWriter.append("Generation")
//  csvWriter.append(",")
//  csvWriter.append("Evaluation Score")
//  csvWriter.append("\n")


  var num_input = 1
  var num_output = 1
  var arity = 2
  var lv_back = 2
  var num_row = 3
  var num_col = 3

  var num_cgps_to_evaluate = 10
  var cgps = ListBuffer[Cgp]()
  var best_cgp_idx = -1
  var lowest_cgp_metric_val: BigDecimal = 0


  var funcs = new Functions()
  var function_options = List(funcs.add, funcs.subtract, funcs.multiply, funcs.divide)

  // Generate sample of points
  def generate_points(): ListBuffer[BigDecimal] = {
    var number_of_points = 500
    val r = scala.util.Random
    var sample_points = ListBuffer[BigDecimal]()
    for (i <- 1 to number_of_points) {
      sample_points += r.nextDouble * 10
      sample_points += r.nextDouble * -10
    }
    return sample_points
  }

  var sample_points = generate_points()

  print("Points: ")
  println(sample_points)

  // define true function to evaluate
  def func(x: BigDecimal): BigDecimal = {
    return x.pow(2) + 2*x + 1
  }

  // Evaluate sample of points on true function
  def evaluate_sample_points_true(points: ListBuffer[BigDecimal]): ListBuffer[BigDecimal] = {
    var true_values = ListBuffer[BigDecimal]()
    for (point <- points) {
      true_values += func(point)
    }
    return true_values
  }

  var true_values = evaluate_sample_points_true(sample_points)

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
    var diff = ListBuffer[BigDecimal]()
    for (i <- 0 to preds.length-1) {
      diff += (true_values(i) - preds(i))
    }
    var diff_squared = ListBuffer[BigDecimal]()
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
    else if (CGP.evaluation_score.compareTo(lowest_cgp_metric_val) < 0) {
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

  var num_generation = 0

//  val future = new FutureTask[String](new Callable[String]() {
//    def call(): String = {
//      searcher.search(target)
//    }
//  })
//
//  executor.execute(future)

  while (CGP_to_mutate.evaluation_score > threshold) {
    var best_cgp = CGP_to_mutate
    var lowest_MSE = CGP_to_mutate.evaluation_score
    num_cgps_to_evaluate = 20
    for (i <- 0 to num_cgps_to_evaluate - 1) {
      // Make new CGP with same properties as parent
      var mutated_cgp = new Cgp(1, num_output, lv_back, num_row, num_col, function_options)
      // Set nodes in node list
      for (node <- CGP_to_mutate.node_list) {
        var copied_node = new Node(node.func_idx, node.number, node.col_where, node.weight)
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
      // lower mutation to change edges
      // adaptive mutation rate
      mutated_cgp.mutate_cgp(0.05, 0.01, 0.01)
      sample_points = generate_points()
      var preds = mutated_cgp.decode_cgp(sample_points)
      true_values =  evaluate_sample_points_true(sample_points)
      // Evaluate predictions
      var diff = ListBuffer[BigDecimal]()
      for (i <- 0 to preds.length - 1) {
        diff += (true_values(i) - preds(i))
      }
      var diff_squared = ListBuffer[BigDecimal]()
      for (i <- 0 to diff.length - 1) {
        diff_squared += diff(i).pow(2)
      }
      mutated_cgp.evaluation_score = diff_squared.sum
      if (mutated_cgp.evaluation_score.compareTo(lowest_MSE) < 0) {
        lowest_MSE = mutated_cgp.evaluation_score
        best_cgp = mutated_cgp
      }
    } // end of while loop for creating mutations

    // assign new parent for generation
    CGP_to_mutate = best_cgp
    println(CGP_to_mutate.evaluation_score)

    print("Points: ")
    println(sample_points)

    print("Predictions: ")
    println(best_cgp.decode_cgp(sample_points))

    print("True vals: ")
    println(evaluate_sample_points_true(sample_points))

//    csvWriter.append(num_generation.toString())
//    csvWriter.append(",")
//    csvWriter.append(CGP_to_mutate.evaluation_score.toString())
//    csvWriter.append("\n")


    num_generation += 1
  } // end of while loop for generations

//  csvWriter.flush()
//  csvWriter.close()

} // end of class
