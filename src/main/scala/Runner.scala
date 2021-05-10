package main.scala

import scala.collection.mutable.ListBuffer

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

  // define true function to evaluate
  def func(x: Int): Int = {
    return (x * x) + (2 * x) + 1 // x^2 + 2x + 1
  }

  // Evaluate sample of points on true function
  val true_values = ListBuffer[Int]()
  for (point <- sample_points) {
    true_values += func(point)
  }

  print("True values: ")
  println(true_values )

  // initially create CGPs and find the best one
  for (i <- 0 to num_cgps_to_evaluate) { // create multiple cgps for each agent
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

    print("Initial difference taken: ")
    println(diff)
    print("Squared difference: ")
    println(diff_squared)

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
  val CGP_to_mutate = cgps(best_cgp_idx)
  println(CGP_to_mutate.evaluation_score)

  for (i <- 0 to num_cgps_to_evaluate - 1) {
    var mutated_cgp = new Cgp(1, num_output, lv_back, num_row, num_col, function_options)
    // copy node list from parent
//    mutated_cgp.set_node_list(CGP_to_mutate.node_list.clone.map(_.clone))
    for (n <- CGP_to_mutate.node_list) { // deep copy the node
      mutated_cgp.node_list += n.deep_copy()
    }

//    mutated_cgp.set_NU(CGP_to_mutate.NU.clone.map(_.clone))
    for (active_node <- CGP_to_mutate.NU) { // copy the boolean array
      mutated_cgp.NU += active_node
    }

    mutated_cgp.mutate_cgp(0.5, false, true) // call mutation function
    var preds = mutated_cgp.decode_cgp(sample_points)
  }

}
