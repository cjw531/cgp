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
  var lowest_cgp_metric_val = Int.MaxValue


  var funcs = new Functions()
//  List(funcs.add, func.subtract, func.multiply, func.divide)

  // initially create CGPs and find the best one
    ///// this part should be threaded
  for (i <- 0 to num_cgps_to_evaluate) {
    var CGP = new Cgp(num_input, num_output, arity, lv_back, num_row, num_col) // create CGP object
    CGP.create_cgp() // Randomly create edges between all nodes of CGP
    // rest of the steps ....
    // CGP.
    if (CGP.evaluation_score < lowest_cgp_metric_val) {
      best_cgp_idx = i
      lowest_cgp_metric_val = CGP.evaluation_score
    }
    cgps += CGP
  }

  var threshold = 10

  while (lowest_cgp_metric_val > threshold) {
    // Mutate to create offspring based on the best cgp
    var CGP_to_mutate = cgps(best_cgp_idx)
    cgps.clear()
    for (i <- 0 to num_cgps_to_evaluate - 1) {
      // Mutate
      var mutated_CGP = None
      cgps += mutated_CGP
    }

    // Evaluate mutated cgps
      //// should be threaded
    for (cgp <- cgps) {

    }
  }

  //  var n = new Node("+", 0)
  //  println(n.incoming)
  //  var nn = new Node("-", 1)
  //  n.add_in(nn)
  //  println(n.incoming)
  //  n.remove_in(nn)
  //  println(n.incoming)
}
