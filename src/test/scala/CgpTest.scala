//package test.scala
//
//import main.scala.{Cgp, Functions}
//
//object CgpTest extends App {
//  /* TEST SUITE */
//  test_add()
//  test_subtract()
//  test_multiply()
//  test_divide()
//  //  test_create_cgp()
//  //  test_find_active_nodes()
//  //  test_decode_cgp()
//  //  test_mutate_cgp()
//  //  test_generate_sample_of_points()
//  //  test_mutate_wrapper()
////  test_evaluate()
//
//  def test_add(): Unit = {
//    var f = new Functions()
//    var test_vals = List(4, 8, 10, 2)
//    assert(f.add(test_vals) == 24)
//  }
//
//  def test_subtract(): Unit = {
//    var f = new Functions()
//    var test_vals = List(4, 8, 10, 2)
//    assert(f.subtract(test_vals) == -16)
//  }
//
//  def test_multiply(): Unit = {
//    var f = new Functions()
//    var test_vals = List(4, 8, 10, 2)
//    assert(f.multiply(test_vals) == 640)
//  }
//
//  def test_divide(): Unit = {
//    var f = new Functions()
//    var test_vals = List(4, 8, 10, 2)
//    assert(f.divide(test_vals) == 0)
//  }
//
//  def test_test(): Unit = {
//    var f = new Functions()
//    var test = List(f.add, f.subtract, f.multiply, f.divide)
//    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
//    cgp.create_cgp()
//  }
//
//  /* TEST 1: create random cgp graph */
//  def test_create_cgp(): Unit = {
//    var f = new Functions()
//    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
//    cgp.create_cgp()
//    println(cgp.node_list)
//    var nodeCheck = cgp.node_list(cgp.node_list.length - 1)
//    println(nodeCheck.incoming)
//    println(nodeCheck.incoming(0).number)
//    //    println(nodeCheck.incoming(1).number)
//  }
//
//  /* TEST 2: Decode cgp */
//  def test_find_active_nodes(): Unit = {
//    var f = new Functions()
//    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
//    cgp.create_cgp()
//    cgp.find_active_nodes()
////    println(cgp.active_node)
//  }
//
//  /* TEST 3: Mutate cgp */
//  def test_mutate_cgp(): Unit = {
//    var f = new Functions()
//    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
//    cgp.create_cgp()
//    var test_node = cgp.node_list(5)
//
//    println("Before--Edge Mutation:")
//    println("Incoming: " + test_node.incoming)
//    cgp.mutate_incoming_edges(test_node)
//    println("After--Edge Mutation:")
//    println("Incoming: " + test_node.incoming)
//
//    println("Before--Operator Mutation:")
//    println("Operator: " + test_node.func_idx)
//    cgp.mutate_operator(test_node)
//    println("After--Operator Mutation:")
//    println("Operator: " + test_node.func_idx)
//
//    println("Before--Node Mutation:")
//    println("Incoming Edges: " + test_node.incoming)
//    println("Operator: " + test_node.func_idx)
//    cgp.mutate_node(test_node)
//    println("After--Node Mutation:")
//    println("Incoming Edges: " + test_node.incoming)
//    println("Operator: " + test_node.func_idx)
//  }
//
////  /* TEST 4: */
////  def test_generate_sample_of_points(): Unit = {
////    var f = new Functions()
////    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
////    cgp.create_cgp()
////    cgp.generate_sample_of_points()
////    println(cgp.sample_points)
////  }
//
////  def test_evaluate(): Unit = {
////    var f = new Functions()
////    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
////    cgp.create_cgp()
////    cgp.generate_sample_of_points()
////    cgp.evaluate_true_values()
////
////  }
//
//  /* TEST 5: test mutation wrapper method */
//  def test_mutate_wrapper(): Unit = {
//    var f = new Functions()
//    var cgp = new Cgp(List(0,1), 1, 2, 2, 3, List(f.add, f.subtract,f.multiply,f.divide))
//    cgp.create_cgp()
//    println("BEFORE--")
//    for (n <- cgp.node_list) println(n.number + "'s edges: " + n.incoming)
//    cgp.mutate_cgp(0.7, true, false)
//    println("AFTER--")
//    for (n <- cgp.node_list) println(n.number + "'s edges: " + n.incoming)
//  }
//}
