package test.scala

import main.scala.Cgp

object CgpTest extends App {
  /* TEST SUITE */
//  test_create_cgp()
//  test_find_active_nodes()
//  test_decode_cgp()
//  test_mutate_cgp()
  test_generate_sample_of_points()

  /* TEST 1: create random cgp graph */
  def test_create_cgp(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    println(cgp.node_list)
    var nodeCheck = cgp.node_list(cgp.node_list.length - 1)
    println(nodeCheck.incoming)
    println(nodeCheck.incoming(0).number)
    //    println(nodeCheck.incoming(1).number)
  }

  /* TEST 2: Decode cgp */
  def test_find_active_nodes(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    cgp.find_active_nodes()
    println(cgp.active_node)
  }

  /* TEST 3: Mutate cgp */
  def test_mutate_cgp(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    var test_node = cgp.node_list(5)

    println("Before--Edge Mutation:")
    println("Incoming: " + test_node.incoming)
    cgp.mutate_incoming_edges(test_node)
    println("After--Edge Mutation:")
    println("Incoming: " + test_node.incoming)

    println("Before--Operator Mutation:")
    println("Operator: " + test_node.operator)
    cgp.mutate_operator(test_node)
    println("After--Operator Mutation:")
    println("Operator: " + test_node.operator)

    println("Before--Node Mutation:")
    println("Incoming Edges: " + test_node.incoming)
    println("Operator: " + test_node.operator)
    cgp.mutate_node(test_node)
    println("After--Node Mutation:")
    println("Incoming Edges: " + test_node.incoming)
    println("Operator: " + test_node.operator)
  }

  def test_generate_sample_of_points(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    cgp.generate_sample_of_points()
  }
}
