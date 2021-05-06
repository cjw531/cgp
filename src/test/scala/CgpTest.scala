package test.scala

import main.scala.Cgp

object CgpTest extends App {
  /* TEST SUITE */
  test_create_cgp()
//  test_decode_cgp()

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
  def test_decode_cgp(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    cgp.decode_cgp()
    println(cgp.active_node)
  }
}
