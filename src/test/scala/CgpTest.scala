package test.scala

import main.scala.Cgp

object CgpTest extends App {
//  test_create_cgp()
  test_determine_nodes_to_process()

   // ===============TEST SUITE
  def test_create_cgp(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    println(cgp.node_list)
    var nodeCheck = cgp.node_list(cgp.node_list.length-1)
    println(nodeCheck.incoming)
    println(nodeCheck.incoming(0).number)
//    println(nodeCheck.incoming(1).number)
  }

  def test_determine_nodes_to_process(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp
    cgp.determine_nodes_to_process()
  }
}
