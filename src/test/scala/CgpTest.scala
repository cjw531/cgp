package test.scala

import main.scala.Cgp

object CgpTest extends App {
  test_create_cgp()

  // ===============TEST SUITE
  def test_create_cgp(): Unit = {
    var cgp = new Cgp(2, 1, 2, 2, 3, 3)
    cgp.create_cgp()
    println(cgp.node_list)
    var nodeCheck = cgp.node_list(8)
    println(nodeCheck.incoming)
    println(nodeCheck.incoming(0).number)
    println(nodeCheck.incoming(1).number)
  }
}
