import scala.collection.mutable.ListBuffer
import scala.util.Random

class Cgp (input: Int, output: Int, ar: Int, level: Int, row: Int, col: Int) {
  def num_input = input
  def num_output = output
  def arity = ar
  def lv_back = level
  def num_row = row
  def num_col = col
  var node_list = ListBuffer[Node]()

  def create_cgp(): Unit = {
    // for node grid (inner nodes)
    var col_start = 0
    for( i <- num_input to (num_input - 1) + (this.num_row * this.num_col)){
      if ((i - input) % num_col == 0) {
        col_start += 1
      }
      var inner_node = new Node(random_function(), i, col_start)
      this.node_list += inner_node
//      println(inner_node.number + " " + inner_node.col_where)
    }

    // randomly connect nodes

  }

  def random_function(): String = {
    val x = List("+", "-", "*", "/")
    x.apply(Random.nextInt(x.size))
  }
}
