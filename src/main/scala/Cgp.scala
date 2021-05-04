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
    // add input nodes to the node list
    for (i <- 0 to (input - 1)) {
      var input_node = new Node("", i, 0)
      this.node_list += input_node
    }

    // for node grid (inner nodes, without input nodes)
    var col_start = 0 // n-th col #
    for (i <- num_input to (num_input - 1) + (this.num_row * this.num_col)) {
      if ((i - input) % num_col == 0) {
        col_start += 1
      }
      var inner_node = new Node(random_function(), i, col_start)
      this.node_list += inner_node // add node to the list
    }

    // randomly connect nodes
    for (i <- num_input to (num_input - 1) + (this.num_row * this.num_col)) {
      var node = this.node_list(i)
      var col_from = node.col_where - this.arity
      if (col_from < 0) col_from = 0
      var col_to = node.col_where - 1
      var subset = get_node_subset(col_from, col_to)
      for (n <- subset) println(i + " Node:" + n.number + " Col#:" + n.col_where)
    }
  }

  def get_node_subset(from: Int, to: Int): ListBuffer[Node] = {
    var subset = ListBuffer[Node]()
    for (node <- this.node_list) {
      if (node.col_where >= from && node.col_where <= to) {
        subset += node
      }
      // TODO: think of an optimization here
      // TODO: e.g. early exit from the loop?
    }

    var adjacent = ListBuffer[Node]()
    for (i <- 1 to this.arity) { // pick arity # of random nodes
      adjacent += subset.apply(Random.nextInt(subset.size))
    }
    return adjacent // return the node that has a connection
  }

  /* returns random operations for each node */
  def random_function(): String = {
    val x = List("+", "-", "*", "/")
    x.apply(Random.nextInt(x.size))
  }
}
