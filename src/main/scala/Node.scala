import scala.collection.mutable.ListBuffer

class Node (op: String, num: Int, col_index: Int) {
  def operator = op
  def number = num
  var incoming = ListBuffer[Node]()
  var outgoing = ListBuffer[Node]()
  def col_where = col_index

  def add_in (new_node: Node): Unit = {
    this.incoming += new_node
  }

  def remove_in (removal: Node): Unit = {
    this.incoming -= removal
  }

}
