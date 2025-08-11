import BinaryTree._
import GeometricShape._
import java.lang.Integer

object Main extends App {
  var start: BinaryTree[Integer] = Node(Integer.valueOf(1), Empty, Empty)
  start = start.insert(Integer.valueOf(2))
  start = start.insert(Integer.valueOf(5))
  start = start.insert(Integer.valueOf(4))
  start = start.insert(Integer.valueOf(3))
  start = start.insert(Integer.valueOf(9))
  start = start.insert(Integer.valueOf(7))
  start = start.insert(Integer.valueOf(8))
  start = start.insert(Integer.valueOf(6))
  println(start)
}
