sealed trait BinaryTree[+A] {
  import BinaryTree._

  def size: Int =
    fold(this, 0, (_: A, leftRes: Int, rightRes: Int) => 1 + leftRes + rightRes)

  def depth: Int =
    fold(
      this,
      -1,
      (_: A, leftRes: Int, rightRes: Int) => 1 + Math.max(leftRes, rightRes)
    )

  def insert[B >: A <: Comparable[B]](newValue: B): BinaryTree[B] = this match {
    case Empty =>
      Node(newValue, Empty, Empty)
    case Node(value, left, right) =>
      val cmp = newValue.compareTo(value)
      if (cmp < 0) Node(value, left.insert(newValue), right)
      else if (cmp > 0) Node(value, left, right.insert(newValue))
      else this
  }
}

object BinaryTree {
  case object Empty extends BinaryTree[Nothing]
  case class Node[A](value: A, left: BinaryTree[A], right: BinaryTree[A])
      extends BinaryTree[A]

  def fold[A, B](tree: BinaryTree[A], initial: B, fn: (A, B, B) => B): B =
    tree match {
      case Empty => initial
      case Node(value, left, right) =>
        fn(value, fold(left, initial, fn), fold(right, initial, fn))
    }
}
