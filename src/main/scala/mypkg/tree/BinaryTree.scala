package mypkg.tree
import mypkg.shape.GeometricShape._

sealed trait Tree[+A] {
  import Tree._

  def size: Int =
    this.fold(0)((l, v, r) => l + 1 + r)

  def depth: Int =
    this.fold(0)((l, v, r) => 1 + Math.max(l, r))

  def insert[B >: A](x: B)(implicit ord: Ordering[B]): Tree[B] = this match {
    case Empty =>
      Node(Empty, x, Empty)
    case Node(left, value, right) => {
      if (ord.lt(x, value)) Node(left.insert(x), value, right)
      else if (ord.gt(x, value)) Node(left, value, right.insert(x))
      else this
    }
  }

  def fold[B](zero: B)(f: (B, A, B) => B): B = this match {
    case Empty => zero
    case Node(left, value, right) =>
      f(left.fold(zero)(f), value, right.fold(zero)(f))
  }

  def filter[B >: A](pred: B => Boolean)(implicit ord: Ordering[B]): Tree[B] =
    this match {
      case Empty => this
      case Node(left, value, right) => {
        if (pred(value)) Node(left.filter(pred), value, right.filter(pred))
        else left.filter(pred).merge(right.filter(pred))
      }
    }

  def merge[B >: A](tree: Tree[B])(implicit ord: Ordering[B]): Tree[B] =
    (this, tree) match {
      case (Empty, t) => t
      case (t, Empty) => t
      case (t1, t2) => {
        val (newLeft, max) =
          t1.pivot // make max of first tree the new root and stitch with the second tree
        Node(newLeft, max, t2)
      }
    }

  def pivot: (Tree[A], A) = this match {
    case Empty                    => throw new NoSuchElementException("no max")
    case Node(left, value, Empty) => (left, value)
    case Node(left, value, right) => {
      val (newRight, max) = right.pivot
      (Node(left, value, newRight), max)
    }
  }
}

object Tree {
  case object Empty extends Tree[Nothing]
  case class Node[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

  implicit val shapeOrdering: Ordering[GeometricShape] = Ordering.by(_.area)
}
