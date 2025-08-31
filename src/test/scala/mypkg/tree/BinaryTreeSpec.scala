package mypkg.tree

import org.scalatest.funsuite.AnyFunSuite
import mypkg.tree.Tree._
import mypkg.shape.GeometricShape._
import java.lang.Integer

class BinaryTreeSpec extends AnyFunSuite {
  test("insert integers into tree") {
    var tree: Tree[Int] =
      Node(Empty, 5, Empty)

    tree = tree.insert(3)
    tree = tree.insert(7)
    tree = tree.insert(1)
    tree = tree.insert(4)
    tree = tree.insert(6)
    tree = tree.insert(8)

    val inOrder = tree.fold(List.empty[Int])((l, v, r) => l ++ List(v) ++ r)

    assert(inOrder == List(1, 3, 4, 5, 6, 7, 8))
  }

  test("size of tree") {
    var tree: Tree[Int] = Node(Empty, 10, Empty)
    tree = tree.insert(5)
    tree = tree.insert(15)
    tree = tree.insert(3)
    tree = tree.insert(7)

    assert(tree.size == 5)
  }

  test("depth of tree") {
    var tree: Tree[Int] = Node(
      Node(Empty, 2, Empty),
      10,
      Node(Node(Empty, 1, Node(Empty, 4, Empty)), 3, Empty)
    )

    assert(tree.depth == 4)
  }

  test("filter tree - keep even") {
    var tree: Tree[Int] = Node(Empty, 10, Empty)
    tree = tree.insert(5)
    tree = tree.insert(15)
    tree = tree.insert(3)
    tree = tree.insert(7)
    tree = tree.insert(12)

    val filtered = tree.filter(_ % 2 == 0)

    val inOrder = filtered.fold(List.empty[Int])((l, v, r) => l ++ List(v) ++ r)
    assert(inOrder == List(10, 12))
  }

  test("merge two trees") {
    var t1: Tree[Int] = Node(Empty, 1, Empty)
    t1 = t1.insert(3)
    var t2: Tree[Int] = Node(Empty, 2, Empty)
    t2 = t2.insert(4)
    val merged = t1.merge(t2)

    val inOrder = merged.fold(List.empty[Int])((l, v, r) => l ++ List(v) ++ r)
    assert(inOrder == List(1, 3, 2, 4))
  }

  test("insert shapes into tree") {
    var tree: Tree[GeometricShape] = Node(Empty, new Circle(1), Empty)
    tree = tree.insert(new Square(1))
    tree = tree.insert(new Rectangle(2, 2))
    tree = tree.insert(new Circle(2))

    val inOrder =
      tree.fold(List.empty[GeometricShape])((l, v, r) => l ++ List(v) ++ r)
    val areas = inOrder.map(_.area)

    assert(areas.sorted.sameElements(areas))
  }

  test("filter shapes by area") {
    var tree: Tree[GeometricShape] = Node(Empty, new Circle(1), Empty)
    tree = tree.insert(new Square(2))
    tree = tree.insert(new Rectangle(2, 2))
    tree = tree.insert(new Circle(2))

    val filtered = tree.filter(_.area > 3.0)
    val areas =
      filtered.fold(List.empty[Double])((l, v, r) => l ++ List(v.area) ++ r)

    assert(areas.forall(_ > 3.0))
  }
}
