package Praktikum2

object Tree {

  sealed trait Tree[+T] {
    def value: Option[T]
    def left: Tree[_]
    def right: Tree[_]
  }

  case class Node[+T](value: Option[T], left: Tree[T], right: Tree[T]) extends Tree[T]

  case object Nil extends Tree[Nothing] {
    override def toString = "."
    def left = Nil
    def right = Nil
    def value = None
  }

  def apply[T](value: T = None, l: Tree[T] = Nil, r: Tree[T] = Nil): Node[T] = {
    value match {
      case x: Option[T] => Node(x, l, r)
      case x: T => Node(Some(value), l, r)
    }
  }
}