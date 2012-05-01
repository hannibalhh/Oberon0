package Praktikum2

object Tree {

  sealed trait Tree[+T] {
    def value: Option[T]
    def left: Tree[_]
    def right: Tree[_]

    def print(n: Int): String = {
      def ->(t: Tree[_]): String = "	"* n + t.value.getOrElse(None) + "\n" 
      -> (this) + left.print(n + 1) + right.print(n + 1)
    }

    override def toString = "AbstractSyntaxTree:\n" +print(0)
  }

  case class Node[+T](value: Option[T], left: Tree[T], right: Tree[T]) extends Tree[T]

  case object Nil extends Tree[Nothing] {
    def left = Nil
    def right = Nil
    def value = None
    
    override def toString = "."
    override def print(n:Int = 0) = ""
  }

  def apply[T](value: T = None, l: Tree[T] = Nil, r: Tree[T] = Nil): Node[T] = {
    value match {
      case x: Option[T] => Node(x, l, r)
      case x: T => Node(Some(value), l, r)
    }
  }
}