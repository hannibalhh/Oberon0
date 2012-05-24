package Praktikum3

object Test extends App {

  println(B)
  println(new a)
  B.x = 2
  println(B)
  println(new a)
  
  trait ta {
    val foo = B.x
  }
  class a extends ta{
    override def toString = "a(" + foo + ")"
  }
  
  case object B {
    var x = 1
  }

}
