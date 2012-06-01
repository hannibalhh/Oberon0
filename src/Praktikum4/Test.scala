package Praktikum4

object Test extends App {
  
  // Arrays
  // Records
  // Statements
  // Procedure (optional)

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
