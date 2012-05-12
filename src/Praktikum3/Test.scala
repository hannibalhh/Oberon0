package Praktikum3
import Praktikum3.Tree.Ident
import Praktikum3.Declarations.IntConst

object Test {

  
  def main(args: Array[String]): Unit = {
    Declarations.start
    Declarations ! Ident(Symbol("",1,1))
    Declarations ! IntConst(1)
  }

}