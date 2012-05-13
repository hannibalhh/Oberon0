package Praktikum3.Memory
import Praktikum3.Tree._
import scala.actors.Actor

object Declarations extends Actor {

  def act {
    loop {
      receive {
        case a: IntConst => println(a)
        case b: Var => println(b)
        case c: Proc => println(c)
        case d: Array => println(d)
        case e: Record => println(e)
        case f: Integer => println(f)
        case g: Str => println(g)
        case h: Bool => println(h)
        case 'Stop => exit 
        case x => println("Invalid Message: " + x)
      }
    }
  }
  case class SymbolTable(map: Map[Ident, Descriptor])
  case object SymbolTable
  case class IntConst(intval: Int) extends Descriptor
  case object IntConst
  case class Var(Address: String, isvarpar: Boolean, _typ: Type) extends Descriptor
  case object Var
  case class Proc(name: String, startaddress: Int, lengthparblock: Int,
    framesize: Int, params: Var) extends Descriptor
  case object Proc

  trait Type extends Descriptor
  case class Array(numberelems: Int, basetype: Type) extends Type
  case object Array
  case class Record(symboltable: SymbolTable) extends Type
  case object Record

  trait SimpleType extends Type
  case class Integer(int: Int) extends SimpleType
  case class Str(string: String) extends SimpleType
  case class Bool(bool: Boolean) extends SimpleType

  trait Descriptor {
    def print(n: Int): String = {
      def after$(s: String) = {
        val i = s.indexOf("$")
        if (i > 0)
          s.substring(s.indexOf("$") + 1)
        else
          s
      }
      def ->(value: String, m: Int = n): String = "	" * m + after$(value) + "\n"
      val c = this.getClass()
      var s = ->(c.getName)
      for (i <- c.getDeclaredFields()) {
        val rawClass = i.getType.getEnclosingClass()
        if (rawClass != null && rawClass.getName().contains("Declarations")) {
          s += ->(i.getName(), n + 1)
          i.setAccessible(true)
          val o: Descriptor = i.get(this).asInstanceOf[Descriptor]
          s += o.print(n + 2)
        } else {
          i.setAccessible(true)
          s += ->(i.getName() + "(" + i.get(this) + ")", n + 1)
        }
      }
      return s
    }
    override def toString = print(0)
  }

  //  val id = Tree.Ident(Symbol("",1,1))
  //  val id2 = Ident(Symbol("a",2,2))
  //  val b = Bool(false)
  //  val arr = Array(2,b)
  //  println(arr)
  //  println(SymbolTable(Map()).map + ((id,1),(b,2)))

}