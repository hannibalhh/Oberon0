package Praktikum3.Memory 
import scala.collection.immutable.HashMap

object Declarations {
 
  case class IntConst(intval: Int) extends Descriptor
  case object IntConst
  case class Variable(address: String, _type: Type) extends Descriptor
  case object Variable
  case class ParameterVariable(address: String, _type: Type) extends Descriptor
  case object ParameterVariable
  case class Procedcure(name: String, startaddress: Int, lengthparblock: Int,
    framesize: Int, params: ParameterVariable) extends Descriptor
  case object Procedcure

  trait Type extends Descriptor
  case class ArrayType(numberOfElems: Int, basetype: Type) extends Type
  case object ArrayType
  case class RecordType(symbolTable: Map[String, Descriptor]) extends Type
  case object RecordType

  trait SimpleType extends Type{
    val name: String
  }
  case object IntegerType extends SimpleType{
    val name = "IntegerType"
  }
  case object StringType extends SimpleType{
    val name = "StringType"
  }
  case object BooleanType extends SimpleType{
    val name = "BooleanType"
  }

  object NilDescriptor extends Descriptor
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
//        if (rawClass != null && rawClass.getName().contains("Declarations")) {
//          s += ->(i.getName(), n + 1)
//          i.setAccessible(true)
//          val o: Descriptor = i.get(this).asInstanceOf[Descriptor]
//          s += o.print(n + 2)
//        } else {
//          i.setAccessible(true)
//          s += ->(i.getName() + "(" + i.get(this) + ")", n + 1)
//        }
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