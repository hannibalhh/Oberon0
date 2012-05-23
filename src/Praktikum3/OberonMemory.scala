package Praktikum3
import scala.collection.immutable.HashMap
import cip.base.CodeGen

object Memory {
  
  import Declarations._
  				  // level -> symbolTabelle
  object SymbolTables{
      var symbolTables = Array(new HashMap[String, Descriptor])
      def +(s: String, d:Descriptor) = {
        trace("+ map")
        trace(Tuple(s,d))
        trace(symbolTables(Level.value) + Tuple(s,d))
        trace(symbolTables(Level.value))
        symbolTables(Level.value) = symbolTables(Level.value) + Tuple(s,d)
      }
      def apply(s:String) = symbolTables(Level.value).get(s)
      def apply() = symbolTables(Level.value)
      override def toString = "SymbolTable:\n" + symbolTables.deep.toString
  }
  var lengthDataSegmentMainProgram = 0
  var curraddr = 0
  
  object Level{
    def inc = CodeGen.level +=1
    def dec = CodeGen.level -=1
    def reset = CodeGen.level = 0
    def value = CodeGen.level
    override def toString = "Level(" + value + ")"
  }
  
  object Declarations {
    case object IntConst
    case class IntConst(intval: Int) extends Descriptor {
      def print(n: Int) = ->("IntConst(" + intval + ")", n)
    }
    trait VariableDescriptor{
      val address: String
      val _type: Type
      val isParameter: Boolean
    }
    case object Variable
    case class Variable(address: String, _type: Type) extends Descriptor with VariableDescriptor {
      def print(n: Int) = ->("Variable(address=" + address + ")", n) + _type.print(n + 1)
      val isParameter = false
    }

    case object ParameterVariable
    case class ParameterVariable(address: String, _type: Type) extends Descriptor with VariableDescriptor{
      def print(n: Int) = ->("ParameterVariable(address=" + address + ")", n) + _type.print(n + 1)
      val isParameter = true
    }

    case object Procedcure
    case class Procedcure(name: String, startaddress: Int, lengthparblock: Int,
      framesize: Int, params: ParameterVariable) extends Descriptor {
      def print(n: Int) = ->("Procedcure(name=" + name + "startaddress=" + startaddress + "lengthparblock=" + lengthparblock + "framesize=" + framesize + ")", n) + params.print(n + 1)
    }

    trait Type extends Descriptor
    case object ArrayType
    case class ArrayType(numberOfElems: Int, basetype: Type) extends Type {
      def print(n: Int) = ->("ArrayType(numberOfElems=" + numberOfElems + ")", n) + basetype.print(n + 1)
    } 

    case object RecordType
    case class RecordType(symbolTable: Map[String, Descriptor]) extends Type {
      def print(n: Int) = ->("RecordType", n) + ->(symbolTable.toString, n + 1)
    }

    trait SimpleType extends Type {
      val name: String
    }
    case object IntegerType extends SimpleType {
      val name = "IntegerType"
      def print(n: Int) = ->(name, n)
    }
    case object StringType extends SimpleType {
      val name = "StringType"
      def print(n: Int) = ->(name, n)
    }
    case object BooleanType extends SimpleType {
      val name = "BooleanType"
      def print(n: Int) = ->(name, n)
    }

    object NilDescriptor extends Descriptor {
      def print(n: Int) = "NilDescriptor"
    }
    
    trait Descriptor {
      val size = 0;
      val level = Level.value

      def print(n: Int): String
      override def toString = print(0)
    }
    
    def ->(value: String, n: Int) = "	" * n + value + "\n"
    def trace(s: Any) = println("Memory:Declarations " + s)

  }
  def trace(s: Any) = println("Memory: " + s)
}