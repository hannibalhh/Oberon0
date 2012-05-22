package Praktikum3
import scala.collection.immutable.HashMap
import cip.base.CodeGen

object Memory {
  
  import Declarations._
  				  // level -> symbolTabelle
  var symbolTables = List(new HashMap[String, Descriptor])
  var lengthDataSegmentMainProgram = 0
  var curraddr = 0
  
  object Declarations {

    def ->(value: String, n: Int) = "	" * n + value + "\n"

    case object IntConst
    case class IntConst(intval: Int) extends Descriptor {
      def print(n: Int) = {
        ->("IntConst(" + intval + ")", n)
      }
    }

    case object Variable
    case class Variable(address: String, _type: Type) extends Descriptor {
      def print(n: Int) = {
        ->("Variable(address=" + address + ")", n) +
          _type.print(n + 1)
      }
    }

    case object ParameterVariable
    case class ParameterVariable(address: String, _type: Type) extends Descriptor {
      def print(n: Int) = {
        ->("ParameterVariable(address=" + address + ")", n) +
          _type.print(n + 1)
      }
    }

    case object Procedcure
    case class Procedcure(name: String, startaddress: Int, lengthparblock: Int,
      framesize: Int, params: ParameterVariable) extends Descriptor {
      def print(n: Int) = {
        ->("Procedcure(name=" + name + "startaddress=" + startaddress + "lengthparblock=" + lengthparblock + "framesize=" + framesize + ")", n) +
          params.print(n + 1)
      }
    }

    trait Type extends Descriptor
    case object ArrayType
    case class ArrayType(numberOfElems: Int, basetype: Type) extends Type {
      def print(n: Int) = {
        ->("ArrayType(numberOfElems=" + numberOfElems + ")", n) +
          basetype.print(n + 1)
      }
    }

    case object RecordType
    case class RecordType(symbolTable: Map[String, Descriptor]) extends Type {
      def print(n: Int) = {
        ->("RecordType", n) +
          ->(symbolTable.toString, n + 1)
      }
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
      def print(n: Int) = ""
    }
    trait Descriptor {

      val size = 0;
      def level = CodeGen.level

      def print(n: Int): String
      override def toString = print(0)
    }
  }

}