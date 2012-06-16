package Praktikum4
import scala.collection.immutable.TreeMap
import cip.base.CodeGen

object Memory {

  import Declarations._
  // level -> symbolTabelle
  object SymbolTables {
    private val symbolTables = Array(Declarations.SymbolTable(new TreeMap[String, Descriptor]), Declarations.SymbolTable(new TreeMap[String, Descriptor]), Declarations.SymbolTable(new TreeMap[String, Descriptor]), Declarations.SymbolTable(new TreeMap[String, Descriptor]), Declarations.SymbolTable(new TreeMap[String, Descriptor]))

    def +(s: String, d: SimpleType) = {
      trace("Level:"+ Level.value + ": new entry: " + s + " -> " + d)
      symbolTables(Level.value) = symbolTables(Level.value) + (s, d)
    }

    def +(s: String, d: Descriptor) = {
      trace("Level:"+ Level.value + ": new entry: " + s + " -> " + d)
      symbolTables(Level.value) = symbolTables(Level.value) + (s, d)
      trace("currentAddress: " + currentAddress + " + " + d.size + " = " + (currentAddress + d.size) + "")
      incCurrAddr(d.size)
    }
    def apply(s: String,level:Int = Level.value):Option[Descriptor] = {
      val r = symbolTables(level)(s)
      if (r.isEmpty && level > 0){
        apply(s,level-1)
      }
      else
        r
    }
    
    def apply() = symbolTables(Level.value)
    override def toString = {
      var s = "\nSymbolTables:\n";
      for (i <- 0 until symbolTables.length) {
        val t = symbolTables(i).print(0)
        if (!t.isEmpty()) {
          s += "  Level " + i + "\n"
          s += t
        }
      }
      s
    }
//    var record = false
    private def incCurrAddr(i: Int) = {
      if (i > 0)
        currAddr += i
    }
    private var currAddr = 0
    def currentAddress = currAddr
    def trace(x: Any) = if (OberonDebug.symbolTables) println("Memory.SymbolTables: " + x)
  }
  def mainProgramLength = cip.TreeGenerator.lengthDataSegmentMainProgram
  def setMainProgramLength(i: Int) = cip.TreeGenerator.lengthDataSegmentMainProgram = i
  var mainProgramStart = 0
  var mainProgramStartSetted = false

  object Level {
    def inc = CodeGen.level += 1
    def dec = CodeGen.level -= 1
    def reset = CodeGen.level = 0
    def value = CodeGen.level
    override def toString = "Level(" + value + ")"
  }

  object Declarations {

    trait IntConstTrait extends Descriptor
    case object IntConst
    case class IntConst(intval: Int) extends IntConstTrait {
      def print(n: Int) = ->("IntConst(" + intval + ")" + sizeString, n)
      def toInt = intval
      override val size = 1
    }

    trait Variable extends Descriptor {
      val address: Int = 0
      val _type: Type 
      val isParameter: Boolean = false
      override val size = _type.size
      override def toInt = address
    }
    object Variable{
      def apply(address: Int, _type: Type,ref:Boolean) = {
        if (ref)
          ValueVariable(address,_type)
        else
          ReferenceVariable(address,_type)
      }
    }
    case object ValueVariable
    case class ValueVariable(override val address: Int, override val _type: Type) extends Descriptor with Variable {
      def print(n: Int) = ->("ValueVariable(address=" + address + ")" + sizeString, n) + _type.print(n + 2)
      override val isParameter = false
    }

    case object ReferenceVariable
    case class ReferenceVariable(override val  address: Int, override val  _type: Type) extends Descriptor with Variable {
      def print(n: Int) = ->("ReferenceVariable(address=" + address + ")" + sizeString, n) + _type.print(n + 1)
      override val isParameter = true
      override val size = 1
    }

    case object Procedcure
    case class Procedcure(name: String, startaddress: Int, lengthparblock: Int,
      var framesize: Int, params: SymbolTableTrait) extends Descriptor {
      def print(n: Int) = ->("Procedure(name=" + name + ",startaddress=" + startaddress + ",lengthparblock=" + lengthparblock + ",framesize=" + framesize + ")" + sizeString, n) + params.print(n + 1)
      override def toInt = startaddress
    }

    trait Type extends Descriptor
    case object ArrayType
    case class ArrayType(numberOfElems: Int, basetype: Type) extends Type {
      def print(n: Int) = ->("ArrayType(numberOfElems=" + numberOfElems + ")" + sizeString, n) + basetype.print(n + 1)
      override def toInt = numberOfElems
      override val size: Int = numberOfElems * basetype.size
    }

    trait SymbolTableTrait extends Descriptor {
      override val size: Int
      def +(s: String, d: Descriptor): SymbolTableTrait = NilDescriptor
      def +(d: Descriptor): SymbolTableTrait = NilDescriptor
      val symbolTable: Map[String, Descriptor] = new TreeMap
      def apply(s: String): Option[Descriptor] = None
      override def toInt: Int = symbolTable.size
    }

    case object SymbolTable
    case class SymbolTable(override val symbolTable: Map[String, Descriptor] = new TreeMap) extends SymbolTableTrait {
      def print(n: Int) = {
        var s = ""
        for ((x, y) <- symbolTable) {
          s += ->(x + "->" + y.print(n), n + 1)
        }
        s
      }
      override val size: Int = {
        var i = 0;
        for ((name, desc) <- symbolTable) {
          i += desc.size
        }
        i
      }

      override def +(s: String, d: Descriptor) = {
        SymbolTable(symbolTable + Tuple(s, d))
      }

      override def +(d: Descriptor): SymbolTableTrait = {

        d match {
          case t: SymbolTableTrait => {
            SymbolTable(symbolTable ++ t.symbolTable)
          }
          case x => {
            error("No SymbolTable found: " + x)
            NilDescriptor
          }
        }
      }

      override def apply(s: String): Option[Descriptor] = {
        symbolTable get s
      }

      override def toInt = symbolTable.size
    }

    case object RecordType
    case class RecordType(symbolTable: SymbolTableTrait, startAddress: Int = 0) extends Type {
      def print(n: Int) = ->("RecordType(startAddress=" + startAddress + ")" + sizeString, n) + symbolTable.print(n + 1)
      override def toInt = symbolTable.symbolTable.size
      override val size = symbolTable.size

    }

    trait SimpleType extends Type {
      val name: String = "None"
      override val size = 1
      def toInt = 1
    }
    case object IntegerType extends SimpleType {
      override val name = "IntegerType"
      def print(n: Int) = ->(name + sizeString, n)
    }
    case object StringType extends SimpleType {
      override val name = "StringType"
      def print(n: Int) = ->(name + sizeString, n)
    }
    case object BooleanType extends SimpleType {
      override val name = "BooleanType"
      def print(n: Int) = ->(name + sizeString, n)
    }

    object NilDescriptor extends Descriptor with SymbolTableTrait {
      def print(n: Int) = "NilDescriptor"
      override val size = 0
      override val isDefined = false
      override val isEmpTy = true
      override def toInt = 0
    }

    trait Descriptor {
      val size = 0;
      def sizeString = "(size=" + size + ")"
      val level = Level.value
      val isDefined = true
      val isEmpTy = false
      def print(n: Int): String
      def toInt: Int
      override def toString = print(0)
    }

    def ->(value: String, n: Int) = "	" * n + addNewLine(value)
    def trace(s: Any) = println("Memory:Declarations " + addNewLine(s))
    def addNewLine(a: Any) = {
      val s = a.toString()
      if (s.last == '\n')
        s
      else
        s + "\n"
    }

  }
  def trace(s: Any) = if (OberonDebug.memory) println("Memory: " + s)
}