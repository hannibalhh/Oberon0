package Praktikum4
import Praktikum4.Memory.Declarations.Descriptor
import scala.collection.immutable.HashMap
import scala.annotation.tailrec

@tailrec
object Tree {

  case object Nil extends Tree[Nothing] with Expression with Statement with Declarations with FormalParameters with ConstIdent with Type with Field with FieldList with ProcedureDeclaration with Ident {
    override def toString = "."
    override def print(n: Int): String = ""
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = Memory.Declarations.NilDescriptor
    override def isDefined = false
  }

  case object Integer
  case class Integer(int: Symbol) extends Tree[Integer] with Expression with IndexExpression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Integer")
      OberonInstructions.IntegerVal(int.value.get.toString.toInt)
      Memory.Declarations.IntegerType
    }

    override def num = int.value.get.toString.toInt

    override def print(n: Int) = ->("Integer(" + int + ")", n)
  }

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  trait Ident extends Tree[Ident] with Expression with Type with Declarations with ConstIdent {
    val identIdent: Symbol = Symbol("", -1, -1)
    val optionalIdent: Expression = Nil
    override def num = Memory.SymbolTables(identIdent.value.toString).get.toInt
  }

  case object Content
  case class Content(address: Expression) extends Ident {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Content")
      def compileIdent(i: Ident): Memory.Declarations.Descriptor = {
        val t = address.compile(symbolTable)
        val e = Memory.SymbolTables(i.identIdent.value.get.toString)
        trace(e)
        e.get match {
          case e: ConstIdent => 
          case c: Memory.Declarations.IntConst => OberonInstructions.IntegerVal(c.intval)
          case _ => {
            t match {
              case v: Memory.Declarations.Variable => {
                v._type match {
                  case a: Memory.Declarations.ArrayType => OberonInstructions.ContInstruction(a.basetype.size)
                  case _ => OberonInstructions.ContInstruction(v._type.size)
                }
              }
              case x => error("Content: VariableDeclaration", x)
            }
          }
        }
        t
      }
      address match {
        case r: RecordReference => {
          val t = r.compile()
          OberonInstructions.ContInstruction(t.size)
          t
        }
        case arrref: ArrayReference => {
          arrref.ident match {
            case i: Ident => compileIdent(i)
            case x => {
              error("Content: Ident", x)
              Memory.Declarations.NilDescriptor
            }
          }
          //          arrref.expr.compile()
        }
        case i: Ident => {
          compileIdent(i)
        }
        case _ => {
          val t = Memory.Declarations.IntegerType
          OberonInstructions.ContInstruction(t.size)
          t
        }
      }
    }

    override def print(n: Int) = ->("Content", n) + address.print(n + 1)
  }

  case object Ident
  case class IdentNode(override val identIdent: Symbol, override val optionalIdent: Expression = Nil) extends Tree[Ident] with Ident {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap): Memory.Declarations.Descriptor = {
      trace("IdentNode(" + identIdent + ")")
      val eOp =
        if (!Memory.SymbolTables.record) // global
          Memory.SymbolTables(identIdent.value.get.toString)
        else { // record
          if (symbolTable.size != 1) {
            error("one elem in map is supported, a record name", symbolTable)
            Memory.Declarations.NilDescriptor
          }
          val (name, desc) = symbolTable.first
          val memValue = Memory.SymbolTables(name)
          if (memValue.isEmpty) {
            error(name + " in symboltable ", memValue)
            None
          } else {
            Memory.SymbolTables(name).get match {
              case r: Memory.Declarations.RecordType => r.symbolTable(identIdent.value.get.toString)
              case x => {
                error("IdentNode: RecordType", symbolTable)
                None
              }
            }
          }
        }
      if (eOp.isEmpty)
        Memory.Declarations.NilDescriptor
      else {
        val e = eOp.get
        e match {
          case x: Memory.Declarations.Type => x
          case x: Memory.Declarations.VariableDescriptor => {
            trace("do variable: " + Memory.SymbolTables.record)
            val addr = x.address
            val t = x._type
            OberonInstructions.IntegerVal(addr)
            if (e.level > 0) {
              if (e.level == Memory.Level.value) {
                OberonInstructions.GetFP
                OberonInstructions.AdditionInstruction
              } else {
                OberonInstructions.IntegerVal(e.level)
                OberonInstructions.GetSL
                OberonInstructions.AdditionInstruction
              }
            }
            if (x.isParameter) {
              OberonInstructions.ContInstruction(1)
            }
            x
          }
          case x: Memory.Declarations.IntConst => {
            OberonInstructions.IntegerVal(x.intval)
            Memory.Declarations.IntegerType
          }
        }
      }
    }

    override def print(n: Int) = ->("IdentNode(" + identIdent + ")", n) + optionalIdent.print(n + 1)
  }

  // string 		 = ...
  case object Str
  case class Str(string: Symbol) extends Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Str")
      OberonInstructions.StringVal(string.value.get.toString)
      Memory.Declarations.StringType
    }

    override def print(n: Int) = ->("Str(" + string + ")", n)
  }

  //Read             = READ [Prompt].
  case object Read
  case class Read(prompt: Expression) extends Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Read")
      Memory.Declarations.IntegerType
    }
    override def print(n: Int) = ->("Read", n) + prompt.print(n + 1)
  }

  //Prompt           = string.
  case object Prompt
  case class Prompt(stringNode: Str) extends Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Prompt")
      Memory.Declarations.IntegerType
    }
  }

  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ.
  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}.
  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression].

  trait Expression extends Statement {
    val left: Expression = Nil
    val right: Expression = Nil
    def value: String = "Expression"

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Expression -> " + value)
      val dl = left.compile(symbolTable)
      right.compile(symbolTable)
      compileOperator
      dl
    }

    override def print(n: Int) = ->(value, n) + left.print(n + 1) + right.print(n + 1)

    def compileOperator = ()
    def *(expr: Expression): Expression = new *(this, expr)
    def /(expr: Expression): Expression = new /(this, expr)
    def +(expr: Expression): Expression = new +(this, expr)
    def -(expr: Expression): Expression = new -(this, expr)
    def :=(expr: Expression): Expression = new :=(this, expr)
    //    def :#(expr: Expression): Expression = new :#(this, expr)
    def <(expr: Expression): Expression = new <(this, expr)
    def <=(expr: Expression): Expression = new <=(this, expr)
    def >(expr: Expression): Expression = new >(this, expr)
    def >=(expr: Expression): Expression = new >=(this, expr)
  }

  case class *(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.MultiplicationInstruction
    override def value = "*"
  }
  case class /(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.DivisionInstruction
    override def value = "/"
  }
  case class +(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.AdditionInstruction
    override def value = "+"
  }
  case class -(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.SubtractionInstruction
    override def value = "-"
  }
  case class :=(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.EqualsInstruction
    override def value = "="
  }
  //  case class :#(override val left: Expression, override val right: Expression) extends Expression{
  //    def compileOperator = CodeGen.outInstr(new ())
  //  }
  case class <(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.LessThanInstruction
    override def value = "<"
  }
  case class <=(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.LessEqualThanInstruction
    override def value = "<="
  }
  case class >(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.GreaterThanInstruction
    override def value = ">"
  }
  case class >=(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = OberonInstructions.GreaterEqualThanInstruction
    override def value = ">="
  }

  case class Neg(override val left: Expression) extends Expression {
    override def compileOperator = {
      trace("Negative")
      OberonInstructions.IntegerVal(-1)
      OberonInstructions.MultiplicationInstruction
    }
    override def value = "Negative"
  }

  //ActualParameters  = Expression {Õ,Õ Expression}.
  case object ActualParameters
  case class ActualParameters(expressionActualParameters: Expression, actualParameters: Tree[ActualParameters]) extends Tree[ActualParameters] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ActualParameters")
      Memory.Declarations.IntegerType
    }
    override def print(n: Int) = ->("ActualParameters", n) + expressionActualParameters.print(n + 1) + actualParameters.print(n + 1)
  }

  //IndexExpression  = integer | ConstIdent.
  trait IndexExpression extends Tree[IndexExpression] {
    def num = Int.MinValue
  }

  //ConstIdent       = ident.
  trait ConstIdent extends IndexExpression {
    override def num = Int.MinValue
  }

  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  case object ArrayType
  case class ArrayType(elemArrayType: IndexExpression, _typeArrayType: Type) extends Type {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ArrayType")
      val num = elemArrayType.num
      val desc = _typeArrayType.compile()
      val size = desc.size
      desc match {
        case t: Memory.Declarations.Type => Memory.Declarations.ArrayType(size * num, t)
        case x => {
          error("ArrayType: Type Descriptor", x)
          Memory.Declarations.NilDescriptor
        }
      }
    }
    override def print(n: Int) = ->("ArrayType", n) + elemArrayType.print(n + 1) + _typeArrayType.print(n + 1)
  }

  case object ArrayReference
  case class ArrayReference(ident: Expression, expr: Expression) extends Tree[ArrayReference] with Expression {

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ArrayReference")
      val basetype = ident.compile()
      expr.compile()
      OberonInstructions.IntegerVal {
        basetype match {
          case v: Memory.Declarations.Variable => {
            v._type match {
              case a: Memory.Declarations.ArrayType => {
                a.basetype.size
              }
              case _ => {
                basetype.size
              }
            }
          }
          case x => {
            error("ArrayReference: VariableDescriptor", x)
            Int.MinValue
          }
        }
      }
      OberonInstructions.MultiplicationInstruction
      OberonInstructions.AdditionInstruction
      Memory.Declarations.NilDescriptor
      basetype
    }

    override def print(n: Int) = ->("ArrayReference", n) + ident.print(n + 1) + expr.print(n + 1)
  }

  // FieldListList
  trait FieldList extends Tree[FieldList] {
    val fields: Field = Nil
    val nextFieldList: FieldList = Nil
    override def print(n: Int) = ->("FieldList", n) + fields.print(n + 1) + nextFieldList.print(n + 1)
  }

  case object FieldListNode
  case class FieldListNode(override val fields: Field, override val nextFieldList: FieldList) extends FieldList {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("FieldListNode")
      Memory.Declarations.SymbolTable() + fields.compile(symbolTable) + nextFieldList.compile(symbolTable)
    }
  }

  //FieldList = [IdentList Õ:Õ Type].
  trait Field extends Tree[Field] {
    val idlField: Ident = Nil
    val _typeField: Type = Nil
    override def print(n: Int) = ->("Field", n) + idlField.print(n + 1) + _typeField.print(n + 1)
  }

  case object FieldNode
  case class FieldNode(override val idlField: Ident, override val _typeField: Type) extends Field {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("FieldNode")
      val t = _typeField.compile()
      def compileIdent(expr: Expression): Memory.Declarations.SymbolTableTrait = {
        if (expr.isDefined) {
          expr match {
            case id: Tree.Ident => {
              t match {
                case tt: Memory.Declarations.Type => {
                  Memory.Declarations.SymbolTable() + (id.identIdent.value.get.toString, t) + compileIdent(id.optionalIdent)
                }
                case x => {
                  error("FieldNode: Type", x)
                  Memory.Declarations.NilDescriptor
                }
              }
            }
            case x => {
              error("FieldNode: Ident", x)
              Memory.Declarations.NilDescriptor
            }
          }
        } else
          Memory.Declarations.NilDescriptor
      }
      Memory.Declarations.SymbolTable(symbolTable) + compileIdent(idlField)
    }

    override def print(n: Int) = ->("FieldNode", n) + idlField.print(n + 1) + _typeField.print(n + 1)
  }

  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  case object RecordType
  case class RecordType(fieldsRecordType: FieldList) extends Type {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("RecordType")
      fieldsRecordType.compile() match {
        case t: Memory.Declarations.SymbolTableTrait => Memory.Declarations.RecordType(t)
        case x => {
          error("SymbolTable", x)
          Memory.Declarations.NilDescriptor
        }
      }
    }

    override def print(n: Int) = ->("RecordType", n) + fieldsRecordType.print(n + 1)
  }

  case object RecordReference
  case class RecordReference(record: Ident, field: Expression) extends Tree[RecordReference] with Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("RecordRefNode")
      val a = record.compile(symbolTable) match {
        case recordTable: Memory.Declarations.RecordType => {
          OberonInstructions.IntegerVal(recordTable.startAddress)
          Memory.SymbolTables.record = true
          val f = field.compile(new HashMap() + Tuple(record.identIdent.value.get.toString, recordTable))
          Memory.SymbolTables.record = false
          f
        }
        case x => {
          error("RecordReference: Record Declaration", x)
          Memory.Declarations.NilDescriptor
        }
      }
      OberonInstructions.AdditionInstruction
      a
    }

    override def print(n: Int) = ->("RecordReference", n) + record.print(n + 1) + field.print(n + 1)
  }

  //Type = ident | ArrayType | RecordType.
  trait Type extends Tree[Type]

  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  case object FPSection
  case class FPSection(override val identFPSection: Ident, override val _typeFPSection: Type, override val optionalFPSection: FormalParameters = Nil) extends FormalParameters {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("FPSection")
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("FPSection", n) + identFPSection.print(n + 1) + _typeFPSection.print(n + 1) + optionalFPSection.print(n + 1)
  }

  //FormalParameters = FPSection {Õ;Õ FPSection}.
  trait FormalParameters extends Tree[FormalParameters] {
    val optionalFPSection: FormalParameters = Nil
    val identFPSection: Ident = Nil
    val _typeFPSection: Type = Nil
    override def print(n: Int) = ->("FormalParameters", n) + optionalFPSection.print(n + 1) + identFPSection.print(n + 1) + _typeFPSection.print(n + 1)
  }

  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  case object ProcedureHeading
  case class ProcedureHeading(identProcedureHeading: Ident, formalParameters: FormalParameters = Nil) extends Tree[ProcedureHeading] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ProecureHeading")
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("ProcedureHeading", n) + identProcedureHeading.print(n + 1) + formalParameters.print(n + 1)
  }

  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ 
  case object ProcedureBody
  case class ProcedureBody(declarationsProcedureBody: Declarations, statementProcedureBody: Statement) extends Tree[ProcedureBody] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ProcedureBody")
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("ProcedureBody", n) + declarationsProcedureBody.print(n + 1) + statementProcedureBody.print(n + 1)
  }

  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  trait ProcedureDeclaration extends Tree[ProcedureDeclaration] with Declarations {
    val procedureHeading: Tree[ProcedureHeading] = Nil
    val procedureBody: Tree[ProcedureBody] = Nil
    val ident: Ident = Nil
    override val next: ProcedureDeclaration = Nil
    override def print(n: Int) = ->("ProcedureDeclaration", n) + procedureHeading.print(n + 1) + procedureBody.print(n + 1) + ident.print(n + 1) + next.print(n + 1)
  }

  case object ProcedureDeclarationNode
  case class ProcedureDeclarationNode(override val procedureHeading: Tree[ProcedureHeading], override val procedureBody: Tree[ProcedureBody], override val ident: Ident, override val next: ProcedureDeclaration = Nil) extends ProcedureDeclaration with Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ProcedureDeclarationNode")
      Memory.Declarations.NilDescriptor
    }
  }

  //Declarations     = [ÕCONSTÕ ident Õ=Õ Expression Õ;Õ
  //                            {ident Õ=Õ Expression Õ;Õ}]
  //                   [ÕTYPEÕ ident Õ=Õ Type Õ;Õ
  //                           {ident Õ=Õ Type Õ;Õ}]
  //                   [ÕVARÕ IdentList Õ:Õ Type Õ;Õ
  //                          {IdentList Õ:Õ Type Õ;Õ}]
  //                   {ProcedureDeclaration Õ;Õ}.
  trait Declarations extends Tree[Declarations] {
    val next: Declarations = Nil
  }

  case object ConstDeclarations
  case class ConstDeclarations(ident: Ident, expression: Expression, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ConstDeclarations")
      val d = expression.compile(symbolTable);
      expression match {
        case i: Integer => {
          Memory.SymbolTables + (ident.identIdent.value.get.toString, Memory.Declarations.IntConst(i.int.value.get.toString.toInt))
          next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, Memory.Declarations.IntConst(i.int.value.get.toString.toInt)));
        }
        case x => {
          error("ConstDeclarations with Integervalue",x)
        }
      }
      d
    }
    override def print(n: Int) = ->("ConstDeclarations", n) + ident.print(n + 1) + expression.print(n + 1) + next.print(n + 1)
  }

  case object TypeDeclarations
  case class TypeDeclarations(ident: Ident, _type: Type, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("TypeDeclarations")
      val d = _type.compile(symbolTable);
      Memory.SymbolTables + (ident.identIdent.value.get.toString, d)
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
    }
    override def print(n: Int) = ->("TypeDeclarations", n) + ident.print(n + 1) + _type.print(n + 1) + next.print(n + 1)
  }

  case object VarDeclarations
  case class VarDeclarations(ident: Ident, _type: Type, override val next: Declarations = Nil) extends Declarations {

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("VarDeclarations")
      val anyType = _type.compile(symbolTable)
      def compileIdent(ident: Expression): Memory.Declarations.Descriptor = {
        if (ident.isDefined) {
          ident match {
            case i: Ident => {
              anyType match {
                case t: Memory.Declarations.SimpleType => {
                  val entry = Memory.Declarations.Variable(Memory.SymbolTables.currentAddress, t)
                  Memory.SymbolTables + (i.identIdent.value.get.toString, entry)
                }
                case t: Memory.Declarations.ArrayType => {
                  val entry = Memory.Declarations.Variable(Memory.SymbolTables.currentAddress, t)
                  Memory.SymbolTables + (i.identIdent.value.get.toString, entry)
                }
                case t: Memory.Declarations.RecordType => {
                  Memory.SymbolTables + (i.identIdent.value.get.toString, concreteRecordType(t, Memory.SymbolTables.currentAddress))
                  Memory.Declarations.NilDescriptor
                }
                case x => {
                  error("VarDeclarations: SimpleType or Record", x)
                  Memory.Declarations.NilDescriptor
                }
              }
              compileIdent(i.optionalIdent)
            }
            case x => {
              error("VarDeclarations: Ident", x)
              Memory.Declarations.NilDescriptor
            }
          }
        } else
          Memory.Declarations.NilDescriptor
      }
      compileIdent(ident)
      next.compile()
    }

    def concreteRecordType(r: Memory.Declarations.RecordType, currentAddress: Int): Memory.Declarations.RecordType = {
      val startAddr = currentAddress
      var i = 0
      var newTable = Memory.Declarations.SymbolTable()
      for ((name, desc) <- r.symbolTable.symbolTable) {
        newTable = desc match {
          case r: Memory.Declarations.RecordType => newTable + (name, concreteRecordType(r, i))
          case t: Memory.Declarations.Type => {
            newTable + (name, Memory.Declarations.Variable(i, t))
          }
          case x => {
            error("concreteRecordType:Type", x)
            Memory.Declarations.SymbolTable()
          }
        }
        i += desc.size
      }
      Memory.Declarations.RecordType(newTable, startAddr)
    }

    override def print(n: Int) = ->("VarDeclarations", n) + ident.print(n + 1) + _type.print(n + 1) + next.print(n + 1)
  }

  case object Print
  case class Print(expression: Expression) extends Tree[Print] with Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Print")
      expression.compile(symbolTable)
      OberonInstructions.PrintInstruction
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("Print", n) + expression.print(n + 1)
  }

  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {Õ;Õ Statement}.
  case object StatementSequence
  case class StatementSequence(st: Statement, sts: Tree[StatementSequence]) extends Statement with Tree[StatementSequence] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("StatementSequence")
      st.compile(symbolTable)
      sts.compile(symbolTable)
    }

    override def print(n: Int) = ->("StatementSequence", n) + st.print(n + 1) + sts.print(n + 1)
  }

  trait Statement extends Tree[Statement]

  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  case object Module
  case class Module(idStart: Ident, declarations: Declarations, statement: Statement, idEnd: Ident) extends Tree[Module] {

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Module");
      OberonInstructions.StringVal(idStart.identIdent.value.get.toString)
      val mainProgramStart = OberonInstructions.newLabel
      OberonInstructions.JumpInstruction(mainProgramStart)
      OberonInstructions.LabelInstruction(mainProgramStart)
      Memory.SymbolTables + ("integer", Memory.Declarations.IntegerType)
      Memory.SymbolTables + ("boolean", Memory.Declarations.BooleanType)
      Memory.SymbolTables + ("string", Memory.Declarations.StringType)
      declarations.compile(Memory.SymbolTables().symbolTable)
      Memory.setMainProgramLength(Memory.SymbolTables.currentAddress)
      OberonInstructions.IntegerVal(Memory.mainProgramLength)
      OberonInstructions.SetSP
      statement.compile(Memory.SymbolTables().symbolTable)
      OberonInstructions.StopInstruction
      Memory.Declarations.NilDescriptor
    }

    override def print(n: Int) = ->("Module", n) + idStart.print(n + 1) + declarations.print(n + 1) + statement.print(n + 1) + idEnd.print(n + 1)
  }

  //Assignment        = ident Selector Õ:=Õ Expression
  case object Assignment
  case class Assignment(ident: Expression, expression: Expression) extends Statement {

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("Assignment")
      val t = expression.compile(symbolTable)
      ident.compile()
      OberonInstructions.AssignmentInstruction(t.size)
      Memory.Declarations.NilDescriptor
    }

    override def print(n: Int) = ->("Assignment", n) + ident.print(n + 1) + expression.print(n + 1)
  }

  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  case object ProcedureCall
  case class ProcedureCall(ident: Ident, expression: Tree[ActualParameters] = Nil) extends Statement {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("ProcedureCall")
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("ProcedureCall", n) + ident.print(n + 1) + expression.print(n + 1)
  }

  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  case object IfStatement
  case class IfStatement(condition: Expression, statementSequence: Statement, alternatve: Statement = Nil) extends Statement with Tree[IfStatement] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("IfStatement")
      val l1 = OberonInstructions.newLabel
      val l2 = OberonInstructions.newLabel
      condition.compile()
      OberonInstructions.BranchFalseInstruction(l1)
      statementSequence.compile()
      OberonInstructions.JumpInstruction(l2)
      OberonInstructions.LabelInstruction(l1)
      alternatve.compile()
      OberonInstructions.LabelInstruction(l2)
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("IfStatement", n) + condition.print(n + 1) + statementSequence.print(n + 1) + alternatve.print(n + 1)
  }

  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  case object WhileStatement
  case class WhileStatement(condition: Expression, statement: Statement) extends Statement {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("WhileStatement")
      val l1 = OberonInstructions.newLabel
      val l2 = OberonInstructions.newLabel
      OberonInstructions.LabelInstruction(l1)
      condition.compile(symbolTable);
      OberonInstructions.BranchFalseInstruction(l2)
      statement.compile(symbolTable);
      OberonInstructions.JumpInstruction(l1)
      OberonInstructions.LabelInstruction(l2)
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("WhileStatement", n) + condition.print(n + 1) + statement.print(n + 1)
  }

  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  case object RepeatStatement
  case class RepeatStatement(statement: Statement, condition: Expression) extends Statement {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      trace("RepeatStatement")
      val l1 = OberonInstructions.newLabel
      val l2 = OberonInstructions.newLabel
      OberonInstructions.LabelInstruction(l1)
      statement.compile(symbolTable);
      condition.compile(symbolTable);
      OberonInstructions.BranchFalseInstruction(l2)
      OberonInstructions.JumpInstruction(l1)
      OberonInstructions.LabelInstruction(l2)
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("RepeatStatement", n) + condition.print(n + 1) + statement.print(n + 1)
  }

  trait Tree[+T] {
    def isDefined = true
    def print(n: Int): String
    override def toString = "AbstractSyntaxTree:\n" + print(0)
    def compile(symbolTable2: Map[String, Descriptor] = new HashMap): Memory.Declarations.Descriptor = Memory.Declarations.NilDescriptor
  }

  def ->(value: String, n: Int) = "	" * n + value + "\n"
  def trace(s: Any) = if (OberonDebug.compile) println("compile: " + s)
  def error(s: Any, found: Any) = {
    Thread.sleep(40)
    Console.err.println("compile error: " + s + " is missing, found: " + found)
    System.exit(-1)
  }
}