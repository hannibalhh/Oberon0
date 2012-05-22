package Praktikum3
import Praktikum3.Memory.Declarations.Descriptor
import scala.collection.immutable.HashMap
import cip.base.CodeGen
import cip.instructions._

object Tree extends App {

  def ->(value: String, n: Int) = "	" * n + value + "\n"

  case object Nil extends Tree[Nothing] with Expression with Statement with Declarations with FormalParameters with ConstIdent with Type with Field with FieldList with ProcedureDeclaration with Ident {
    override def toString = "."
    override def print(n: Int): String = ""
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = Memory.Declarations.NilDescriptor
  }

  case class Integer(int: Symbol) extends Tree[Integer] with Expression with IndexExpression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      CodeGen.outInstr(new IntegerVal(int.value.get.toString.toInt))
      Memory.Declarations.IntegerType
    }

    override def print(n: Int) = ->("Integer(" + int + ")", n)
  }
  case object Integer

  //Selector         = {�.� ident | �[� Expression �]�}.
  trait Ident extends Tree[Ident] with Expression with Type with Declarations with ConstIdent {
    val identIdent: Symbol = Symbol("", -1, -1)
    val optionalIdent: Expression = Nil
  }
  case class Content(address: Expression) extends Ident {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      address match {
        case i: Ident => {
          val t = address.compile(symbolTable)
          val e = CodeGen.search(i.identIdent.value.get.toString, i.identIdent.line)
          e match {
            case e: ConstIdent =>
            case _ => CodeGen.outInstr(new ContInstruction(t.size))
          }
          t
        }
        case _ => {
          val t = Memory.Declarations.IntegerType
          CodeGen.outInstr(new ContInstruction(t.size))
          t
        }
      }
    }

    override def print(n: Int) = ->("Content", n) + address.print(n + 1)
  }
  case object Content
  case class IdentNode(override val identIdent: Symbol, override val optionalIdent: Expression = Nil) extends Tree[Ident] with Ident {
    //    def compile {
    //		val e = Memory.symbolTables(identIdent.value.get.toString,identIdent.line);
    //		if (e.level >= 0) {
    //			if ((e instanceof ArrayDescr) || (e instanceof RecordDescr)
    //					|| (e instanceof SimpleTypeDescr))
    //				return e;
    //		} else
    //			return null;
    //		trace("IdfNode " + idName + " " + level);
    //		if (e instanceof VarDescr) {
    //			addr = ((VarDescr) e).getAddr();
    //			t = ((VarDescr) e).getTyp();
    //			CodeGen.outInstr(new IntegerVal(addr));
    //			if (level > 0) {
    //				if (level == CodeGen.level) {
    //					CodeGen.outInstr(new GetFP());
    //					CodeGen.outInstr(new AdditionInstruction());
    //				} else {
    //					CodeGen.outInstr(new IntegerVal(level));
    //					CodeGen.outInstr(new GetSL());
    //					CodeGen.outInstr(new AdditionInstruction());
    //				}
    //			}
    //			if (((VarDescr) e).getIsvarpar()) {
    //				CodeGen.outInstr(new ContInstruction(1));
    //			}
    //		} else if (e instanceof IntConstDescr) {
    //			CodeGen.outInstr(new IntegerVal(((IntConstDescr) e).getIntVal()));
    //			t = new SimpleTypeDescr("integer");
    //		}
    //		unindent();
    //		return t;
    //    }

    override def print(n: Int) = ->("IdentNode(" + identIdent + ")", n) + optionalIdent.print(n + 1)
  }
  case object Ident

  // string 		 = ...
  case class Str(string: Symbol) extends Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      CodeGen.outInstr(new StringVal(string.value.get.toString));
      Memory.Declarations.StringType
    }

    override def print(n: Int) = ->("Str(" + string + ")", n)
  }
  case object Str

  //Read             = READ [Prompt].
  case class Read(prompt: Expression) extends Expression {
    override def print(n: Int) = ->("Read", n) + prompt.print(n + 1)
  }
  case object Read
  //Prompt           = string.
  case class Prompt(stringNode: Str) extends Expression
  case object Prompt

  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   �(� Expression �)�.
  //Term             = Factor {(�*� | �/�) Factor}.
  //SimpleExpression = [�-�] Term
  //                   {(�+� | �-�) Term}.
  //Expression       = SimpleExpression
  //                   [(�=� | �#� | �<� |
  //                     �<=� | �>� | �>=�)
  //                    SimpleExpression].

  trait Expression extends Statement {
    val left: Expression = Nil
    val right: Expression = Nil
    val value = classOf[Expression].getName

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
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
    override def compileOperator = CodeGen.outInstr(new MultiplicationInstruction())
  }
  case class /(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new DivisionInstruction())
  }
  case class +(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new AdditionInstruction())
  }
  case class -(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new SubtractionInstruction())
  }
  case class :=(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new EqualsInstruction())
  }
  //  case class :#(override val left: Expression, override val right: Expression) extends Expression{
  //    def compileOperator = CodeGen.outInstr(new ())
  //  }
  case class <(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new LessThanInstruction())
  }
  case class <=(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new LessEqualThanInstruction())
  }
  case class >(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new GreaterThanInstruction())
  }
  case class >=(override val left: Expression, override val right: Expression) extends Expression {
    override def compileOperator = CodeGen.outInstr(new GreaterEqualThanInstruction())
  }

  case class Neg(override val left: Expression) extends Expression {
    override def compileOperator = {
      CodeGen.outInstr(new IntegerVal(-1))
      CodeGen.outInstr(new MultiplicationInstruction())
    }
  }

  //ActualParameters  = Expression {�,� Expression}.
  case class ActualParameters(expressionActualParameters: Expression, actualParameters: Tree[ActualParameters]) extends Tree[ActualParameters] {
    override def print(n: Int) = ->("ActualParameters", n) + expressionActualParameters.print(n + 1) + actualParameters.print(n + 1)
  }

  //IndexExpression  = integer | ConstIdent.
  trait IndexExpression extends Tree[IndexExpression]

  //ConstIdent       = ident.
  trait ConstIdent extends IndexExpression

  //ArrayType = �ARRAY� �[� IndexExpression �]� �OF� Type.
  case class ArrayType(elemArrayType: IndexExpression, _typeArrayType: Type) extends Type {
    override def print(n: Int) = ->("ArrayType", n) + elemArrayType.print(n + 1) + _typeArrayType.print(n + 1)
  }
  case object ArrayType

  // FieldListList
  trait FieldList extends Tree[FieldList] {
    val fields: Field = Nil
    val nextFieldList: FieldList = Nil
    override def print(n: Int) = ->("FieldList", n) + fields.print(n + 1) + nextFieldList.print(n + 1)

  }
  case class FieldListNode(override val fields: Field, override val nextFieldList: FieldList) extends FieldList
  case object FieldListNode

  //FieldList = [IdentList �:� Type].
  trait Field extends Tree[Field] {
    val idlField: Ident = Nil
    val _typeField: Type = Nil
    override def print(n: Int) = ->("Field", n) + idlField.print(n + 1) + _typeField.print(n + 1)
  }

  case class FieldNode(override val idlField: Ident, override val _typeField: Type) extends Field
  case object FieldNode

  //RecordType = �RECORD� FieldList {�;� FieldList} �END�.
  case class RecordType(fieldsRecordType: FieldList) extends Type {
    override def print(n: Int) = ->("RecordType", n) + fieldsRecordType.print(n + 1)
  }

  //Type = ident | ArrayType | RecordType.
  trait Type extends Tree[Type]

  //FPSection = [�VAR�] IdentList �:� Type.
  case class FPSection(override val identFPSection: Ident, override val _typeFPSection: Type, override val optionalFPSection: FormalParameters = Nil) extends FormalParameters {
    override def print(n: Int) = ->("FPSection", n) + identFPSection.print(n + 1) + _typeFPSection.print(n + 1) + optionalFPSection.print(n + 1)
  }

  //FormalParameters = FPSection {�;� FPSection}.
  trait FormalParameters extends Tree[FormalParameters] {
    val optionalFPSection: FormalParameters = Nil
    val identFPSection: Ident = Nil
    val _typeFPSection: Type = Nil
    override def print(n: Int) = ->("FormalParameters", n) + optionalFPSection.print(n + 1) + identFPSection.print(n + 1) + _typeFPSection.print(n + 1)
  }

  //ProcedureHeading = �PROCEDURE� ident �(� [FormalParameters] �)�.
  case class ProcedureHeading(identProcedureHeading: Ident, formalParameters: FormalParameters = Nil) extends Tree[ProcedureHeading] {
    override def print(n: Int) = ->("ProcedureHeading", n) + identProcedureHeading.print(n + 1) + formalParameters.print(n + 1)
  }
  case object ProcedureHeading

  //ProcedureBody    = Declarations �BEGIN� StatementSequence �END� 
  case class ProcedureBody(declarationsProcedureBody: Declarations, statementProcedureBody: Statement) extends Tree[ProcedureBody] {
    override def print(n: Int) = ->("ProcedureBody", n) + declarationsProcedureBody.print(n + 1) + statementProcedureBody.print(n + 1)
  }
  case object ProcedureBody

  //ProcedureDeclaration = ProcedureHeading �;� ProcedureBody ident.
  trait ProcedureDeclaration extends Tree[ProcedureDeclaration] with Declarations {
    val procedureHeading: Tree[ProcedureHeading] = Nil
    val procedureBody: Tree[ProcedureBody] = Nil
    val ident: Ident = Nil
    override val next: ProcedureDeclaration = Nil
    override def print(n: Int) = ->("ProcedureDeclaration", n) + procedureHeading.print(n + 1) + procedureBody.print(n + 1) + ident.print(n + 1) + next.print(n + 1)
  }
  case class ProcedureDeclarationNode(override val procedureHeading: Tree[ProcedureHeading], override val procedureBody: Tree[ProcedureBody], override val ident: Ident, override val next: ProcedureDeclaration = Nil) extends ProcedureDeclaration with Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      Memory.Declarations.NilDescriptor
    }
  }
  case object ProcedureDeclarationNode

  //Declarations     = [�CONST� ident �=� Expression �;�
  //                            {ident �=� Expression �;�}]
  //                   [�TYPE� ident �=� Type �;�
  //                           {ident �=� Type �;�}]
  //                   [�VAR� IdentList �:� Type �;�
  //                          {IdentList �:� Type �;�}]
  //                   {ProcedureDeclaration �;�}.
  trait Declarations extends Tree[Declarations] {
    val next: Declarations = Nil
  }

  case class ConstDeclarations(ident: Ident, expression: Expression, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val d = expression.compile(symbolTable);
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("ConstDeclarations", n) + ident.print(n + 1) + expression.print(n + 1) + next.print(n + 1)
  }
  case object ConstDeclarations

  case class TypeDeclarations(ident: Ident, _type: Type, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val d = _type.compile(symbolTable);
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("TypeDeclarations", n) + ident.print(n + 1) + _type.print(n + 1) + next.print(n + 1)
  }
  case object TypeDeclarations

  case class VarDeclarations(ident: Ident, _type: Type, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val d = _type.compile(symbolTable);
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("VarDeclarations", n) + ident.print(n + 1) + _type.print(n + 1) + next.print(n + 1)

  }
  case object VarDeclarations

  case object Print
  case class Print(expression: Expression) extends Tree[Print] with Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      expression.compile(symbolTable)
      CodeGen.outInstr(new PrintInstruction())
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("Print", n) + expression.print(n + 1)
  }

  //   IfStatement | �PRINT� Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {�;� Statement}.
  case class StatementSequence(st: Statement, sts: Tree[StatementSequence]) extends Statement with Tree[StatementSequence] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      st.compile(symbolTable)
      sts.compile(symbolTable)
    }

    override def print(n: Int) = ->("StatementSequence", n) + st.print(n + 1) + sts.print(n + 1)

  }
  case object StatementSequence

  trait Statement extends Tree[Statement]

  //Module           = �MODULE� ident �;� Declarations
  //                   �BEGIN� StatementSequence
  //                   �END� ident �.�.
  case class Module(idStart: Ident, declarations: Declarations, statement: Statement, idEnd: Ident) extends Tree[Module] {

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      CodeGen.outInstr(new StringVal(idStart.identIdent.value.get.toString));
      val mainProgramStart = CodeGen.newLabel();
      CodeGen.outInstr(new JumpInstruction(mainProgramStart));
      val s = symbolTable + (
        Tuple("Integer", Memory.Declarations.IntegerType),
        Tuple("Boolean", Memory.Declarations.BooleanType),
        Tuple("String", Memory.Declarations.StringType))
      declarations.compile(s)
      statement.compile(s)
      Memory.lengthDataSegmentMainProgram = Memory.curraddr
      CodeGen.outInstr(new LabelInstruction(mainProgramStart))
      CodeGen.outInstr(new IntegerVal(
        Memory.lengthDataSegmentMainProgram))
      CodeGen.outInstr(new SetSP())
      CodeGen.outInstr(new StopInstruction())
      Memory.Declarations.NilDescriptor
    }

    override def print(n: Int) = ->("Module", n) + idStart.print(n + 1) + declarations.print(n + 1) + statement.print(n + 1) + idEnd.print(n + 1)

  }
  case object Module

  //Assignment        = ident Selector �:=� Expression
  case class Assignment(ident: Ident, expression: Expression) extends Statement {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      ident.compile(symbolTable);
      val t = expression.compile(symbolTable);
      CodeGen.outInstr(new AssignmentInstruction(t.size));
      Memory.Declarations.NilDescriptor
    }
    override def print(n: Int) = ->("Assignment", n) + ident.print(n + 1) + expression.print(n + 1)

  }
  case object Assignment

  //ProcedureCall = ident �(� [ActualParameters] �)�.
  case class ProcedureCall(ident: Ident, expression: Tree[ActualParameters] = Nil) extends Statement {
    override def print(n: Int) = ->("ProcedureCall", n) + ident.print(n + 1) + expression.print(n + 1)

  }
  case object ProcedureCall

  //IfStatement = �IF� Expression �THEN� StatementSequence
  //	{�ELSIF� Expression �THEN� StatementSequence}
  //  	[�ELSE� StatementSequence] �END�.
  case class IfStatement(condition: Expression, statementSequence: Statement, ifStatement: Tree[IfStatement], alternatve: Statement = Nil) extends Statement with Tree[IfStatement] {
    override def print(n: Int) = ->("IfStatement", n) + condition.print(n + 1) + statementSequence.print(n + 1) + ifStatement.print(n + 1) + alternatve.print(n + 1)
  }
  case object IfStatement

  //WhileStatement = �WHILE� Expression �DO� StatementSequence �END�.
  case class WhileStatement(condition: Expression, statement: Statement) extends Statement{
        override def print(n: Int) = ->("WhileStatement", n) + condition.print(n + 1) + statement.print(n + 1) 

  }
  case object WhileStatement

  //RepeatStatement = �REPEAT� StatementSequence �UNTIL� Expression.
  case class RepeatStatement(statement: Statement, condition: Expression) extends Statement{
            override def print(n: Int) = ->("RepeatStatement", n) + condition.print(n + 1) + statement.print(n + 1) 

  }
  case object RepeatStatement

  sealed trait Tree[+T] {
    def print(n: Int): String
    override def toString = "AbstractSyntaxTree:\n" + print(0)
    def compile(symbolTable2: Map[String, Descriptor] = new HashMap): Memory.Declarations.Descriptor = Memory.Declarations.NilDescriptor
  }
}