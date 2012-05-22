package Praktikum3
import Praktikum3.Memory.Declarations.Descriptor
import scala.collection.immutable.HashMap
import cip.base.CodeGen
import cip.instructions._

object Tree extends App {

  case object Nil extends Tree[Nothing] with Expression with Statement with Declarations with FormalParameters with ConstIdent with Type with Field with FieldList with ProcedureDeclaration with Ident {
    override def toString = "."
    override def print(n: Int): String = ""
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = Memory.Declarations.NilDescriptor
  }

  case class Integer(int: Symbol) extends Tree[Integer] with Expression with IndexExpression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      println("wichtig:" + int.value.get.toString)
      CodeGen.outInstr(new IntegerVal(int.value.get.toString.toInt))
      Memory.Declarations.IntegerType
    }
  }
  case object Integer

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
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
  }
  case object Ident

  // string 		 = ...
  case class Str(string: Symbol) extends Expression {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      CodeGen.outInstr(new StringVal(string.value.get.toString));
      Memory.Declarations.StringType
    }
  }
  case object Str

  //Read             = READ [Prompt].
  case class Read(prompt: Expression) extends Expression{

  }
  case object Read
  //Prompt           = string.
  case class Prompt(stringNode: Str) extends Expression
  case object Prompt

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
    val value = classOf[Expression].getName
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val dl = left.compile(symbolTable)
      right.compile(symbolTable)
      compileOperator
      dl
    }
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

  //ActualParameters  = Expression {Õ,Õ Expression}.
  case class ActualParameters(expressionActualParameters: Expression, actualParameters: Tree[ActualParameters]) extends Tree[ActualParameters]

  //IndexExpression  = integer | ConstIdent.
  trait IndexExpression extends Tree[IndexExpression]

  //ConstIdent       = ident.
  trait ConstIdent extends IndexExpression

  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  case class ArrayType(elemArrayType: IndexExpression, _typeArrayType: Type) extends Type
  case object ArrayType

  // FieldListList
  trait FieldList extends Tree[FieldList] {
    val fields: Field = Nil
    val nextFieldList: FieldList = Nil
  }
  case class FieldListNode(override val fields: Field, override val nextFieldList: FieldList) extends FieldList
  case object FieldListNode

  //FieldList = [IdentList Õ:Õ Type].
  trait Field extends Tree[Field] {
    val idlField: Ident = Nil
    val _typeField: Type = Nil
  }

  case class FieldNode(override val idlField: Ident, override val _typeField: Type) extends Field
  case object FieldNode
  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  case class RecordType(fieldsRecordType: FieldList) extends Type

  //Type = ident | ArrayType | RecordType.
  trait Type extends Tree[Type]

  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  case class FPSection(override val identFPSection: Ident, override val _typeFPSection: Type, override val optionalFPSection: FormalParameters = Nil) extends FormalParameters

  //FormalParameters = FPSection {Õ;Õ FPSection}.
  trait FormalParameters extends Tree[FormalParameters] {
    val optionalFPSection: FormalParameters = Nil
    val identFPSection: Ident = Nil
    val _typeFPSection: Type = Nil
  }

  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  case class ProcedureHeading(identProcedureHeading: Ident, formalParameters: FormalParameters = Nil) extends Tree[ProcedureHeading]
  case object ProcedureHeading

  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ 
  case class ProcedureBody(declarationsProcedureBody: Declarations, statementProcedureBody: Statement) extends Tree[ProcedureBody]
  case object ProcedureBody

  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  trait ProcedureDeclaration extends Tree[ProcedureDeclaration] with Declarations {
    val procedureHeading: Tree[ProcedureHeading] = Nil
    val procedureBody: Tree[ProcedureBody] = Nil
    val ident: Ident = Nil
    override val next: ProcedureDeclaration = Nil
  }
  case class ProcedureDeclarationNode(override val procedureHeading: Tree[ProcedureHeading], override val procedureBody: Tree[ProcedureBody], override val ident: Ident, override val next: ProcedureDeclaration = Nil) extends ProcedureDeclaration with Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      Memory.Declarations.NilDescriptor
    }
  }
  case object ProcedureDeclarationNode

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

  case class ConstDeclarations(ident: Ident, expression: Expression, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val d = expression.compile(symbolTable);
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
      Memory.Declarations.NilDescriptor
    }
  }
  case object ConstDeclarations

  case class TypeDeclarations(ident: Ident, _type: Type, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val d = _type.compile(symbolTable);
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
      Memory.Declarations.NilDescriptor
    }
  }
  case object TypeDeclarations

  case class VarDeclarations(ident: Ident, _type: Type, override val next: Declarations = Nil) extends Declarations {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      val d = _type.compile(symbolTable);
      next.compile(symbolTable + Tuple(ident.identIdent.value.get.toString, d));
      Memory.Declarations.NilDescriptor
    }
  }
  case object VarDeclarations
  
  case object Print
  case class Print(expression:Expression) extends Tree[Print] with Expression{
        override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
        	expression.compile(symbolTable)
        	CodeGen.outInstr(new PrintInstruction())
        	Memory.Declarations.NilDescriptor
		}
  }

  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {Õ;Õ Statement}.
  case class StatementSequence(st: Statement, sts: Tree[StatementSequence]) extends Statement with Tree[StatementSequence] {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      st.compile(symbolTable)
      sts.compile(symbolTable)
    }
  }
  case object StatementSequence

  trait Statement extends Tree[Statement]

  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  case class Module(idStart: Ident, declarations: Declarations, statement: Statement, idEnd: Ident) extends Tree[Module] {

    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      println("Module")
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
  }
  case object Module

  //Assignment        = ident Selector Õ:=Õ Expression
  case class Assignment(ident: Ident, expression: Expression) extends Statement {
    override def compile(symbolTable: Map[String, Descriptor] = new HashMap) = {
      ident.compile(symbolTable);
      val t = expression.compile(symbolTable);
      CodeGen.outInstr(new AssignmentInstruction(t.size));
      Memory.Declarations.NilDescriptor
    }
  }
  case object Assignment

  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  case class ProcedureCall(ident: Ident, expression: Tree[ActualParameters] = Nil) extends Statement
  case object ProcedureCall

  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  case class IfStatement(condition: Expression, statementSequence: Statement, ifStatement: Tree[IfStatement], alternatve: Statement = Nil) extends Statement with Tree[IfStatement]
  case object IfStatement

  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  case class WhileStatement(condition: Expression, statement: Statement) extends Statement
  case object WhileStatement

  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  case class RepeatStatement(statement: Statement, condition: Expression) extends Statement
  case object RepeatStatement

  sealed trait Tree[+T] {
    def print(n: Int): String = {
      def after$(s: String) = {
        val i = s.indexOf("$")
        if (i > 0) {
          s.substring(s.indexOf("$") + 1) match {
            case "$less$eq" => "<="
            case "$less" => "<"
            case "$greater" => ">"
            case "$greater$eq" => ">="
            case "$colon$eq" => "="
            case "$plus" => "+"
            case "$minus" => "-"
            case "$times" => "*"
            case "$div" => "/"
            case x => x
          }
        } else
          s
      }
      def ->(value: String, m: Int = n): String = "   " * m + after$(value) + "\n"
      val c = this.getClass()
      var s = ->(c.getName)
      for (i <- c.getDeclaredFields()) {
        val rawClass = i.getType.getEnclosingClass()
        if (rawClass != null && rawClass.getName().contains("Tree")) {
          i.setAccessible(true)
          val o: Tree[_] = i.get(this).asInstanceOf[Tree[_]]
          val p = o.print(n + 1)
          if (!p.isEmpty()) {
            s += ->(p, n + 1)
          }
        } else {
          i.setAccessible(true)
          s += ->(i.get(this).toString, n + 2)
        }
      }
      return s
    }
    override def toString = "AbstractSyntaxTree:\n" + print(0)
    def compile(symbolTable2: Map[String, Descriptor] = new HashMap): Memory.Declarations.Descriptor = Memory.Declarations.NilDescriptor
  }
}