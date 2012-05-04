package Praktikum3

object Tree extends App{

  sealed trait Tree[+T] {
    def print(n: Int): String = ""
//    override def toString = "AbstractSyntaxTree:\n" +print(0)
  }
  
  val id = Ident(Symbol("blub",1,1))
  val foo = id * id
  println(foo)
  
  case object Nil extends Tree[Nothing] with Expression {
    override def toString = "."
    override val left = None
    override val right = None
  }
  
  case class Integer(int: Symbol) extends Expression with IndexExpression{
    override val left = None
    override val right = None
  }
  case object Integer
  
   //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  case class Ident(ident: Symbol, optional:Option[Expression] = None) extends Expression with Type with ConstIdent{
    override val left = None
    override val right = None
  }
  case object Ident
  
  // string 		 = ...
  case class StringNode(string: Symbol) extends Expression{
    override val left = None
    override val right = None
  }
  
  case object StringNode
  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ.
  
  //Read             = READ [Prompt].
  case class Read(stringNode: Option[Prompt] = None) extends Tree[Read]
  case object Read
  //Prompt           = string.
  case class Prompt(stringNode: StringNode) extends Tree[Prompt]
  case object Prompt
  
  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}.
  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression].
  //ActualParameters  = Expression {Õ,Õ Expression}.
  implicit def expToOption(expr: Expression) = Some(expr)
  
  trait Expression extends Tree[Expression]{
    val left:Option[Expression]
    val right:Option[Expression]
    val value = classOf[Expression].getName
    
    def *(expr:Expression): Expression = new *(this,expr)
    def /(expr:Expression): Expression = new /(this,expr)
    def +(expr:Expression): Expression = new +(this,expr)
    def -(expr:Expression): Expression = new -(this,expr)
    def :=(expr:Expression): Expression = new :=(this,expr)
    def :#(expr:Expression): Expression = new :#(this,expr)
    def <(expr:Expression): Expression = new <(this,expr)
    def <=(expr:Expression): Expression = new <=(this,expr)
    def >(expr:Expression): Expression = new >(this,expr)
    def >=(expr:Expression): Expression = new >=(this,expr)
  }

  case class *(left: Option[Expression],right: Option[Expression]) extends Expression
  case class /(left: Option[Expression],right: Option[Expression]) extends Expression
  case class +(left: Option[Expression],right: Option[Expression]) extends Expression
  case class -(left: Option[Expression],right: Option[Expression]) extends Expression
  case class :=(left: Option[Expression],right: Option[Expression]) extends Expression
  case class :#(left: Option[Expression],right: Option[Expression]) extends Expression
  case class <(left: Option[Expression],right: Option[Expression]) extends Expression
  case class <=(left: Option[Expression],right: Option[Expression]) extends Expression
  case class >(left: Option[Expression],right: Option[Expression]) extends Expression
  case class >=(left: Option[Expression],right: Option[Expression]) extends Expression
  
  //IndexExpression  = integer | ConstIdent.
  trait IndexExpression
  
  //ConstIdent       = ident.
  trait ConstIdent extends IndexExpression
  
  
  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  //  ArrayNode -> elem, Type
  case class ArrayType(elem: Expression, _type: Type) extends Type
  
  //FieldList = [IdentList Õ:Õ Type].
  case class Field(field:Option[Field] = None)
  
  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  // RecordNode -> ident, type
  case class RecordType(field: Field) extends Type
    
  //Type = ident | ArrayType | RecordType.
  trait Type
  
  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  case class FPSection(ident:Ident, _type:Type) extends FormalParameters
  
  //FormalParameters = FPSection {Õ;Õ FPSection}.
  case class FormalParameters(optional: Option[FormalParameters] = None) extends Tree[FormalParameters]
  case object FormalParameters
 
  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  case class ProcedureHeading(ident:Ident, formalParameters:FormalParameters) extends Tree[ProcedureHeading] 
  case object ProcedureHeading
  
  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ ident Õ.Õ.
  case class ProcedureBody(declarations:Declarations, statement:Statement,ident:Ident) extends Tree[ProcedureBody]
  case object ProcedureBody
  
  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  case class ProcedureDeclaration(procedureHeading:ProcedureHeading,procedureBody:ProcedureBody, ident:Ident) extends Declarations
  case object ProcedureDeclaration
  
  //Declarations     = [ÕCONSTÕ ident Õ=Õ Expression Õ;Õ
  //                            {ident Õ=Õ Expression Õ;Õ}]
  //                   [ÕTYPEÕ ident Õ=Õ Type Õ;Õ
  //                           {ident Õ=Õ Type Õ;Õ}]
  //                   [ÕVARÕ IdentList Õ:Õ Type Õ;Õ
  //                          {IdentList Õ:Õ Type Õ;Õ}]
  //                   {ProcedureDeclaration Õ;Õ}.
  trait Declarations extends Tree[Declarations]
  case class ConstDeclarations(ident: Ident, expression: Expression) extends Declarations
  case object ConstDeclarations
  case class TypeDeclarations(ident: Ident, expression: Expression) extends Declarations
  case object TypeDeclarations
  case class VarDeclarations(ident: Ident, expression: Expression) extends Declarations  //Statement = [Assignment | ProcedureCall |
  case object VarDeclarations
  
  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {Õ;Õ Statement}.
  trait Statement extends Tree[Statement]{
    val next: Option[Statement]
  }
  
  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  case class Module(idStart:Ident,declarations:Declarations,statement:Statement,idEnd:Ident) extends Tree[Module]
  case object Module
  
  // Module -> ident, Declrations, Statementsequence, ident 
  //Assignment        = ident Selector Õ:=Õ Expression
  case class Asssignment(ident:Ident,expression:Expression) extends Declarations  
  
  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  case class ProcedureCall(ident:Ident,expression:Option[Expression] = None) extends Tree[ProcedureCall]
   
  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  case class IfStatement(condition: Statement, statement: Statement, ifStatement:IfStatement, alternatve: Statement,next:Option[Statement] = None) extends Statement
  case object IfStatement
  
  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  case class WhileStatement(condition: Expression, statement: Statement,next:Option[Statement] = None) extends Statement
  case object WhileStatement
  
  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  case class RepeatStatement(statement: Statement, condition: Expression,next:Option[Statement] = None) extends Statement
  case object RepeatStatement
}