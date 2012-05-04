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
  
   //Selector         = {�.� ident | �[� Expression �]�}.
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
  //                   �(� Expression �)�.
  
  //Read             = READ [Prompt].
  case class Read(stringNode: Option[Prompt] = None) extends Tree[Read]
  case object Read
  //Prompt           = string.
  case class Prompt(stringNode: StringNode) extends Tree[Prompt]
  case object Prompt
  
  //Term             = Factor {(�*� | �/�) Factor}.
  //SimpleExpression = [�-�] Term
  //                   {(�+� | �-�) Term}.
  //Expression       = SimpleExpression
  //                   [(�=� | �#� | �<� |
  //                     �<=� | �>� | �>=�)
  //                    SimpleExpression].
  //ActualParameters  = Expression {�,� Expression}.
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
  
  
  //ArrayType = �ARRAY� �[� IndexExpression �]� �OF� Type.
  //  ArrayNode -> elem, Type
  case class ArrayType(elem: Expression, _type: Type) extends Type
  
  //FieldList = [IdentList �:� Type].
  case class Field(field:Option[Field] = None)
  
  //RecordType = �RECORD� FieldList {�;� FieldList} �END�.
  // RecordNode -> ident, type
  case class RecordType(field: Field) extends Type
    
  //Type = ident | ArrayType | RecordType.
  trait Type
  
  //FPSection = [�VAR�] IdentList �:� Type.
  case class FPSection(ident:Ident, _type:Type) extends FormalParameters
  
  //FormalParameters = FPSection {�;� FPSection}.
  case class FormalParameters(optional: Option[FormalParameters] = None) extends Tree[FormalParameters]
  case object FormalParameters
 
  //ProcedureHeading = �PROCEDURE� ident �(� [FormalParameters] �)�.
  case class ProcedureHeading(ident:Ident, formalParameters:FormalParameters) extends Tree[ProcedureHeading] 
  case object ProcedureHeading
  
  //ProcedureBody    = Declarations �BEGIN� StatementSequence �END� ident �.�.
  case class ProcedureBody(declarations:Declarations, statement:Statement,ident:Ident) extends Tree[ProcedureBody]
  case object ProcedureBody
  
  //ProcedureDeclaration = ProcedureHeading �;� ProcedureBody ident.
  case class ProcedureDeclaration(procedureHeading:ProcedureHeading,procedureBody:ProcedureBody, ident:Ident) extends Declarations
  case object ProcedureDeclaration
  
  //Declarations     = [�CONST� ident �=� Expression �;�
  //                            {ident �=� Expression �;�}]
  //                   [�TYPE� ident �=� Type �;�
  //                           {ident �=� Type �;�}]
  //                   [�VAR� IdentList �:� Type �;�
  //                          {IdentList �:� Type �;�}]
  //                   {ProcedureDeclaration �;�}.
  trait Declarations extends Tree[Declarations]
  case class ConstDeclarations(ident: Ident, expression: Expression) extends Declarations
  case object ConstDeclarations
  case class TypeDeclarations(ident: Ident, expression: Expression) extends Declarations
  case object TypeDeclarations
  case class VarDeclarations(ident: Ident, expression: Expression) extends Declarations  //Statement = [Assignment | ProcedureCall |
  case object VarDeclarations
  
  //   IfStatement | �PRINT� Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {�;� Statement}.
  trait Statement extends Tree[Statement]{
    val next: Option[Statement]
  }
  
  //Module           = �MODULE� ident �;� Declarations
  //                   �BEGIN� StatementSequence
  //                   �END� ident �.�.
  case class Module(idStart:Ident,declarations:Declarations,statement:Statement,idEnd:Ident) extends Tree[Module]
  case object Module
  
  // Module -> ident, Declrations, Statementsequence, ident 
  //Assignment        = ident Selector �:=� Expression
  case class Asssignment(ident:Ident,expression:Expression) extends Declarations  
  
  //ProcedureCall = ident �(� [ActualParameters] �)�.
  case class ProcedureCall(ident:Ident,expression:Option[Expression] = None) extends Tree[ProcedureCall]
   
  //IfStatement = �IF� Expression �THEN� StatementSequence
  //	{�ELSIF� Expression �THEN� StatementSequence}
  //  	[�ELSE� StatementSequence] �END�.
  case class IfStatement(condition: Statement, statement: Statement, ifStatement:IfStatement, alternatve: Statement,next:Option[Statement] = None) extends Statement
  case object IfStatement
  
  //WhileStatement = �WHILE� Expression �DO� StatementSequence �END�.
  case class WhileStatement(condition: Expression, statement: Statement,next:Option[Statement] = None) extends Statement
  case object WhileStatement
  
  //RepeatStatement = �REPEAT� StatementSequence �UNTIL� Expression.
  case class RepeatStatement(statement: Statement, condition: Expression,next:Option[Statement] = None) extends Statement
  case object RepeatStatement
}