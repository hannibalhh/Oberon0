package Praktikum3

object Tree extends App{

  sealed trait Tree[+T] {
    
    def print(n: Int): String = {
      def after$(s:String) = {
        val i = s.indexOf("$")
        if (i > 0){
            s.substring(s.indexOf("$")+1) match {
              case "$less$eq" => "<="
              case "$less" => "<"
              case "$greater" => ">"
              case "$greater$eq" => ">="            
              case "colon$eq" => ":="
              case "$plus" => "+"
              case "$minus" => "-"
              case "$times" => "*"
              case "$div" => "/"
              case x => x
            }            
        }    
        else
        	s
      }
      def ->(value: String,m:Int = n): String = "   "* m + after$(value) + "\n" 
      val c = this.getClass()
      var s = -> (c.getName) 
      for(i<-c.getDeclaredFields()){   
        val rawClass = i.getType.getEnclosingClass()
        if (rawClass != null && rawClass.getName().contains("Tree")){        
            i.setAccessible(true)
        	val o:Tree[_] = i.get(this).asInstanceOf[Tree[_]] 
            val p = o.print(n+1)
            if (!p.isEmpty()){
                s += ->(p,n+1)
            }
        }
        else{
            i.setAccessible(true)
            s += -> (i.get(this).toString,n+2)
        }
      }
      return s
    }
    override def toString = "AbstractSyntaxTree:\n" + print(0)
//	  def compile
  }
  
  val id = Ident(Symbol("blub",1,1))
  val foo = id > (id := (id + id))
  println(foo)
  
  case object Nil extends Tree[Nothing] with Expression with Statement{
    override def toString = "."
    override def print(n: Int): String = ""
  }
  
  case class Integer(int: Symbol) extends Expression with IndexExpression
  case object Integer
  
   //Selector         = {�.� ident | �[� Expression �]�}.
  case class Ident(ident: Symbol, optional:Expression = Nil) extends Expression with Type with ConstIdent
  case object Ident
  
  // string 		 = ...
  case class StringNode(string: Symbol) extends Expression
  
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
  
  trait Expression extends Tree[Expression]{
    val left:Expression = Nil
    val right:Expression = Nil
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

  case class *(override val left: Expression,override val right: Expression) extends Expression
  case class /(override val left: Expression,override val right: Expression) extends Expression
  case class +(override val left: Expression,override val right: Expression) extends Expression
  case class -(override val left: Expression,override val right: Expression) extends Expression
  case class :=(override val left: Expression,override val right: Expression) extends Expression
  case class :#(override val left: Expression,override val right: Expression) extends Expression
  case class <(override val left: Expression,override val right: Expression) extends Expression
  case class <=(override val left: Expression,override val right: Expression) extends Expression
  case class >(override val left: Expression,override val right: Expression)extends Expression
  case class >=(override val left: Expression,override val right: Expression) extends Expression
  
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
    val next: Statement = Nil
  }
  
  //Module           = �MODULE� ident �;� Declarations
  //                   �BEGIN� StatementSequence
  //                   �END� ident �.�.
  case class Module(idStart:Ident,declarations:Declarations,statement:Statement,idEnd:Ident) extends Tree[Module]
  case object Module
  
  // Module -> ident, Declrations, Statementsequence, ident 
  //Assignment        = ident Selector �:=� Expression
  case class Asssignment(ident:Ident,expression:Expression) extends Declarations  
  case object Asssignment
  
  //ProcedureCall = ident �(� [ActualParameters] �)�.
  case class ProcedureCall(ident:Ident,expression:Expression = Nil) extends Tree[ProcedureCall]
  case object ProcedureCall
  
  //IfStatement = �IF� Expression �THEN� StatementSequence
  //	{�ELSIF� Expression �THEN� StatementSequence}
  //  	[�ELSE� StatementSequence] �END�.
  case class IfStatement(condition: Statement, statement: Statement, ifStatement:IfStatement, alternatve: Statement,override val next:Statement = Nil) extends Statement
  case object IfStatement
  
  //WhileStatement = �WHILE� Expression �DO� StatementSequence �END�.
  case class WhileStatement(condition: Expression, statement: Statement,override val next:Statement = Nil) extends Statement
  case object WhileStatement
  
  //RepeatStatement = �REPEAT� StatementSequence �UNTIL� Expression.
  case class RepeatStatement(statement: Statement, condition: Expression,override val next:Statement = Nil) extends Statement
  case object RepeatStatement
}