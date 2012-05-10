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
  
  case object Nil extends Tree[Nothing] with Expression with Statement with Declarations with FormalParameters with ConstIdent with Type with Field with FieldList with ProcedureDeclaration{
    override def toString = "."
    override def print(n: Int): String = ""
  }
  
  case class Integer(int: Symbol) extends Expression  // with IndexExpression
  case object Integer
  
   //Selector         = {�.� ident | �[� Expression �]�}.
//  trait Ident extends Expression  {
//    val ident: Option[Symbol] = None
//    val optional:Expression = Nil
//  }
  case class Ident(identIdent: Symbol,optionalIdent:Expression = Nil) extends Tree[Ident] with Expression with Type with Declarations with IndexExpression
  case object Ident
  
  // string 		 = ...
  case class Str(string: Symbol) extends Expression 
  case object Str
  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   �(� Expression �)�.
  
  //Read             = READ [Prompt].
  case class Read(prompt: Expression) extends Expression
  case object Read
  //Prompt           = string.
  case class Prompt(stringNode: Str) extends Expression
  case object Prompt
  
  //Term             = Factor {(�*� | �/�) Factor}.
  //SimpleExpression = [�-�] Term
  //                   {(�+� | �-�) Term}.
  //Expression       = SimpleExpression
  //                   [(�=� | �#� | �<� |
  //                     �<=� | �>� | �>=�)
  //                    SimpleExpression].
  
  trait Expression extends Statement{
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
  
  case class Neg(override val left:Expression) extends Expression
  
  //ActualParameters  = Expression {�,� Expression}.
  case class ActualParameters(expressionActualParameters:Expression, actualParameters:Tree[ActualParameters]) extends Tree[ActualParameters]
  
  //IndexExpression  = integer | ConstIdent.
  trait IndexExpression extends Tree[IndexExpression]
  
  //ConstIdent       = ident.
  trait ConstIdent extends IndexExpression
  
  //ArrayType = �ARRAY� �[� IndexExpression �]� �OF� Type.
  //  ArrayNode -> elem, Type
  case class ArrayType(elemArrayType: IndexExpression, _typeArrayType: Type) extends Type
  case object ArrayType
  
  // FieldListList
  trait FieldList extends Tree[FieldList]{
    val fields: Field = Nil
    val nextFieldList: FieldList = Nil
  }
  case class FieldListNode(override val fields:Field, override val nextFieldList: FieldList) extends FieldList
  case object FieldListNode
  
  //FieldList = [IdentList �:� Type].
  trait Field extends Tree[Field]{
    val idlField: Tree[Ident] = Nil
    val _typeField: Type = Nil
  }
 
  case class FieldNode(override val idlField:Tree[Ident],override val _typeField: Type) extends Field
  case object FieldNode
  //RecordType = �RECORD� FieldList {�;� FieldList} �END�.
  // RecordNode -> ident, type
  case class RecordType(fieldsRecordType: FieldList) extends Type
    
  //Type = ident | ArrayType | RecordType.
  trait Type extends Tree[Type]
  
  //FPSection = [�VAR�] IdentList �:� Type.
  case class FPSection(override val identFPSection:Tree[Ident], override val _typeFPSection:Type,override val optionalFPSection: FormalParameters = Nil) extends FormalParameters
  
  //FormalParameters = FPSection {�;� FPSection}.
  trait FormalParameters extends Tree[FormalParameters]{
    val optionalFPSection: FormalParameters = Nil
    val identFPSection:Tree[Ident] = Nil
    val _typeFPSection:Type = Nil
  }
 
  //ProcedureHeading = �PROCEDURE� ident �(� [FormalParameters] �)�.
  case class ProcedureHeading(identProcedureHeading:Tree[Ident], formalParameters:FormalParameters = Nil) extends Tree[ProcedureHeading] 
  case object ProcedureHeading
  
  //ProcedureBody    = Declarations �BEGIN� StatementSequence �END� 
  case class ProcedureBody(declarationsProcedureBody:Declarations, statementProcedureBody:Statement) extends Tree[ProcedureBody]
  case object ProcedureBody
  
  //ProcedureDeclaration = ProcedureHeading �;� ProcedureBody ident.
  trait ProcedureDeclaration extends Tree[ProcedureDeclaration] with Declarations{
   val procedureHeading:Tree[ProcedureHeading] = Nil
   val procedureBody:Tree[ProcedureBody] = Nil
   val ident:Tree[Ident] = Nil
   override val next: ProcedureDeclaration = Nil
  }
  case class ProcedureDeclarationNode(override val procedureHeading:Tree[ProcedureHeading],override val procedureBody:Tree[ProcedureBody],override val  ident:Tree[Ident],override val next: ProcedureDeclaration = Nil) extends ProcedureDeclaration with Declarations
  case object ProcedureDeclarationNode
  
  //Declarations     = [�CONST� ident �=� Expression �;�
  //                            {ident �=� Expression �;�}]
  //                   [�TYPE� ident �=� Type �;�
  //                           {ident �=� Type �;�}]
  //                   [�VAR� IdentList �:� Type �;�
  //                          {IdentList �:� Type �;�}]
  //                   {ProcedureDeclaration �;�}.
  trait Declarations extends Tree[Declarations]{
    val next: Declarations = Nil
  }
  case class ConstDeclarations(ident: Tree[Ident], expression: Expression,override val next: Declarations = Nil) extends Declarations
  case object ConstDeclarations
  case class TypeDeclarations(ident: Tree[Ident], _type: Type,override val next: Declarations = Nil) extends Declarations
  case object TypeDeclarations
  case class VarDeclarations(ident: Tree[Ident], _type: Type,override val next: Declarations = Nil) extends Declarations  //Statement = [Assignment | ProcedureCall |
  case object VarDeclarations
  
  //   IfStatement | �PRINT� Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {�;� Statement}.
  case class StatementSequence(st:Statement,sts:Tree[StatementSequence]) extends Statement with Tree[StatementSequence] 
  case object StatementSequence
  
  trait Statement extends Tree[Statement]
  
  //Module           = �MODULE� ident �;� Declarations
  //                   �BEGIN� StatementSequence
  //                   �END� ident �.�.
  case class Module(idStart:Tree[Ident],declarations:Declarations,statement:Statement,idEnd:Tree[Ident]) extends Tree[Module]
  case object Module
  
  //Assignment        = ident Selector �:=� Expression
  case class Assignment(ident:Tree[Ident],selector:Expression,expression:Expression) extends Statement  
  case object Assignment
  
  //ProcedureCall = ident �(� [ActualParameters] �)�.
  case class ProcedureCall(ident:Tree[Ident],expression:Tree[ActualParameters]  = Nil) extends Statement
  case object ProcedureCall
  
  //IfStatement = �IF� Expression �THEN� StatementSequence
  //	{�ELSIF� Expression �THEN� StatementSequence}
  //  	[�ELSE� StatementSequence] �END�.
  case class IfStatement(condition: Expression, statementSequence: Statement, ifStatement:Tree[IfStatement], alternatve: Statement = Nil) extends Statement with Tree[IfStatement]
  case object IfStatement
  
  //WhileStatement = �WHILE� Expression �DO� StatementSequence �END�.
  case class WhileStatement(condition: Expression, statement: Statement) extends Statement
  case object WhileStatement
  
  //RepeatStatement = �REPEAT� StatementSequence �UNTIL� Expression.
  case class RepeatStatement(statement: Statement, condition: Expression) extends Statement
  case object RepeatStatement
}