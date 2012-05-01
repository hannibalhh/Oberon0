package Praktikum3

object Tree {

  sealed trait Tree[+T] {
    def value: Option[T]
    def left: Tree[_]
    def right: Tree[_]

    def print(n: Int): String = {
      def ->(t: Tree[_]): String = "	"* n + t.value.getOrElse(None) + "\n" 
      -> (this) + left.print(n + 1) + right.print(n + 1)
    }

    override def toString = "AbstractSyntaxTree:\n" +print(0)
  }

  case class Node[+T](value: Option[T], left: Tree[T], right: Tree[T]) extends Tree[T]

  case object Nil extends Tree[Nothing] {
    override def toString = "."
    def left = Nil
    def right = Nil
    def value = None
  }

  def apply[T](value: T = None, l: Tree[T] = Nil, r: Tree[T] = Nil): Node[T] = {
    value match {
      case x: Option[T] => Node(x, l, r)
      case x: T => Node(Some(value), l, r)
    }
  }

  //string           = ...
  //Selector         = {�.� ident | �[� Expression �]�}.
  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   �(� Expression �)�.
  //Read             = READ [Prompt].
  //Prompt           = string.
  //Term             = Factor {(�*� | �/�) Factor}.
  //SimpleExpression = [�-�] Term
  //                   {(�+� | �-�) Term}.
  //Expression       = SimpleExpression
  //                   [(�=� | �#� | �<� |
  //                     �<=� | �>� | �>=�)
  //                    SimpleExpression].
  //IndexExpression  = integer | ConstIdent.
  //ConstIdent       = ident.
  //  
  // IdentList = ident {�,� ident}.
  //ArrayType = �ARRAY� �[� IndexExpression �]� �OF� Type.
  //  ArrayNode -> elem, Type
  //FieldList = [IdentList �:� Type].
  //RecordType = �RECORD� FieldList {�;� FieldList} �END�.
  // RecordNode -> ident, type
  //Type = ident | ArrayType | RecordType.
  //FPSection = [�VAR�] IdentList �:� Type.
  //FormalParameters = FPSection {�;� FPSection}.
  //ProcedureHeading = �PROCEDURE� ident �(� [FormalParameters] �)�.
  //ProcedureBody    = Declarations �BEGIN� StatementSequence �END� ident �.�.
  //ProcedureDeclaration = ProcedureHeading �;� ProcedureBody ident.
  //Declarations     = [�CONST� ident �=� Expression �;�
  //                            {ident �=� Expression �;�}]
  //                   [�TYPE� ident �=� Type �;�
  //                           {ident �=� Type �;�}]
  //                   [�VAR� IdentList �:� Type �;�
  //                          {IdentList �:� Type �;�}]
  //                   {ProcedureDeclaration �;�}.
  //Module           = �MODULE� ident �;� Declarations
  //                   �BEGIN� StatementSequence
  //                   �END� ident �.�.
  // Module -> ident, Declrations, Statementsequence, ident 
  //Assignment        = ident Selector �:=� Expression.
  //ActualParameters  = Expression {�,� Expression}.
  //ProcedureCall = ident �(� [ActualParameters] �)�.
  //IfStatement = �IF� Expression �THEN� StatementSequence
  //	{�ELSIF� Expression �THEN� StatementSequence}
  //  	[�ELSE� StatementSequence] �END�.
  //WhileStatement = �WHILE� Expression �DO� StatementSequence �END�.
  //RepeatStatement = �REPEAT� StatementSequence �UNTIL� Expression.
  //Statement = [Assignment | ProcedureCall |
  //   IfStatement | �PRINT� Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {�;� Statement}.
}