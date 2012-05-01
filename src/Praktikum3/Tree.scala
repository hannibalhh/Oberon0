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
  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}.
  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ.
  //Read             = READ [Prompt].
  //Prompt           = string.
  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}.
  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}.
  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression].
  //IndexExpression  = integer | ConstIdent.
  //ConstIdent       = ident.
  //  
  // IdentList = ident {Õ,Õ ident}.
  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type.
  //  ArrayNode -> elem, Type
  //FieldList = [IdentList Õ:Õ Type].
  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  // RecordNode -> ident, type
  //Type = ident | ArrayType | RecordType.
  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type.
  //FormalParameters = FPSection {Õ;Õ FPSection}.
  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ.
  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ ident Õ.Õ.
  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  //Declarations     = [ÕCONSTÕ ident Õ=Õ Expression Õ;Õ
  //                            {ident Õ=Õ Expression Õ;Õ}]
  //                   [ÕTYPEÕ ident Õ=Õ Type Õ;Õ
  //                           {ident Õ=Õ Type Õ;Õ}]
  //                   [ÕVARÕ IdentList Õ:Õ Type Õ;Õ
  //                          {IdentList Õ:Õ Type Õ;Õ}]
  //                   {ProcedureDeclaration Õ;Õ}.
  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ.
  // Module -> ident, Declrations, Statementsequence, ident 
  //Assignment        = ident Selector Õ:=Õ Expression.
  //ActualParameters  = Expression {Õ,Õ Expression}.
  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ.
  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ.
  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ.
  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression.
  //Statement = [Assignment | ProcedureCall |
  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement].
  //StatementSequence = Statement {Õ;Õ Statement}.
}