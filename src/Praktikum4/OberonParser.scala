package Praktikum4
import scala.annotation.tailrec
import scala.actors.Actor

@tailrec
object OberonParser extends App {
  import Praktikum4.OberonScanner._
  import Praktikum4.Tree._
  val pre = "src/OberonExamples/"
  val preC = pre + "Compile/"
  val preV = pre + "Voeller/"
  val preNT = pre + "NT/"
  val t0 = "test0-integer-ops.txt" // ok
  val t1 = "test1-statements.txt" // ok
  val t2 = "test2-named-constants.txt" // ok
  val t3 = "test3-array.txt" // ok
  val t4 = "test4-record.txt" // ok
  val t5 = "test5-named-types.txt" // ok
  val t6 = "test6-simple-procedures.txt" // parameter laufen nicht
  val t7 = "test7-procedures-more.txt"
  val t8 = "test8-local-procedures.txt"
  val t9 = "test9-recursive procedures.txt"
//    val scanner = oberonScanner(preV + t5)
  val scanner = oberonScanner(preC + "procedure")
  //  val scanner = oberonScanner(preNT + "FormalParameters")
  var current = next

  //  test(FormalParameters)
  OberonCodeGenerator.run(parser)
  OberonRunner.run

  def parser = {
    val m = Module
    if (!current.isEmpty) {
      error("No Token")
      Nil
    }
    m
  }

  /*
   *  terminals
   */
  // operators
  def mul = checkPrimitive(Token.mul)
  def plus = checkPrimitive(Token.plus)
  def sub = checkPrimitive(Token.sub)
  def div = checkPrimitive(Token.div)
  def _def = checkPrimitive(Token._def)
  def equ = checkPrimitive(Token.equ)
  def sharp = checkPrimitive(Token.sharp)
  def smaller = checkPrimitive(Token.smaller)
  def smallereq = checkPrimitive(Token.smallereq)
  def bigger = checkPrimitive(Token.bigger)
  def biggereq = checkPrimitive(Token.biggereq)
  // terminals with values
  def ident = checkPrimitiveValue(Token.ident)
  def string = checkPrimitiveValue(Token.string)
  def integer = checkPrimitiveValue(Token.integer)
  // only syntax terminals
  def comma = checkPrimitive(Token.comma)
  def semicolon = checkPrimitive(Token.semicolon)
  def bracketOn = checkPrimitive(Token.bracketOn)
  def bracketOff = checkPrimitive(Token.bracketOff)
  def camberedBracketOn = checkPrimitive(Token.camberedBracketOn)
  def camberedBracketOff = checkPrimitive(Token.camberedBracketOff)
  def colon = checkPrimitive(Token.colon)
  def DOT = checkPrimitive(Token.DOT)
  def edgeBracketOn = checkPrimitive(Token.edgeBracketOn)
  def edgeBracketOff = checkPrimitive(Token.edgeBracketOff)
  def OF = checkPrimitive(Token.OF)
  def THEN = checkPrimitive(Token.THEN)
  def DO = checkPrimitive(Token.DO)
  def PRINT = checkPrimitive(Token.PRINT)
  def READ = checkPrimitive(Token.READ)
  def END = checkPrimitive(Token.END)
  def ELSE = checkPrimitive(Token.ELSE)
  def ELSIF = checkPrimitive(Token.ELSIF)
  def IF = checkPrimitive(Token.IF)
  def WHILE = checkPrimitive(Token.WHILE)
  def REPEAT = checkPrimitive(Token.REPEAT)
  def UNTIL = checkPrimitive(Token.UNTIL)
  def ARRAY = checkPrimitive(Token.ARRAY)
  def RECORD = checkPrimitive(Token.RECORD)
  def CONST = checkPrimitive(Token.CONST)
  def TYPE = checkPrimitive(Token.TYPE)
  def VAR = checkPrimitive(Token.VAR)
  def PROCEDURE = checkPrimitive(Token.PROCEDURE)
  def BEGIN = checkPrimitive(Token.BEGIN)
  def MODULE = checkPrimitive(Token.MODULE)

  /*
   *  NonTerminals 
   *  with Precondition: current token is NOT used
   */

  //Selector         = {Õ.Õ ident | Õ[Õ Expression Õ]Õ}. // tested
  def Selector(before: Tree.Ident): Tree.Expression = {
    trace("Selector")
    if (DOT) {
      inc
      val id = ident
      if (id.isDefined) {
        inc
        if (before.isDefined)
          RecordReference(before, Selector(Tree.IdentNode(id.get)))
        else
          RecordReference(Tree.IdentNode(id.get), Selector(Nil))
      } else {
        error("Selector with ident after dot")
        Nil
      }
    } else if (edgeBracketOn) {
      inc
      val expr = Expression
      if (edgeBracketOff) {
        inc
        if (edgeBracketOn) {
          ArrayReference(before, ArrayReference(expr, Selector(Nil)))
        } else {
          if (before.isDefined)
            ArrayReference(before, ArrayReference(expr, Selector(Nil)))
          else
            ArrayReference(expr, Nil)
        }
      } else {
        error("Selector with ] after Expression")
        Nil
      }
    } else {
      // no error because its optional
      before
    }
  }

  //Factor           = ident Selector | integer | string |
  //                    Read |
  //                   Õ(Õ Expression Õ)Õ. // tested
  def Factor: Tree.Expression = {
    trace("Factor")
    val id = ident
    val int = integer
    val s = string
    if (id.isDefined) {
      inc
      Content(Selector(IdentNode(id.get)))
    } else if (int.isDefined) {
      inc
      Integer(int.get)
    } else if (s.isDefined) {
      inc
      Str(s.get)
    } else if (bracketOn) {
      inc
      val expr = Expression
      if (bracketOff) {
        inc
        expr
      } else {
        error("Factor ends with )")
        Nil
      }
    } else if (READ) {
      // no inc, because we look forward
      Tree.Read(Read)
    } else {
      error("Factor with ident or integer or string or Read or Õ(Õ")
      Nil
    }
  }

  //Read             = READ [Prompt]. // tested
  def Read: Tree.Expression = {
    trace("Read")
    val r = READ
    if (r) {
      inc
      Prompt
    } else {
      error("Read with READ")
      Nil
    }
  }

  //Prompt           = string. // tested
  def Prompt: Expression = {
    trace("Prompt")
    val s = string
    if (s.isDefined) {
      inc
      Tree.Prompt(Str(s.get))
    } else {
      error("Prompt with string")
      Nil
    }
  }

  //Term             = Factor {(Õ*Õ | Õ/Õ) Factor}. // tested
  def Term: Tree.Expression = {
    trace("Term")
    val f = Factor
    if (f.isDefined) {
      OptionalTerm(f)
    } else {
      error("Term with Factor")
      Nil
    }
  }

  def OptionalTerm(factor: Expression): Tree.Expression = {
    trace("OptionalTerm")
    val m = mul
    val d = div
    if (m || d) {
      inc
      val f = Factor
      if (f.isDefined) {
        if (m)
          factor * OptionalTerm(f)
        else if (d)
          factor / OptionalTerm(f)
        else
          Nil
      } else {
        error("OptionalTerm with Factor after * or /")
        Nil
      }
    } else {
      // no error because its optional
      factor
    }
  }

  //SimpleExpression = [Õ-Õ] Term
  //                   {(Õ+Õ | Õ-Õ) Term}. // tested
  def SimpleExpression: Tree.Expression = {
    trace("SimpleExpression")
    val s = sub
    if (s) {
      inc
      val t = Term
      if (t.isDefined) {
        Neg(OptionalSimpleExpression(t))
      } else {
        error("SimpleExpression with Term after -")
        Nil
      }
    } else {
      val t = Term
      if (t.isDefined) {
        OptionalSimpleExpression(t)
      } else {
        error("SimpleExpression with Term")
        Nil
      }
    }
  }

  def OptionalSimpleExpression(expr: Expression): Tree.Expression = {
    trace("OptionalSimpleExpression")
    val p = plus
    val s = sub
    if (p) {
      inc
      expr + OptionalSimpleExpression(Term)
      //      Tree('SimpleExpression, p, Tree('SimpleExpression, Term, OptionalSimpleExpression))
    } else if (s) {
      inc
      expr - OptionalSimpleExpression(Term)
    } else {
      // no error because its optional
      expr
    }
  }

  //Expression       = SimpleExpression
  //                   [(Õ=Õ | Õ#Õ | Õ<Õ |
  //                     Õ<=Õ | Õ>Õ | Õ>=Õ)
  //                    SimpleExpression]. // tested
  def Expression: Tree.Expression = {
    trace("Expression")
    val s = SimpleExpression
    if (s.isDefined) {
      OptionalExpression(s)
    } else {
      error("Expression with SimpleExpression")
      Nil
    }
  }

  def OptionalExpression(simple: Expression): Tree.Expression = {
    trace("OptionalExpression")
    val e = equ
    //    val sh = sharp
    val smal = smaller
    val smaleq = smallereq
    val big = bigger
    val bigeq = biggereq
    if (e || /* sh || */ smal || smaleq || big || bigeq) {
      inc
      val s = SimpleExpression
      if (s.isDefined) {
        if (e) {
          simple := s
          //        } else if (sh) {
          //          simple :# s
        } else if (smal) {
          simple < s
        } else if (smaleq) {
          simple <= s
        } else if (big) {
          simple > s
        } else if (bigeq) {
          simple >= s
        } else {
          Nil
        }
      } else {
        error("Expression with OptionalExpression after operation")
        Nil
      }
    } else {
      // no error because its optional
      simple
    }
  }

  //IndexExpression  = integer | ConstIdent. // tested
  def IndexExpression: Tree.IndexExpression = {
    trace("IndexExpression")
    val i = integer
    if (i.isDefined) {
      inc
      Tree.Integer(i.get)
    } else if (ident.isDefined) {
      // no inc because we look forward
      ConstIdent
    } else {
      error("IndexExpression with integer or ConstIdent")
      Nil
    }
  }

  //ConstIdent       = ident.  // tested
  def ConstIdent: Tree.ConstIdent = {
    trace("ConstIdent")
    val id = ident
    if (id.isDefined) {
      inc
      IdentNode(id.get)
    } else {
      error("ConstIdent with ident")
      Nil
    }
  }

  //IdentList = ident {Õ,Õ ident}. // tested
  def IdentList: Ident = {
    trace("IdentList")
    val id = ident
    if (id.isDefined) {
      inc
      IdentNode(id.get, OptionalIdentList)
    } else {
      error("IdentList with ident")
      Nil
    }
  }

  def OptionalIdentList: Tree.Expression = {
    trace("OptionalIdentList")
    if (comma) {
      inc
      val id = ident
      if (id.isDefined) {
        inc
        IdentNode(id.get, OptionalIdentList)
      } else
        Nil
    } else {
      // no error because its optional
      Nil
    }

  }

  //ArrayType = ÕARRAYÕ Õ[Õ IndexExpression Õ]Õ ÕOFÕ Type. // 
  def ArrayType: Tree.Type = {
    trace("ArrayType")
    if (ARRAY) {
      inc
      if (edgeBracketOn) {
        inc
        val i = IndexExpression
        if (i.isDefined) {
          if (edgeBracketOff) {
            inc
            if (OF) {
              inc
              val t = Type
              if (t.isDefined) {
                Tree.ArrayType(i, t)
              } else {
                error("Arraytype with Type after OF")
                Nil
              }
            } else {
              error("Arraytype with OF after ]")
              Nil
            }
          } else {
            error("Arraytype with ] after IndexExpression")
            Nil
          }
        } else {
          error("Arraytype with IndexExpression after [")
          Nil
        }
      } else {
        error("Arraytype with [ after ARRAY")
        Nil
      }
    } else {
      error("Arraytype with ARRAY")
      Nil
    }
  }

  //FieldList = [IdentList Õ:Õ Type]. // tested
  def FieldList: Tree.Field = {
    trace("FieldList")
    if (ident.isDefined) {
      val idl = IdentList
      if (idl.isDefined) {
        if (colon) {
          inc
          val t = Type
          if (t.isDefined) {
            FieldNode(idl, t)
          } else {
            error("Identlist with Type after :")
            Nil
          }
        } else {
          error("Idenlist with :")
          Nil
        }
      } else {
        error("Fieldlist with complete Identlist after ident")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //RecordType = ÕRECORDÕ FieldList {Õ;Õ FieldList} ÕENDÕ.
  def RecordType: Tree.Type = {
    trace("RecordType")
    if (RECORD) {
      inc
      val f = FieldListNode(FieldList, OptionalFieldList)
      if (f.isDefined) {
        if (END) {
          inc
          Tree.RecordType(f)
        } else {
          error("RecordType with END")
          Nil
        }
      } else {
        error("RecordType with FieldList")
        Nil
      }
    } else {
      error("RecordType with RECORD")
      Nil
    }
  }

  def OptionalFieldList: Tree.FieldList = {
    trace("OptionalFieldList")
    if (semicolon) {
      inc
      val f = FieldList
      if (f.isDefined) {
        FieldListNode(f, OptionalFieldList)
      } else {
        OptionalFieldList
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //Type = ident | ArrayType | RecordType. // tested
  def Type: Tree.Type = {
    trace("Type")
    val id = ident
    val arr = ARRAY
    val rec = RECORD
    if (id.isDefined) {
      inc
      Tree.IdentNode(id.get)
    } else if (arr) {
      ArrayType
    } else if (RECORD) {
      RecordType
    } else {
      error("Type with ident or ARRAY or Record")
      Nil
    }
  }

  //FPSection = [ÕVARÕ] IdentList Õ:Õ Type. // tested
  def FPSection: FormalParameters = {
    trace("FPSection")
    val variable = if (VAR) {
      inc
      true
    } else
      false
    val idl = IdentList
    if (colon) {
      inc
      val t = Type
      if (t.isDefined) {
        if (variable)
          Tree.ReferenceParameter(idl, t, OptionalFormalParameters)
        else
          Tree.ValueParameter(idl, t, OptionalFormalParameters)
      } else {
        error("FPSection with Type after :")
        Nil
      }
    } else {
      error("FPSection with : after [VAR]")
      Nil
    }
  }

  //FormalParameters = FPSection {Õ;Õ FPSection}. // tested
  def FormalParameters: FormalParameters = {
    trace("FormalParamters")
    val fps = FPSection
    if (fps.isDefined) {
      fps
    } else {
      error("FormalParameters with FPSection")
      Nil
    }
  }

  def OptionalFormalParameters: FormalParameters = {
    trace("OptionalFPSection")
    if (semicolon) {
      inc
      FPSection
    } else {
      trace("blub")
      // no error because its optional
      Nil
    }
  }

  //ProcedureHeading = ÕPROCEDUREÕ ident Õ(Õ [FormalParameters] Õ)Õ. // tested
  def ProcedureHeading: Tree[ProcedureHeading] = {
    trace("ProcedureHeading")
    if (PROCEDURE) {
      inc
      val id = ident
      if (id.isDefined) {
        inc
        if (bracketOn) {
          inc
          if (bracketOff) {
            inc
            Tree.ProcedureHeading(IdentNode(id.get))
          } else {
            val fp = FormalParameters
            if (bracketOff) {
              inc
              Tree.ProcedureHeading(IdentNode(id.get), fp)
            } else {
              error("ProcedureHeading with ) after FormalParameters")
              Nil
            }
          }
        } else {
          error("ProcedureHeading with ( after ident")
          Nil
        }
      } else {
        error("ProcedureHeading with id")
        Nil
      }
    } else {
      error("ProcedureHeading with PROCEDURE")
      Nil
    }
  }

  //ProcedureBody    = Declarations ÕBEGINÕ StatementSequence ÕENDÕ // tested
  def ProcedureBody: Tree[ProcedureBody] = {
    trace("ProcedureBody")
    val decl = Declarations
    if (decl.isDefined) {
      if (BEGIN) {
        inc
        val sts = StatementSequence
        if (sts.isDefined) {
          if (END) {
            inc
            Tree.ProcedureBody(decl, sts)
          } else {
            error("ProcedureBody with END")
            Nil
          }
        } else {
          error("ProcedureBody with Statementsequence")
          Nil
        }
      } else {
        error("ProcedureBody with BEGIN")
        Nil
      }
    } else {
      error("ProcedureBody with Declarations")
      Nil
    }
  }

  //ProcedureDeclaration = ProcedureHeading Õ;Õ ProcedureBody ident.
  def ProcedureDeclaration: Tree.ProcedureDeclaration = {
    trace("ProcedureDeclaration")
    val ph = ProcedureHeading
    if (ph.isDefined) {
      if (semicolon) {
        inc
        val pb = ProcedureBody
        if (pb.isDefined) {
          val id = ident
          if (id.isDefined) {
            inc
            Tree.ProcedureDeclarationNode(ph, pb, IdentNode(id.get))
          } else {
            error("ProcedureDeclaration with ident")
            Nil
          }
        } else {
          error("ProcedureDeclaration with ProcedureBody")
          Nil
        }
      } else {
        error("ProcedureDeclaration with ;")
        Nil
      }
    } else {
      error("ProcedureDeclaration with ProcedureHeading")
      Nil
    }
  }

  //Declarations     = [ÕCONSTÕ ident Õ=Õ Expression Õ;Õ
  //                            {ident Õ=Õ Expression Õ;Õ}]
  //                   [ÕTYPEÕ ident Õ=Õ Type Õ;Õ
  //                           {ident Õ=Õ Type Õ;Õ}]
  //                   [ÕVARÕ IdentList Õ:Õ Type Õ;Õ
  //                          {IdentList Õ:Õ Type Õ;Õ}]
  //                   {ProcedureDeclaration Õ;Õ}. // tested
  def Declarations: Tree.Declarations = {
    trace("Declarations")
    if (CONST) {
      inc
      val d = OptionalConstDeclarations
      if (d.isDefined) {
        d
      } else {
        error("Declarations with OptionalConstDeclrations")
        Nil
      }
    } else if (TYPE) {
      inc
      val d = OptionalTypeDeclarations
      if (d.isDefined) {
        d
      } else {
        error("Declarations with OptionalTypeDeclrations")
        Nil
      }
    } else if (VAR) {
      inc
      val d = OptionalVarDeclarations
      if (d.isDefined) {
        d
      } else {
        error("Declarations with OptionalVarDeclrations")
        Nil
      }
    } else {
      // no error because all is optional
      OptionalProcedureDeclarations
    }
  }

  def OptionalConstDeclarations: Tree.Declarations = {
    val id = ident
    if (id.isDefined) {
      inc
      if (equ) {
        inc
        val expr = Expression
        if (expr.isDefined) {
          if (semicolon) {
            inc
            Tree.ConstDeclarations(IdentNode(id.get), expr, OptionalConstDeclarations)
          } else {
            error("CONST Declarations with ; after Expression")
            Nil
          }
        } else {
          error("CONST Declarations with Expression after =")
          Nil
        }
      } else {
        error("CONST Declarations with = after ident")
        Nil
      }
    } else {
      // no error because its optional
      Declarations
    }
  }

  def OptionalTypeDeclarations: Tree.Declarations = {
    val id = ident
    if (id.isDefined) {
      inc
      if (equ) {
        inc
        val t = Type
        if (t.isDefined) {
          if (semicolon) {
            inc
            Tree.TypeDeclarations(IdentNode(id.get), t, OptionalTypeDeclarations)
          } else {
            error("TYPE Declarations with ; after Type")
            Nil
          }
        } else {
          error("TYPE Declarations with Type after =")
          Nil
        }
      } else {
        error("TYPE Declarations with = after ident")
        Nil
      }
    } else {
      // no error because its optional
      Declarations
    }
  }

  def OptionalVarDeclarations: Tree.Declarations = {
    val id = ident
    if (id.isDefined) {
      val idl = IdentList
      if (colon) {
        inc
        val t = Type
        if (t.isDefined) {
          if (semicolon) {
            inc
            Tree.VarDeclarations(idl, t, OptionalVarDeclarations)
          } else {
            error("VAR Declarations with ; after Type")
            Nil
          }
        } else {
          error("VAR Declarations with Type after :")
          Nil
        }
      } else {
        error("VAR Declarations with : after IdentList")
        Nil
      }
    } else {
      // no error because its optional
      Declarations
    }
  }

  def OptionalProcedureDeclarations: Tree.ProcedureDeclaration = {
    if (PROCEDURE) {
      val p: ProcedureDeclaration = ProcedureDeclaration
      if (p.isDefined) {
        if (semicolon) {
          inc
          Tree.ProcedureDeclarationNode(p.procedureHeading, p.procedureBody, p.ident, OptionalProcedureDeclarations)
        } else {
          error("OptionalProcedureDeclarations with ; after ProcedureDeclaration")
          Nil
        }
      } else {
        error("OptionalProcedureDeclarations with complete ProcedureDeclaration after PROCEDURE")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //Module           = ÕMODULEÕ ident Õ;Õ Declarations
  //                   ÕBEGINÕ StatementSequence
  //                   ÕENDÕ ident Õ.Õ. // tested
  def Module: Tree[Module] = {
    trace("Module")
    if (MODULE) {
      inc
      val id = ident
      if (id.isDefined) {
        inc
        if (semicolon) {
          inc
          val d = Declarations
          if (BEGIN) {
            inc
            val sts = StatementSequence
            if (sts.isDefined) {
              if (END) {
                inc
                val id2 = ident
                if (id2.isDefined) {
                  inc
                  if (DOT) {
                    inc
                    Tree.Module(IdentNode(id.get), d, sts, IdentNode(id2.get))
                  } else {
                    error("Module with . after ident")
                    Nil
                  }
                } else {
                  error("Module with ident after END")
                  Nil
                }
              } else {
                error("Module with END after StatementSequence")
                Nil
              }
            } else {
              error("Module with StatementSequence after BEGIN")
              Nil
            }
          } else {
            error("Module with BEGIN after Declarations")
            Nil
          }

        } else {
          error("Module with ; after ident")
          Nil
        }
      } else {
        error("Module with ident after MODULE")
        Nil
      }
    } else {
      error("Module with MODULE")
      Nil
    }
  }

  //Assignment        = ident Selector Õ:=Õ Expression. // tested
  def Assignment(id: Option[Symbol]): Statement = {
    trace("Assignment")
    if (id.isDefined) {
      val s = Selector(IdentNode(id.get))
      if (_def) {
        inc
        val expr = Expression
        if (expr.isDefined) {
          Tree.Assignment(s, expr)
        } else {
          error("Assignment with Expression after Selector")
          Nil
        }
      } else {
        error("Assignment with Selector after :=")
        Nil
      }
    } else {
      error("Assignment with ident")
      Nil
    }
  }

  //ActualParameters  = Expression {Õ,Õ Expression}. // tested
  def ActualParameters: Tree[ActualParameters] = {
    trace("ActualParameters")
    val expr = Expression
    if (expr.isDefined) {
      Tree.ActualParameters(expr, OptionalActualParameters)
    } else {
      error("ActualParameters with Expression")
      Nil
    }
  }

  def OptionalActualParameters: Tree[ActualParameters] = {
    if (comma) {
      inc
      val expr = Expression
      if (expr.isDefined) {
        Tree.ActualParameters(expr, OptionalActualParameters)
      } else {
        error("OptionalExpresion with Expression")
        Nil
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  //ProcedureCall = ident Õ(Õ [ActualParameters] Õ)Õ. // tested
  def ProcedureCall(id: Option[Symbol]): Statement = {
    trace("ProcedureCall")
    if (id.isDefined) {
      if (bracketOn) {
        inc
        if (bracketOff) {
          inc
          Tree.ProcedureCall(IdentNode(id.get))
        } else {
          val a = ActualParameters;
          if (bracketOff) {
            inc
            Tree.ProcedureCall(IdentNode(id.get), a)
          } else {
            error("ProcedureCall ends with )")
            Nil
          }
        }
      } else {
        error("ProcedureCall with (")
        Nil
      }
    } else {
      error("ProcedureCall with ident")
      Nil
    }
  }

  //IfStatement = ÕIFÕ Expression ÕTHENÕ StatementSequence
  //	{ÕELSIFÕ Expression ÕTHENÕ StatementSequence}
  //  	[ÕELSEÕ StatementSequence] ÕENDÕ. // tested
  def IfStatement: Statement = {
    trace("IfStatement")
    if (IF) {
      inc
      val expr = Expression
      if (expr.isDefined) {
        if (THEN) {
          inc
          val sts = StatementSequence
          if (sts.isDefined) {
            val elseif = OptionalELSIF
            if (END) {
              inc
              Tree.IfStatement(expr, sts, elseif)
            } else {
              error("IfStatement with END after StatementSequence")
              Nil
            }
          } else {
            error("IfStatement with StatementSequence after THEN")
            Nil
          }
        } else {
          error("IfStatement with THEN after Expression")
          Nil
        }
      } else {
        error("IfStatement with Expression after IF")
        Nil
      }
    } else {
      error("IfStatement with If")
      Nil
    }
  }

  def OptionalELSIF: Statement = {
    if (ELSIF) {
      inc
      val expr = Expression
      if (expr.isDefined) {
        if (THEN) {
          inc
          val sts = StatementSequence
          if (sts.isDefined) {
            Tree.IfStatement(expr, sts, OptionalELSIF)
          } else {
            error("OptionalELSIF with StatementSequence after THEN")
            Nil
          }
        } else {
          error("OptionalELSIF with THEN after Expression")
          Nil
        }
      } else {
        error("OptionalELSIF with Expression after ELSEIF")
        Nil
      }
    } else {
      if (ELSE) {
        inc
        StatementSequence
      } else
        Nil
    }
  }

  //WhileStatement = ÕWHILEÕ Expression ÕDOÕ StatementSequence ÕENDÕ. // tested
  def WhileStatement: Statement = {
    trace("WhileStatement")
    if (WHILE) {
      inc
      val expr = Expression
      if (expr.isDefined) {
        if (DO) {
          inc
          val sts = StatementSequence
          if (sts.isDefined) {
            if (END) {
              inc
              Tree.WhileStatement(expr, sts)
            } else {
              error("WhileStatement with END after StatementSequence")
              Nil
            }
          } else {
            error("WhileStatement with StatementSequence after DO ")
            Nil
          }
        } else {
          error("WhileStatement with DO after Expression")
          Nil
        }
      } else {
        error("WhileStatement with Expression after WHILE")
        Nil
      }
    } else {
      error("WhileStatement with WHILE")
      Nil
    }
  }

  //RepeatStatement = ÕREPEATÕ StatementSequence ÕUNTILÕ Expression. // tested
  def RepeatStatement: Statement = {
    trace("RepeatStatement")
    if (REPEAT) {
      inc
      val sts = StatementSequence
      if (sts.isDefined) {
        if (UNTIL) {
          inc
          Tree.RepeatStatement(sts, Expression)
        } else {
          error("RepeatStatement with UNTIL after StatementSequence")
          Nil
        }
      } else {
        error("RepeatStatement with StatementSequence after REPEAT")
        Nil
      }
    } else {
      error("RepeatStatement with REPEAT")
      Nil
    }
  }

  //Statement = [Assignment | ProcedureCall |
  //   IfStatement | ÕPRINTÕ Expression |
  //   WhileStatement | RepeatStatement]. // tested
  def Statement: Statement = {
    trace("Statement")
    val assignOrProccall = ident
    val ifst = IF
    val p = PRINT
    val whi = WHILE
    val r = REPEAT
    if (assignOrProccall.isDefined) {
      // we have to look two symbols forward
      val id = assignOrProccall
      inc
      if (bracketOn) {
        ProcedureCall(id)
      } else {
        Assignment(id)
      }
    } else if (ifst) {
      IfStatement
    } else if (p) {
      inc
      Print(Expression)
    } else if (whi) {
      WhileStatement
    } else if (r) {
      RepeatStatement
    } else {
      // no error because its optional
      Nil
    }
  }

  //StatementSequence = Statement {Õ;Õ Statement}. // tested
  def StatementSequence: Statement = {
    trace("StatementSequence")
    Tree.StatementSequence(Statement, OptionalStatementSequence)
  }

  def OptionalStatementSequence: Tree[StatementSequence] = {
    if (semicolon) {
      inc
      val sts = Statement
      if (sts.isDefined) {
        Tree.StatementSequence(sts, OptionalStatementSequence)
      } else {
        // no error because semicolons are optional
        OptionalStatementSequence
      }
    } else {
      // no error because its optional
      Nil
    }
  }

  /*
   *  helper methods
   */

  def inc = {
    current = next
  }

  def next: Option[Symbol] = {
    if (scanner.isEOF)
      None
    else {
      val symbol = scanner.next_token
      if (scanner.isEOF)
        None
      else if (symbol.token != Token.blank)
        Option(symbol)
      else
        next
    }
  }

  def urparserWithBlanks = {
    while (!scanner.isEOF) {
      val s: Symbol = scanner.next_token()
      if (!scanner.isEOF) {
        println(s.toLongString);
      }
    }
  }

  def urparser: Unit = {
    val s = next
    if (s != None) {
      println(s.get.toLongString);
      urparser
    }
  }

  def checkPrimitive(token: String) = {
    //    println(current.get.token + " == " + token)
    if (current.isEmpty)
      false
    else if (current.get.token == token) {
      trace(token)
      true
    } else {
      false
    }
  }

  def checkPrimitiveValue(token: String) = {
    if (current.isEmpty)
      None
    else if (current.get.token == token) {
      trace(token + " " + current.get.value.getOrElse(""))
      current
    } else {
      None
    }
  }

  def trace(s: Any) = {
    if (OberonDebug.parser) {
      if (current.isEmpty)
        println("(n/a)    " + s)
      else
        println(Symbol.linecolumn(current.get.line, current.get.column) + " " + s)
    }
  }

  import java.io.FileReader
  def oberonScanner(path: String) = new OberonScanner(new FileReader(path))

  def error(expectedToken: String) {
    Thread.sleep(40)
    if (current.isEmpty) {
      Console.err.println("(n/a)    Error => " + current + " not expected")
      Console.err.println("(n/a)    We expect:" + expectedToken)
    } else {
      Console.err.println(Symbol.linecolumn(current.get.line, current.get.column) + " Error => " + current.get.token + " not expected")
      Console.err.println(Symbol.linecolumn(current.get.line, current.get.column) + " We expect:" + expectedToken)
    }
    System.exit(-1)
  }

  def test(t: Tree[_]) = {
    if (!current.isEmpty) {
      error("No Token")
      Nil
    } else
      t
  }
}

