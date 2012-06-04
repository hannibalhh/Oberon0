package cip;

import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;

import cip.astnodes.*;
import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.base.Operator;
import cip.debug.Debug;
import cip.exceptions.ParsingException;
import cip.gen.Oberon0Lexer;
import cip.interpreter.Interpreter;

/**
 * Die Klasse <code>TreeGenerator</code> implementiert den Parser für die
 * Sprache Oberon0. Für jede Produktion der Grammatik ist ein Methode
 * implementiert. Das Parser startet mit der Methode <code>program()</code> und
 * implementiert dann den rekursiven Abstieg.
 * 
 * @author Voeller
 * 
 */
public class TreeGenerator {

	static int nextsymbol			= 0;
	static int labcnt 				= 0;
	public static HashMap<String, AbstractDescr> symbolTable = new HashMap<String, AbstractDescr>();
	public static int curraddr 		= 0;
	public static int lengthDataSegmentMainProgram = 0;

	public static Boolean debug 	= false;
	static String infile;
	static Oberon0Lexer scanner 	= null;

	/**
	 * Diese Methode liest das nächste erkannte Symnbol des Lexers, bis ein
	 * whitespace im Eingabestrom auftaucht.
	 */

	public static void inSymbol() {
		try {
			while ((nextsymbol = scanner.yylex()) == Oberon0Lexer.whitespace) {
				//System.out.println(Oberon0Lexer.line);
			}
		} catch (java.io.FileNotFoundException e) {
			System.out.println("File not found : \"" + infile + "\"");
		} catch (java.io.IOException e) {
			System.out.println("IO error scanning file \"" + infile + "\"");
			System.out.println(e);
		} catch (Exception e) {
			System.out.println("Unexpected exception:");
			e.printStackTrace();
		}
	}

		private AbstractNode selector(AbstractNode n) throws ParsingException {
		AbstractNode e = null;
		AbstractNode n1 = null;

		n1 = n;
		while ((nextsymbol == Oberon0Lexer.lbrack)
				|| (nextsymbol == Oberon0Lexer.period)) {
			if (nextsymbol == Oberon0Lexer.lbrack) {
				inSymbol();
				e = expression();
				n1 = new ArrayRefNode(n1, e);
				if (nextsymbol == Oberon0Lexer.rbrack)
					inSymbol();
				else {
					throw new ParsingException("] expected!",
							Oberon0Lexer.line, Oberon0Lexer.column);
				}
			} else if (nextsymbol == Oberon0Lexer.period) {
				inSymbol();
				if (nextsymbol == Oberon0Lexer.ident) {
					e = new FieldIdNode(Oberon0Lexer.strval);
					n1 = new RecordRefNode(n1, (FieldIdNode) e);
					inSymbol();
				}
			} else {
				throw new ParsingException("Fieldname expected!",
						Oberon0Lexer.line, Oberon0Lexer.column);
			}
		}
		return n1;
	}

	private AbstractNode factor() throws ParsingException {
		AbstractNode n = null;
		String name = "";

		if (nextsymbol == Oberon0Lexer.lpar) {
			inSymbol();
			n = expression();
			if (nextsymbol == Oberon0Lexer.rpar)
				inSymbol();
			else
				throw new ParsingException(" ) expected", Oberon0Lexer.line,
						Oberon0Lexer.column);
		} else if (nextsymbol == Oberon0Lexer.intconst) {
			n = new IntNode(Oberon0Lexer.intval);
			inSymbol();
		} else if (nextsymbol == Oberon0Lexer.readsy) {
			inSymbol();
			if (nextsymbol == Oberon0Lexer.stringconst) {
				n = new ReadNode(new StringNode(Oberon0Lexer.strval));
				inSymbol();
			} else
				n = new ReadNode(null);
		} else if (nextsymbol == Oberon0Lexer.stringconst) {
			n = new StringNode(Oberon0Lexer.strval);
			inSymbol();
		} else if (nextsymbol == Oberon0Lexer.ident) {
			name = Oberon0Lexer.strval;
			inSymbol();
			if (nextsymbol == Oberon0Lexer.lpar) {
				n = call(name);
			} else
				n = new ContNode(selector(new IdfNode(name)));
		} else if (nextsymbol == Oberon0Lexer.notsy) {
			inSymbol();
			// n = expression();
			throw new ParsingException("NOT not yet implemented",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		return n;
	}

	private AbstractNode term() throws ParsingException {
		AbstractNode r = null, n;
		int lop = 0;
		int lsy;

		n = factor();
		while ((nextsymbol == Oberon0Lexer.multop)
				|| (nextsymbol == Oberon0Lexer.divop)
				|| (nextsymbol == Oberon0Lexer.divsy)
				|| (nextsymbol == Oberon0Lexer.andsy)
				|| (nextsymbol == Oberon0Lexer.modsy)) {
			// DIV, MOD, AND not yet implemented!!
			lsy = nextsymbol;
			inSymbol();
			r = factor();
			if (lsy == Oberon0Lexer.multop) {
				lop = Operator.MUL;
			} else {
				lop = Operator.DIV;
			}
			n = new BinNode(lop, n, r);
		}
		return n;
	}

	private AbstractNode simpleExpression() throws ParsingException {
		AbstractNode r = null, n;
		int lsy;
		int lop = 0;
		int unminus = 0;

		if (nextsymbol == Oberon0Lexer.subop) {
			unminus = 1;
			inSymbol();
		}
		n = term();
		if (unminus == 1)
			n = new BinNode(Operator.SUB, new IntNode(0), n);
		while ((nextsymbol == Oberon0Lexer.addop)
				|| (nextsymbol == Oberon0Lexer.subop)
				|| (nextsymbol == Oberon0Lexer.orsy)) {
			// OR not yet implemented
			lsy = nextsymbol;
			inSymbol();
			r = term();
			if (lsy == Oberon0Lexer.addop) {
				lop = Operator.ADD;
			} else {
				lop = Operator.SUB;
			}
			n = new BinNode(lop, n, r);
		}
		return n;
	}

	private AbstractNode expression() throws ParsingException {
		AbstractNode n, r = null;
		int lop = 0;
		int lsy;

		n = simpleExpression();
		if ((nextsymbol >= Oberon0Lexer.eqsy)
				&& (nextsymbol <= Oberon0Lexer.gesy)) {
			lsy = nextsymbol;
			inSymbol();
			r = simpleExpression();
			if (lsy == Oberon0Lexer.eqsy) {
				lop = Operator.EQUALS;
			} else if (lsy == Oberon0Lexer.neqsy) {
				lop = Operator.NOT_EQUALS;
			} else if (lsy == Oberon0Lexer.ltsy) {
				lop = Operator.LOWER_THAN;
			} else if (lsy == Oberon0Lexer.gtsy) {
				lop = Operator.GREATER_THAN;
			} else if (lsy == Oberon0Lexer.lesy) {
				lop = Operator.LOWER_EQUAL_THAN;
			} else if (lsy == Oberon0Lexer.gesy) {
				lop = Operator.GREATER_EQUAL_THAN;
			}
			n = new BinNode(lop, n, r);
		}
		return n;
	}

// Anweisungen
	
	private AssNode assignment(String id) throws ParsingException {
		AbstractNode l = null, r = null;
		l = selector(new IdfNode(id));
		if (nextsymbol == Oberon0Lexer.becomesy) {
			inSymbol();
		} else
			throw new ParsingException("= expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		r = expression();
		return new AssNode(l, r);
	}

	private CallNode call(String id) throws ParsingException {
		ArrayList<AbstractNode> params = null;
		if (nextsymbol == Oberon0Lexer.lpar)
			inSymbol();
		else {
			throw new ParsingException("( expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if ((nextsymbol != Oberon0Lexer.rpar)) {
			params = new ArrayList<AbstractNode>();
			params.add(expression());
			while (nextsymbol == Oberon0Lexer.comma) {
				inSymbol();
				params.add(expression());
			}
		}
		if (nextsymbol == Oberon0Lexer.rpar)
			inSymbol();
		else {
			throw new ParsingException(") expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}

		;
		return new CallNode(new IdfNode(id), params);
	}

	private ReturnNode returnStatement() throws ParsingException {
		AbstractNode e;

		inSymbol();
		e = expression();
		return new ReturnNode(e);
	}

	private PrintNode printStatement() throws ParsingException {
		AbstractNode e;
		inSymbol();
		e = expression();
		return new PrintNode(e);
	}

	private IfNode conditionalStatement() throws ParsingException {
		AbstractNode e = null, st1 = null, st2 = null;
		IfNode n, n1, n2;
		if (nextsymbol == Oberon0Lexer.ifsy)
			inSymbol();
		else
			throw new ParsingException("IF expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		e = expression();
		if (nextsymbol == Oberon0Lexer.thensy)
			inSymbol();
		else
			throw new ParsingException("THEN expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		st1 = statementSequence();
		n = new IfNode(e, st1, null);
		n1 = n;
		while (nextsymbol == Oberon0Lexer.elsifsy) {
			inSymbol();
			e = expression();
			if (nextsymbol == Oberon0Lexer.thensy)
				inSymbol();
			else
				throw new ParsingException("THEN expected", Oberon0Lexer.line,
						Oberon0Lexer.column);
			st2 = statementSequence();
			n2 = new IfNode(e, st2, null);
			n1.setElsePart(n2);
			n1 = n2;
		}
		if (nextsymbol == Oberon0Lexer.elsesy) {
			inSymbol();
			st2 = statementSequence();
			n1.setElsePart(st2);
		}
		if (nextsymbol == Oberon0Lexer.endsy)
			inSymbol();
		else
			throw new ParsingException("END expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		return n;
	}

	private WhileNode whileStatement() throws ParsingException {
		AbstractNode e = null, st = null;
		if (nextsymbol == Oberon0Lexer.whilesy)
			inSymbol();
		else
			throw new ParsingException("WHILE expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		e = expression();
		if (nextsymbol == Oberon0Lexer.dosy)
			inSymbol();
		else
			throw new ParsingException("DO expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		st = statementSequence();
		if (nextsymbol == Oberon0Lexer.endsy)
			inSymbol();
		else
			throw new ParsingException("END expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		return new WhileNode(e, st);
	}

	private LoopNode loopStatement() throws ParsingException {
		ListNode l1, l2;
		AbstractNode e = null;

		if (nextsymbol == Oberon0Lexer.loopsy)
			inSymbol();
		else
			throw new ParsingException("LOOP expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		l1 = statementSequence();
		if (nextsymbol == Oberon0Lexer.exitsy)
			inSymbol();
		else
			throw new ParsingException("EXIT expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		if (nextsymbol == Oberon0Lexer.ifsy)
			inSymbol();
		else
			throw new ParsingException("IF expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		e = expression();
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else
			throw new ParsingException("';' expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		l2 = statementSequence();
		if (nextsymbol == Oberon0Lexer.endsy)
			inSymbol();
		else
			throw new ParsingException("END expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		return new LoopNode(e, l1, l2);
	}

	private LoopNode repeatStatement() throws ParsingException {
		ListNode l1;
		AbstractNode e = null;

		if (nextsymbol == Oberon0Lexer.repeatsy)
			inSymbol();
		else
			throw new ParsingException("REPEAT expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		l1 = statementSequence();
		if (nextsymbol == Oberon0Lexer.untilsy)
			inSymbol();
		else
			throw new ParsingException("UNTIL expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		e = expression();
		return new LoopNode(e, null, l1);
	}

	private AbstractNode statement() throws ParsingException {
		AbstractNode st = null;
		String id;

		if (nextsymbol == Oberon0Lexer.beginsy) {
			inSymbol();
			st = statementSequence();
			if (nextsymbol == Oberon0Lexer.endsy)
				inSymbol();
			else {
				throw new ParsingException("END expected", Oberon0Lexer.line,
						Oberon0Lexer.column);
			}
		} else if (nextsymbol == Oberon0Lexer.ident) {
			id = Oberon0Lexer.strval;
			inSymbol();
			if ((nextsymbol == Oberon0Lexer.becomesy)
					|| (nextsymbol == Oberon0Lexer.lbrack)
					|| (nextsymbol == Oberon0Lexer.period))
				st = assignment(id);
			else
				st = call(id);
		} else if (nextsymbol == Oberon0Lexer.ifsy)
			st = conditionalStatement();
		else if (nextsymbol == Oberon0Lexer.whilesy)
			st = whileStatement();
		else if (nextsymbol == Oberon0Lexer.loopsy)
			st = loopStatement();
		else if (nextsymbol == Oberon0Lexer.repeatsy)
			st = repeatStatement();
		else if (nextsymbol == Oberon0Lexer.returnsy)
			st = returnStatement();
		else if (nextsymbol == Oberon0Lexer.printsy)
			st = printStatement();
		else
			;
		return st;
	}

	private ListNode statementSequence() throws ParsingException {
		ArrayList<AbstractNode> l = new ArrayList<AbstractNode>();

		l.add(statement());
		while (nextsymbol == Oberon0Lexer.semicolon) {
			inSymbol();
			l.add(statement());
		}
		;
		return new ListNode(l);
	}

	// Typen und Deklarationen
	
	private ArrayNode arrayType() throws ParsingException {
		AbstractNode length = null;
		AbstractNode basetype = null;

		inSymbol(); // arraysy
		if (nextsymbol == Oberon0Lexer.lbrack)
			inSymbol();
		else {
			throw new ParsingException("[ expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.intconst) {
			length = new IntNode(Oberon0Lexer.intval);
			inSymbol();
		} else if (nextsymbol == Oberon0Lexer.ident) {
			length = new IdfNode(Oberon0Lexer.strval);
			inSymbol();
		} else {
			throw new ParsingException("Only integer constant implemented!",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.rbrack)
			inSymbol();
		else {
			throw new ParsingException("] expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.ofsy)
			inSymbol();
		else {
			throw new ParsingException("OF expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		basetype = type();
		return new ArrayNode(length, basetype);
	}

	private ArrayList<IdfNode> identList() throws ParsingException {
		ArrayList<IdfNode> vl = new ArrayList<IdfNode>();

		vl.add(new IdfNode(Oberon0Lexer.strval));
		inSymbol();
		while (nextsymbol == Oberon0Lexer.comma) {
			inSymbol();
			if (nextsymbol == Oberon0Lexer.ident) {
				vl.add(new IdfNode(Oberon0Lexer.strval));
				inSymbol();
			} else {
				throw new ParsingException("idf expected\n", Oberon0Lexer.line,
						Oberon0Lexer.column);
			}
		}
		return vl;
	}

	private FieldNode fieldList() throws ParsingException {
		ArrayList<IdfNode> vl = new ArrayList<IdfNode>();
		AbstractNode t = null;

		if (nextsymbol == Oberon0Lexer.ident) {
			vl = identList();
			if (nextsymbol == Oberon0Lexer.colon)
				inSymbol();
			else {
				throw new ParsingException("Colon expected\n",
						Oberon0Lexer.line, Oberon0Lexer.column);
			}
			t = type();
			return new FieldNode(vl, t);
		} else
			return null;
	}

	private RecordNode recordType() throws ParsingException

	{
		ArrayList<FieldNode> vl = new ArrayList<FieldNode>();
		FieldNode fn;
		RecordNode rn;

		inSymbol(); // record
		fn = fieldList();
		if (fn != null)
			vl.add(fn);
		while (nextsymbol == Oberon0Lexer.semicolon) {
			inSymbol();
			fn = fieldList();
			if (fn != null)
				vl.add(fn);
		}
		if (vl.size() > 0)
			rn = new RecordNode(new FieldListNode(vl));
		else
			rn = new RecordNode(null);
		if (nextsymbol == Oberon0Lexer.endsy)
			inSymbol();
		else {
			throw new ParsingException("END expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		return rn;
	}

	private AbstractNode type() throws ParsingException {
		AbstractNode t;

		if ((nextsymbol == Oberon0Lexer.ident)
				|| (nextsymbol == Oberon0Lexer.integersy)
				|| (nextsymbol == Oberon0Lexer.realsy)
				|| (nextsymbol == Oberon0Lexer.booleansy)
				|| (nextsymbol == Oberon0Lexer.charsy)
				|| (nextsymbol == Oberon0Lexer.stringsy)) {
			t = new IdfNode(Oberon0Lexer.strval);
			inSymbol();
			return (t);
		} else if (nextsymbol == Oberon0Lexer.arraysy) {
			return arrayType();
		} else if (nextsymbol == Oberon0Lexer.recordsy) {
			return recordType();
		} else {
			throw new ParsingException("Type expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
	}

	private ConstNode constantDeclaration() throws ParsingException {
		IdfNode ci = null;
		AbstractNode v = null;

		if (nextsymbol == Oberon0Lexer.ident) {
			ci = new IdfNode(Oberon0Lexer.strval);
			inSymbol();
		} else {
			throw new ParsingException("idf expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.eqsy)
			inSymbol();
		else {
			throw new ParsingException("= expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.intconst) {
			v = new IntNode(Oberon0Lexer.intval);
			inSymbol();
		}
		else {
			throw new ParsingException("Constant expected\n",
					Oberon0Lexer.line, Oberon0Lexer.column);}
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else {
			throw new ParsingException("Semicolon expected\n",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		return new ConstNode(ci, v);
	}

	private ConstantDeclarationNode constantDeclarations()
			throws ParsingException {
		ArrayList<ConstNode> vl = new ArrayList<ConstNode>();

		if (nextsymbol == Oberon0Lexer.constsy)
			inSymbol();
		else {
			throw new ParsingException("CONTOP expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		vl.add(constantDeclaration());
		while (nextsymbol == Oberon0Lexer.ident)
			vl.add(constantDeclaration());
		return new ConstantDeclarationNode(vl);

	};

	private VarNode variableDeclaration() throws ParsingException {
		ArrayList<AbstractNode> vl = new ArrayList<AbstractNode>();
		AbstractNode t;

		if (nextsymbol == Oberon0Lexer.ident) {
			vl.add(new IdfNode(Oberon0Lexer.strval));
			inSymbol();
		}
		while (nextsymbol == Oberon0Lexer.comma) {
			if (nextsymbol == Oberon0Lexer.comma)
				inSymbol();
			else {
				throw new ParsingException("comma expected\n",
						Oberon0Lexer.line, Oberon0Lexer.column);
			}
			if (nextsymbol == Oberon0Lexer.ident) {
				vl.add(new IdfNode(Oberon0Lexer.strval));
				inSymbol();
			} else {
				throw new ParsingException("idf expected\n", Oberon0Lexer.line,
						Oberon0Lexer.column);
			}
		}
		if (nextsymbol == Oberon0Lexer.colon)
			inSymbol();
		else {
			throw new ParsingException("Colon expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		t = type();
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else {
			throw new ParsingException("Semicolon expected\n",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		return new VarNode(vl, t);
	}

	private VariableDeclarationNode variableDeclarations()
			throws ParsingException {

		ArrayList<VarNode> vl = new ArrayList<VarNode>();

		if (nextsymbol == Oberon0Lexer.varsy)
			inSymbol();
		else {
			throw new ParsingException("VAR expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		vl.add(variableDeclaration());
		while (nextsymbol == Oberon0Lexer.ident)
			vl.add(variableDeclaration());
		return new VariableDeclarationNode(vl);
	}

	private TypeNode typeDeclaration() throws ParsingException {
		IdfNode ci = null;
		AbstractNode v = null;

		if (nextsymbol == Oberon0Lexer.ident) {
			ci = new IdfNode(Oberon0Lexer.strval);
			inSymbol();
		} else {
			throw new ParsingException("idf expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.eqsy)
			inSymbol();
		else {
			throw new ParsingException("= expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		v = type();
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else {
			throw new ParsingException("Semicolon expected\n",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		return new TypeNode(ci, v);
	}

	private TypeDeclarationNode typeDeclarations() throws ParsingException {
		ArrayList<TypeNode> vl = new ArrayList<TypeNode>();

		if (nextsymbol == Oberon0Lexer.typesy)
			inSymbol();
		else {
			throw new ParsingException("TYPE expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		vl.add(typeDeclaration());
		while (nextsymbol == Oberon0Lexer.ident)
			vl.add(typeDeclaration());
		return new TypeDeclarationNode(vl);

	};

	private ParNode parameterDeclaration() throws ParsingException {
		ArrayList<AbstractNode> pl = new ArrayList<AbstractNode>();
		AbstractNode t;
		boolean isvarpar = false;

		if (nextsymbol == Oberon0Lexer.varsy) {
			isvarpar = true;
			inSymbol();
		}

		if (nextsymbol == Oberon0Lexer.ident) {
			pl.add(new IdfNode(Oberon0Lexer.strval));
			inSymbol();
		}
		while (nextsymbol == Oberon0Lexer.comma) {
			inSymbol();
			if (nextsymbol == Oberon0Lexer.ident) {
				pl.add(new IdfNode(Oberon0Lexer.strval));
				inSymbol();
			} else {
				throw new ParsingException("idf expected\n", Oberon0Lexer.line,
						Oberon0Lexer.column);
			}
		}
		if (nextsymbol == Oberon0Lexer.colon)
			inSymbol();
		else {
			throw new ParsingException("Colon expected\n", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		t = type(); // only TypeId allowed, not checked!!
		return new ParNode(isvarpar, pl, t);
	}

	private ParDecNode parameterList() throws ParsingException {
		ArrayList<ParNode> pl = new ArrayList<ParNode>();
		if (nextsymbol != Oberon0Lexer.rpar) {
			pl.add(parameterDeclaration());
			while (nextsymbol == Oberon0Lexer.semicolon) {
				inSymbol();
				pl.add(parameterDeclaration());
			}
			return new ParDecNode(pl);
		} else
			return null;
	}

	private ProcedureNode procedureDeclaration() throws ParsingException {
		String procname 					= "";
		ParDecNode pardecs 					= null;
		IdfNode returntype 					= null;
		ConstantDeclarationNode constdecs 	= null;
		VariableDeclarationNode vardecs 	= null;
		TypeDeclarationNode typedecs 		= null;
		ProcedureDeclarationNode procdecs 	= null;
		ListNode body 						= null;
		boolean isFunction 					= false;

		if ((nextsymbol == Oberon0Lexer.proceduresy)
				|| (nextsymbol == Oberon0Lexer.functionsy)) {
			isFunction = (nextsymbol == Oberon0Lexer.functionsy);
			inSymbol();
		} else {
			throw new ParsingException("procedure or function expected",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.ident) {
			procname = Oberon0Lexer.strval;
			inSymbol();
		} else {
			throw new ParsingException("procedure name expected",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}

		if (nextsymbol == Oberon0Lexer.lpar)
			inSymbol();
		else {
			throw new ParsingException("left parenthesis expected",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		pardecs = parameterList();
		if (nextsymbol == Oberon0Lexer.rpar)
			inSymbol();
		else {
			throw new ParsingException("right parenthesis expected",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		if (isFunction) {// parse ": type"
			if (nextsymbol == Oberon0Lexer.colon) {
				inSymbol();
				if ((nextsymbol == Oberon0Lexer.ident)
						|| (nextsymbol == Oberon0Lexer.integersy)) {
					returntype = new IdfNode(Oberon0Lexer.strval);
					inSymbol();
				} else {
					throw new ParsingException("Typeidentifier expected",
							Oberon0Lexer.line, Oberon0Lexer.column);
				}
			} else {
				throw new ParsingException("Colon expected", Oberon0Lexer.line,
						Oberon0Lexer.column);
			}
		}
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else {
			throw new ParsingException("Semicolon expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		if (nextsymbol == Oberon0Lexer.forwardsy) {
			inSymbol();
		} else {
			if (nextsymbol == Oberon0Lexer.constsy)
				constdecs = constantDeclarations();

			if (nextsymbol == Oberon0Lexer.typesy)
				typedecs = typeDeclarations();

			if (nextsymbol == Oberon0Lexer.varsy)
				vardecs = variableDeclarations();

			if (nextsymbol == Oberon0Lexer.proceduresy)
				procdecs = procedureDeclarations();

			if (nextsymbol == Oberon0Lexer.beginsy) {
				inSymbol();
				body = statementSequence();
			}
			if (nextsymbol == Oberon0Lexer.endsy)
				inSymbol();
			else {
				throw new ParsingException("end expected", Oberon0Lexer.line,
						Oberon0Lexer.column);
			}
			if (nextsymbol == Oberon0Lexer.ident)
				inSymbol();
			else {
				throw new ParsingException("procedure name expected",
						Oberon0Lexer.line, Oberon0Lexer.column);
			}
		}
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else {
			throw new ParsingException("Semicolon expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}

		return new ProcedureNode(procname, pardecs, returntype, constdecs,
				typedecs, vardecs, procdecs, body);
	}

	/**
	 * Alle vorhandenen Prozedurdeklarationen werden in eine Liste gepackt und
	 * zurückgegeben.
	 * 
	 * @return Liste der gefundenen Prozedurdeklarationen.
	 * @throws ParsingException
	 */
	private ProcedureDeclarationNode procedureDeclarations()
			throws ParsingException {
		ArrayList<ProcedureNode> procedureDeclarationList = new ArrayList<ProcedureNode>();

		while ((nextsymbol == Oberon0Lexer.proceduresy)
				|| (nextsymbol == Oberon0Lexer.functionsy)) {
			procedureDeclarationList.add(procedureDeclaration());
		}

		return new ProcedureDeclarationNode(procedureDeclarationList);
	}

	/**
	 * Die Einstiegsmethode des Parsers. Sie implementiert folgende Produktion
	 * PROGRAM-> module ID
	 * 
	 * @return
	 * @throws ParsingException
	 */
	private AbstractNode program() throws ParsingException {
		String moduleName = "";
		ConstantDeclarationNode constantDeclarations = null;
		VariableDeclarationNode variableDeclarations = null;
		TypeDeclarationNode typeDeclarations = null;
		ProcedureDeclarationNode procedurDeclarations = null;
		ListNode body = null;

		// Keyword 'module' muss das erste TOKEN im Tokenstream sein.
		if (nextsymbol == Oberon0Lexer.modulesy)
			inSymbol();
		else {
			throw new ParsingException("module expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		// Als nächstes muss der ID des Moduls kommen.
		if (nextsymbol == Oberon0Lexer.ident) {
			moduleName = Oberon0Lexer.strval;
			inSymbol();
		} else {
			throw new ParsingException("program name expected",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		// Dann muss ein Semikolon folgen.
		if (nextsymbol == Oberon0Lexer.semicolon)
			inSymbol();
		else {
			throw new ParsingException("Semicolon expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		// Dann können verschiedene Deklarationen folgen.
		if (nextsymbol == Oberon0Lexer.constsy)
			constantDeclarations = constantDeclarations();

		if (nextsymbol == Oberon0Lexer.typesy)
			typeDeclarations = typeDeclarations();

		if (nextsymbol == Oberon0Lexer.varsy)
			variableDeclarations = variableDeclarations();

		if ((nextsymbol == Oberon0Lexer.proceduresy)
				|| (nextsymbol == Oberon0Lexer.functionsy))
			procedurDeclarations = procedureDeclarations();

		// Dann kommt das 'begin' Symbol und die statementSequence().
		if (nextsymbol == Oberon0Lexer.beginsy) {
			inSymbol();
			body = statementSequence();
		}
		// Das Ende des Programmblocks
		if (nextsymbol == Oberon0Lexer.endsy)
			inSymbol();
		else {
			throw new ParsingException("end expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}
		// Der Modulname muss dem am Anfang gleichen
		if (nextsymbol == Oberon0Lexer.ident)
			inSymbol();
		else {
			throw new ParsingException("Module name expected",
					Oberon0Lexer.line, Oberon0Lexer.column);
		}
		// Punkt beendet das Modul
		if (nextsymbol == Oberon0Lexer.period)
			inSymbol();
		else {
			throw new ParsingException("period expected", Oberon0Lexer.line,
					Oberon0Lexer.column);
		}

		return new ProgramNode(moduleName, constantDeclarations,
				typeDeclarations, variableDeclarations, procedurDeclarations,
				body);
	}

	/**
	 * Startet den Parser
	 */

	public static void main(String[] argv) {

		AbstractNode root = null;
		Interpreter myIntp = null;
		CodeGen myCodeGen = null;
		TreeGenerator recursiveDecentParser = new TreeGenerator();

		System.out.println("------------------------ start");

		if (argv.length < 2) {
			System.out
					.println("Usage : java MyRecursiveDescent <inputfile> cip.debug=(0|1)");
		} else {
			try {

				infile = argv[0];
				Debug.debug = Integer.parseInt(argv[1]);
				debug = Debug.debug > 0;
				scanner = new Oberon0Lexer(new java.io.FileReader(infile));
				inSymbol();

				if (nextsymbol > 0)
					root = recursiveDecentParser.program();
				((ProgramNode) root).print();
				String nodefile = "tmpnodes";
				String instrfile = "tmpinstr";

				FileOutputStream f = new FileOutputStream(nodefile);
				ObjectOutputStream os = new ObjectOutputStream(f);

				if (debug)
					System.out.println("\n"
							+ "== abstract tree rausschreiben ==");
				os.writeObject(root);
				os.flush();
				myCodeGen = new CodeGen();
				myCodeGen.start(instrfile, nodefile);
				myIntp = new Interpreter(instrfile);
				myIntp.start();

			} catch (java.io.FileNotFoundException e) {
				System.out.println("File not found : \"" + argv[0] + "\"");
			} catch (ParsingException e) {
				System.out.println("Unexpected exception:");
				System.out.println(e.getMessage());
				e.printStackTrace();
			} catch (Exception e) {
				System.out.println("Unexpected exception:");
				e.printStackTrace();
			}
		}
	}
}
