package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.*;
import cip.exceptions.CompileException;
import cip.gen.Oberon0Lexer;
import cip.instructions.AdditionInstruction;
import cip.instructions.ContInstruction;
import cip.instructions.GetFP;
import cip.instructions.GetSL;
import cip.instructions.IntegerVal;

public class IdfNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	String idName;
	int line;

	public int getLine() {
		return line;
	}

	public void setLine(int line) {
		this.line = line;
	}

	public IdfNode() {
		idName = "";
	};

	public IdfNode(String fs) {
		idName = fs;
		line = Oberon0Lexer.line;
	};

	public void setIdName(String fs) {
		idName = fs;
	};

	public String getIdName() {
		return idName;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		AbstractDescr e;
		int level, addr = 0;

		AbstractDescr t = null; // assume integer!!!!

		e = CodeGen.search(idName,line);
		level = e.getLevel();
		if (level >= 0) {
			if ((e instanceof ArrayDescr) || (e instanceof RecordDescr)
					|| (e instanceof SimpleTypeDescr))
				return e;
		} else
			return null;
		trace("IdfNode " + idName + " " + level);
		if (e instanceof VarDescr) {
			addr = ((VarDescr) e).getAddr();
			t = ((VarDescr) e).getTyp();
			CodeGen.outInstr(new IntegerVal(addr));
			if (level > 0) {
				if (level == CodeGen.level) {
					CodeGen.outInstr(new GetFP());
					CodeGen.outInstr(new AdditionInstruction());
				} else {
					CodeGen.outInstr(new IntegerVal(level));
					CodeGen.outInstr(new GetSL());
					CodeGen.outInstr(new AdditionInstruction());
				}
			}
			if (((VarDescr) e).getIsvarpar()) {
				CodeGen.outInstr(new ContInstruction(1));
			}
		} else if (e instanceof IntConstDescr) {
			CodeGen.outInstr(new IntegerVal(((IntConstDescr) e).getIntVal()));
			t = new SimpleTypeDescr("integer");
		}
		unindent();
		return t;
	};

	public void print() {
		trace("IdfNode " + idName);
		unindent();
	};
}
