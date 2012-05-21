package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class ReturnNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode returnExpression;

	public ReturnNode() {
		returnExpression = null;
	};

	public ReturnNode(AbstractNode fn) {
		returnExpression = fn;
	};

	public void setReturnExpression(AbstractNode fn) {
		returnExpression = fn;
	};

	public AbstractNode getReturnExpression() {
		return returnExpression;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("ReturnNode ");
		returnExpression.compile(symbolTable);
		CodeGen.outInstr(new JumpInstruction(CodeGen.getStartExitCode()));
		unindent();
		return null;
	}

	public void print() {
		trace("ReturnNode ");
		returnExpression.print();
		unindent();
	};
}
