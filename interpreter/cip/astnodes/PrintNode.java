package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.exceptions.CompileException;
import cip.instructions.PrintInstruction;

public class PrintNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode outputString;

	public PrintNode() {
		outputString = null;
	};

	public PrintNode(AbstractNode fn) {
		outputString = fn;
	};

	public void setOutputString(AbstractNode fn) {
		outputString = fn;
	};

	public AbstractNode getOutputString() {
		return outputString;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("PrintNode ");
		outputString.compile(symbolTable);
		CodeGen.outInstr(new PrintInstruction());
		unindent();
		return null;
	}

	public void print() {
		trace("PrintNode ");
		outputString.print();
		unindent();
	};
}
