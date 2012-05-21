package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.descriptor.ArrayDescr;
import cip.exceptions.CompileException;
import cip.instructions.AdditionInstruction;
import cip.instructions.IntegerVal;
import cip.instructions.MultiplicationInstruction;

public class ArrayRefNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode arrayVariable;
	AbstractNode index;

	public ArrayRefNode() {
		arrayVariable = null;
		index = null;
	};

	public ArrayRefNode(AbstractNode fv, AbstractNode fi) {
		arrayVariable = fv;
		index = fi;
	};

	public void setArrayVariable(AbstractNode fv) {
		arrayVariable = fv;
	};

	public void setIndex(AbstractNode fi) {
		index = fi;
	};

	public AbstractNode getArrayVariable() {
		return arrayVariable;
	};

	public AbstractNode getIndex() {
		return index;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		AbstractDescr e;

		trace("ArrayRefNode ");
		e = arrayVariable.compile(symbolTable);
		index.compile(symbolTable);
		CodeGen.outInstr(new IntegerVal((((ArrayDescr) e).GetBasetype()).getSize()));
		CodeGen.outInstr(new MultiplicationInstruction());
		CodeGen.outInstr(new AdditionInstruction());
		unindent();
		return ((ArrayDescr) e).GetBasetype();
	};

	public void print() {
		trace("ArrayRefNode ");
		arrayVariable.print();
		index.print();
		unindent();
	};
}
