package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.instructions.IntegerVal;

public class IntNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	int intVal;

	public IntNode() {
		intVal = 0;
	};

	public IntNode(int fi) {
		intVal = fi;
	};

	public void setIntval(int fi) {
		intVal = fi;
	};

	public int getIntVal() {
		return intVal;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		trace("IntNode " + intVal);
		CodeGen.outInstr(new IntegerVal(intVal));
		unindent();
		return new SimpleTypeDescr("integer");
	};

	public void print() {
		trace("IntNode " + intVal);
		unindent();
	};
}
