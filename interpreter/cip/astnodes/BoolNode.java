package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.instructions.BoolVal;

public class BoolNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	private boolean b;

	public BoolNode() {
		b = false;
	};

	public BoolNode(boolean fb) {
		b = fb;
	};

	public void setB(boolean fb) {
		b = fb;
	};

	public boolean getB() {
		return b;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		trace("BoolNode " + b);
		CodeGen.outInstr(new BoolVal(b));
		unindent();
		return new SimpleTypeDescr("boolean");
	};

	public void print() {
		trace("BoolNode " + b);
		unindent();
	};
}
