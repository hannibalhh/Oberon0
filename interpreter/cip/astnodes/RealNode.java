package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.instructions.RealVal;

public class RealNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	float realVal;

	public RealNode() {
		realVal = 0;
	};

	public RealNode(int fr) {
		realVal = fr;
	};

	public void setRealVal(float fr) {
		realVal = fr;
	};

	public float getRealVal() {
		return realVal;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		trace("RealNode " + realVal);
		CodeGen.outInstr(new RealVal(realVal));
		unindent();
		return new SimpleTypeDescr("real");
	};

	public void print() {
		trace("RealNode " + realVal);
		unindent();
	};
}
