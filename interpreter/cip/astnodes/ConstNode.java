package cip.astnodes;

import java.util.HashMap;

import cip.base.*;
import cip.descriptor.IntConstDescr;

public class ConstNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	IdfNode constName;
	AbstractNode constVal;

	public ConstNode() {
		constName = null;
		constVal = null;
	};

	public ConstNode(IdfNode fi, AbstractNode fv) {
		constName = fi;
		constVal = fv;
	};

	public void setConstName(IdfNode fi) {
		constName = fi;
	};

	public void setConstVal(AbstractNode fv) {
		constVal = fv;
	};

	public IdfNode getConstName() {
		return constName;
	};

	public AbstractNode getConstVal() {
		return constVal;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		IntConstDescr descr;

		trace("ConstNode ");
		if (constVal instanceof IntNode) {
			descr = new IntConstDescr(((IntNode) constVal).getIntVal());
			symbolTable.put(constName.getIdName(), descr);
		}
		unindent();
		return null;
	};

	public void print() {
		trace("ConstNode ");
		unindent();
	};
}
