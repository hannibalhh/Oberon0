package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class AssNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode l;
	AbstractNode r;

	public AssNode() {
		l = null;
		r = null;
	};

	public AssNode(AbstractNode fl, AbstractNode fr) {
		l = fl;
		r = fr;
	};

	public void setL(AbstractNode fl) {
		l = fl;
	};

	public void setR(AbstractNode fr) {
		r = fr;
	};

	public AbstractNode getL() {
		return l;
	};

	public AbstractNode getR() {
		return r;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		AbstractDescr t;
		trace("AssNode");
		r.compile(symbolTable);
		t = l.compile(symbolTable);
		CodeGen.outInstr(new AssignmentInstruction(t.getSize()));
		unindent();
		return null;
	};

	public void print() {
		trace("AssNode");
		l.print();
		r.print();
		unindent();
	};
}
