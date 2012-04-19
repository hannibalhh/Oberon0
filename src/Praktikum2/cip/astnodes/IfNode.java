package Praktikum2.cip.astnodes;

import java.util.HashMap;

import Praktikum2.cip.base.AbstractDescr;
import Praktikum2.cip.base.AbstractNode;
import Praktikum2.cip.base.CodeGen; //import cip.base.Operator;
import Praktikum2.cip.instructions.*;

public class IfNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode condition;
	AbstractNode thenPart, elsePart;

	public IfNode() {
		condition = null;
		thenPart = null;
		elsePart = null;
	};

	public IfNode(AbstractNode fe, AbstractNode fst1, AbstractNode fst2) {
		condition = fe;
		thenPart = fst1;
		elsePart = fst2;
	};

	public void setCondition(AbstractNode fe) {
		condition = fe;
	};

	public void setThenPart(AbstractNode fst1) {
		thenPart = fst1;
	};

	public void setElsePart(AbstractNode fst2) {
		elsePart = fst2;
	};

	public AbstractNode getCondition() {
		return condition;
	};

	public AbstractNode getThenPart() {
		return thenPart;
	};

	public AbstractNode getElsePart() {
		return elsePart;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		int l1, l2;

		trace("IfNode");
		l1 = CodeGen.newLabel();
		l2 = CodeGen.newLabel();
		condition.compile(symbolTable);
		CodeGen.outInstr(new BranchFalseInstruction(l1));
		if (thenPart != null)
			thenPart.compile(symbolTable);
		CodeGen.outInstr(new JumpInstruction(l2));
		CodeGen.outInstr(new LabelInstruction(l1));
		if (elsePart != null)
			elsePart.compile(symbolTable);
		CodeGen.outInstr(new LabelInstruction(l2));
		unindent();
		return null;
	};

	public void print() {
		trace("IfNode ");
		condition.print();
		if (thenPart != null)
			thenPart.print();
		if (elsePart != null)
			elsePart.print();
		unindent();
	};
}