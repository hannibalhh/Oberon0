package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class LoopNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode loopCondition;
	AbstractNode firstPart, secondPart;

	public LoopNode() {
		loopCondition = null;
		firstPart = null;
		secondPart = null;
	};

	public LoopNode(AbstractNode fe, AbstractNode fs1, AbstractNode fs2) {
		loopCondition = fe;
		firstPart = fs1;
		secondPart = fs2;
	};

	public void setLoopCondition(AbstractNode fe) {
		loopCondition = fe;
	};

	public void setFirstPart(AbstractNode fs1) {
		firstPart = fs1;
	};

	public void setSecondPart(AbstractNode fs2) {
		secondPart = fs2;
	};

	public AbstractNode getLoopCondition() {
		return loopCondition;
	};

	public AbstractNode getFirstPart() {
		return firstPart;
	};

	public AbstractNode getSecondPart() {
		return secondPart;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		int l1, l2;

		trace("LoopNode");
		l1 = CodeGen.newLabel();
		l2 = CodeGen.newLabel();
		CodeGen.outInstr(new LabelInstruction(l1));
		if (firstPart != null)
			firstPart.compile(symbolTable);
		loopCondition.compile(symbolTable);
		CodeGen.outInstr(new BranchTrueInstruction(l2));
		if (secondPart != null)
			secondPart.compile(symbolTable);
		CodeGen.outInstr(new JumpInstruction(l1));
		CodeGen.outInstr(new LabelInstruction(l2));
		unindent();
		return null;
	};

	public void print() {
		trace("LoopNode ");
		if (firstPart != null)
			firstPart.print();
		loopCondition.print();
		if (secondPart != null)
			secondPart.print();
		unindent();
	};
}
