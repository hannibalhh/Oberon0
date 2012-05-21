package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.base.Operator;
import cip.exceptions.CompileException;
import cip.instructions.AdditionInstruction;
import cip.instructions.DivisionInstruction;
import cip.instructions.EqualsInstruction;
import cip.instructions.GreaterEqualThanInstruction;
import cip.instructions.GreaterThanInstruction;
import cip.instructions.LessEqualThanInstruction;
import cip.instructions.LessThanInstruction;
import cip.instructions.MultiplicationInstruction;
import cip.instructions.NotEqualInstruction;
import cip.instructions.SubtractionInstruction;

public class BinNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode leftNode;
	AbstractNode rightNode;

	private int operator;

	public int getOperator() {
		return operator;
	}

	public void setOperator(int foperator) {
		this.operator = foperator;
	}

	public BinNode() {
		setOperator(0);
		leftNode = null;
		rightNode = null;
	};

	public BinNode(int fop) {
		setOperator(fop);
		leftNode = null;
		rightNode = null;
	};

	public BinNode(int fop, AbstractNode fl, AbstractNode fr) {
		setOperator(fop);
		leftNode = fl;
		rightNode = fr;
	};

	public void setLeftNode(AbstractNode fl) {
		leftNode = fl;
	};

	public void setRightNode(AbstractNode fr) {
		rightNode = fr;
	};

	public AbstractNode getLeftNode() {
		return leftNode;
	};

	public AbstractNode getRightNode() {
		return rightNode;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException {
		AbstractDescr dl;
		trace("BinNode " + getOperator());
		dl = leftNode.compile(symbolTable);
		dl = rightNode.compile(symbolTable);
		if (this.getOperator() == Operator.ADD) {
			CodeGen.outInstr(new AdditionInstruction());
		} else if (this.getOperator() == Operator.SUB) {
			CodeGen.outInstr(new SubtractionInstruction());
		} else if (this.getOperator() == Operator.DIV) {
			CodeGen.outInstr(new DivisionInstruction());
		} else if (this.getOperator() == Operator.MUL) {
			CodeGen.outInstr(new MultiplicationInstruction());
		} else if (this.getOperator() == Operator.EQUALS) {
			CodeGen.outInstr(new EqualsInstruction());
		} else if (this.getOperator() == Operator.NOT_EQUALS) {
			CodeGen.outInstr(new NotEqualInstruction());
		} else if (this.getOperator() == Operator.LOWER_EQUAL_THAN) {
			CodeGen.outInstr(new LessEqualThanInstruction());
		} else if (this.getOperator() == Operator.LOWER_THAN) {
			CodeGen.outInstr(new LessThanInstruction());
		} else if (this.getOperator() == Operator.GREATER_EQUAL_THAN) {
			CodeGen.outInstr(new GreaterEqualThanInstruction());
		} else if (this.getOperator() == Operator.GREATER_THAN) {
			CodeGen.outInstr(new GreaterThanInstruction());
		} else {
			System.err.println("BinInstr()");
		}
		unindent();
		return dl;
	};

	public void print() {
		trace("BinNode " + getOperator());
		leftNode.print();
		rightNode.print();
		unindent();
	};
}