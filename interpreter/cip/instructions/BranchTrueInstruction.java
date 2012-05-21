package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class BranchTrueInstruction extends AbstractInstruction implements
		Instruction {

	private int destination;

	public BranchTrueInstruction(int fd) {
		destination = fd;
	};

	public void setDestination(int fd) {
		destination = fd;
	};

	public int getDestination() {
		return destination;
	}

	private static final long serialVersionUID = 1L;

	@Override
	public void interpreter() {
		myPrint(Interpreter.programCounter + " Interpret ");
		int label = destination;
		this.prepareLeftOperand();
		myPrintln("BT " + label);
		if (((IntegerVal) this.getLeftOperand()).getIntVal() == 0)
			Interpreter.programCounter = label;
		else
			Interpreter.programCounter++;
	}

	@Override
	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret BT " + destination);
		Interpreter.programCounter++;
	}

}
