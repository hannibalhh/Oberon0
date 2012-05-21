package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class BranchFalseInstruction extends AbstractInstruction implements
		Instruction {

	private static final long serialVersionUID = 1L;

	private int destination;
	
	public BranchFalseInstruction(int fd)
	{destination = fd;};
	
	public void setDestination(int fd)
	{destination = fd;};

	public int getDestination(){return destination;}
	
	@Override
	public void interpreter() {
		myPrint(Interpreter.programCounter + " Interpret ");
		int label = destination;
		this.prepareLeftOperand();
		myPrintln("BF " + label);
		if (((IntegerVal)this.getLeftOperand()).getIntVal() != 0)
			Interpreter.programCounter = label;
		else
			Interpreter.programCounter++;
	}

	@Override
	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret BF " + destination);
		Interpreter.programCounter++;
	}

}
