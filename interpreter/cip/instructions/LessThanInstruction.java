package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class LessThanInstruction extends AbstractInstruction implements
		Instruction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
	public void interpreter() {
		this.prepareForExecution();
		myPrintln(((IntegerVal) this.getLeftOperand()).getIntVal() + " < "
				+ ((IntegerVal) this.getRightOperand()).getIntVal());
		if (((IntegerVal) this.getLeftOperand()).getIntVal() < ((IntegerVal) this
				.getRightOperand()).getIntVal())
			Interpreter.computationStack.push(new IntegerVal(0));
		else
			Interpreter.computationStack.push(new IntegerVal(1));
		Interpreter.programCounter++;
	}

	@Override
	public void print() {
		myPrintln(Interpreter.programCounter + " Interpret " + "LESS_THAN");
		Interpreter.programCounter++;
	}
}
