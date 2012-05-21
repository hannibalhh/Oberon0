package cip.instructions;

import cip.interpreter.Interpreter;

public class AdditionInstruction extends cip.base.AbstractInstruction{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@Override
	public void interpreter() {
		this.prepareForExecution();
		myPrintln(((IntegerVal) this.getLeftOperand()).getIntVal() + " + "
				+ ((IntegerVal) this.getRightOperand()).getIntVal());
		Interpreter.computationStack.push(new IntegerVal(((IntegerVal) this.getLeftOperand()).getIntVal()
				+ ((IntegerVal) this.getRightOperand()).getIntVal()));
		Interpreter.programCounter++;
	}
	
	public void print(){
		myPrintln(Interpreter.programCounter + " Interpret " + "ADD");
		Interpreter.programCounter++;
	}

}
