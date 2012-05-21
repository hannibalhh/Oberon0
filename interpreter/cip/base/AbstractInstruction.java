package cip.base;

import java.io.Serializable;

import cip.debug.Debug;
import cip.interpreter.Interpreter;

public abstract class AbstractInstruction  
implements Serializable, Instruction {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	Instruction leftOperand;
	Instruction rightOperand;

	
	public Instruction getLeftOperand() {
		return leftOperand;
	}
	public void setLeftOperand(Instruction leftOperand) {
		this.leftOperand = leftOperand;
	}
	public Instruction getRightOperand() {
		return rightOperand;
	}
	public void setRightOperand(Instruction rightOperand) {
		this.rightOperand = rightOperand;
	}
	
	public void prepareForExecution(){
		this.setRightOperand(Interpreter.computationStack.pop());
		this.setLeftOperand(Interpreter.computationStack.pop());
	}

	public void print(){
		// wird nur noch in dumpInstructions verwendet!
		System.out.println(Interpreter.programCounter + " Interpret " + this.getClass().getName());
		Interpreter.programCounter++;
	}

	public void myPrint(String s){
		if (Debug.debug > 0) System.out.print(s);
	}

	public void myPrintln(String s){
		if (Debug.debug > 0) System.out.println(s);
	}

	protected void prepareRightOperand() {
		this.setRightOperand(Interpreter.computationStack.pop());
	}
	protected void prepareLeftOperand() {
		this.setLeftOperand(Interpreter.computationStack.pop());
	}

}
