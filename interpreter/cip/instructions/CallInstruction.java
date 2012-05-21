package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class CallInstruction extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	private int destination;

	public void setDestination(int fd) {
		destination = fd;
	};

	public int getDestination() {
		return destination;
	}

	public CallInstruction(int fd) {
		destination = fd;
	};

	public void interpreter() {

		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret Call " + destination);
		Interpreter.RK = Interpreter.programCounter + 1;
		Interpreter.programCounter = destination;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret Call " + destination);
		Interpreter.programCounter++;
	}
}
