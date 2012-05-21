package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;

public class JumpInstruction extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;

	private int destination;

	public JumpInstruction(int fd) {
		destination = fd;
	};

	public void setDestination(int fd) {
		destination = fd;
	};

	public int getDestination() {
		return destination;
	}

	@Override
	public void interpreter() {
		myPrint(Interpreter.programCounter + " Interpret ");
		int label = destination;
		myPrintln("JMP " + label);
		Interpreter.programCounter = label;
	}

	@Override
	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret JUMP " + destination);
		Interpreter.programCounter++;
	}

}
