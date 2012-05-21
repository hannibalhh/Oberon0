package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction;
import cip.interpreter.Interpreter;
import myIOPackage.*;

public class ReadInstruction extends AbstractInstruction implements Instruction {

	private static final long serialVersionUID = 1L;
	String prompt;

	public ReadInstruction(String fp) {
		prompt = fp;
	};

	public String getPrompt() {
		return prompt;
	};

	public void interpreter() {
		int i;

		myPrint(Interpreter.programCounter + " ");
		i = MyIO.readInt(prompt + ":", Interpreter.In);
		myPrintln("Interpret ReadInstruction " + i);
		Interpreter.computationStack.push(new IntegerVal(i));
		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret ReadInstruction ");
		Interpreter.programCounter++;
	}
}
