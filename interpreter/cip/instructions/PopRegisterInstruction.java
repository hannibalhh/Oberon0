package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class PopRegisterInstruction extends AbstractInstruction implements
		Instruction {

	private static final long serialVersionUID = 1L;

	String register;

	public PopRegisterInstruction(String fr) {
		register = fr;
	};

	public String getRegister() {
		return register;
	};

	public void setRegister(String fr) {
		register = fr;
	};

	public void interpreter() {
		int wert;

		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret PopRegister " + register);

		// / SP := SP - 1
		Interpreter.SP = Interpreter.SP - 1;

		// / wert = pop
		wert = ((IntegerVal) (Interpreter.runTimeStack.get(Interpreter.SP)))
				.getIntVal();

		// / Schreiben RK/FP/SLi
		if (register.equals("RK"))
			Interpreter.RK = wert;
		else if (register.equals("FP"))
			Interpreter.FP = wert;
		else {
			int level;
			level = ((IntegerVal) Interpreter.computationStack.pop())
					.getIntVal();
			Interpreter.SL[level] = wert;
		}

		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret PopRegister " + register);
		Interpreter.programCounter++;
	}
}
