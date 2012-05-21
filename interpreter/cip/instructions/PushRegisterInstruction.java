package cip.instructions;

import cip.base.AbstractInstruction;
import cip.base.Instruction; //import cip.base.Operator;
import cip.interpreter.Interpreter;

public class PushRegisterInstruction extends AbstractInstruction implements
		Instruction {

	private static final long serialVersionUID = 1L;

	String register;

	public PushRegisterInstruction(String fr) {
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
		myPrintln("Interpret PushRegister " + register);

		// Lesen RK/FP/SLi
		if (register.equals("RK"))
			wert = Interpreter.RK;
		else if (register.equals("FP"))
			wert = Interpreter.FP;
		else {
			int level;
			level = ((IntegerVal) Interpreter.computationStack.pop())
					.getIntVal();
			wert = Interpreter.SL[level];
		}

		// storage[SP] := RK/FP/SLi
		Interpreter.runTimeStack.set(Interpreter.SP, new IntegerVal(wert));

		// SP := SP +1
		Interpreter.SP = Interpreter.SP + 1;

		Interpreter.programCounter++;
	};

	public void print() {
		myPrint(Interpreter.programCounter + " ");
		myPrintln("Interpret PushRegister " + register);
		Interpreter.programCounter++;
	}
}
