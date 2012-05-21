package cip.interpreter;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Stack;

//import cip.TreeGenerator;
import cip.base.*;
import cip.debug.Debug;
import cip.descriptor.ArrayDescr;
import cip.descriptor.SimpleTypeDescr;
import cip.descriptor.VarDescr;
import cip.instructions.*;

public class Interpreter {

	public static int programCounter = 0;

	public static ArrayList<Instruction> instructions;

	public static Stack<Instruction> computationStack;

	public static ArrayList<Instruction> runTimeStack;

	public static BufferedReader In = null;

	public static int RK = 0, // return register
			SP = 0, // stack pointer
			FP = 0; // frame pointer

	static final int MAXLEVEL = 5; // maximum static nesting depth
	public static int[] SL = // static chain
	new int[MAXLEVEL + 1];

	// für Testausgabe der Variablenbelegung des Hauptprogramms!!
	public static ArrayList<String> l = new ArrayList<String>();

	FileInputStream inStream;
	ObjectInputStream instructionStream;

	@SuppressWarnings("unchecked")
	public Interpreter(String filename) throws IOException,
			ClassNotFoundException {
		inStream = new FileInputStream(filename);
		instructionStream = new ObjectInputStream(inStream);

		if (Debug.debug > 0)
			System.out.println("== code für Interpretierer wieder einlesen ==");

		instructions = (ArrayList<Instruction>) instructionStream.readObject();
		programCounter = 0;

		runTimeStack = new ArrayList<Instruction>();
		computationStack = new Stack<Instruction>();
	};

	public void printValues() {
		int i;
		AbstractDescr d = null;
		String name = "";
		VarDescr entry = null;
		int addr = 0;
		int arraysize = 0;

		i = 0;
		while (i < l.size()) {
			name = l.get(i);
			entry = (VarDescr) ((CodeGen.symbolTables.get(CodeGen.level))
					.get(name));
			addr = entry.getAddr();
			d = entry.getTyp();
			if (d instanceof SimpleTypeDescr) {
				name = ((SimpleTypeDescr) d).getName();
				addr = entry.getAddr();
				System.out.println(name + "(" + addr + ") = "
						+ ((IntegerVal) (runTimeStack.get(addr))).getIntVal());
			} else if (d instanceof ArrayDescr) {
				int j = 0;
				arraysize = ((ArrayDescr) d).getSize();
				while (j < arraysize) {
					System.out.println(name
							+ "("
							+ addr
							+ " + "
							+ j
							+ ") = "
							+ ((IntegerVal) (runTimeStack.get(addr + j)))
									.getIntVal());
					j++;
				}
			} else {
				System.out.println("record");
			}
			i++;
		}

	}

	public void start() {
		int i;

		if (Debug.debug > 0)
			dumpInstructions();

		initializeMemory();
		
		// In brauchen wir für Read!!
		In = new BufferedReader(new InputStreamReader(System.in));

		// Instruktionen interpretieren
		i = instructions.size();
		programCounter = 0;
		while (programCounter < i) {
			instructions.get(programCounter).interpreter();
		}

		// am Ende Speicherbelegung für Hauptprogrammvariable
		// ausgeben
		if (Debug.debug > 0) {
			System.out.println("Und das ist die Variablenbelegung:");
			printValues();
		}

	}

	private void initializeMemory() {
		SL[0] = 0;
		SL[1] = 0;
		SL[2] = 0;
		SL[3] = 0;
		SL[4] = 0;
		SL[5] = 0;
	}

	private void dumpInstructions() {
		int i;
		// Instruktionen ausgeben
		System.out.println("*** dump instructions begin");
		i = instructions.size();
		programCounter = 0; // 27. 2. bei 0 anfangen!!
		while (programCounter < i) {
			instructions.get(programCounter).print();
		}
		System.out.println("*** dump instructions end");
	}

	public static void main(String[] argv) {

		Interpreter myIntp = null;
		String instrfile;

		System.out.println("Interpreter Standalone 0.1");

		if (argv.length == 0) {
			instrfile = "tmpinstr";
			Debug.debug = 1;
		} else {
			instrfile = argv[0];
			Debug.debug = Integer.parseInt(argv[1]);
		}
		try {
			myIntp = new Interpreter(instrfile);
			myIntp.start();

		} catch (java.io.FileNotFoundException e) {
			System.out.println("File not found : \"" + instrfile + "\"");
		} catch (Exception e) {
			System.out.println("Unexpected exception:");
			e.printStackTrace();
		}
	}
}
