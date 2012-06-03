package cip.standaloneCodeGen;

import java.io.*;
import java.util.ArrayList;

import cip.base.*;
import cip.debug.Debug;
import cip.instructions.*;
import cip.interpreter.Interpreter;

public class StandaloneCG {

	/**
	 * @param args
	 */

	public static ArrayList<Instruction> programmCode;
	private static int programCounter;
	private static int pc = 0;

	private static boolean fertig = false;

	public static final int MAXLABEL = 1000;
	public static int[] labels = new int[MAXLABEL];

	public StandaloneCG() {
		programmCode = new ArrayList<Instruction>();
		programCounter = 0;
	}

	public static String readString(BufferedReader In) {
		String str = "";
		try {
			str = (In.readLine());
		} catch (IOException e) {
			System.out.println("Problem mit Eingabe");
			System.exit(-1);
		}
		;
		return str;
	}

	void outInstr1(Instruction instr) {

		programmCode.add(instr);
		programCounter++;

		if (instr instanceof StopInstruction)
			fixUp();
	}

	public static void myPrintln(String s) {
		if (Debug.debug > 0)
			System.out.println(s);
	}

	public static void defineLabel(int flab) {
		labels[flab] = programCounter;
		myPrintln("*** LABEL " + flab + " = " + programCounter);
	}

	public static void fixUp() {
		Instruction instr;
		int i = 0;
		int destination;

		myPrintln("+++ FixUp program length = " + programmCode.size());

		while (i < programmCode.size()) {
			instr = programmCode.get(i);
			if (instr != null)
				if (instr instanceof BranchFalseInstruction) {
					destination = ((BranchFalseInstruction) instr)
							.getDestination();
					((BranchFalseInstruction) (programmCode.get(i)))
							.setDestination(labels[destination]);
					myPrintln("position " + i + " " + destination);
				} else if (instr instanceof BranchTrueInstruction) {
					destination = ((BranchTrueInstruction) instr)
							.getDestination();
					((BranchTrueInstruction) (programmCode.get(i)))
							.setDestination(labels[destination]);
					myPrintln("position " + i + " " + destination);
				} else if (instr instanceof JumpInstruction) {
					destination = ((JumpInstruction) instr).getDestination();
					((JumpInstruction) (programmCode.get(i)))
							.setDestination(labels[destination]);
					myPrintln("position " + i + " " + destination);
				} else if (instr instanceof CallInstruction) {
					destination = ((CallInstruction) instr).getDestination();
					((CallInstruction) (programmCode.get(i)))
							.setDestination(labels[destination]);
					myPrintln("position " + i + " " + destination);
				}

			i++;
		}
		myPrintln("+++ Ende Fixup");
	}

	void GenInstr(String s, BufferedWriter lst) throws IOException {
		String[] ops;
		ops = s.split("\\s*,\\s*");
		lst.write(pc + " " + s + "\n"); pc++; 
		
		if (ops[0].equals("ADD"))
			outInstr1(new AdditionInstruction());
		else if (ops[0].equals("ASSIGN"))
			outInstr1(new AssignmentInstruction(Integer.parseInt(ops[1])));
		else if (ops[0].equals("PUSHB"))
			outInstr1(new BoolVal(Boolean.parseBoolean(ops[1])));
		else if (ops[0].equals("BF"))
			outInstr1(new BranchFalseInstruction(Integer.parseInt(ops[1])));
		else if (ops[0].equals("BT"))
			outInstr1(new BranchTrueInstruction(Integer.parseInt(ops[1])));
		else if (ops[0].equals("CALL"))
			outInstr1(new CallInstruction(Integer.parseInt(ops[1])));
		else if (ops[0].equals("PUSHC"))
			outInstr1(new CharVal(ops[1].charAt(0)));
		else if (ops[0].equals("CONT"))
			outInstr1(new ContInstruction(Integer.parseInt(ops[1])));
		else if (ops[0].equals("DIV"))
			outInstr1(new DivisionInstruction());
		else if (ops[0].equals("EQ"))
			outInstr1(new EqualsInstruction());
		else if (ops[0].equals("GETFP"))
			outInstr1(new GetFP());
		else if (ops[0].equals("GETRK"))
			outInstr1(new GetRK());
		else if (ops[0].equals("GETSL"))
			outInstr1(new GetSL());
		else if (ops[0].equals("GETSP"))
			outInstr1(new GetSP());
		else if (ops[0].equals("GE"))
			outInstr1(new GreaterEqualThanInstruction());
		else if (ops[0].equals("GT"))
			outInstr1(new GreaterThanInstruction());
		else if (ops[0].equals("INIT"))
			outInstr1(new InitStack(Integer.parseInt(ops[1])));
		else if (ops[0].equals("PUSHI"))
			outInstr1(new IntegerVal(Integer.parseInt(ops[1])));
		else if (ops[0].equals("JMP"))
			outInstr1(new JumpInstruction(Integer.parseInt(ops[1])));
		else if (ops[0].equals("LABEL")) {
			defineLabel(Integer.parseInt(ops[1]));
			outInstr1(new LabelInstruction(Integer.parseInt(ops[1])));
		} else if (ops[0].equals("LE"))
			outInstr1(new LessEqualThanInstruction());
		else if (ops[0].equals("LT"))
			outInstr1(new LessThanInstruction());
		else if (ops[0].equals("MUL"))
			outInstr1(new MultiplicationInstruction());
		else if (ops[0].equals("NEQ"))
			outInstr1(new NotEqualInstruction());
		else if (ops[0].equals("PRINT"))
			outInstr1(new PrintInstruction());
		else if (ops[0].equals("PUSHF"))
			outInstr1(new RealVal(Float.parseFloat(ops[1])));
		else if (ops[0].equals("REDUCE"))
			outInstr1(new ReduceStack(Integer.parseInt(ops[1])));
		else if (ops[0].equals("RET"))
			outInstr1(new ReturnInstruction());
		else if (ops[0].equals("SETFP"))
			outInstr1(new SetFP());
		else if (ops[0].equals("SETRK"))
			outInstr1(new SetRK());
		else if (ops[0].equals("SETSL"))
			outInstr1(new SetSL());
		else if (ops[0].equals("SETSP"))
			outInstr1(new SetSP());
		else if (ops[0].equals("PUSHS"))
			outInstr1(new StringVal(ops[1]));
		else if (ops[0].equals("SUB"))
			outInstr1(new SubtractionInstruction());
		else if (ops[0].equals("PUSHREG"))
			outInstr1(new PushRegisterInstruction(ops[1]));
		else if (ops[0].equals("POPREG"))
			outInstr1(new PopRegisterInstruction(ops[1]));
		else if (ops[0].equals("READ")) {
			if (ops.length == 2)
				outInstr1(new ReadInstruction(ops[1]));
			else
				outInstr1(new ReadInstruction(""));
		} else if (ops[0].equals("STOP")) {
			outInstr1(new StopInstruction());
			fertig = true;
		}
	}

	void Encode(BufferedReader in, BufferedWriter lst) throws IOException {
		String str;

		while (!fertig) {
			str = readString(in);
			GenInstr(str, lst);
		}
		lst.close();
	}

	public static void main(String[] argv) throws ClassNotFoundException {
		String infile;
		System.out.println("------------------------ run");
		if (argv.length < 2) {
			System.out
					.println("Usage : java StandAloneCG <inputfile> debug=(0|1)");
		} else {
			try {

				infile = argv[0];
				Debug.debug = Integer.parseInt(argv[1]);
				BufferedReader in = new BufferedReader(new FileReader(infile));

				// Ausgabedatei "tmpinstr" mit den "abstrakten"
				// Instruktionen
				String instrfilename = "tmpinstr";
				FileOutputStream f = new FileOutputStream(instrfilename);
				ObjectOutputStream os = new ObjectOutputStream(f);
				String listFilename = "codelst";
				FileWriter flst = new FileWriter(new File(listFilename));
				BufferedWriter outlst = new BufferedWriter(flst);

				StandaloneCG scg = new StandaloneCG();
				scg.Encode(in, outlst);

				os.writeObject(StandaloneCG.programmCode);
				os.flush();

				Interpreter myIntp = new Interpreter(instrfilename);
				myIntp.start();

			} catch (java.io.FileNotFoundException e) {
				System.out.println("File not found : \"" + argv[0] + "\"");
			} catch (IOException e) {
				System.out.println("IO exception:");
				e.printStackTrace();
			}
		}
	}
}
