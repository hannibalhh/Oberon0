package cip.base;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import cip.debug.*;
//import cip.exceptions.ParsingException;
//import cip.TreeGenerator;
import cip.exceptions.CompileException;
import cip.astnodes.ProgramNode;
//import cip.gen.Oberon0Lexer;
import cip.instructions.*;

/**
 * CodeGen übersetzt einen abstrakten Syntaxbaum in eine Folge von Instruktionen
 * 
 * @author VLL
 * 
 */
public class CodeGen implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private FileInputStream inStream;
	private ObjectInputStream inputNodes;

	static private String codeFilename = "assemblerCorrectCode";
	static private FileWriter fw;
	static BufferedWriter outtxt;

	private static AbstractNode root;
	public static int level = 0;
	private static int startexitcode = 0;
	public static ArrayList<HashMap<String, AbstractDescr>> symbolTables = new ArrayList<HashMap<String, AbstractDescr>>();

	public static ArrayList<Instruction> programmCode = new ArrayList<Instruction>();
	private static int programCounter = 0;
	private static int labelCounter = 0;

	public static final int MAXLABEL = 1000;
	public static int[] labels = new int[MAXLABEL];

	public CodeGen() {
	}
	
	public void start() {
		// Ausgabedatei "codetxt" mit den lesbaren Instruktionen
		try {
			fw = new FileWriter(new File("assemblerCode"));
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
		outtxt = new BufferedWriter(fw);
	}
	
	public void close(){
		try {
			outtxt.close();
		} catch (IOException e) {
			System.err.println(e.getMessage());
		}
	}

	/**
	 * Gibt eine neue Sprungmarke zurŸck.
	 * 
	 * @return The new Label
	 */
	public static int newLabel() {
		return labelCounter++;
	}

	private static void defineLabel(int flab) {
		labels[flab] = programCounter;
	}

	/**
	 * 
	 * define/read start of exitcode needed for return instruction
	 */
	public static void setStartExitCode(int start) {
		startexitcode = start;
	}

	public static int getStartExitCode() {
		return startexitcode;
	}

	/**
	 * 
	 * @param sizeOfStaticProgramDataSegment
	 */

	private static void myPrintln(String s) {
		if (Debug.debug > 0)
			System.out.println(s);
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
							.setDestination(CodeGen.labels[destination]);
					myPrintln("position " + i + " " + destination);
				} else if (instr instanceof BranchTrueInstruction) {
					destination = ((BranchTrueInstruction) instr)
							.getDestination();
					((BranchTrueInstruction) (programmCode.get(i)))
							.setDestination(CodeGen.labels[destination]);
					myPrintln("position " + i + " " + destination);
				} else if (instr instanceof JumpInstruction) {
					destination = ((JumpInstruction) instr).getDestination();
					((JumpInstruction) (programmCode.get(i)))
							.setDestination(CodeGen.labels[destination]);
					myPrintln("position " + i + " " + destination);
				} else if (instr instanceof CallInstruction) {
					destination = ((CallInstruction) instr).getDestination();
					((CallInstruction) (programmCode.get(i)))
							.setDestination(CodeGen.labels[destination]);
					myPrintln("position " + i + " " + destination);
				}
			i++;
		}
		myPrintln("+++ Ende Fixup");
	}

	public static void outText(String txt) throws IOException {
		myPrintln(txt);
		outtxt.write(txt + "\n");
	}

	public static void outTextInstr(Instruction instr) throws IOException {

		if (instr instanceof AdditionInstruction)
			outText("ADD");
		else if (instr instanceof AssignmentInstruction)
			outText("ASSIGN, " + ((AssignmentInstruction) instr).getLength());
		else if (instr instanceof BoolVal)
			outText("PUSHB, " + ((BoolVal) instr).getBoolVal());
		else if (instr instanceof BranchFalseInstruction)
			outText("BF, " + ((BranchFalseInstruction) instr).getDestination());
		else if (instr instanceof BranchTrueInstruction)
			outText("BT, " + ((BranchTrueInstruction) instr).getDestination());
		else if (instr instanceof CallInstruction)
			outText("CALL, " + ((CallInstruction) instr).getDestination());
		else if (instr instanceof CharVal)
			outText("PUSHC, " + ((CharVal) instr).getcVal());
		else if (instr instanceof ContInstruction)
			outText("CONT, " + ((ContInstruction) instr).getLength());
		else if (instr instanceof DivisionInstruction)
			outText("DIV");
		else if (instr instanceof EqualsInstruction)
			outText("EQ");
		else if (instr instanceof GetFP)
			outText("GETFP");
		else if (instr instanceof GetRK)
			outText("GETRK");
		else if (instr instanceof GetSL)
			outText("GETSL");
		else if (instr instanceof GetSP)
			outText("GETSP");
		else if (instr instanceof GreaterEqualThanInstruction)
			outText("GE");
		else if (instr instanceof GreaterThanInstruction)
			outText("GT");
		else if (instr instanceof InitStack)
			outText("INIT, " + ((InitStack) instr).getLength());
		else if (instr instanceof IntegerVal)
			outText("PUSHI, " + ((IntegerVal) instr).getIntVal());
		else if (instr instanceof JumpInstruction)
			outText("JMP, " + ((JumpInstruction) instr).getDestination());
		else if (instr instanceof LessEqualThanInstruction)
			outText("LE");
		else if (instr instanceof LabelInstruction)
			outText("LABEL, " + ((LabelInstruction) instr).getLabelVal());
		else if (instr instanceof LessThanInstruction)
			outText("LT");
		else if (instr instanceof MultiplicationInstruction)
			outText("MUL");
		else if (instr instanceof NotEqualInstruction)
			outText("NEQ");
		else if (instr instanceof PrintInstruction)
			outText("PRINT");
		else if (instr instanceof RealVal)
			outText("PUSHF, " + ((RealVal) instr).getRVal());
		else if (instr instanceof ReduceStack)
			outText("REDUCE, " + ((ReduceStack) instr).getLength());
		else if (instr instanceof ReturnInstruction)
			outText("RET");
		else if (instr instanceof SetFP)
			outText("SETFP");
		else if (instr instanceof SetRK)
			outText("SETRK");
		else if (instr instanceof SetSL)
			outText("SETSL");
		else if (instr instanceof SetSP)
			outText("SETSP");
		else if (instr instanceof StringVal)
			outText("PUSHS, " + ((StringVal) instr).getSVal());
		else if (instr instanceof SubtractionInstruction)
			outText("SUB");
		else if (instr instanceof ReadInstruction) {
			String prompt = "";
			prompt = ((ReadInstruction) instr).getPrompt();
			if (prompt.length() > 0)
				outText("READ, " + prompt);
			else
				outText("READ");
		} else if (instr instanceof PushRegisterInstruction)
			outText("PUSHREG, "
					+ ((PushRegisterInstruction) instr).getRegister());
		else if (instr instanceof PopRegisterInstruction)
			outText("POPREG, " + ((PopRegisterInstruction) instr).getRegister());
		else if (instr instanceof StopInstruction)
			outText("STOP");
	}

	/**
	 * Legt eine Instruktion auf den Programmspeicher und erhšht den
	 * Programcounter um eins.
	 * 
	 * @param instr
	 */

	public static void outInstr(Instruction instr) {

		if (instr instanceof LabelInstruction)
			defineLabel(((LabelInstruction) instr).getLabelVal());

		programmCode.add(instr);
		programCounter++;

		if (instr instanceof StopInstruction)
			fixUp();
		try {
			outTextInstr(instr);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void start(String instrfile, String filename) throws IOException,
			ClassNotFoundException,CompileException {

		// Abstrakter Baum in Eingabedatei "tmpnodes"
		inStream = new FileInputStream(filename);
		inputNodes = new ObjectInputStream(inStream);

		// Ausgabedatei "tmpinstr" mit den erzeugten Instruktionen
		FileOutputStream ostream = new FileOutputStream(instrfile);
		ObjectOutputStream os = new ObjectOutputStream(ostream);

		// Ausgabedatei "codetxt" mit den lesbaren Instruktionen
		fw = new FileWriter(new File(codeFilename));
		outtxt = new BufferedWriter(fw);

		myPrintln("== CodeGen abstract tree wieder einlesen ==");

		root = (AbstractNode) inputNodes.readObject();

		myPrintln("== CodeGen auf gehts ==");

		int i = 0;
		while (i < 10) {
			CodeGen.symbolTables.add(new HashMap<String, AbstractDescr>());
			i++;
		}
		CodeGen.level = 0;

		((ProgramNode) root).compile(CodeGen.symbolTables.get(CodeGen.level));

		myPrintln("== CodeGen code für Interpretierer rausschreiben ==");
		// "tmpinstr" schließen
		os.writeObject(programmCode);
		os.flush();

		// "outtxt schließen"
		outtxt.close();
	}

	public static AbstractDescr search(String s, int line) throws CompileException {
		int level;
		AbstractDescr e = null;
		HashMap<String, AbstractDescr> lsymbolTable;
		try{
		level = CodeGen.level;
		while ((level >= 0) && (e == null)) {
			lsymbolTable = CodeGen.symbolTables.get(level);
			e = lsymbolTable.get(s);
			if (e == null)
				level--;
		}
		if (e == null) throw new CompileException(s+" in line " + line + " not found!!");
		} catch (CompileException ex) {
			System.out.println("===================> "+ s+" in line " + line + " not found!!");
			System.out.println("Und Tschüss!");
			System.exit(1);}
		return e;
	}

	public static int getProgramCounter() {
		return CodeGen.programCounter;
	}

}
