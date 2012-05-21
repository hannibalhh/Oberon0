package cip.astnodes;

import java.util.*;

import cip.TreeGenerator;
import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.SimpleTypeDescr;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class ProgramNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	String name;
	ConstantDeclarationNode constdecs;
	TypeDeclarationNode typedecs;
	VariableDeclarationNode vardecs;
	ProcedureDeclarationNode procdecs;
	ListNode body;

	public ProgramNode() {
		name = "";
		constdecs = null;
		typedecs = null;
		vardecs = null;
		procdecs = null;
		body = null;
	};

	public ProgramNode(String fn, ConstantDeclarationNode fc,
			TypeDeclarationNode ft, VariableDeclarationNode fv,
			ProcedureDeclarationNode fp, ListNode fb) {
		name = fn;
		constdecs = fc;
		vardecs = fv;
		typedecs = ft;
		procdecs = fp;
		body = fb;
	};

	public void setName(String fn) {
		name = fn;
	};

	public void setBody(ListNode fb) {
		body = fb;
	};

	public void setConstdecs(ConstantDeclarationNode fc) {
		constdecs = fc;
	};

	public void setVardecs(VariableDeclarationNode fv) {
		vardecs = fv;
	};

	public void setTypedecs(TypeDeclarationNode ft) {
		typedecs = ft;
	};

	public ListNode getBody() {
		return body;
	};

	public String getName() {
		return name;
	};

	public ConstantDeclarationNode getConstdecs() {
		return constdecs;
	};

	public VariableDeclarationNode getVardecs() {
		return vardecs;
	};

	public TypeDeclarationNode getTypedecs() {
		return typedecs;
	};

	/**
	 * 
	 */
	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("ProgNode " + name + " " + TreeGenerator.curraddr);
		CodeGen.outInstr(new StringVal(name));

		int mainProgramStart = CodeGen.newLabel();

		CodeGen.outInstr(new JumpInstruction(mainProgramStart));

		// Typbeschreibungen für Simpletypes per default in Symoltabelle
		symbolTable.put("integer", new SimpleTypeDescr("integer"));
		symbolTable.put("real", new SimpleTypeDescr("real"));
		symbolTable.put("char", new SimpleTypeDescr("char"));
		symbolTable.put("boolean", new SimpleTypeDescr("boolean"));
		symbolTable.put("string", new SimpleTypeDescr("string"));

		// Deklarationen abarbeiten wenn vorhanden.
		if (constdecs != null)
			constdecs.compile(symbolTable);
		if (typedecs != null)
			typedecs.compile(symbolTable);
		if (vardecs != null)
			vardecs.compile(symbolTable);

		TreeGenerator.lengthDataSegmentMainProgram = TreeGenerator.curraddr;

		// Prozedurdeklarationen abarbeiten wenn vorhanden.
		if (procdecs != null)
			procdecs.compile(symbolTable);

		CodeGen.outInstr(new LabelInstruction(mainProgramStart));
		CodeGen.outInstr(new IntegerVal(
				TreeGenerator.lengthDataSegmentMainProgram));
		CodeGen.outInstr(new SetSP());
		if (body != null)
			body.compile(symbolTable);

		CodeGen.outInstr(new StopInstruction());

		Set<String> ks = symbolTable.keySet();
		String s = "";
		Iterator<String> it = ks.iterator();
		while (it.hasNext()) {
			s = it.next();
			trace(s);
			unindent();
			symbolTable.get(s).print();
		}
		unindent();
		return null;
	};

	public void print() {
		trace("ProgNode " + name);
		if (constdecs != null)
			constdecs.print();
		if (typedecs != null)
			typedecs.print();
		if (vardecs != null)
			vardecs.print();
		if (procdecs != null)
			procdecs.print();
		if (body != null)
			body.print();
		unindent();
	};
}
