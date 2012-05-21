package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.TreeGenerator;
import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;
import cip.descriptor.ProcedureDescr;
import cip.descriptor.VarDescr;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class ProcedureNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	String name;
	ParDecNode pardecs;
	IdfNode returntype;
	ConstantDeclarationNode constdecs;
	TypeDeclarationNode typedecs;
	VariableDeclarationNode vardecs;
	ProcedureDeclarationNode procdecs;
	ListNode body;

	public ProcedureNode() {
		name = "";
		pardecs = null;
		returntype = null;
		constdecs = null;
		typedecs = null;
		vardecs = null;
		procdecs = null;
		body = null;
	};

	public ProcedureNode(String fn, ParDecNode fp, IdfNode ftp,
			ConstantDeclarationNode fc, TypeDeclarationNode ft,
			VariableDeclarationNode fv, ProcedureDeclarationNode fpd,
			ListNode fb) {
		name = fn;
		pardecs = fp;
		returntype = ftp;
		constdecs = fc;
		typedecs = ft;
		vardecs = fv;
		procdecs = fpd;
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

	public void setPardecs(ParDecNode fp) {
		pardecs = fp;
	};

	public void setProcDecs(ProcedureDeclarationNode fpd) {
		procdecs = fpd;
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

	public IdfNode getReturnType() {
		return returntype;
	};

	public ConstantDeclarationNode getConstdecs() {
		return constdecs;
	};

	public VariableDeclarationNode getVardecs() {
		return vardecs;
	};

	public ParDecNode getPardecs() {
		return pardecs;
	};

	public ProcedureDeclarationNode getProcDecs() {
		return procdecs;
	};

	public TypeDeclarationNode getTypedecs() {
		return typedecs;
	};

	int AddressParams(ArrayList<AbstractDescr> params) {
		int paddr = -3;
		int i = params.size() - 1;
		int size = 0;
		int lengthparblock = 0;
		VarDescr e;

		while (i >= 0) {
			e = ((VarDescr) params.get(i));
			if (e.getIsvarpar())
				size = 1;
			else
				size = e.getTyp().getSize();
			lengthparblock = lengthparblock + size;
			paddr = paddr - size;
			e.setAddr(paddr);
			i--;
		}
		return lengthparblock;
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		int localFP, startaddress = 0;
		int lengthparblock = 0;
		boolean wasForwardDeclared = false;

		// Anlegen einer neuen Symboltabelle für den Scope in einer PROCEDURE
		HashMap<String, AbstractDescr> lsymbolTable = new HashMap<String, AbstractDescr>();

		ArrayList<AbstractDescr> params = new ArrayList<AbstractDescr>();

		ProcedureDescr pd;

		CodeGen.level++;
		CodeGen.symbolTables.set(CodeGen.level, lsymbolTable);
		pd = (ProcedureDescr) CodeGen.symbolTables.get(CodeGen.level - 1).get(
				name);
		if (pd == null)
			startaddress = CodeGen.newLabel();
		else {
			startaddress = pd.getStart();
			wasForwardDeclared = true;
		}
		trace("ProcNode " + name + " " + TreeGenerator.curraddr + " "
				+ startaddress);
		if (pardecs != null) {
			pardecs.compile(lsymbolTable, params);
			lengthparblock = AddressParams(params);
		}
		if (constdecs != null)
			constdecs.compile(lsymbolTable);
		TreeGenerator.curraddr = 0;
		if (vardecs != null)
			vardecs.compile(lsymbolTable);
		localFP = TreeGenerator.curraddr;
		if (typedecs != null)
			typedecs.compile(lsymbolTable);
		if (wasForwardDeclared) {
			pd.setFrameSize(localFP);
		} else {
			pd = new ProcedureDescr(name, localFP, lengthparblock,
					startaddress, params);
			pd.setLevel(CodeGen.level - 1);
			CodeGen.symbolTables.get(CodeGen.level - 1).put(pd.getName(), pd);
		}
		if (procdecs != null)
			procdecs.compile(lsymbolTable);

		// System.out.println("ProgNode vor body " + name + " " +
		if (body != null) {
			CodeGen.outInstr(new LabelInstruction(startaddress));
			CodeGen.setStartExitCode(CodeGen.newLabel());
			// für return-Statement

			// Prolog:
			//revo 21.11. CodeGen.outInstr(new InitStack(localFP + 3));

			CodeGen.outInstr(new PushRegisterInstruction("RK"));
			CodeGen.outInstr(new PushRegisterInstruction("FP"));
			CodeGen.outInstr(new IntegerVal(CodeGen.level));
			CodeGen.outInstr(new PushRegisterInstruction("SL"));

			// FP := SP;
			CodeGen.outInstr(new GetSP());
			CodeGen.outInstr(new SetFP());

			// SL(level) := FP
			CodeGen.outInstr(new GetFP());
			CodeGen.outInstr(new IntegerVal(CodeGen.level));
			CodeGen.outInstr(new SetSL());

			// SP := SP + framesize;
			CodeGen.outInstr(new GetSP());
			CodeGen.outInstr(new IntegerVal(localFP));
			CodeGen.outInstr(new AdditionInstruction());
			CodeGen.outInstr(new SetSP());

			// Ende Prolog

			body.compile(lsymbolTable);

			// Epilog
			CodeGen.outInstr(new LabelInstruction(CodeGen.getStartExitCode()));
			// SP := FP
			CodeGen.outInstr(new GetFP());
			CodeGen.outInstr(new SetSP());

			CodeGen.outInstr(new IntegerVal(CodeGen.level));
			CodeGen.outInstr(new PopRegisterInstruction("SL"));
			CodeGen.outInstr(new PopRegisterInstruction("FP"));
			CodeGen.outInstr(new PopRegisterInstruction("RK"));

			// SP := SP-lengthparblock;
			CodeGen.outInstr(new GetSP());
			CodeGen.outInstr(new IntegerVal(lengthparblock));
			CodeGen.outInstr(new SubtractionInstruction());
			CodeGen.outInstr(new SetSP());

			// Speicher freigeben
			CodeGen.outInstr(new ReduceStack(localFP + 3 + lengthparblock));

			CodeGen.outInstr(new ReturnInstruction());
		}
		CodeGen.level--;
		unindent();
		return pd;
	};

	public void print() {
		trace("ProcNode " + name);
		if (pardecs != null)
			pardecs.print();
		if (returntype != null)
			returntype.print();
		if (constdecs != null)
			constdecs.print();
		if (vardecs != null)
			vardecs.print();
		if (typedecs != null)
			typedecs.print();
		if (procdecs != null)
			procdecs.print();
		if (body != null)
			body.print();
		unindent();
	};
}
