package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.exceptions.CompileException;

public class ProcedureDeclarationNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<ProcedureNode> procedureDeclarations;

	public ProcedureDeclarationNode() {
		procedureDeclarations = null;
	};

	public ProcedureDeclarationNode(ArrayList<ProcedureNode> fl) {
		procedureDeclarations = fl;
	};

	public void setProcedureDeclarations(ArrayList<ProcedureNode> fl) {
		procedureDeclarations = fl;
	};

	public ArrayList<ProcedureNode> getProcedureDeclarations() {
		return procedureDeclarations;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("ProcDecNode ");
		int i;
		if (procedureDeclarations != null)
			for (i = 0; i < procedureDeclarations.size(); i++) {
				procedureDeclarations.get(i).compile(symbolTable);
			}
		unindent();
		return null;
	};

	public void print() {
		trace("ProcDecNode ");
		int i;

		if (procedureDeclarations != null)
			for (i = 0; i < procedureDeclarations.size(); i++) {
				procedureDeclarations.get(i).print();
			}
		unindent();
	};
}
