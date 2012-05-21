package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.exceptions.CompileException;

public class VariableDeclarationNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<VarNode> variableDeclarations;

	public VariableDeclarationNode() {
		variableDeclarations = null;
	};

	public VariableDeclarationNode(ArrayList<VarNode> fl) {
		variableDeclarations = fl;
	};

	public void setVariableDeclarations(ArrayList<VarNode> fl) {
		variableDeclarations = fl;
	};

	public ArrayList<VarNode> getVariableDeclarations() {
		return variableDeclarations;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("VardecNode ");
		int i;

		if (variableDeclarations != null)
			for (i = 0; i < variableDeclarations.size(); i++) {
				variableDeclarations.get(i).compile(symbolTable);
			}
		unindent();
		return null;
	};

	public void print() {
		trace("VardecNode ");
		int i;

		if (variableDeclarations != null)
			for (i = 0; i < variableDeclarations.size(); i++) {
				variableDeclarations.get(i).print();
			}
		unindent();
	};
}
