package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;

public class ConstantDeclarationNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<ConstNode> constDecls;

	public ConstantDeclarationNode() {
		constDecls = null;
	};

	public ConstantDeclarationNode(ArrayList<ConstNode> fl) {
		constDecls = fl;
	};

	public void setConstDecls(ArrayList<ConstNode> fl) {
		constDecls = fl;
	};

	public ArrayList<ConstNode> getConstDecls() {
		return constDecls;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		trace("ConstDecNode ");
		int i;

		if (constDecls != null)
			for (i = 0; i < constDecls.size(); i++) {
				constDecls.get(i).compile(symbolTable);
			}
		unindent();
		return null;
	};

	public void print() {
		trace("ConstDecNode ");
		int i;

		if (constDecls != null)
			for (i = 0; i < constDecls.size(); i++) {
				constDecls.get(i).print();
			}
		unindent();
	};
}
