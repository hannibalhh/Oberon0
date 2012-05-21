package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.exceptions.CompileException;

public class TypeDeclarationNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<TypeNode> typeDeclarations;

	public TypeDeclarationNode() {
		typeDeclarations = null;
	};

	public TypeDeclarationNode(ArrayList<TypeNode> fl) {
		typeDeclarations = fl;
	};

	public void setTypedeclarations(ArrayList<TypeNode> fl) {
		typeDeclarations = fl;
	};

	public ArrayList<TypeNode> getTypeDeclarations() {
		return typeDeclarations;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("TypeDecNode ");
		int i;

		if (typeDeclarations != null)
			for (i = 0; i < typeDeclarations.size(); i++) {
				typeDeclarations.get(i).compile(symbolTable);
			}
		unindent();
		return null;
	};

	public void print() {
		trace("TypeDecNode ");
		int i;

		if (typeDeclarations != null)
			for (i = 0; i < typeDeclarations.size(); i++) {
				typeDeclarations.get(i).print();
			}
		unindent();
	};
}
