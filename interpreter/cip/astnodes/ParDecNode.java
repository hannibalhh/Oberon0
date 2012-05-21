package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.exceptions.CompileException;

public class ParDecNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<ParNode> parameters;

	public ParDecNode() {
		parameters = null;
	};

	public ParDecNode(ArrayList<ParNode> fl) {
		parameters = fl;
	};

	public void setParameters(ArrayList<ParNode> fl) {
		parameters = fl;
	};

	public ArrayList<ParNode> getParameters() {
		return parameters;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable,
			ArrayList<AbstractDescr> pl) throws CompileException {
		trace("PardecNode ");
		int i;

		if (parameters != null)
			for (i = 0; i < parameters.size(); i++) {
				parameters.get(i).compile(symbolTable, pl);
			}
		unindent();
		return null;
	};

	public void print() {
		trace("PardecNode ");
		int i;

		if (parameters != null)
			for (i = 0; i < parameters.size(); i++) {
				parameters.get(i).print();
			}
		unindent();
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		return null;
	};
}
