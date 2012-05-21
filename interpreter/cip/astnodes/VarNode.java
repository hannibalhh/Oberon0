package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.TreeGenerator;
import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.descriptor.VarDescr;
import cip.exceptions.CompileException;
import cip.interpreter.Interpreter;

public class VarNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<AbstractNode> variables;
	AbstractNode type;

	public VarNode() {
		variables = null;
		type = null;
	};

	public VarNode(ArrayList<AbstractNode> fl, AbstractNode ft) {
		variables = fl;
		type = ft;
	};

	public void setVariables(ArrayList<AbstractNode> fl) {
		variables = fl;
	};

	public void setType(IdfNode ft) {
		type = ft;
	};

	public ArrayList<AbstractNode> getVariables() {
		return variables;
	};

	public AbstractNode getType() {
		return type;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		trace("VarNode ");
		int i;
		int addr = 0;
		VarDescr entry = null;
		AbstractDescr d = null;

		if (variables != null) {
			if (type instanceof ArrayNode) {
				d = ((ArrayNode) type).compile(symbolTable);
			} else if (type instanceof RecordNode) {
				d = ((RecordNode) type).compile(symbolTable);
			} else {
				d = CodeGen.search(((IdfNode) (type)).getIdName(), ((IdfNode) (type)).getLine());
			}
			for (i = 0; i < variables.size(); i++) {
				addr = TreeGenerator.curraddr;
				entry = new VarDescr(addr, d);
				TreeGenerator.curraddr = TreeGenerator.curraddr + d.getSize();
				(CodeGen.symbolTables.get(CodeGen.level)).put(
						((IdfNode) (variables.get(i))).getIdName(), entry);
				if (CodeGen.level == 0)
					Interpreter.l.add(((IdfNode) (variables.get(i)))
							.getIdName());
			}
		}
		unindent();
		return null;
	};

	public void print() {
		trace("VarNode ");
		int i;
		if (variables != null)
			for (i = 0; i < variables.size(); i++) {
				variables.get(i).print();
			}
		type.print();
		unindent();
	};
}
