package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.TreeGenerator;
import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen;

import cip.descriptor.*;
import cip.exceptions.CompileException;

public class ParNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	ArrayList<AbstractNode> parameterNames;
	AbstractNode parameterType;
	boolean isVarParameter;

	public ParNode() {
		parameterNames = null;
		parameterType = null;
		isVarParameter = false;
	}

	public ParNode(boolean fi, ArrayList<AbstractNode> fl, AbstractNode ft) {
		isVarParameter = fi;
		parameterNames = fl;
		parameterType = ft;
	};

	public void setParameterNames(ArrayList<AbstractNode> fl) {
		parameterNames = fl;
	};

	public void setParametertype(IdfNode ft) {
		parameterType = ft;
	};

	public boolean GetIsvarpar() {
		return isVarParameter;
	}

	public void SetIsvarpar(boolean fi) {
		isVarParameter = fi;
	}

	public ArrayList<AbstractNode> getParameterNames() {
		return parameterNames;
	}

	public AbstractNode getParameterType() {
		return parameterType;
	}

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable,
			ArrayList<AbstractDescr> pl) throws CompileException{
		trace("ParNode ");

		int i;
		int addr = 0;
		VarDescr entry = null;
		AbstractDescr d = null;

		if (parameterNames != null)
			for (i = 0; i < parameterNames.size(); i++) {
				if (parameterType instanceof IdfNode) // named type!
				{
					d = CodeGen.search(((IdfNode) parameterType).getIdName(),((IdfNode) parameterType).getLine());
					if (d instanceof ArrayDescr) {
						entry = new VarDescr(addr, d);
					} else if (d instanceof RecordDescr) {
						entry = new VarDescr(addr, d);
					} else // SimpleType
					{
						d = new SimpleTypeDescr(((IdfNode) (parameterNames
								.get(i))).getIdName());
						entry = new VarDescr(addr, d);
					}
				} else if (parameterType instanceof ArrayNode) {
					addr = TreeGenerator.curraddr;
					d = ((ArrayNode) parameterType).compile(symbolTable);
					entry = new VarDescr(addr, d);
					TreeGenerator.curraddr = TreeGenerator.curraddr
							+ d.getSize();
				} else if (parameterType instanceof RecordNode) {
					addr = TreeGenerator.curraddr;
					d = ((RecordNode) parameterType).compile(symbolTable);
					entry = new VarDescr(addr, d);
					TreeGenerator.curraddr = TreeGenerator.curraddr
							+ d.getSize();
				}
				if (isVarParameter)
					entry.setIsVarpar(true);
				(CodeGen.symbolTables.get(CodeGen.level)).put(
						((IdfNode) (parameterNames.get(i))).getIdName(), entry);
				pl.add(entry);
			}
		unindent();
		return null;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) {
		return null;
	}

	public void print() {
		trace("ParNode " + isVarParameter);
		int i;
		if (parameterNames != null)
			for (i = 0; i < parameterNames.size(); i++) {
				parameterNames.get(i).print();
			}
		parameterType.print();
		unindent();
	}

}
