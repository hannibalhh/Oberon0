package cip.astnodes;

import java.util.ArrayList;
import java.util.HashMap;

import cip.base.*; //import cip.base.Operator;
import cip.descriptor.ProcedureDescr;
import cip.descriptor.VarDescr;
import cip.exceptions.CompileException;
import cip.instructions.AdditionInstruction;
import cip.instructions.AssignmentInstruction;
import cip.instructions.CallInstruction;
import cip.instructions.GetSP;
import cip.instructions.InitStack;
import cip.instructions.IntegerVal;
import cip.instructions.SetSP;

public class CallNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	IdfNode name;
	ArrayList<AbstractNode> parameterList;

	public CallNode() {
		name = null;
		parameterList = null;
	};

	public CallNode(IdfNode fn, ArrayList<AbstractNode> fl) {
		name = fn;
		parameterList = fl;
	};

	public void setParameterList(ArrayList<AbstractNode> fl) {
		parameterList = fl;
	};

	public void setName(IdfNode fn) {
		name = fn;
	};

	public ArrayList<AbstractNode> getParameterList() {
		return parameterList;
	};

	public IdfNode GetName() {
		return name;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable)
			throws CompileException {
		trace("CallNode ");
		int i, size;
		AbstractDescr r;
		AbstractDescr d;
		ArrayList<AbstractDescr> fparams;
		boolean isvarpar = false;
		VarParNode varpar = null;

		r = CodeGen.search(name.getIdName(), name.getLine());
		if (parameterList == null)
			CodeGen.outInstr(new InitStack(
					((ProcedureDescr) r).getFramesize() + 3)); // revo 21. 11.
		else {
			CodeGen.outInstr(new InitStack(((ProcedureDescr) r)
					.getLengthparblock()
					+ ((ProcedureDescr) r).getFramesize() + 3)); // revo 21. 11.
			fparams = ((ProcedureDescr) r).getParams();
			for (i = 0; i < parameterList.size(); i++) {
				isvarpar = ((VarDescr) fparams.get(i)).getIsvarpar();
				if (isvarpar) {
					varpar = new VarParNode(((ContNode) parameterList.get(i))
							.getAddress());
					d = varpar.compile(symbolTable);

				} else
					d = parameterList.get(i).compile(symbolTable);
				CodeGen.outInstr(new GetSP());
				if (isvarpar)
					size = 1;
				else
					size = d.getSize();
				CodeGen.outInstr(new AssignmentInstruction(size));

				CodeGen.outInstr(new GetSP());
				if (isvarpar)
					CodeGen.outInstr(new IntegerVal(1));
				else
					CodeGen.outInstr(new IntegerVal(d.getSize()));
				CodeGen.outInstr(new AdditionInstruction());
				CodeGen.outInstr(new SetSP());

			}
		}
		CodeGen.outInstr(new CallInstruction(((ProcedureDescr) r).getStart()));
		unindent();
		return null;
	};

	public void print() {
		trace("CallNode ");
		int i;
		if (parameterList != null)
			for (i = 0; i < parameterList.size(); i++) {
				parameterList.get(i).print();
			}
		name.print();
		unindent();
	};
}
