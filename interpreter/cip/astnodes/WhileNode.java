package cip.astnodes;

import java.util.HashMap;

import cip.base.AbstractDescr;
import cip.base.AbstractNode;
import cip.base.CodeGen; //import cip.base.Operator;
import cip.exceptions.CompileException;
import cip.instructions.*;

public class WhileNode extends AbstractNode {

	private static final long serialVersionUID = 1L;

	AbstractNode condition;
	AbstractNode statement;

	public WhileNode() {
		condition = null;
		statement = null;
	};

	public WhileNode(AbstractNode fe, AbstractNode fst) {
		condition = fe;
		statement = fst;
	};

	public void setCondition(AbstractNode fe) {
		condition = fe;
	};

	public void setStatement(AbstractNode fst) {
		statement = fst;
	};

	public AbstractNode getCondition() {
		return condition;
	};

	public AbstractNode getStatement() {
		return statement;
	};

	public AbstractDescr compile(HashMap<String, AbstractDescr> symbolTable) throws CompileException{
		int l1, l2;

		trace("WhileNode");
		l1 = CodeGen.newLabel();
		l2 = CodeGen.newLabel();
		CodeGen.outInstr(new LabelInstruction(l1));
		condition.compile(symbolTable);
		CodeGen.outInstr(new BranchFalseInstruction(l2));
		if (statement != null)
			statement.compile(symbolTable);
		CodeGen.outInstr(new JumpInstruction(l1));
		CodeGen.outInstr(new LabelInstruction(l2));
		unindent();
		return null;
	};

	public void print() {
		trace("WhileNode");
		condition.print();
		if (statement != null)
			statement.print();
		unindent();
	};
}
