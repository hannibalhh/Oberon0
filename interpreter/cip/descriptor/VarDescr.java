package cip.descriptor;

import cip.base.AbstractDescr;

public class VarDescr extends AbstractDescr {

	private static final long serialVersionUID = 1L;

	int addr;
	boolean isvarpar;

	AbstractDescr typ;

	public VarDescr() {
		isvarpar = false;
		addr = 0;
		typ = null;
	}

	public VarDescr(int fa, AbstractDescr ftyp) {
		isvarpar = false;
		addr = fa;
		typ = ftyp;
	}

	public int getAddr() {
		return addr;
	}

	public void setAddr(int fa) {
		addr = fa;
	}

	public boolean getIsvarpar() {
		return isvarpar;
	}

	public void setIsVarpar(boolean fv) {
		isvarpar = fv;
	}

	public AbstractDescr getTyp() {
		return typ;
	}
	
	public void print()
	{
	 trace("VarDescr: " + addr + " size: " + size + " level: " + level);	
     typ.print();
     unindent();
	}
}
