package cip.descriptor;

import cip.base.AbstractDescr;

public class IntConstDescr extends AbstractDescr {

	private static final long serialVersionUID = 1L;

	int intVal;

	public IntConstDescr() {
		intVal = 0;
	}

	public IntConstDescr(int fi) {
		intVal = fi;
	}

	public void setIntVal(int fi) {
		intVal = fi;
	};

	public int getIntVal() {
		return intVal;
	}
	
	public void print(){
	 trace("IntConstDescr: " + intVal);
	 unindent();
	}
}
