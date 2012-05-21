package cip.exceptions;

public class CompileException extends Exception {
	private static final long serialVersionUID = 1L;

	private int line;
	private int column;

	public CompileException(String message /*, int line, int column*/) {
		super(message);
		this.line = line + 1;
		this.column = column + 1;
	}

	@Override
	public String getMessage() {
		StringBuffer buffer = new StringBuffer(super.getMessage());
		/**
		buffer.append(" at line : ");		 
		buffer.append(line);
		buffer.append(" column : ");
		buffer.append(column);
		**/
		return buffer.toString();
	}

	@Override
	public void printStackTrace() {
		// TODO Auto-generated method stub
		super.printStackTrace();
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}

}
