package Praktikum3;

import java.io.IOException;

import cip.base.CodeGen;
import cip.exceptions.CompileException;

public class VTest2 {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		CodeGen codeGen = new CodeGen();
		try {
			codeGen.start("foo","src/OberonExamples/NT/AdressbuchTest");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (CompileException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
