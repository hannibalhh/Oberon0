package myIOPackage;

import java.io.BufferedReader;
import java.io.IOException;

/**
 * @author Voeller
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class MyIO {

	/////////////////////////////////////////////////////////////////////
	
	public static int readInt(String prompt, BufferedReader In)
	{
	 Integer i;	
	 int res = 0;
	 String expr;
	 try{
	 	 System.out.print(prompt);
	 	 expr = (In.readLine());
	 	 i = new Integer(expr);
	 	 res = i.intValue();
	 	}
	 	catch (IOException e)
	 	{System.out.println("Problem mit Eingabe");
	 	 System.exit(-1);
	 	};	
	 return res;	
    }

	/////////////////////////////////////////////////////////////////////
	
	public static long readLong(String prompt, BufferedReader In)
	{
	 return readInt(prompt, In);    }

	/////////////////////////////////////////////////////////////////////
	
	public static String readString(String prompt, BufferedReader In)
	{
	 String str = "";
	 try{
	 	 System.out.print(prompt + " = ");
	 	 str = (In.readLine());
	 	}
	 	catch (IOException e)
	 	{System.out.println("Problem mit Eingabe");
	 	 System.exit(-1);
	 	};	
	 return str;	
    }

	/////////////////////////////////////////////////////////////////////
	
	public static float readFloat(String prompt, BufferedReader In)
	{
		 Float i;	
		 float res = 0;
		 String expr;
		 try{
		 	 System.out.print(prompt + " = ");
		 	 expr = (In.readLine());
		 	 i = new Float(expr);
		 	 res = i.floatValue();
		 	}
		 	catch (IOException e)
		 	{System.out.println("Problem mit Eingabe");
		 	 System.exit(-1);
		 	};	
		 return res;	
    }


}
