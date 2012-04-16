package beispiel9;

import java.util.ArrayList;
import java.util.List;

class Scanner9 {
    public static void main(String argv[]) throws java.io.IOException {
    System.out.println("Scanner9");
	SymTab symTab = new SymTab();
	symTab.printSymbolList();
   System.out.println("gib ein Ende ^Z");
	Yylex yy = new Yylex(System.in);
	Yytoken t;
	while ( (t = yy.yylex()) != null ) {
	    int symId = -1;
	    switch (t.token) {
	      case Yylex.ID: {
			int token = symTab.getToken(t.lexem);
			// Schlüsselwort?
			if (token != -1) t.token = token;
			symId = symTab.insert(t.token, t.lexem);
			break;
		}
	      case Yylex.ICONST: {
			symId = symTab.insert(t.token, t.lexem);
			break;
		}
	    }
	    System.out.println(t.token+"\t"+t.lexem);
	    //System.out.println("symId = "+symId);
	}
	symTab.printSymbolList();
    }
}

class Yytoken {
  public int token;
  public String lexem;
  Yytoken (int aToken, String aLexem) {
	token = aToken;
	lexem = new String(aLexem);
  }
}

%%

%{
  public static final int KOMMA = 1;
  public static final int ID = 20;
  public static final int ICONST = 30;
%}


digit =	[0-9]
letter=	[a-zA-Z]
id =	{letter}({letter}|{digit}|"_"})*

%% 
","		{ return new Yytoken(KOMMA, yytext()); }
{digit}+ 	{ return new Yytoken(ICONST, yytext()); }
{id}		{ return new Yytoken(ID, yytext()); }

[^\r\n]	{}
\r	{}
\n	{}


