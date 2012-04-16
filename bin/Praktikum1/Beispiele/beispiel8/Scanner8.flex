package beispiel8;

import java.util.ArrayList;
import java.util.List;

class Scanner8 {
    public static void main(String argv[]) throws java.io.IOException {
	System.out.println("Scanner8");
	SymTab symTab = new SymTab();
	symTab.printSymbolList() ;
	System.out.println("nu gib ein!");
	Yylex yy = new Yylex(System.in, symTab);
	Yytoken t;
	while ( (t = yy.yylex()) != null ) {
	    System.out.println(t.token+"\t"+t.lexem);
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

%ctorarg SymTab aSymTab

%init{
	symTab = aSymTab;
%init}

%{
  public static final int KOMMA = 1;
  public static final int ID = 20;
  public static final int ICONST = 30;

  SymTab symTab;

%}


digit =	[0-9]
letter=	[a-zA-Z]
id =	{letter}({letter}|{digit}|"_"})*

%% 
","		{ return new Yytoken(KOMMA, yytext()); }

{digit}+ 	{ int symId = symTab.insert(ICONST, yytext());
		  return new Yytoken(ICONST, yytext()); 
		}

{id}       	{ String lexem = yytext();
		  // Schlüsselwort?
		  int token = symTab.getToken(lexem);
	  	  if (token == -1) {
			token=ID;	// identifier
		  	int symId = symTab.insert(token, lexem);
			//symTab.printSymbolList();
			//System.out.println("symId = "+symId);
		  } else System.out.println("schon drin"+token);
	  	  return new Yytoken(token, lexem); 
		}

.	{}
\r	{}
\n	{}


