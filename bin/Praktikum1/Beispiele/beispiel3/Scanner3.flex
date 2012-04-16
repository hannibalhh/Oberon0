package beispiel3;

%%

%{
  public static void main (String args[]) throws java.io.IOException {
	Scanner3 scanner = new Scanner3(System.in);
	System.out.println("Nu tipp mal ein: ");
	while (scanner.yylex());
  }

  protected void integer(String text)
	{ System.out.println("\tinteger: "+text); }
  protected void sign(String ch)
	{ System.out.println("\tsign: "+ch); }
  protected void character(char ch)
	{ System.out.println("\tcharacter: <"+ch+">"); }
  protected void show(String token, String lexem)
	{ System.out.println(token+": "+lexem); }

%}

%public
%class Scanner3
%type boolean
%eofval{
  return false;
%eofval}

digit	= 	[0-9]

%%

{digit}+	{ show("integer", yytext()); return true; }
[+—]?		{ sign(yytext()); return true; }
[^\r\n] 	{ show("\t", yytext().toString()); return true; }
\n 		{ return true; }
\r 		{ return true; }
