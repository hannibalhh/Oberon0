
package beispiel5;

%%	JLex Direktiven

%{ /* interner Code für den Scanner */

   static int currVal= 0;
   static char currOp= '+';
   static String op= "+";

   public static void currentValue(String strg){
	System.out.print(currVal + op + strg + "=");
	int i= Integer.parseInt(strg);
	switch(currOp){
	  case '+': currVal= currVal+i; break;
	  case '*': currVal= currVal*i; break;
	  case '/': currVal= currVal/i; break;
	  case '-': currVal= currVal-i; break;
	};
	show(Integer.toString(currVal));
   }

	public static void show(String strg) {
		System.out.println(strg);
	}
%}

%line		/* zählt Zeilen in yyline, startet mit 0 */
%state AnwTeil

digit	= 	[0-9]
integer	= 	{digit}+
operator = 	[+*/-]
rowComment = 	"//"[^\r\n]*
whiteSpace = 	[ \t\b]
BEGIN	=	"BEGIN"{whiteSpace}
END	= 	"END"{whiteSpace}*

%%

^{BEGIN}		{ show ("<<"); yybegin(AnwTeil); }
<AnwTeil>^{END}		{ show(">>"); yybegin(YYINITIAL); }
<AnwTeil>{integer}	{ currentValue (yytext()); }
<AnwTeil>{operator}	{ currOp= yytext().charAt(0); op= yytext(); }
<YYINITIAL>{integer}	{ show ("YYINITIAL: " + yytext()); }
{operator}		{ show (yytext()); }
^{rowComment}		{}
{whiteSpace} 		{}

[^\r\n]			{show("ungueltiges Zeichen '"+yytext()+"'");}
\n			{ show ("# "+Integer.toString(yyline+2)); }
\r 			{ /* überlesen wegen Windows */ }
