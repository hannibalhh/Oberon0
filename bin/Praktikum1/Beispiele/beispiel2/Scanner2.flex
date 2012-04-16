package beispiel2;
%%
%public
%class Scanner2
%standalone

%{

	public static void show(String strg) {
		System.out.println(strg);
	}
%}

%line		/* zählt Zeilen in yyline, startet mit 0 */

digit	= 	[0-9]
digits	= 	{digit}+
operator = 	[+*/-]
rowComment = 	"//"[^\r\n]*

%%

{digit}		{ show ("digit: "+yytext()); }
{digits}	{ show ("digits: "+yytext()); }
{operator}	{ show ("operator: "+yytext()); }
^{rowComment}	{ show ("rowComment <"+yytext()+">"); }
. 		{}
\n		{ show ("# "+Integer.toString(yyline+1)); }
