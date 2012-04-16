package Praktikum1;
import static Praktikum1.OberonSymbols.*;
import java_cup.runtime.Symbol;
import JFlex.sym;
%%
%public
%standalone
%class OberonScanner
%line
%column
%ignorecase
%cup
%{
  public boolean isEOF(){
  	return zzAtEOF;
  }

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
  }
  
  private int line(){
  	return yyline+1;
  }
  
  private int column(){
  	return yycolumn+1;
  }
  
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, line(), column(), value);
  }
%}

char = 			[a-zA-Z]
digit = 		[0-9]
id = 			{char}({char}|{digit})*
string = 		(\")({char} | {digit} | {blank})+(\")
blank=			[ \t\n\r]
rowComment = 	"//"[^\r\n]*
%%
\*       		{return symbol(mul());}
\+       		{return symbol(plus());}
-       		{return symbol(sub());}
\/       		{return symbol(div());}	
:=       		{return symbol(_def());}
=       		{return symbol(equ());}
#       		{return symbol(sharp());}
\<       		{return symbol(smaller());}
\<=       		{return symbol(smallereq());}
\>       		{return symbol(bigger());}
\>=       		{return symbol(biggereq());}
,       		{return symbol(comma());}
;       		{return symbol(semicolon());}
\(       		{return symbol(bracketOn());}
\)       		{return symbol(bracketOff());}
\{      		{return symbol(camberedBracketOn());}
\}      		{return symbol(camberedBracketOff());}
:       		{return symbol(colon());}
OF 				{return symbol(OF());}
THEN       		{return symbol(THEN());}
DO       		{return symbol(DO());}
PRINT       	{return symbol(PRINT());}
READ       		{return symbol(READ());}
END       		{return symbol(END());}
ELSE       		{return symbol(ELSE());}
ESEIF       	{return symbol(ELSEIF());}
IF       		{return symbol(IF());}
WHILE       	{return symbol(WHILE());}
REPEAT       	{return symbol(REPEAT());}
UNTIL       	{return symbol(UNTIL());}
ARRAY       	{return symbol(ARRAY());}
RECORD       	{return symbol(RECORD());}
CONST       	{return symbol(CONST());}
TYPE       		{return symbol(TYPE());}
VAR       		{return symbol(VAR());}
PROCEDURE       {return symbol(PROCEDURE());}
BEGIN       	{return symbol(BEGIN());}
MODULE      	{return symbol(MODULE());}
\.				{return symbol(DOT());}
{blank}*		{}
{id}			{return symbol(id(),yytext());}
{digit}			{return symbol(digit(),yytext());}
{string}		{return symbol(string(),yytext());}
^{rowComment}	{}
\n 				{}
\r				{}
. 				{System.err.println (SymbolUtil.symbolError(line(),column(),yytext()));System.exit(0);}