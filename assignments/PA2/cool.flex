/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%option noyywrap
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int string_buf_len;
bool str_contain_null_char;
bool str_too_long;
bool str_contain_escaped_null_char;
int comment_level;

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
CLASS           (?i:class)
ELSE            (?i:else)
FI              (?i:fi)
IF              (?i:if)
IN              (?i:in)
INHERITS        (?i:inherits)
LET             (?i:let)
LOOP            (?i:loop)
POOL            (?i:pool)
THEN            (?i:then)
WHILE           (?i:while)
CASE            (?i:case)
ESAC            (?i:esac)
OF              (?i:of)
NEW             (?i:new)
ISVOID          (?i:isvoid)
ASSIGN          <-
NOT             (?i:not)
LE              <=

ONELINECOMMENT  --.*  
COEND           "*)"
COBEGIN         "(*"
STRBEGIN        \"
STREND          [^\"\\]*\"
TRUE            t[Rr][Uu][Ee]
FALSE           f[Aa][Ll][Ss][Ee]  
DIGIT           [0-9]+
TYPEID          [A-Z_][A-Za-z0-9_]*
OBJECTID        [a-z_][A-Za-z0-9_]*
BLANK           [ \t\r\f\b]
LINKBREAK       [\n]        

%x comment
%x string
%%
 /*
  *  Nested comments
  */


 /*
  *  The multiple-character operators.
  */

<INITIAL>{ONELINECOMMENT} {}
<INITIAL>{COBEGIN} { BEGIN(comment); comment_level=1; }
<comment>{COBEGIN} { BEGIN(comment); comment_level++; }
<INITIAL>{COEND} {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}
<comment>{COEND} {
  comment_level--;
  if(comment_level == 0){
    BEGIN(INITIAL); 
  }
    
}
<comment>[^(\*\))] {
  if (yytext[0] == '\n') {
      curr_lineno++;
  }
}


<comment><<EOF>> {
  BEGIN (INITIAL);
  cool_yylval.error_msg = "EOF in comment";
  return (ERROR);
}
<comment>.		{ 
	;
}

\" {
	memset(string_buf, 0, sizeof(string_buf));
	string_buf_len = 0; str_contain_null_char = false;
  str_contain_escaped_null_char = false;
	BEGIN (string);
}

<string><<EOF>>	{
	cool_yylval.error_msg = "EOF in string constant";
	BEGIN (INITIAL);
  return (ERROR);
}



<string>\\(.|\n|\0)		{
  if (string_buf_len >= MAX_STR_CONST  - 1) {
		str_too_long = true;
	}else{
    switch(yytext[1]) {
      case '\"': string_buf[string_buf_len++] = '\"'; break;
      case '\\': string_buf[string_buf_len++] = '\\'; break;
      case 'b' : string_buf[string_buf_len++] = '\b'; break;
      case 'f' : string_buf[string_buf_len++] = '\f'; break;
      case 'n' : string_buf[string_buf_len++] = '\n'; break;
      case 't' : string_buf[string_buf_len++] = '\t'; break;
      case '0' : string_buf[string_buf_len++] = '0';  break;
      case '\0': str_contain_escaped_null_char = true; break;
      case '\n': string_buf[string_buf_len++] = '\n'; curr_lineno++; break;
      default  : string_buf[string_buf_len++] = yytext[1];
    }
  }
	
}

<string>\n		{
  if (str_too_long) {
		cool_yylval.error_msg = "String constant too long";
		BEGIN (INITIAL); 
    return (ERROR);
	} 
  
  if (str_contain_null_char) {
		cool_yylval.error_msg = "String contains null character";
		BEGIN (INITIAL); 
    return (ERROR);
	}

  if (str_contain_escaped_null_char) {
		cool_yylval.error_msg = "String contains escaped null character";
		BEGIN (INITIAL); 
    return (ERROR);
	}
	curr_lineno++;
	cool_yylval.error_msg = "Unterminated string constant";
	BEGIN (INITIAL); 
  return (ERROR);
}

<string>\0 {
  str_contain_null_char = true;
}

<string>\"		{ 
  if (str_too_long) {
		cool_yylval.error_msg = "String constant too long";
		BEGIN (INITIAL); 
    return (ERROR);
	} 
	if (str_contain_null_char) {
		cool_yylval.error_msg = "String contains null character.";
		BEGIN (INITIAL); 
    return (ERROR);
	}
  if (str_contain_escaped_null_char) {
		cool_yylval.error_msg = "String contains escaped null character.";
		BEGIN (INITIAL); 
    return (ERROR);
	}
	cool_yylval.symbol = stringtable.add_string(string_buf);
	BEGIN (INITIAL); 
  return (STR_CONST);
}

<string>.		{ 
	if (string_buf_len >= MAX_STR_CONST - 1) {
		str_too_long = true;
	} 
  else{
    string_buf[string_buf_len++] = yytext[0]; 
  }
	
}

 /*
  * symbol
  */

"{"			{ return '{'; }
"}"			{ return '}'; }
"("			{ return '('; }
")"			{ return ')'; }
"~"			{ return '~'; }
","			{ return ','; }
";"			{ return ';'; }
":"			{ return ':'; }
"+"			{ return '+'; }
"-"			{ return '-'; }
"*"			{ return '*'; }
"/"			{ return '/'; }
"%"			{ return '%'; }
"."			{ return '.'; }
"<"			{ return '<'; }
"="			{ return '='; }
"@"			{ return '@'; }

 /*
  * keywords
  */

{DARROW}		{ return (DARROW); }
{CLASS} { return (CLASS); }
{ELSE} { return (ELSE); }
{FI} { return (FI); }
{IF} { return (IF); }
{IN} { return (IN); }
{INHERITS} { return (INHERITS); }
{LET} { return (LET); }
{LOOP} { return (LOOP); }
{POOL} { return (POOL); }
{THEN} { return (THEN); }
{WHILE} { return (WHILE); }
{CASE} { return (CASE); }
{ESAC} { return (ESAC); }
{OF} { return (OF); }
{NEW} { return (NEW); }
{ISVOID} { return (ISVOID); }
{ASSIGN} { return (ASSIGN); }
{NOT} { return (NOT); }
{LE} { return (LE); }

{TRUE} {
  cool_yylval.boolean = true;
  return (BOOL_CONST);
}

{FALSE} {
  cool_yylval.boolean = false;
  return (BOOL_CONST);
}

{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}
{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}

{DIGIT} {
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST);
}

{BLANK} {}

{LINKBREAK} {curr_lineno++;}

. {
  cool_yylval.error_msg = yytext; 
	return (ERROR); 
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */



 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%
