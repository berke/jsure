%{
#include "ecmaparser.tab.h"
#include "ast.h"
#include <stdlib.h>
#include <string.h>
  #undef YY_INPUT

#define YY_INPUT(buf, result, max_size) do {                                    \
  if (js_parser_input_buffer_pos < js_parser_input_buffer_size) {               \
    unsigned int size =                                                         \
    (js_parser_input_buffer_pos + (unsigned int) max_size < js_parser_input_buffer_size) ?     \
      (unsigned int) max_size : (unsigned int) (js_parser_input_buffer_size - js_parser_input_buffer_pos);      \
    memcpy(buf, js_parser_input_buffer, size);                               \
    js_parser_input_buffer_pos += size;                                         \
    result = size;                                                                \
  } else {                                                                      \
    buf[0] = EOF; \
    result = 0;                                                                \
  }                                                                             \
} while(0)

%}
%%

"\x09"		{}
"\x0B"		{}
"\x0C"		{}
"\x20"		{}
"\xA0"		{}
"\x0A"		{}
"\x0D"		{}

"<!--" {}

\/\/.* {}

\/\*(.|\\n)*\*\/ {}

(0|([1-9][0-9]*))	{
			yylval.numeric_literal_value.value_type = INTVALUETYPE;
			yylval.numeric_literal_value.int_value = atoi(yytext);
			return NUMERICLITERAL;
			}

(0|([1-9][0-9]*))(e|E)(\+|-)?[0-9]* 	{} 

(0|([1-9][0-9]*))\.[0-9]+ 	{
				yylval.numeric_literal_value.value_type = FLOATVALUETYPE;
				yylval.numeric_literal_value.float_value = atof(yytext);
				return NUMERICLITERAL;
				}

(0|([1-9][0-9]*))(\.[0-9]+)?((e|E)(\+|-)?[0-9]*)? {}

0x[0-9A-F]*		{}

\"([^\"]|\\(\"|b|f|n|r|t|v))*\" 	{
				int size = (int) strlen(yytext);
	                        char * str = malloc(size-1);
	                        strncpy(str,yytext+1,size-2);
				str[size-2]='\0';
				yylval.string_value = str;
				return STRINGLITERAL;
				}

'([^\']|\\(\"|b|f|n|r|t|v))*' 	{
				int size = (int) strlen(yytext);
                                char * str = malloc(size-1);
                                strncpy(str,yytext+1,size-2);
	                   	str[size-2]='\0';
				yylval.string_value = str;
				return STRINGLITERAL;
				}
												
"true"			{
			yylval.literal_value.value_type = BOOLVALUETYPE;
			yylval.literal_value.bool_value = 1;
			return LITERAL;
			}

"false"			{
			yylval.literal_value.value_type = BOOLVALUETYPE;
			yylval.literal_value.bool_value = 0;
			return LITERAL;
			}

"null"			{
			yylval.literal_value.value_type = NULLVALUETYPE;
			yylval.literal_value.null_value = 1;
			return LITERAL;
			}

"break" 	{return BREAK;}
"case"		{return CASE;}
"catch"		{return CATCH;}
"continue"	{return CONTINUE;}
"default"	{return DEFAULT;}
"delete"	{return DELETE;}
"do"		{return DO;}
"else"		{return ELSE;}
"finally"	{return FINALLY;}
"for"		{return FOR;}
"function"	{return FUNCTION;}
"if"		{return IF;}
"in"		{return IN;}
"instanceof"	{return INSTANCEOF;}
"new"		{return NEW;}
"return"	{return RETURN;}
"switch"	{return SWITCH;}
"this"		{return THIS;}
"throw"		{return THROW;}
"try"		{return TRY;}
"typeof"	{return TYPEOF;}
"var"		{return VAR;}
"void"		{return VOID;}
"while"		{return WHILE;}
"with"		{return WITH;}

"abstract"	{}
"boolean"	{}
"byte"		{}
"char"		{}
"class"		{}
"const"		{}
"debugger"	{}
"enum"		{}
"export"	{}
"extends"	{}
"final"		{}
"float"		{}
"goto"		{}
"implements"	{}
"int"		{}
"interface"	{}
"long"		{}
"native"	{}
"package"	{}
"private"	{}
"protected"	{}
"short"		{}
"static"	{}
"super"		{}
"synchronized"	{}
"throws"	{}
"transient"	{}
"volatile"	{}
"double"	{}
"import"	{}
"public"	{}

"{"		{return LBRACK;}
"}"		{return RBRACK;}
"("		{return LPAR;}
")"		{return RPAR;}
"["		{return LCRO;}
"]"		{return RCRO;}
"."		{return DOT;}
";"		{return DOTCOM;}
","		{return COMA;}
"<"		{return L;}
">"		{return R;}
"<="		{return LEQ;}
">="		{return REQ;}
"=="		{return EQEQ;}
"!="		{return NOTEQ;}
"==="		{return EQEQEQ;}
"!=="		{return NOTEQEQ;}
"+"		{return PLUS;}
"-"		{return MINUS;}
"*"		{return TIMES;}
"%"		{return MOD;}
"++"		{return PLUSPLUS;}
"--"		{return MINUSMINUS;}
"<<"		{return LL;}
">>"		{return RR;}
">>>"		{return RRR;}
"&"		{return AND;}
"|"		{return OR;}
"^"		{return HAT;}
"!"		{return EXCL;}
"~"		{return TILDE;}
"&&"		{return ANDAND;}
"||"		{return OROR;}
"?"		{return QUESTION;}
":"		{return DOTDOT;}
"="		{return EQ;}
"+="		{return PLUSEQ;}
"-="		{return MINUSEQ;}
"*="		{return TIMESEQ;}
"%="		{return MODEQ;}
"<<="		{return LLEQ;}
">>="		{return RREQ;}
">>>="		{return RRREQ;}
"&="		{return ANDEQ;}
"|="		{return OREQ;}
"^="		{return HATEQ;}
"/"		{return DIV;}
"/="		{return DIVEQ;}

([A-Z]|[a-z]|$|_)+[0-9]?        {
				int size = (int) strlen(yytext);
				char * str = malloc(size+1);
				strncpy(str,yytext,size);
                                str[size] = '\0';
				yylval.string_value = str;
				return IDENTIFIER;
	                        }
%%
