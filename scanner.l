/* $Id: scanner.ll 44 2008-10-23 09:03:19Z tb $ -*- mode: c++ -*- */
/** \file scanner.ll Define the sqlimp Flex lexical scanner */

%{ /*** C/C++ Declarations ***/

#include <string>

#include "../scanner.h"
#include "colormod.h"
#include "mm_util.h"

/* import the parser's token type into a local typedef */
typedef sqlimp::Parser::token token;
typedef sqlimp::Parser::token_type token_type;

/* By default yylex returns int, we use token_type. Unfortunately yyterminate
 * by default returns 0, which is not of token_type. */
#define yyterminate() return token::END

/* This disables inclusion of unistd.h, which is not available under Visual C++
 * on Win32. The C++ scanner uses STL streams instead. */
#define YY_NO_UNISTD_H

//void yyerror(char *s, ...);

int oldstate;

/* handle locations */
//int yycolumn = 1;


%}

/*** Flex Declarations and Options ***/

/* enable c++ scanner class generation */
%option c++

/* change the name of the scanner class. results in "ExampleFlexLexer" */
%option prefix="Example"

/* the manual says "somewhat more optimized" */
%option batch

/* enable scanner to generate debug output. disable this for release
 * versions. */
%option debug

/* no support for include files is planned */
%option yywrap nounput

/* %option noyywrap nodefault yylineno case-insensitive */
%option case-insensitive

/* enables the use of start condition stacks */
%option stack

/* The following paragraph suffices to track locations accurately. Each time
 * yylex is invoked, the begin position is moved onto the end position. */
%{
#define YY_USER_ACTION          yylloc->columns(yyleng);
#define YYX_DUP_STR2(ofs,siz)   yylval->stringVal = new std::string(yytext+(ofs), yyleng+(siz))
#define YYX_DUP_STR1(ofs)       yylval->stringVal = new std::string(yytext+(ofs), yyleng)
#define YYX_COPY_STR            yylval->stringVal = new std::string(yytext, yyleng)
#define YYX_COPY_INT            yylval->integerVal = atoi(yytext)
#define YYX_COPY_FLT            yylval->doubleVal = atof(yytext)
#define YYX_SET_INT(val)        yylval->integerVal = (val)
#define YYX_STEP                yylloc->step()
#define YYX_SKIP                yylloc->step()
%}

/* Start Condition */
%x COMMENT
/* Start Condition : #include "xxx.sql" 扩展语句 */
%x INCFILE
/* the leading '#' character found */
%x SHARPMODE
/* Start Condition */
%x S_QUOTE
%x D_QUOTE
/* Start Condition */
%s BTWMODE

%% /*** Regular Expressions Part ***/

 /* code to place at the beginning of yylex() */
%{
    // reset location
    yylloc->step();
%}






 /*** BEGIN EXAMPLE - Change the sqlimp lexer rules below ***/
 /*
  [0-9]+ {
	  YYX_COPY_INT;
	  return token::INTEGER;
  }

  [0-9]+"."[0-9]* {
	  YYX_COPY_FLT;
	  return token::DOUBLE;
  }

  [A-Za-z][A-Za-z0-9_,.-]* {
	  YYX_COPY_STR;
	  return token::STRING;
  }
 */
 /* gobble up white-spaces *//*
  [ \t\r]+ {
	  YYX_SKIP;
  }
 */
 /* gobble up end-of-lines *//*
  \n {
	  yylloc->lines(yyleng); yylloc->step();
	  return token::EOL;
  }
  */
 /* pass all other characters up to bison *//*
  . {
	  return static_cast<token_type>(*yytext);
  }
  */
 /*** END EXAMPLE - Change the sqlimp lexer rules above ***/



  /* keywords */

ACTION			{ return token::T_ACTION; }
ADD			{ return token::T_ADD; }
AFTER			{ return token::T_AFTER; }
ALL			{ return token::T_ALL; }
ALTER			{ return token::T_ALTER; }
ANALYZE			{ return token::T_ANALYZE; }

  /* Hack for BETWEEN ... AND ...
   * return special AND token if BETWEEN seen
   */
<BTWMODE>AND		{ BEGIN INITIAL; return token::T_AND; }
ALGORITHM		{ return token::T_ALGORITHM; }
AND			{ return token::T_ANDOP; }
ANY     		{ return token::T_ANY; }
AS			{ return token::T_AS; }
ASC			{ return token::T_ASC; }
AUTO_INCREMENT		{ return token::T_AUTO_INCREMENT; }
BEFORE			{ return token::T_BEFORE; }
BEGIN			{ return token::T_BEGIN; }
BETWEEN			{ BEGIN BTWMODE; return token::T_BETWEEN; }
INT8|BIGINT		{ return token::T_BIGINT; }
BINARY			{ return token::T_BINARY; }
BIT			{ return token::T_BIT; }
BOOL(EAN)?		{ return token::T_BIT; }
BLOB			{ return token::T_BLOB; }
BOTH			{ return token::T_BOTH; }
BTREE			{ return token::T_BTREE; }
BY			{ return token::T_BY; }
CALL			{ return token::T_CALL; }
CASCADE			{ return token::T_CASCADE; }
CASE			{ return token::T_CASE; }
CHANGE			{ return token::T_CHANGE; }
CHAR(ACTER)?		{ return token::T_CHAR; }
CHARSET 		{ return token::T_CHARSET; }
CHECK			{ return token::T_CHECK; }
COLLATE			{ return token::T_COLLATE; }
COLUMN			{ return token::T_COLUMN; }
COLUMNS			{ return token::T_COLUMNS; }
COMMENT			{ return token::T_COMMENT; }
COMMIT  		{ return token::T_COMMIT; }
CONDITION		{ return token::T_CONDITION; }
CONSTRAINT		{ return token::T_CONSTRAINT; }
CONTAINS		{ return token::T_CONTAINS; }
CONTINUE		{ return token::T_CONTINUE; }
CONVERT			{ return token::T_CONVERT; }
COPY			{ return token::T_COPY; }
CREATE			{ return token::T_CREATE; }
CROSS			{ return token::T_CROSS; }
CURRENT_DATE		{ return token::T_CURRENT_DATE; }
CURRENT_TIME		{ return token::T_CURRENT_TIME; }
CURRENT_TIMESTAMP	{ return token::T_CURRENT_TIMESTAMP; }
CURRENT_USER		{ return token::T_CURRENT_USER; }
CURSOR			{ return token::T_CURSOR; }
DATA			{ return token::T_DATA; }
DATABASE		{ return token::T_DATABASE; }
DATABASES		{ return token::T_DATABASES; }
DATE			{ return token::T_DATE; }
DATETIME		{ return token::T_DATETIME; }
DAY_HOUR		{ return token::T_DAY_HOUR; }
DAY_MICROSECOND		{ return token::T_DAY_MICROSECOND; }
DAY_MINUTE		{ return token::T_DAY_MINUTE; }
DAY_SECOND		{ return token::T_DAY_SECOND; }
NUMERIC|DEC|DECIMAL	{ return token::T_DECIMAL; }
DECLARE			{ return token::T_DECLARE; }
DEFAULT			{ return token::T_DEFAULT; }
DEFINE			{ return token::T_DEFINE; }
DEFINER			{ return token::T_DEFINER; }
DELAYED			{ return token::T_DELAYED; }
DELETE			{ return token::T_DELETE; }
DESC			{ return token::T_DESC; }
DESCRIBE		{ return token::T_DESCRIBE; }
DETERMINISTIC		{ return token::T_DETERMINISTIC; }
DIRECTORY		{ return token::T_DIRECTORY; }
DIRECTORY[ \t]+NAME	{ return token::T_DIRECTORY_NAME; }
DISABLE			{ return token::T_DISABLE; }
DISCARD			{ return token::T_DISCARD; }
DISK			{ return token::T_DISK; }
DISTINCT		{ return token::T_DISTINCT; }
DISTINCTROW		{ return token::T_DISTINCTROW; }
DIV			{ return token::T_DIV; }
FLOAT8|DOUBLE		{ return token::T_DOUBLE; }
DROP			{ return token::T_DROP; }
DUAL			{ return token::T_DUAL; }
EACH			{ return token::T_EACH; }
ELSE			{ return token::T_ELSE; }
ELSEIF			{ return token::T_ELSEIF; }
ENABLE			{ return token::T_ENABLE; }
END			{ return token::T_END; }
ENGINE  		{ return token::T_ENGINE; }
ENUM 			{ return token::T_ENUM; }
ESCAPED			{ return token::T_ESCAPED; }
EXCLUSIVE		{ return token::T_EXCLUSIVE; }
EXISTS			{ yylval->subtok = 0; return token::T_EXISTS; }
NOT[ \t]+EXISTS		{ yylval->subtok = 1; return token::T_EXISTS; }
EXIT			{ return token::T_EXIT; }
EXPLAIN			{ return token::T_EXPLAIN; }
FETCH			{ return token::T_FETCH; }
FIRST			{ return token::T_FIRST; }
FLOAT4?			{ return token::T_FLOAT; }
FOR			{ return token::T_FOR; }
FORCE			{ return token::T_FORCE; }
FOREIGN			{ return token::T_FOREIGN; }
FROM			{ return token::T_FROM; }
FUNCTION		{ return token::T_FUNCTION; }
FULLTEXT		{ return token::T_FULLTEXT; }
GENERATED[ \t]+ALWAYS	{ return token::T_GENERATED_ALWAYS; }
GEOMETRY		{ return token::T_GEOMETRY; }
GEOMETRYCOLLECTION	{ return token::T_GEOMETRYCOLLECTION; }
GRANT			{ return token::T_GRANT; }
GROUP			{ return token::T_GROUP; }
HASH			{ return token::T_HASH; }
HAVING			{ return token::T_HAVING; }
HIGH_PRIORITY		{ return token::T_HIGH_PRIORITY; }
HOUR_MICROSECOND	{ return token::T_HOUR_MICROSECOND; }
HOUR_MINUTE		{ return token::T_HOUR_MINUTE; }
HOUR_SECOND		{ return token::T_HOUR_SECOND; }
IF			{ return token::T_IF; }
IGNORE			{ return token::T_IGNORE; }
IMPORT			{ return token::T_IMPORT; }
IN			{ return token::T_IN; }
INFILE			{ return token::T_INFILE; }
INNER			{ return token::T_INNER; }
INOUT			{ return token::T_INOUT; }
INPLACE			{ return token::T_INPLACE; }
INSENSITIVE		{ return token::T_INSENSITIVE; }
INSERT			{ return token::T_INSERT; }
INT4?			{ return token::T_INT; }
INTEGER			{ return token::T_INTEGER; }
INTERVAL		{ return token::T_INTERVAL; }
INVOKER			{ return token::T_INVOKER; }
INTO			{ return token::T_INTO; }
IS			{ return token::T_IS; }
ITERATE			{ return token::T_ITERATE; }
JOIN			{ return token::T_JOIN; }
INDEX|KEY		{ return token::T_KEY; }
KEYS			{ return token::T_KEYS; }
KEY_BLOCK_SIZE		{ return token::T_KEY_BLOCK_SIZE; }
KILL			{ return token::T_KILL; }
LANGUAGE		{ return token::T_LANGUAGE; }
LEADING			{ return token::T_LEADING; }
LEAVE			{ return token::T_LEAVE; }
LEFT			{ return token::T_LEFT; }
LESS			{ return token::T_LESS; }
LIKE			{ return token::T_LIKE; }
LIST			{ return token::T_LIST; }
LIMIT			{ return token::T_LIMIT; }
LINEAR			{ return token::T_LINEAR; }
LINES			{ return token::T_LINES; }
LINESTRING		{ return token::T_LINESTRING; }
LOAD			{ return token::T_LOAD; }
LOCALTIME		{ return token::T_LOCALTIME; }
LOCALTIMESTAMP		{ return token::T_LOCALTIMESTAMP; }
LOCK			{ return token::T_LOCK; }
LONG			{ return token::T_LONG; }
LONGBLOB		{ return token::T_LONGBLOB; }
LONGTEXT		{ return token::T_LONGTEXT; }
LOOP			{ return token::T_LOOP; }
LOW_PRIORITY		{ return token::T_LOW_PRIORITY; }
MAX			{ return token::T_MAX; }
MAXVALUE		{ return token::T_MAXVALUE; }
MATCH			{ return token::T_MATCH; }
MEDIUMBLOB		{ return token::T_MEDIUMBLOB; }
MIDDLEINT|MEDIUMINT	{ return token::T_MEDIUMINT; }
MEDIUMTEXT		{ return token::T_MEDIUMTEXT; }
MEMORY			{ return token::T_MEMORY; }
MIN			{ return token::T_MIN; }
MINUTE_MICROSECOND	{ return token::T_MINUTE_MICROSECOND; }
MINUTE_SECOND		{ return token::T_MINUTE_SECOND; }
MOD			{ return token::T_MOD; }
MODIFY			{ return token::T_MODIFY; }
MODIFIES		{ return token::T_MODIFIES; }
MULTIPOINT		{ return token::T_MULTIPOINT; }
MULTILINESTRING		{ return token::T_MULTILINESTRING; }
MULTIPOLYGON		{ return token::T_MULTIPOLYGON; }
NATURAL			{ return token::T_NATURAL; }

NO			{ return token::T_NO; }
NONE			{ return token::T_NONE; }
NOT			{ return token::T_NOT; }
NO_WRITE_TO_BINLOG	{ return token::T_NO_WRITE_TO_BINLOG; }
NODEGROUP		{ return token::T_NODEGROUP; }
NULL			{ return token::T_NULL; }
NUMBER			{ return token::T_NUMBER; }
ON			{ return token::T_ON; }
ON[ \t]+DUPLICATE	{ return token::T_ONDUPLICATE; } /* hack due to limited lookahead */
OPTIMIZE		{ return token::T_OPTIMIZE; }
OPTION			{ return token::T_OPTION; }
OPTIONALLY		{ return token::T_OPTIONALLY; }
OR			{ return token::T_OR; }
ORDER			{ return token::T_ORDER; }
OUT			{ return token::T_OUT; }
OUTER			{ return token::T_OUTER; }
OUTFILE			{ return token::T_OUTFILE; }
PARSER			{ return token::T_PARSER; }
PARTITION		{ return token::T_PARTITION; }
PARTITIONS		{ return token::T_PARTITIONS; }
POINT			{ return token::T_POINT; }
POLYGON			{ return token::T_POLYGON; }
PRECISION		{ return token::T_PRECISION; }
PRIMARY			{ return token::T_PRIMARY; }
PROCEDURE		{ return token::T_PROCEDURE; }
PURGE			{ return token::T_PURGE; }
QUICK			{ return token::T_QUICK; }
RANGE			{ return token::T_RANGE; }
READ			{ return token::T_READ; }
READS			{ return token::T_READS; }
REAL			{ return token::T_REAL; }
REORGANIZE		{ return token::T_REORGANIZE; }
REFERENCES		{ return token::T_REFERENCES; }
REGEXP|RLIKE		{ return token::T_REGEXP; }
RELEASE			{ return token::T_RELEASE; }
RENAME			{ return token::T_RENAME; }
REPEAT			{ return token::T_REPEAT; }
REPLACE			{ return token::T_REPLACE; }
REQUIRE			{ return token::T_REQUIRE; }
RESTRICT		{ return token::T_RESTRICT; }
RETURN			{ return token::T_RETURN; }
RETURNS			{ return token::T_RETURNS; }
REVOKE			{ return token::T_REVOKE; }
RIGHT			{ return token::T_RIGHT; }
ROLLUP			{ return token::T_ROLLUP; }
ROLLBACK		{ return token::T_ROLLBACK; }
ROWS			{ return token::T_ROWS; }
SCHEMA			{ return token::T_SCHEMA; }
SCHEMAS			{ return token::T_SCHEMAS; }
SECOND_MICROSECOND	{ return token::T_SECOND_MICROSECOND; }
SECURITY		{ return token::T_SECURITY; }
SELECT			{ return token::T_SELECT; }
SENSITIVE		{ return token::T_SENSITIVE; }
SEPARATOR		{ return token::T_SEPARATOR; }
SET			{ return token::T_SET; }
SHARED			{ return token::T_SHARED; }
SHOW			{ return token::T_SHOW; }
INT2|SMALLINT		{ return token::T_SMALLINT; }
SOME			{ return token::T_SOME; }
SONAME			{ return token::T_SONAME; }
SPATIAL			{ return token::T_SPATIAL; }
SPECIFIC		{ return token::T_SPECIFIC; }
SQL			{ return token::T_SQL; }
SQLEXCEPTION		{ return token::T_SQLEXCEPTION; }
SQLSTATE		{ return token::T_SQLSTATE; }
SQLWARNING		{ return token::T_SQLWARNING; }
SQL_BIG_RESULT		{ return token::T_SQL_BIG_RESULT; }
SQL_CALC_FOUND_ROWS	{ return token::T_SQL_CALC_FOUND_ROWS; }
SQL_SMALL_RESULT	{ return token::T_SQL_SMALL_RESULT; }
SSL			{ return token::T_SSL; }
START			{ return token::T_START; }
STARTING		{ return token::T_STARTING; }
STORAGE			{ return token::T_STORAGE; }
STORED			{ return token::T_STORED; }
STRAIGHT_JOIN		{ return token::T_STRAIGHT_JOIN; }
SUBPARTITION		{ return token::T_SUBPARTITION; }
SUBPARTITIONS		{ return token::T_SUBPARTITIONS; }
TABLE			{ return token::T_TABLE; }
TABLES			{ return token::T_TABLES; }
TABLESPACE		{ return token::T_TABLESPACE; }
TEMPORARY		{ return token::T_TEMPORARY; }
TERMINATED		{ return token::T_TERMINATED; }
TEXT			{ return token::T_TEXT; }
THAN			{ return token::T_THAN; }
THEN			{ return token::T_THEN; }
TIME			{ return token::T_TIME; }
TIMESTAMP		{ return token::T_TIMESTAMP; }
INT1|TINYINT		{ return token::T_TINYINT; }
TINYTEXT		{ return token::T_TINYTEXT; }
TINYBLOB		{ return token::T_TINYBLOB; }
TO			{ return token::T_TO; }
TRAILING		{ return token::T_TRAILING; }
TRANSACTION		{ return token::T_TRANSACTION; }
TRIGGER			{ return token::T_TRIGGER; }
UNDO			{ return token::T_UNDO; }
UNION			{ return token::T_UNION; }
UNIQUE			{ return token::T_UNIQUE; }
UNLOCK			{ return token::T_UNLOCK; }
UNSIGNED		{ return token::T_UNSIGNED; }
UPDATE			{ return token::T_UPDATE; }
UPGRADE			{ return token::T_UPGRADE; }
USAGE			{ return token::T_USAGE; }
USE			{ /*emit(yylloc, "'USE' token"); */return token::T_USE; }
USING			{ return token::T_USING; }
UTC_DATE		{ return token::T_UTC_DATE; }
UTC_TIME		{ return token::T_UTC_TIME; }
UTC_TIMESTAMP		{ return token::T_UTC_TIMESTAMP; }
VALUE			{ return token::T_VALUE; }
VALUES			{ return token::T_VALUES; }
VARBINARY		{ return token::T_VARBINARY; }
VARCHAR(ACTER)?		{ return token::T_VARCHAR; }
VARYING			{ return token::T_VARYING; }
VIRTUAL			{ return token::T_VIRTUAL; }
WHEN			{ return token::T_WHEN; }
WHERE			{ return token::T_WHERE; }
WHILE			{ return token::T_WHILE; }
WITH			{ return token::T_WITH; }
WRITE			{ return token::T_WRITE; }
XOR			{ return token::T_XOR; }
YEAR			{ return token::T_YEAR; }
YEAR_MONTH		{ return token::T_YEAR_MONTH; }
ZEROFILL		{ return token::T_ZEROFILL; }

   /* numbers */

[0-9]+				{ YYX_COPY_INT; return token::INTNUM; }

[0-9]+"."[0-9]* 		|
"."[0-9]+			|
[0-9]+E[-+]?[0-9]+		|
[0-9]+"."[0-9]*E[-+]?[0-9]+	|
"."[0-9]*E[-+]?[0-9]+		{ YYX_COPY_FLT; return token::APPROXNUM; }

   /* booleans */

TRUE			{ YYX_SET_INT(1); return token::BOOL; }
UNKNOWN			{ YYX_SET_INT(-1); return token::BOOL; }
FALSE			{ YYX_SET_INT(0); return token::BOOL; }

   /* strings */
   /*
'(\\.|''|[^'\n])*'	|
\"(\\.|\"\"|[^"\n])*\"  { yylval.strval = strdup(yytext); return STRING; }

'(\\.|[^'\n])*$	        { yyerror("Unterminated string %s", yytext); }
\"(\\.|[^"\n])*$	{ yyerror("Unterminated string %s", yytext); }
*/

'		{ oldstate = YY_START;
		  int newstate = S_QUOTE; BEGIN newstate;
		  char c; //TODO: 启用c -malloc/free- string 专用 api,buffer[256],
		  char *pbuf=mmstr_assign();
		  //int len=0;
		  /*emit("enter S_QUOTE mode");*/
		  do {//yycolumn++;
		  	c = yyinput(); mmstr_append(&pbuf,c); /*YY_USER_ACTION; emit("c: %c", c);*/
		  	if (c == EOF) { mmstr_unput(&pbuf); break; }
		  	else if (c == '\n') {yylineno++;continue;}
		  	else if (c == '\\') {c = yyinput();mmstr_append(&pbuf,c);}
		  	else if (c == '\'') {
		  		c = yyinput();
		  		if (c!='\''){
		  			unput(c);
		  			newstate = oldstate;
		  			break;
		  		}
		  		mmstr_append(&pbuf,c);
		  	}
		  } while (c != EOF);
		  //YY_USER_ACTION;
		  if (newstate == S_QUOTE){
		  	lerror(yylloc, "Unterminated single string %s", yytext);
		  	mmstr_destroy(&pbuf);
		  	BEGIN oldstate;
		  }else{
		  	/*emit("leave S_QUOTE mode");*/
		  	mmstr_unput(&pbuf);//从字符串结果缓冲区中移除结尾的'符号
		  	yylval->stringVal = new std::string(mmstr_get_cstr(&pbuf)); /*emit("STRING-: '%s'.", txt_ptr);*/
		  	mmstr_destroy(&pbuf);
		  	BEGIN oldstate;
		  	return token::STRING;
		  }
		}

  /* <S_QUOTE>(([\\']')|.)*'  { BEGIN oldstate; yylval.strval = strdup(yytext); return STRING; }
<S_QUOTE>(([\\']')|.)*$  { yyerror("Unterminated single string %s", yytext); BEGIN oldstate; }
*/

  /*\"               { oldstate = YY_START; BEGIN D_QUOTE; }
<D_QUOTE>(\\\"|\"\"|[^'\n])*\"  { BEGIN oldstate; yylval.strval = strdup(yytext); return STRING; }
<D_QUOTE>(\\\"|\"\"|[^'\n])*$  { yyerror("Unterminated double string %s", yytext); BEGIN oldstate; }
*/


\"		{ oldstate = YY_START;
		  int newstate = D_QUOTE; BEGIN newstate;
		  char c; //TODO: 启用c -malloc/free- string 专用 api,buffer[256],
		  char *pbuf=mmstr_assign();
		  //int len=0;
		  /*emit("enter D_QUOTE mode");*/
		  do {//yycolumn++;
		  	c = yyinput(); mmstr_append(&pbuf,c); /*YY_USER_ACTION; emit("c: %c", c);*/
		  	if (c == EOF) { mmstr_unput(&pbuf); break; }
		  	else if (c == '\n') {yylineno++;continue;}
		  	else if (c == '\\') {c = yyinput();mmstr_append(&pbuf,c);}
		  	else if (c == '\"') {
		  		c = yyinput();
		  		if (c!='\"'){
		  			unput(c);
		  			newstate = oldstate;
		  			break;
		  		}
		  		mmstr_append(&pbuf,c);
		  	}
		  } while (c != EOF);
		  //YY_USER_ACTION;
		  if (newstate == D_QUOTE){
		  	lerror(yylloc, "Unterminated double string %s", yytext);
		  	mmstr_destroy(&pbuf);
		  	BEGIN oldstate;
		  }else{
		  	/*emit("leave D_QUOTE mode");*/
		  	mmstr_unput(&pbuf);//从字符串结果缓冲区中移除结尾的'符号
		  	yylval->stringVal = new std::string(mmstr_get_cstr(&pbuf));
		  	//emit("STRING-: '%s'.", yylval->stringVal->c_str());
		  	mmstr_destroy(&pbuf);
		  	BEGIN oldstate;
		  	return token::STRING;
		  }
		}

   /* hex strings */

X'[0-9A-F]+'		|
0X[0-9A-F]+		{ YYX_COPY_STR; return token::STRING; }

   /* bit strings */

0B[01]+			|
B'[01]+'		{ YYX_COPY_STR; return token::STRING; }

   /* double dollar string */

   /* operators */
[-+&~|^/%*(),.;!]	{ return static_cast<token_type>(*yytext);/* (Parser::token_type)yytext[0];*/ }

"&&"			{ return token::T_ANDOP; }
"||"			{ return token::T_OR; }

"="			{ yylval->subtok = COMPARISON_ASSIGN; return token::T_COMPARISON; }
"<=>"			{ yylval->subtok = 12; return token::T_COMPARISON; }
">="			{ yylval->subtok = COMPARISON_GE; return token::T_COMPARISON; }
">"			{ yylval->subtok = COMPARISON_GT; return token::T_COMPARISON; }
"<="			{ yylval->subtok = COMPARISON_LE; return token::T_COMPARISON; }
"<"			{ yylval->subtok = COMPARISON_LT; return token::T_COMPARISON; }
"!="			|
"<>"			{ yylval->subtok = COMPARISON_NE; return token::T_COMPARISON; }

"<<"			{ yylval->subtok = SHIFT_LEFT; return token::T_SHIFT; }
">>"			{ yylval->subtok = SHIFT_RIGHT; return token::T_SHIFT; }

  /* functions */

SUBSTR(ING)?/"("	{ return token::TF_SUBSTRING; }
TRIM/"("		{ return token::TF_TRIM; }
DATE_ADD/"("		{ return token::TF_DATE_ADD; }
DATE_SUB/"("		{ return token::TF_DATE_SUB; }
EXP/"("			{ return token::TF_EXP; }

  /*
   * peek ahead and return function if name(ƒ
   */
COUNT	{ int c = yyinput(); unput(c);
	  if(c == '(') return token::TF_COUNT;
	  YYX_COPY_STR;
	  return token::NAME; }

[A-Za-z][A-Za-z0-9_]*	{ YYX_COPY_STR;
			  //emit(yylloc,"ll:NAME=%s.",yylval->stringVal->c_str());
			  return token::NAME; }
`[^`/\\.\n]+`		{ YYX_DUP_STR2(1, -2); //yylval.strval = strdup(yytext+1); //yylval.strval[yyleng-2] = 0;
			  //emit(yylloc,"ll:NAME=%s.",yylval->stringVal->c_str());
			  return token::NAME; }
`[^`\n]*$		{ lerror(yylloc, "unterminated quoted name %s", yytext); }

	/* user variables */
@[0-9a-z_.$]+ |
@\"[^"\n]+\" |
@`[^`\n]+` |
@'[^'\n]+'		{ YYX_DUP_STR1(1); //yylval.strval = strdup(yytext+1);
			  return token::USERVAR; }

@\"[^"\n]*$		{ lerror(yylloc, "unterminated quoted user variable %s", yytext); }
@`[^`\n]*$		{ lerror(yylloc, "unterminated quoted user variable %s", yytext); }
@'[^'\n]*$		{ lerror(yylloc, "unterminated quoted user variable %s", yytext); }


":="			{ return token::T_ASSIGN; }


	/* comments */
  /*#.*			{ emit("-- '#' comment skipped."); };*/
"--"[ \t].*		;
"--"			;

"/*"			{ oldstate = YY_START; BEGIN COMMENT; }
<COMMENT>"*/"		{ /*emit(yylloc, "-- comments skipped.");*/ BEGIN oldstate; }
<COMMENT>.		;
<COMMENT>\n		{ /*yycolumn = 1;*/ }
<COMMENT><<EOF>>	{ lerror(yylloc, "[LEX ERROR] unclosed comment"); }

  /* include clause

^[ \t]*"#"                               { BEGIN SHARPMODE; }
<SHARPMODE>[ \t]*include[ \t]*[\"<]      { BEGIN INCFILE; }
<SHARPMODE>.    ;
<SHARPMODE>\n                            { BEGIN INITIAL; }
<INCFILE>[^ \t\n\">]+                    {
    {
      int c;
      while((c=input()) && c != '\n')
	;
    }
    yylineno++;
    if(newfile(yytext))
      yyterminate();// no such file
    BEGIN INITIAL;
    }
<INCFILE>.|\n    { yyerror("%4d bad include line", yylineno); yyterminate(); }
*/

	/* everything else */
[ \t]		/* white space */
\r?\n		{ //yylloc->lines(yyleng);
		  yylloc->step(); yylloc->lines();
		  /*yycolumn = 1;*/ }
.		{ lerror(yylloc, "[LEX ERROR] mystery character '%c'", *yytext); }

<<EOF>>		{ emit(yylloc, "-- <<EOF>>"); if(!popfile()) yyterminate(); }



%% /*** Additional Code ***/


namespace sqlimp {

Scanner::Scanner(std::istream* in,
		 std::ostream* out)
    : ExampleFlexLexer(in, out)
{
}

Scanner::~Scanner()
{
}

void Scanner::set_debug(bool b)
{
    yy_flex_debug = b;
}

}

/* This implementation of ExampleFlexLexer::yylex() is required to fill the
 * vtable of the class ExampleFlexLexer. We define the scanner's main yylex
 * function via YY_DECL to reside in the Scanner class instead. */

#ifdef yylex
#undef yylex
#endif

int ExampleFlexLexer::yylex()
{
    std::cerr << "in ExampleFlexLexer::yylex() !" << std::endl;
    return 0;
}

/* When the scanner receives an end-of-file indication from YY_INPUT, it then
 * checks the yywrap() function. If yywrap() returns false (zero), then it is
 * assumed that the function has gone ahead and set up `yyin' to point to
 * another input file, and scanning continues. If it returns true (non-zero),
 * then the scanner terminates, returning 0 to its caller. */

int ExampleFlexLexer::yywrap()
{
    return 1;
}


namespace sqlimp {

  void lerror(Parser::location_type* l, const char* fmt, ...)
  {
	va_list ap;
	va_start(ap, fmt);

	char str[8192];
	vsprintf(str, fmt, ap);

	std::cout << "-- " << Color::Modifier::red() << *l << ": " << str  << Color::Modifier::def() << std::endl;//driver.error(l, str);
  }

  void emit(Parser::location_type* l, const char* fmt, ...)
  {
	va_list ap;
	va_start(ap, fmt);

	char str[8192];
	vsprintf(str, fmt, ap);

	std::cout << "-- "  << Color::Modifier::cyan() << *l << ": " << str  << Color::Modifier::def() << std::endl;//driver.error(l, str);
  }

  void
  emit(const char *s, ...)
  {
	/*extern yylineno;*/

	va_list ap;
	va_start(ap, s);

	//printf("[emit]: %s:%d.%d-%d.%d: ", yylloc.filename, yylloc.first_line, yylloc.first_column,
	//	    yylloc.last_line, yylloc.last_column);
	printf("    \033[36m");
	vfprintf(stdout, s, ap);
	printf("\033[39m\n");
  }

// void emit(char* fmt, ...)
// {
//   va_list ap;
//   va_start(ap, fmt);
//
//   char str[8192];
//   vsprintf(str, fmt, ap);
//
//   std::cout << str << std::endl;//driver.error(l, str);
// }

int ensure_subtok(Parser::location_type* loc, int subtok, int want)
{
  if(subtok != want){
    lerror(loc, "expect subtok %d, but %d found.", want, subtok);
    return 0;//to yyterminate();
  }
  return 1;
}

int popfile(){
  return 0;
}

int newfile(const char* filename){
  return 0;
}

// void
// yyerror(char *s, ...)
// {
//   va_list ap;
//   va_start(ap, s);
//
//   if(yylloc.first_line)
//     fprintf(stderr, "[yyerror] %s:%d.%d-%d.%d: error: ", yylloc.filename, yylloc.first_line, yylloc.first_column,
// 	    yylloc.last_line, yylloc.last_column);
//   vfprintf(stderr, s, ap);
//   fprintf(stderr, "\n");
//
// }
//
// void
// lyyerror(YYLTYPE t, char *s, ...)
// {
//   va_list ap;
//   va_start(ap, s);
//
//   if(t.first_line)
//     fprintf(stderr, "[lyyerror] %s:%d.%d-%d.%d: error: ", t.filename, t.first_line, t.first_column,
// 	    t.last_line, t.last_column);
//   vfprintf(stderr, s, ap);
//   fprintf(stderr, "\n");
// }
//
// int ensure_subtok(YYLTYPE t, int subtok, int want)
// {
//   if(subtok != want){
//     lyyerror(t, "expect subtok %d, but %d found.", want, subtok);
//     return 0;//to yyterminate();
//   }
//   return 1;
// }

}

