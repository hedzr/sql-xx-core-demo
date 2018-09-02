/* $Id: parser.yy 48 2009-09-05 08:07:10Z tb $ -*- mode: c++ -*- */
/** \file parser.yy Contains the sqlimp Bison parser source */

%{ /*** C/C++ Declarations ***/

#include <stdio.h>
#include <stdarg.h>
#include <string>
#include <vector>

#include "expression.h"

%}

/*** yacc/bison Declarations ***/

/* Require bison 2.4 or later */
%require "2.4"

/* add debug output code to generated parser. disable this for release
 * versions. */
%debug

/* start symbol is named "start" */
%start stmt_list

/* write out a header file containing the token defines */
%defines

%define api.location.type "sqlimp::location"
%code requires { #include <location.hh> }

/* use newer C++ skeleton file */
%skeleton "lalr1.cc"

/* namespace to enclose parser in */
%name-prefix="sqlimp"

/* set the parser's class identifier */
%define "parser_class_name" "Parser"

/* keep track of the current position within the input */
%locations
%initial-action
{
    /* initialize the initial location object */
    @$.begin.filename = @$.end.filename = &driver.streamname;
};

/* The driver is passed by reference to the parser and to the scanner. This
 * provides a simple but effective pure interface, not relying on global
 * variables. */
%parse-param { class Driver& driver }

/* verbose error messages */
%error-verbose
/*
%code {

  void lerror(const sqlimp::location& l, char* fmt, ...);

  void emit(const sqlimp::location& l, char* fmt, ...);
  void emit(char* fmt, ...);

}
*/

 /*** BEGIN EXAMPLE - Change the sqlimp grammar's tokens below ***/

%union {
	int  							integerVal;
	double 							doubleVal;
	std::string*					stringVal;
	bool							boolVal;
	class CalcNode*					calcnode;
	int								subtok;
	class sqlimp::StmtNode*			stmtnode;
	class StmtExpr*					stmtexpr;
	class StmtRule*					stmtrule;
	class sqlimp::RI_col_att_list*	col_att_list;
	class sqlimp::RI_string_list*	string_list;
	struct sqlimp::RI_opt_csc*		opt_csc_val;
	class sqlimp::RI_col_sort_list*	col_sort_list;
	struct sqlimp::RI_opt_name*		opt_name;
	class sqlimp::RI_opt_name_list*	opt_name_list;
	class sqlimp::RI_part_type*		partition_type;
	class sqlimp::RI_part_num_def*	partition_numb;
	class sqlimp::RI_part_def*		partition_def;
	class sqlimp::RI_part_item*		partition_item;
	class sqlimp::RI_part_items*	partition_items;
	class sqlimp::RI_part_opts*		partition_opts;
}

%token				END	0	"end of file"
%token				EOL		"end of line"
%token <integerVal> INTEGER		"integer"
%token <doubleVal> 	DOUBLE		"double"
%token <stringVal> 	STRING		"string"
  /*
%type <calcnode>	constant variable
%type <calcnode>	atomexpr powexpr unaryexpr mulexpr addexpr expr
*/

%token <stringVal>	NAME
  /*%token <stringVal>	DOUBL_DOLLAR_STRING	"$string$"*/
%token <integerVal>	INTNUM
%token <integerVal>	BOOL
%token <doubleVal>	APPROXNUM

       /* user @abc names */

%token <stringVal>	USERVAR

       /* operators and precedence levels */

%right T_ASSIGN
%left T_OR
%left T_XOR
%left T_ANDOP
%nonassoc T_IN T_IS T_LIKE T_REGEXP
%left T_NOT '!'
%left T_BETWEEN
%left <subtok> T_COMPARISON /* = <> < > <= >= <=> */
%left '|'
%left '&'
%left <subtok> T_SHIFT /* << >> */
%left '+' '-'
%left '*' '/' '%' T_MOD
%left '^'
%nonassoc T_UMINUS

	  /* tokens: the reserved keywords */

%token T_ACTION
%token T_ADD
%token T_AFTER
%token T_ALGORITHM
%token T_ALL
%token T_ALTER
%token T_ANALYZE
%token T_AND
%token T_ANY
%token T_AS
%token T_ASC
%token T_AUTO_INCREMENT
%token T_BEFORE
%token T_BEGIN
%token T_BETWEEN
%token T_BIGINT
%token T_BINARY
%token T_BIT
%token T_BLOB
%token T_BOTH
%token T_BTREE
%token T_BY
%token T_CALL
%token T_CASCADE
%token T_CASE
%token T_CHANGE
%token T_CHAR
%token T_CHARSET
%token T_CHECK
%token T_COLLATE
%token T_COLUMN
%token T_COLUMNS
%token T_COMMENT
%token T_COMMIT
%token T_CONDITION
%token T_CONSTRAINT
%token T_CONTAINS
%token T_CONTINUE
%token T_CONVERT
%token T_COPY
%token T_CREATE
%token T_CROSS
%token T_CURRENT_DATE
%token T_CURRENT_TIME
%token T_CURRENT_TIMESTAMP
%token T_CURRENT_USER
%token T_CURSOR
%token T_DATA
%token T_DATABASE
%token T_DATABASES
%token T_DATE
%token T_DATETIME
%token T_DAY_HOUR
%token T_DAY_MICROSECOND
%token T_DAY_MINUTE
%token T_DAY_SECOND
%token T_DECIMAL
%token T_DECLARE
%token T_DEFAULT
%token T_DEFINE
%token T_DEFINER
%token T_DELAYED
%token T_DELETE
%token T_DESC
%token T_DESCRIBE
%token T_DETERMINISTIC
%token T_DIRECTORY
%token T_DIRECTORY_NAME
%token T_DISABLE
%token T_DISCARD
%token T_DISK
%token T_DISTINCT
%token T_DISTINCTROW
%token T_DIV
%token T_DOUBLE
%token T_DROP
%token T_DUAL
%token T_EACH
%token T_ELSE
%token T_ELSEIF
%token T_ENABLE
%token T_ENCLOSED
%token T_END
%token T_ENGINE
%token T_ENUM
%token T_ESCAPED
%token T_EXCLUSIVE
%token <subtok> T_EXISTS
%token T_EXIT
%token T_EXPLAIN
%token T_FETCH
%token T_FIRST
%token T_FLOAT
%token T_FOR
%token T_FORCE
%token T_FOREIGN
%token T_FROM
%token T_FUNCTION
%token T_FULLTEXT
%token T_GENERATED_ALWAYS
%token T_GEOMETRY
%token T_GEOMETRYCOLLECTION
%token T_GRANT
%token T_GROUP
%token T_HASH
%token T_HAVING
%token T_HIGH_PRIORITY
%token T_HOUR_MICROSECOND
%token T_HOUR_MINUTE
%token T_HOUR_SECOND
%token T_IF
%token T_IGNORE
%token T_IMPORT
%token T_IN
  /*%token T_INDEX */
%token T_INFILE
%token T_INNER
%token T_INOUT
%token T_INPLACE
%token T_INSENSITIVE
%token T_INSERT
%token T_INT
%token T_INTEGER
%token T_INTERVAL
%token T_INTO
%token T_INVOKER
%token T_ITERATE
%token T_JOIN
%token T_KEY
%token T_KEYS
%token T_KEY_BLOCK_SIZE
%token T_KILL
%token T_LANGUAGE
%token T_LEADING
%token T_LEAVE
%token T_LEFT
%token T_LESS
%token T_LIKE
%token T_LIST
%token T_LIMIT
%token T_LINEAR
%token T_LINES
%token T_LINESTRING
%token T_LOAD
%token T_LOCALTIME
%token T_LOCALTIMESTAMP
%token T_LOCK
%token T_LONG
%token T_LONGBLOB
%token T_LONGTEXT
%token T_LOOP
%token T_LOW_PRIORITY
%token T_MATCH
%token T_MAX
%token T_MAXVALUE
%token T_MEDIUMBLOB
%token T_MEDIUMINT
%token T_MEDIUMTEXT
%token T_MEMORY
%token T_MIN
%token T_MINUTE_MICROSECOND
%token T_MINUTE_SECOND
%token T_MOD
%token T_MODIFY
%token T_MODIFIES
%token T_MULTIPOINT
%token T_MULTILINESTRING
%token T_MULTIPOLYGON
%token T_NATURAL
%token T_NO
%token T_NONE
%token T_NOT
%token T_NO_WRITE_TO_BINLOG
%token T_NODEGROUP
%token T_NULL
%token T_NUMBER
%token T_ON
%token T_ONDUPLICATE
%token T_OPTIMIZE
%token T_OPTION
%token T_OPTIONALLY
%token T_OR
%token T_ORDER
%token T_OUT
%token T_OUTER
%token T_OUTFILE
%token T_PARSER
%token T_PARTITION
%token T_PARTITIONS
%token T_POINT
%token T_POLYGON
%token T_PRECISION
%token T_PRIMARY
%token T_PROCEDURE
%token T_PURGE
%token T_QUICK
%token T_RANGE
%token T_READ
%token T_READS
%token T_REAL
%token T_REORGANIZE
%token T_REFERENCES
%token T_REGEXP
%token T_RELEASE
%token T_RENAME
%token T_REPEAT
%token T_REPLACE
%token T_REQUIRE
%token T_RESTRICT
%token T_RETURN
%token T_RETURNS
%token T_REVOKE
%token T_RIGHT
%token T_ROLLUP
%token T_ROLLBACK
%token T_ROWS
%token T_SCHEMA
%token T_SCHEMAS
%token T_SECOND_MICROSECOND
%token T_SECURITY
%token T_SELECT
%token T_SENSITIVE
%token T_SEPARATOR
%token T_SET
%token T_SHARED
%token T_SHOW
%token T_SMALLINT
%token T_SOME
%token T_SONAME
%token T_SPATIAL
%token T_SPECIFIC
%token T_SQL
%token T_SQLEXCEPTION
%token T_SQLSTATE
%token T_SQLWARNING
%token T_SQL_BIG_RESULT
%token T_SQL_CALC_FOUND_ROWS
%token T_SQL_SMALL_RESULT
%token T_SSL
%token T_START
%token T_STARTING
%token T_STORAGE
%token T_STORED
%token T_STRAIGHT_JOIN
%token T_SUBPARTITION
%token T_SUBPARTITIONS
%token T_TABLE
%token T_TABLES
%token T_TABLESPACE
%token T_TEMPORARY
%token T_TEXT
%token T_TERMINATED
%token T_THAN
%token T_THEN
%token T_TIME
%token T_TIMESTAMP
%token T_TINYBLOB
%token T_TINYINT
%token T_TINYTEXT
%token T_TO
%token T_TRAILING
%token T_TRANSACTION
%token T_TRIGGER
%token T_UNDO
%token T_UNION
%token T_UNIQUE
%token T_UNLOCK
%token T_UNSIGNED
%token T_UPDATE
%token T_UPGRADE
%token T_USAGE
%token T_USE
%token T_USING
%token T_UTC_DATE
%token T_UTC_TIME
%token T_UTC_TIMESTAMP
%token T_VALUE
%token T_VALUES
%token T_VARBINARY
%token T_VARCHAR
%token T_VARYING
%token T_VIRTUAL
%token T_WHEN
%token T_WHERE
%token T_WHILE
%token T_WITH
%token T_WRITE
%token T_XOR
%token T_YEAR
%token T_YEAR_MONTH
%token T_ZEROFILL

 /* functions with special syntax */
%token TF_SUBSTRING
%token TF_TRIM
%token TF_DATE_ADD TF_DATE_SUB
%token TF_COUNT
%token TF_EXP

%type <integerVal> select_opts
%type <integerVal> opt_with_rollup opt_asc_desc
%type <integerVal> opt_inner_cross opt_outer
%type <integerVal> left_or_right opt_left_or_right_outer
%type <integerVal> opt_for_join

%type <integerVal> delete_opts opt_dot_star
%type <integerVal> insert_opts
%type <integerVal> opt_if_not_exists update_opts
%type <integerVal> opt_temporary opt_length opt_uz
%type <integerVal> opt_ignore_replace

%type <stringVal>  create_database_name
%type <stringVal>  cdo_create_db_option_value

  /*%type <stringVal>  lock_tables_stmt*/
%type <integerVal> opt_lock_tables

%type <stringVal>  part_item_opt_name
%type <stringVal>  part_item_opt_string
%type <integerVal> part_item_opt_num
%type <stmtexpr>   subpartition_def subpartition_def_item

%type <integerVal>    trim_ltb opt_algor opt_as index_lock_opt_i index_lock_opt index_algor_opt_val_i index_algor_opt_val index_algor_opt opt_create_index opt_index_algor_opt opt_index_lock_opt cto_storage_type_opt cto_storage_type_def

%type <integerVal>    TO_AS FULLTEXT_SPATIAL opt_COLUMN opt_DEFAULT opt_IGNORE opt_constraint_fk_ref opt_constraint_fk_refs opt_ref opt_index_type index_type opt_index_type_algor sort_def database_or_schema opt_generated_always opt_virtual_stored opt_null_def opt_unique_key opt_primary_key opt_key

%type <stringVal>     opt_comment

%type <boolVal>       opt_binary opt_into

%type <stringVal>     cto_create_db_option_value cto_create_db_option_string_value cto_create_db_option_int_value

%type <string_list>   enum_list name_list column_list opt_column_list opt_into_column_list opt_col_names

%type <col_sort_list> index_column_list opt_index_column_list col_name_def groupby_list

%type <stmtnode>      stmt select_stmt delete_stmt insert_stmt update_stmt replace_stmt create_database_stmt create_function_stmt create_procedure_stmt create_table_stmt drop_database_stmt drop_table_stmt alter_table_stmt alter_table_lead alter_database_stmt start_stmt commit_stmt rollback_stmt use_stmt lock_tables_stmt unlock_tables_stmt set_stmt create_function_lead drop_index_stmt create_index_stmt

%type <stmtexpr>      expr set_expr set_list val_list opt_val_list case_list expr_constant expr_variable atomexpr interval_expr collate_expr binary_expr binary_collate_expr unary2expr unaryexpr xxor_expr mulexpr addexpr shift_expr bitand_expr bitxor_expr bitor_expr in_clause_expr regexp_clause_expr like_clause_expr is_clause_expr comparison_expr case_clause_expr between_clause_expr clause_expr notexpr andexpr xorexpr orexpr default_literal default_literal_tm default_def default_clause part_item_values part_item_value_lt part_item_storage part_item_comment part_item_datadir part_item_indexdir part_item_maxrows part_item_minrows part_item_tablespace part_item_nodegroup part_item_subpartition_list subpartition_list part_item_it part_item_expr insert_asgn_list opt_ondupupdate insert_vals insert_vals_list opt_where opt_groupby opt_having opt_orderby opt_limit select_expr_list select_expr table_reference table_references table_refs opt_table_join_refs table_factor join_table table_subquery opt_join_condition join_condition index_hint drop_index_opt drop_index_opts index_opts index_opt update_asgn_list update_asgn_item opt_as_alias opt_as_expr

%type <stringVal>     symbol index_name charset_name collation_name new_table_name table_name new_col_name old_col_name col_name assign_collation_name assign_charset_name opt_index_name CONSTRAINT_symbol func_name db_name tablespace_name

%type <stmtrule>      opt_alter_tbl_spec_list alter_tbl_spec_list alter_tbl_spec col_position data_type spatial_type opt_fk_references_definitions opt_alter_tbl_create_table_opts create_definition create_col_list create_select_statement opt_alter_db_sub_clauses

%type <opt_csc_val>   opt_csc

%type <opt_name>      cto_create_opt cto_engine_opt cto_charset_opt cto_collate_opt cto_comment_opt cto_autoinc_opt cdo_charset_opt cdo_collate_opt cdo_comment_opt cdo_create_opt

%type <opt_name_list> create_table_opts create_database_opts index_list delete_list

%type <col_att_list>  column_atts column_att column_def2

%type <partition_type>  partition_type subpartition_type
%type <partition_numb>  opt_partitions_num_def opt_subpartitions_num_def
%type <partition_def>   opt_subpartitions_opts
	/*%type <partition_item>  part_item*/
%type <partition_items> opt_partition_items partition_items
%type <partition_opts>  partition_opts


  /* %type <string> col_name*/

%destructor { if($$!=NULL) delete $$; } STRING
%destructor { if($$!=NULL) delete $$; } NAME
%destructor { if($$!=NULL) delete $$; } create_database_name
%destructor { if($$!=NULL) delete $$; } cdo_create_db_option_value
%destructor { if($$!=NULL) delete $$; } part_item_opt_name
%destructor { if($$!=NULL) delete $$; } part_item_opt_string
%destructor { if($$!=NULL) delete $$; } subpartition_def subpartition_def_item
%destructor { if($$!=NULL) delete $$; } cto_create_db_option_value cto_create_db_option_string_value cto_create_db_option_int_value
%destructor { if($$!=NULL) delete $$; } enum_list name_list column_list opt_column_list opt_into_column_list opt_col_names
%destructor { if($$!=NULL) delete $$; } index_column_list opt_index_column_list col_name_def groupby_list
%destructor { if($$!=NULL) delete $$; } stmt select_stmt delete_stmt insert_stmt update_stmt replace_stmt create_database_stmt create_function_stmt create_procedure_stmt create_table_stmt drop_database_stmt drop_table_stmt alter_table_stmt alter_table_lead alter_database_stmt start_stmt commit_stmt rollback_stmt use_stmt lock_tables_stmt unlock_tables_stmt set_stmt create_function_lead drop_index_stmt create_index_stmt
%destructor { if($$!=NULL) delete $$; } expr set_expr set_list val_list opt_val_list case_list expr_constant expr_variable atomexpr interval_expr collate_expr binary_expr binary_collate_expr unary2expr unaryexpr xxor_expr mulexpr addexpr shift_expr bitand_expr bitxor_expr bitor_expr in_clause_expr regexp_clause_expr like_clause_expr is_clause_expr comparison_expr case_clause_expr between_clause_expr clause_expr notexpr andexpr xorexpr orexpr default_literal default_literal_tm default_def default_clause part_item_values part_item_value_lt part_item_storage part_item_comment part_item_datadir part_item_indexdir part_item_maxrows part_item_minrows part_item_tablespace part_item_nodegroup part_item_subpartition_list subpartition_list part_item_it part_item_expr insert_asgn_list opt_ondupupdate insert_vals insert_vals_list opt_where opt_groupby opt_having opt_orderby opt_limit select_expr_list select_expr table_reference table_references table_refs opt_table_join_refs table_factor join_table table_subquery opt_join_condition join_condition index_hint drop_index_opt drop_index_opts index_opts index_opt update_asgn_list update_asgn_item
%destructor { if($$!=NULL) delete $$; } symbol index_name charset_name collation_name new_table_name table_name new_col_name old_col_name col_name assign_collation_name assign_charset_name opt_index_name CONSTRAINT_symbol func_name opt_as_alias opt_as_expr db_name tablespace_name opt_comment
%destructor { if($$!=NULL) delete $$; } opt_alter_tbl_spec_list alter_tbl_spec_list alter_tbl_spec col_position data_type spatial_type opt_fk_references_definitions opt_alter_tbl_create_table_opts create_definition create_col_list create_select_statement opt_alter_db_sub_clauses
%destructor { if($$!=NULL) delete $$; } opt_csc
%destructor { if($$!=NULL) delete $$; } cto_create_opt cto_engine_opt cto_charset_opt cto_collate_opt cto_comment_opt cto_autoinc_opt cdo_charset_opt cdo_collate_opt cdo_comment_opt cdo_create_opt
%destructor { if($$!=NULL) delete $$; } create_table_opts create_database_opts index_list delete_list
%destructor { if($$!=NULL) delete $$; } column_atts column_att column_def2
%destructor { if($$!=NULL) delete $$; } partition_type subpartition_type
%destructor { if($$!=NULL) delete $$; } opt_partitions_num_def opt_subpartitions_num_def
%destructor { if($$!=NULL) delete $$; } opt_subpartitions_opts
%destructor { if($$!=NULL) delete $$; } opt_partition_items partition_items
%destructor { if($$!=NULL) delete $$; } partition_opts
  /* %destructor { delete $$; } constant variable */
  /* %destructor { delete $$; } atomexpr powexpr unaryexpr mulexpr addexpr expr */

 /*** END EXAMPLE - Change the sqlimp grammar's tokens above ***/

%{

#include "driver.h"
#include "../scanner.h"

/* this "connects" the bison parser in the driver to the flex scanner class
 * object. it defines the yylex() function call to pull the next token from the
 * current lexer object of the driver context. */
#undef yylex
#define yylex driver.lexer->lex

#define POST_STMT(x,y) (x)->onNodeEnd(&yyloc);(y)=(x)
#define CURR_STMT driver.context.currentStmt()
%}

%% /*** Grammar Rules ***/

 /*** BEGIN EXAMPLE - Change the sqlimp grammar rules below ***/
 /*
 constant : INTEGER
           {
	       $$ = new CNConstant($1);
	   }
         | DOUBLE
           {
	       $$ = new CNConstant($1);
	   }

 variable : STRING
           {
	       if (!driver.calc.existsVariable(*$1)) {
		   error(yyloc, std::string("Unknown variable \"") + *$1 + "\"");
		   delete $1;
		   YYERROR;
	       }
	       else {
		   $$ = new CNConstant( driver.calc.getVariable(*$1) );
		   delete $1;
	       }
	   }

 atomexpr : constant
           {
	       $$ = $1;
	   }
         | variable
           {
	       $$ = $1;
	   }
         | '(' expr ')'
           {
	       $$ = $2;
	   }

 powexpr	: atomexpr
          {
	      $$ = $1;
	  }
        | atomexpr '^' powexpr
          {
	      $$ = new CNPower($1, $3);
	  }

 unaryexpr : powexpr
            {
		$$ = $1;
	    }
          | '+' powexpr
            {
		$$ = $2;
	    }
          | '-' powexpr
            {
		$$ = new CNNegate($2);
	    }

 mulexpr : unaryexpr
          {
	      $$ = $1;
	  }
        | mulexpr '*' unaryexpr
          {
	      $$ = new CNMultiply($1, $3);
	  }
        | mulexpr '/' unaryexpr
          {
	      $$ = new CNDivide($1, $3);
	  }
        | mulexpr '%' unaryexpr
          {
	      $$ = new CNModulo($1, $3);
	  }

 addexpr : mulexpr
          {
	      $$ = $1;
	  }
        | addexpr '+' mulexpr
          {
	      $$ = new CNAdd($1, $3);
	  }
        | addexpr '-' mulexpr
          {
	      $$ = new CNSubtract($1, $3);
	  }

 expr	: addexpr
          {
	      $$ = $1;
	  }

 assignment : STRING '=' expr
             {
		 driver.calc.variables[*$1] = $3->evaluate();
		 std::cout << "Setting variable " << *$1
			   << " = " << driver.calc.variables[*$1] << "\n";
		 delete $1;
		 delete $3;
	     }

 start	: // empty
        | start ';'
        | start EOL
	| start assignment ';'
	| start assignment EOL
	| start assignment END
        | start expr ';'
          {
	      driver.calc.expressions.push_back($2);
	  }
        | start expr EOL
          {
	      driver.calc.expressions.push_back($2);
	  }
        | start expr END
          {
	      driver.calc.expressions.push_back($2);
	  }
 */
 /*** END EXAMPLE - Change the sqlimp grammar rules above ***/




  /* start: stmt_list; */

stmt_list: stmt ';'			{ driver.context.addStatement($1); }
  | stmt_list stmt ';'		{ driver.context.addStatement($2); }
  /*| stmt_list stmt_block	{  } */
  ;

stmt_list: error ';'
  | stmt_list error ';' ;

stmt_block: stmt ';'				{ driver.context.addStatement($1); }
  | T_BEGIN ';' stmt_list T_END ';'	{ }
  ;

stmt: /* nil */   { $$=NULL; emit(&yyloc, "STMT (empty) ----------"); }
  ;

  /* statements: select statement */

stmt: select_stmt { POST_STMT($1,$$);
     //emit(&yyloc, "SELECT STMT -----------------------------------------\n");
     }
   ;

select_stmt: T_SELECT select_opts select_expr_list
	 { $$=new sqlimp::SNSelect($2, $3, NULL,NULL,NULL,NULL, NULL,NULL,NULL);
	   //emit("SELECTNODATA %d %d", $2, $3);
	 }
   | T_SELECT select_opts select_expr_list
     T_FROM table_references
     opt_where opt_groupby opt_having opt_orderby opt_limit
     opt_into_column_list
	 { $$=new sqlimp::SNSelect($2, $3, $5, $6, $7, $8, $9, $10, $11);
	   //emit("SELECT %d %d %d", $2, $3, $5);
	 }
   ;

opt_where: /* nil */	{ $$=NULL; }
   | T_WHERE expr		{ $$=new StmtExpr($2); $$->set_prefix("WHERE"); };

opt_groupby: /* nil */	{ $$=NULL; }
   | T_GROUP T_BY groupby_list opt_with_rollup  {
	 $$=new StmtExpr(sqlimp::EXPR_CLAUSE_GROUPBY, $3, $4);
	 }
   ;

groupby_list: expr opt_asc_desc          { $$=new RI_col_sort_list($1,(sqlimp::SORT_DEF)$2); }
   | groupby_list ',' expr opt_asc_desc  { $$=$1; $$->add($3,(sqlimp::SORT_DEF)$4); }
   ;

opt_asc_desc:   /* nil */			{ $$ = 0; }
   | T_ASC							{ $$ = 1; }
   | T_DESC							{ $$ = 2; }
   ;

opt_with_rollup: /* nil */			{ $$ = 0; }
   | T_WITH T_ROLLUP				{ $$ = 1; }
   ;

opt_having: /* nil */ 				{ $$=NULL; }
  | T_HAVING expr 					{ $$=$2; }
  ;

opt_orderby: /* nil */				{ $$=NULL; }
  | T_ORDER T_BY groupby_list		{ $$=new StmtExpr($3); $$->set_prefix("ORDER BY"); }
  ;

opt_limit: /* nil */				{ $$=NULL; }
  | T_LIMIT expr 					{ $$=new StmtExpr($2); $$->set_prefix("LIMIT"); }
  | T_LIMIT expr ',' expr			{ $$=new StmtExpr($2); $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $4); $$->set_prefix("LIMIT"); }
  ;

opt_into_column_list: /* nil */		{ $$=NULL; }
  | T_INTO column_list              { $$=$2; }
  ;

column_list: col_name				{ $$=new sqlimp::RI_string_list(*$1); delete($1);
							          //emit("COLUMN %s", $1->c_str());
									}
  | column_list ',' col_name		{ //emit("COLUMN %s", $3->c_str());
									  $$=$1; $$->push_back(*$3);
									  delete($3); }
  ;

index_column_list: col_name_def				{ $$=new RI_col_sort_list(); $$->move($1);
	//emit("_+. %s.",$$->toString().c_str());
	}
  | index_column_list ',' col_name_def		{ $$=$1; $$->move($3);
	//emit("++. %s.",$$->toString().c_str());
    }
  ;

col_name_def: col_name sort_def				{ $$=new RI_col_sort_list(); $$->move($1, (SORT_DEF)$2); } ;

sort_def:/* nil */ 							{ $$=sqlimp::SORT_NO_DEF; }
  | T_ASC									{ $$=sqlimp::SORT_ASC; }
  | T_DESC									{ $$=sqlimp::SORT_DESC; }
  ;

select_opts:								{ $$ = 0; }
  | select_opts T_ALL						{ if($$ & 01) lerror(&yyloc,"duplicate ALL option"); $$ = $1 | 01; }
  | select_opts T_DISTINCT					{ if($$ & 02) lerror(&yyloc,"duplicate DISTINCT option"); $$ = $1 | 02; }
  | select_opts T_DISTINCTROW				{ if($$ & 04) lerror(&yyloc,"duplicate DISTINCTROW option"); $$ = $1 | 04; }
  | select_opts T_HIGH_PRIORITY				{ if($$ & 010) lerror(&yyloc,"duplicate HIGH_PRIORITY option"); $$ = $1 | 010; }
  | select_opts T_STRAIGHT_JOIN				{ if($$ & 020) lerror(&yyloc,"duplicate STRAIGHT_JOIN option"); $$ = $1 | 020; }
  | select_opts T_SQL_SMALL_RESULT			{ if($$ & 040) lerror(&yyloc,"duplicate SQL_SMALL_RESULT option"); $$ = $1 | 040; }
  | select_opts T_SQL_BIG_RESULT			{ if($$ & 0100) lerror(&yyloc,"duplicate SQL_BIG_RESULT option"); $$ = $1 | 0100; }
  | select_opts T_SQL_CALC_FOUND_ROWS		{ if($$ & 0200) lerror(&yyloc,"duplicate SQL_CALC_FOUND_ROWS option"); $$ = $1 | 0200; }
  ;

select_expr_list: select_expr				{ $$ = $1; }
  | select_expr_list ',' select_expr		{ $$ = $1; $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $3); }
  | '*'										{ $$ = new StmtExpr(sqlimp::EXPR_TOKEN, "*"); }
  ;

select_expr: expr opt_as_alias				{
    $$=new StmtExpr($1); /*在expr($1)外包裹一层StmtExpr之后再进行as_alias描述，避免处理表达式格式时发生意外：对于tall*2 as double_tall, 应将tall*2部分、即$1部分包围起来。*/
    $$->as_alias_op($2);
  }
  ;

table_references: table_refs opt_table_join_refs
  {
    $$=new StmtExpr($1);
    if($2) $$->list_append($2);
  }
  ;

table_refs:    table_reference		{ $$ = $1;
	//emit("-- 1. %s", $$->toString().c_str());
	}
  | table_refs ',' table_reference	{
    //emit("-- 1> %s", $1->toString().c_str()); debug_point();
    $$ = $1; $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $3);
    //emit("-- *. %s", $$->toString().c_str());
    }
  ;

table_reference:  table_factor				{ $$=$1; }
  ;

table_factor:
    NAME opt_as_alias index_hint			{
    //if(*$1=="eee") debug_point();
    $$=new StmtExpr();
    $$->set(sqlimp::EXPR_NAME,*$1);
    $$->as_alias_op($2);
    $$->list_append($3);
    delete($1); delete($2);
    //emit("____. %s", $$->toString().c_str());
    }
  | NAME '.' NAME opt_as_alias index_hint	{
    $$=new StmtExpr(sqlimp::EXPR_NAME,*$1,*$3);
    $$->as_alias_op($4);
    $$->list_append($5);
    delete($1); delete($3); delete($4);
    }
  | table_subquery opt_as expr				{ $$=$1; $$->as_alias_op($3); delete($3); }
  | '(' table_references ')'				{ $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS,$2); }
  ;

opt_as: T_AS			{ $$=1; }
  | /* nil */			{ $$=0; }
  ;

opt_as_alias: T_AS expr { $$=$2; }
  | expr                { $$=$1; }
  | /* nil */			{ $$=NULL; }
  ;
opt_as_expr: T_AS '(' expr ')'	{ $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS,$3);$$->set_prefix("AS");}
  | /* nil */					{ $$=NULL; }
  ;

opt_table_join_refs:				{ $$=NULL; }
  | join_table						{ $$=$1; }
  | opt_table_join_refs join_table	{ $$=$1; $$->list_append($2); }
  ;

join_table:
    opt_inner_cross T_JOIN table_factor opt_join_condition
                  { int join_type=$1; //0,1,2 xxx JOIN
                    StmtExpr* e;
                    sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list(' ');
                    nl->add(join_type==1?"INNER":join_type==2?"CROSS":"");
                    nl->add("JOIN");
                    e=new StmtExpr(sqlimp::EXPR_CLAUSE_NAME_LIST, nl, join_type);
                    e->list_append($3);
                    e->list_append($4);
                    $$=new StmtExpr(e);
                    //emit("JOIN %d", 0100+$2);
                  }
  | T_STRAIGHT_JOIN table_factor
                  { int join_type=4;
                    sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list(' ');
                    nl->add("STRAIGHT_JOIN");
                    $$=new StmtExpr(sqlimp::EXPR_CLAUSE_NAME_LIST, nl, join_type);
                    $$->list_append($2);
                    //emit("JOIN %d", 0200);
                  }
  | T_STRAIGHT_JOIN table_factor T_ON expr
                  { int join_type=4;
                    sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list(' ');
                    nl->add("STRAIGHT_JOIN");
                    $$=new StmtExpr(sqlimp::EXPR_CLAUSE_NAME_LIST, nl, join_type);
                    $$->list_append($2);
                    $4->set_prefix("ON");
                    $$->list_append($4);
                    //emit("JOIN %d", 0200);
                  }
  | left_or_right opt_outer T_JOIN table_factor join_condition
                  { int join_type=$1|$2; // {16,32} | {8}
                    sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list(' ');
                    if($1==16||$1==32) nl->add($1==16?"LEFT":$1==32?"RIGHT":"");
                    if($2==8) nl->add("OUTER");
                    nl->add("JOIN");
                    $$=new StmtExpr(sqlimp::EXPR_CLAUSE_NAME_LIST, nl, join_type);
                    $$->list_append($4);
                    $$->list_append($5);
                    //emit("JOIN %d", 0300+$2+$3);
                  }
  | T_NATURAL opt_left_or_right_outer T_JOIN table_factor
                  { int join_type=$2+64;  // {16,32} | {8} | {64}
                    sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list(' ');
                    nl->add("NATURAL");
                    int lr=join_type&0x30;
                    if(lr!=0) nl->add(lr==16?"LEFT":lr==32?"RIGHT":"");
                    int o=join_type&8;
                    if(o!=0) nl->add("OUTER");
                    nl->add("JOIN");
                    $$=new StmtExpr(sqlimp::EXPR_CLAUSE_NAME_LIST, nl, join_type);
                    $$->list_append($4);
                    //emit("JOIN %d", 0400+$3);
                  }
  ;

opt_inner_cross: /* nil */ { $$ = 0; }
   | T_INNER { $$ = 1; }
   | T_CROSS  { $$ = 2; }
;

opt_outer: /* nil */  { $$ = 0; }
   | T_OUTER {$$ = 8; }
   ;

left_or_right: T_LEFT { $$ = 16; }
   | T_RIGHT { $$ = 32; }
   ;

opt_left_or_right_outer: T_LEFT opt_outer { $$ = 16 + $2; }
   | T_RIGHT opt_outer  { $$ = 32 + $2; }
   | /* nil */ { $$ = 0; }
   ;

opt_join_condition: join_condition	{ $$=$1; }
   | /* nil */						{ $$=NULL; }
   ;

join_condition: T_ON expr 			{ $$=$2; $$->set_prefix("ON"); }
   | T_USING '(' column_list ')'	{ $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS,new StmtExpr($3)); $$->set_prefix("USING"); }
   ;

index_hint:
     T_USE T_KEY opt_for_join '(' index_list ')'
                  { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INDX_HINT, $5, ($3<<16)|1);
                    //emit("index_hint: %s.", $$->toString().c_str());
                    }
   | T_IGNORE T_KEY opt_for_join '(' index_list ')'
                  { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INDX_HINT, $5, ($3<<16)|2); }
   | T_FORCE T_KEY opt_for_join '(' index_list ')'
                  { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INDX_HINT, $5, ($3<<16)|4); }
   | /* nil */	  { $$=NULL; }
   ;

opt_for_join: T_FOR T_JOIN 			{ $$ = 1; }
   | /* nil */						{ $$ = 0; }
   ;

index_list: NAME					{ $$ = new sqlimp::RI_opt_name_list(','); $$->add($1); }
   | index_list ',' NAME			{ $$ = $1; $$->add($3); }
   ;

table_subquery: '(' select_stmt ')'	{ $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS, new StmtExpr($2)); }
   ;




   /* statements: delete statement */

stmt: delete_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "DELETE STMT -----------------------------------------\n");
	  }
   ;

delete_stmt: T_DELETE delete_opts T_FROM NAME
    opt_where opt_orderby opt_limit
    { $$=new sqlimp::SNDelete($2,0,NULL,$4,NULL,$5,$6,$7);
      //emit("DELETEONE %d %s", $2, $4->c_str());
      delete($4); }
;

delete_opts: delete_opts T_LOW_PRIORITY { $$ = $1 + 01; }
   | delete_opts T_QUICK { $$ = $1 + 02; }
   | delete_opts T_IGNORE { $$ = $1 + 04; }
   | /* nil */ { $$ = 0; }
   ;

delete_stmt: T_DELETE delete_opts
    delete_list
    T_FROM table_references opt_where
	{ //emit("DELETEMULTI-0 %d %d %d", $2, $3, $5);
	  $$=new sqlimp::SNDelete($2,0,$3,NULL,$5,$6);
	  }

delete_stmt: T_DELETE delete_opts
    T_FROM delete_list
    T_USING table_references opt_where
    { //emit("DELETEMULTI-1 %d %d %d", $2, $4, $6);
	  $$=new sqlimp::SNDelete($2,1,$4,NULL,$6,$7);
	  }
  ;

delete_list: NAME opt_dot_star {
     $$=new RI_opt_name_list(','); $$->add($2==1?ONM_DOT_STAR:ONM_NAME, $1);
     //emit("TABLE %s", $1->c_str());
     delete($1); }
   | delete_list ',' NAME opt_dot_star {
     $$=$1; $$->add($4==1?ONM_DOT_STAR:ONM_NAME, $3);
     //emit("TABLE %s", $3->c_str());
     delete($3); }
   ;

opt_dot_star: /* nil */ { $$=0; }
   | '.' '*' { $$=1; }
   ;




   /* statements: insert statement */

stmt: insert_stmt { POST_STMT($1,$$);
     //emit(&yyloc, "INSERT STMT -----------------------------------------\n");
     }
   ;

insert_stmt: T_INSERT insert_opts opt_into NAME
     opt_col_names
     T_VALUES insert_vals_list
     opt_ondupupdate
	{ $$=new sqlimp::SNInsert($2, $3, $4, $5, $7, $8);
	  //emit("INSERTVALS %d %d %s", $2, $7, $4->c_str());
	  delete($4);
	}
   ;

opt_ondupupdate: /* nil */ { $$=new StmtExpr(); }
   | T_ONDUPLICATE T_KEY T_UPDATE insert_asgn_list { $$=new StmtExpr($4); $$->set_prefix("ON DUPLICATE KEY UPDATE"); }
   ;

insert_opts: /* nil */ { $$ = 0; }
   | insert_opts T_LOW_PRIORITY { $$ = $1 | 01 ; }
   | insert_opts T_DELAYED { $$ = $1 | 02 ; }
   | insert_opts T_HIGH_PRIORITY { $$ = $1 | 04 ; }
   | insert_opts T_IGNORE { $$ = $1 | 010 ; }
   ;

opt_into: T_INTO { $$=true; }| /* nil */ { $$=false; }
   ;

opt_col_names: /* nil */ { $$=NULL; }
   | '(' column_list ')' { $$=$2; }
   ;

insert_vals_list: '(' insert_vals ')' 			{ $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS,$2); }
   | insert_vals_list ',' '(' insert_vals ')'	{ $$=$1; $$->list_append(sqlimp::EXPR_OP_COMMA_NL_LEAD, new StmtExpr(sqlimp::EXPR_PARENTHESIS,$4)); }

insert_vals:
     expr { $$ = $1; }
   | T_DEFAULT { $$ = new StmtExpr(sqlimp::EXPR_TOKEN, "DEFAULT"); }
   | T_NULL { $$ = new StmtExpr(sqlimp::EXPR_NULL); }
   | insert_vals ',' expr { $$ = $1; $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $3); }
   | insert_vals ',' T_DEFAULT { $$ = $1; $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, new StmtExpr(sqlimp::EXPR_TOKEN, "DEFAULT")); }
   | insert_vals ',' T_NULL { $$ = $1; $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, new StmtExpr(sqlimp::EXPR_NULL)); }
   ;

insert_stmt: T_INSERT insert_opts opt_into NAME
     T_SET insert_asgn_list
     opt_ondupupdate
	 { $$=new sqlimp::SNInsert($2, $3, $4, NULL, $6, $7);
	   //emit("INSERTASGN %d %d %s", $2, $6, $4->c_str());
	   delete($4); }
   ;

insert_stmt: T_INSERT insert_opts opt_into NAME opt_col_names
    select_stmt
    opt_ondupupdate
		{ $$=new sqlimp::SNInsert($2, $3, $4, $5, $6, $7);
		  //emit("INSERTSELECT %d %s", $2, $4->c_str());
		  delete($4);
		  delete($6);
		  }
  ;

insert_asgn_list:
     NAME T_COMPARISON expr
     { if ($2 != 4) { lerror(&yyloc,"bad insert assignment to %s", $1->c_str()); YYERROR; }
       //emit("ASSIGN %s", $1->c_str());
       $$ = new StmtExpr(sqlimp::EXPR_NAME,*$1); $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),$3);
       delete($1); }
   | NAME T_COMPARISON T_DEFAULT
     { if ($2 != 4) { lerror(&yyloc,"bad insert assignment to %s", $1->c_str()); YYERROR; }
       //emit("DEFAULT"); emit("ASSIGN %s", $1->c_str());
       $$ = new StmtExpr(sqlimp::EXPR_NAME,*$1); $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),new StmtExpr(sqlimp::EXPR_TOKEN, "DEFAULT"));
       delete($1); }
   | insert_asgn_list ',' NAME T_COMPARISON expr
     { if ($4 != 4) { lerror(&yyloc,"bad insert assignment to %s", $3->c_str()); YYERROR; }
       //emit("ASSIGN %s", $3->c_str());
       $$ = $1;
       StmtExpr* expr = new StmtExpr(sqlimp::EXPR_NAME,*$3); expr->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$4),$5);
       $$->list_append(expr);
       delete($3); }
   | insert_asgn_list ',' NAME T_COMPARISON T_DEFAULT
     { if ($4 != 4) { lerror(&yyloc,"bad insert assignment to %s", $3->c_str()); YYERROR; }
       //emit("DEFAULT"); emit("ASSIGN %s", $3->c_str());
       $$ = $1;
       StmtExpr* expr = new StmtExpr(sqlimp::EXPR_NAME,*$3); expr->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$4),new StmtExpr(sqlimp::EXPR_TOKEN, "DEFAULT"));
       $$->list_append(expr);
       delete($3); }
   ;

   /** replace just like insert **/
stmt: replace_stmt { POST_STMT($1,$$); emit(&yyloc, "REPLACE STMT -----------------------------------------\n"); }
   ;

replace_stmt: T_REPLACE insert_opts opt_into NAME
     opt_col_names
     T_VALUES insert_vals_list
     opt_ondupupdate
     { $$=new sqlimp::SNReplace($2,$3,$4,$5,$7,$8);
	   //emit("REPLACEVALS %d %d %s", $2, $7, $4->c_str());
	   delete($4); }
   ;

replace_stmt: T_REPLACE insert_opts opt_into NAME
     T_SET insert_asgn_list
     opt_ondupupdate
     { $$=new sqlimp::SNReplace($2,$3,$4,$6,$7);
	   //emit("REPLACEASGN %d %d %s", $2, $6, $4->c_str());
	   delete($4); }
   ;

replace_stmt: T_REPLACE insert_opts opt_into NAME
	 opt_col_names
     select_stmt
     opt_ondupupdate
	 { $$=new sqlimp::SNReplace($2, $3, $4, $5, $6, $7);
	   emit("REPLACESELECT %d %s", $2, $4->c_str());
	   delete($4); delete($6); }
  ;





   /** update **/
stmt: update_stmt { POST_STMT($1,$$);
     //emit(&yyloc, "UPDATE STMT -----------------------------------------\n");
     }
   ;

update_stmt: T_UPDATE update_opts table_references
    T_SET update_asgn_list
    opt_where
    opt_orderby opt_limit
    { $$=new sqlimp::SNUpdate($2, $3, $5, $6, $7, $8);
      //emit("UPDATE %d %d %d", $2, $3, $5);
    }
   ;

update_opts: /* nil */ { $$ = 0; }
   | insert_opts T_LOW_PRIORITY { $$ = $1 | 01 ; }
   | insert_opts T_IGNORE { $$ = $1 | 010 ; }
   ;

update_asgn_list: update_asgn_item { $$=$1; }
   | update_asgn_list ',' update_asgn_item { $$=$1; $$->list_append(EXPR_OP_COMMA_NL_LEAD, $3); }
   ;
update_asgn_item:
     NAME T_COMPARISON expr
     { if ($2 != 4) { lerror(&yyloc,"bad update assignment to %s", $1->c_str()); YYERROR; }
	   $$=new StmtExpr(EXPR_NAME, *$1);
	   $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2), $3);
	   //emit("ASSIGN %s", $1->c_str());
	   delete($1);
	 }
   | NAME '.' NAME T_COMPARISON expr
     { if ($4 != 4) { lerror(&yyloc,"bad update assignment to %s", $1->c_str()); YYERROR; }
	   $$=new StmtExpr(EXPR_NAME, *$1, *$3);
	   $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$4), $5);
	   //emit("ASSIGN %s.%s", $1->c_str(), $3->c_str());
	   delete($1); delete($3); }
   ;





   /** create database **/

stmt: create_database_stmt create_database_opts
     { sqlimp::SNCreate* curr=(sqlimp::SNCreate*)$1;
       curr->set_database_opts($2);
       POST_STMT($1,$$);
       //emit("CREATE DATABASE STMT -----------------------------------------\n");
     }
   ;

create_database_stmt:
     T_CREATE T_DATABASE opt_if_not_exists create_database_name
	  { $$=new sqlimp::SNCreateDatabase($3, $4);
	    //emit("CREATE DATABASE %d %s", $3, $4->c_str());
	    delete($4); }
   | T_CREATE T_SCHEMA opt_if_not_exists create_database_name
	  { $$=new sqlimp::SNCreateSchema($3, $4);
	    //emit("CREATE SCHEMA %d %s", $3, $4->c_str());
	    delete($4); }
   ;

opt_if_not_exists:  /* nil */ 				{ $$ = -1; }
   | T_IF T_EXISTS        					{ //emit("if exists: %d.",$2);
     //0:exists, 1:not exists
	 /* if(!$2) { lyyerror(@2,"IF [NOT] EXISTS doesn't exist"); YYERROR; } */
	 /* ke shi bie chu "IF (NOT)? EXISTS"... */
     $$ = $2; /* NOT EXISTS hack */
     }
   ;
create_database_name: NAME					{ $$=$1; }
create_database_opts: /* nil */				{ $$=new sqlimp::RI_opt_name_list(); }
   | create_database_opts cdo_create_opt	{ $$=$1; $$->add($2); }
   ;

cdo_create_opt: cdo_charset_opt 			{ $$=$1; }
   | cdo_collate_opt 						{ $$=$1; }
   | cdo_comment_opt 						{ $$=$1; }
   ;

cdo_charset_opt: T_DEFAULT T_CHAR T_SET cdo_create_db_option_value	{ $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_CHARACTER_SET, *$4); /*emit("default character set = %s", $4->c_str());*/ delete($4); };
cdo_collate_opt: /* nil */											{ $$=NULL; }
   | T_COLLATE cdo_create_db_option_value							{ $$=new sqlimp::RI_opt_name(sqlimp::ONM_COLLATE, *$2); /*emit("collate = %s", $2->c_str());*/ delete($2); }
   | T_DEFAULT T_COLLATE cdo_create_db_option_value					{ $$=new sqlimp::RI_opt_name(sqlimp::ONM_COLLATE, *$3); /*emit("collate = %s", $3->c_str());*/ delete($3); }
   ;
cdo_comment_opt: T_COMMENT cdo_create_db_option_value				{ $$=new sqlimp::RI_opt_name(sqlimp::ONM_COMMENT, *$2); /*emit("comment = %s", $2->c_str());*/ delete($2); };

cdo_create_db_option_value: NAME			{ $$=$1; }
   | T_COMPARISON NAME 						{ $$=$2; }
   | STRING									{ $$=$1; }
   ;


   /** create table **/
stmt: create_table_stmt  { POST_STMT($1,$$);
	  //emit(&yyloc, "CREATE TABLE STMT -----------------------------------------\n");
	}
   ;

create_table_stmt: T_CREATE opt_temporary T_TABLE opt_if_not_exists NAME
   '(' create_col_list ')' create_table_opts partition_opts
		{ $$=new sqlimp::SNCreateTable($4, $5, $2, $7, $9, $10);
		  //emit("  CREATE %d %d %d TABLE `%s` (%s\n)", $2, $4, $7, $5->c_str(), $7->toString().c_str());
		  delete($5);
		}
   ;

create_table_stmt: T_CREATE opt_temporary T_TABLE opt_if_not_exists NAME '.' NAME
   '(' create_col_list ')' create_table_opts partition_opts
		{ $$=new sqlimp::SNCreateTable($4, $5, $7, $2, $9, $11, $12);
		  //emit("  CREATE %d %d %d TABLE `%s`.`%s`", $2, $4, $9, $5->c_str(), $7->c_str());
		  delete($5); delete($7); }
   ;

create_table_stmt: T_CREATE opt_temporary T_TABLE opt_if_not_exists NAME
   '(' create_col_list ')' create_table_opts partition_opts
	create_select_statement
		{ $$=new sqlimp::SNCreateTable($4, $5, $2, $7, $9, $10, $11);
		  //emit("  CREATESELECT %d %d %d TABLE `%s`", $2, $4, $7, $5->c_str());
		  delete($5); }
   ;

create_table_stmt: T_CREATE opt_temporary T_TABLE opt_if_not_exists NAME
     create_table_opts partition_opts
     create_select_statement
		{ $$=new sqlimp::SNCreateTable($4, $5, $2, NULL, $6, $7, $8);
		  //emit("  CREATESELECT %d %d 0 TABLE `%s`", $2, $4, $5->c_str());
		  delete($5); }
   ;

create_table_stmt: T_CREATE opt_temporary T_TABLE opt_if_not_exists NAME '.' NAME
   '(' create_col_list ')' create_table_opts partition_opts
     create_select_statement
		{ $$=new sqlimp::SNCreateTable($4, $5, $7, $2, $9, $11, $12, $13);
		  //emit("  CREATESELECT %d %d 0 TABLE `%s`.`%s`", $2, $4, $5->c_str(), $7->c_str());
		  delete($5); delete($7); }
   ;

create_table_stmt: T_CREATE opt_temporary T_TABLE opt_if_not_exists NAME '.' NAME
     create_table_opts partition_opts
     create_select_statement
		{ $$=new sqlimp::SNCreateTable($4, $5, $7, $2, NULL, $8, $9, $10);
		  //emit("  CREATESELECT %d %d 0 TABLE `%s`.`%s`", $2, $4, $5->c_str(), $7->c_str());
		  delete($5); delete($7); }
   ;


partition_opts: /* nil */				{ $$=new sqlimp::RI_part_opts(); }
   | T_PARTITION T_BY partition_type opt_partitions_num_def
     opt_subpartitions_opts
     opt_partition_items
     { //emit("|3|%s||", $3->toString().c_str());
       //emit("|4|%s||", $4->toString().c_str());
       //emit("|5|%s||", !$5?" ":$5->toString().c_str());
       //emit("|6|%s||", !$6?" ":$6->toString().c_str());
       $$=new sqlimp::RI_part_opts($3,$4,$5,$6);
     }
   ;

opt_subpartitions_opts: /* nil */		{ $$=new sqlimp::RI_part_def(); }
   | T_SUBPARTITION T_BY subpartition_type opt_subpartitions_num_def
     { $$=new sqlimp::RI_part_def($3,$4);
       //$$->add_sub_rule($3);
     }
   ;

opt_column_list:/*nil*/ 				{ $$=new sqlimp::RI_string_list(); }
   | column_list						{ $$=$1; }
   ;

opt_algor:/*nil*/ { $$=0; }
   | T_ALGORITHM T_COMPARISON INTNUM	{
	 if($2!=sqlimp::COMPARISON_ASSIGN){ YYERROR; }
	 if($3!=1&&$3!=2){ lerror(&yyloc, "Algorithm {1,2} is wanted, but number value '%d' FOUND.",$3); YYERROR; }
	 $$=$3;
   }
   ;

partition_type: T_HASH '(' expr ')'		{ $$=new sqlimp::RI_part_type(sqlimp::PT_HASH, $3); }
   | T_LINEAR T_HASH '(' expr ')'		{ $$=new sqlimp::RI_part_type(sqlimp::PT_LINEAR_HASH, $4); }
   | T_KEY opt_algor '(' opt_column_list ')'			{ $$=new sqlimp::RI_part_type(sqlimp::PT_KEY, $4); }
   | T_LINEAR T_KEY opt_algor '(' opt_column_list ')'	{ $$=new sqlimp::RI_part_type(sqlimp::PT_LINEAR_KEY, $5);
     //emit("**%s**", $$->toString().c_str());
   }
   | T_RANGE '(' expr ')'					{ $$=new sqlimp::RI_part_type(sqlimp::PT_RANGE, $3);
     //emit("**%s**", $$->toString().c_str());
   }
   | T_RANGE T_COLUMNS '(' column_list ')'	{ $$=new sqlimp::RI_part_type(sqlimp::PT_RANGE_COLS, $4); }
   | T_LIST '(' expr ')'					{ $$=new sqlimp::RI_part_type(sqlimp::PT_LIST, $3); }
   | T_LIST T_COLUMNS '(' column_list ')'	{ $$=new sqlimp::RI_part_type(sqlimp::PT_LIST_COLS, $4); }
   ;
subpartition_type: T_HASH '(' expr ')'	{ $$=new sqlimp::RI_part_type(sqlimp::PT_HASH, $3); }
   | T_LINEAR T_HASH '(' expr ')'		{ $$=new sqlimp::RI_part_type(sqlimp::PT_LINEAR_HASH, $4); }
   ;

opt_partitions_num_def:    /* nil */	{ $$=new sqlimp::RI_part_num_def(); }
   | T_PARTITIONS INTNUM				{ $$=new sqlimp::RI_part_num_def(true,$2); }
   ;
opt_subpartitions_num_def: /* nil */	{ $$=new sqlimp::RI_part_num_def(); }
   | T_SUBPARTITIONS INTNUM				{ $$=new sqlimp::RI_part_num_def(false,$2); }
   ;

opt_partition_items: /* nil */			{ $$=new sqlimp::RI_part_items(); }
   | '(' partition_items ')'			{ $$=$2; }
   ;
partition_items: part_item_expr			{ $$=new sqlimp::RI_part_items(); $$->add(new sqlimp::RI_part_item($1));
	 //emit("-+  %s.", $$->toString().c_str());
	 }
   | partition_items ',' part_item_expr	{ $$=$1; $$->add(new sqlimp::RI_part_item($3));
     //emit("-++ %s.", $$->toString().c_str());
     }
   ;

part_item_expr: part_item_it			{ $$=$1; }
   | part_item_expr part_item_it		{ $$=$1; $$->list_append($2); }
   ;
part_item_it: T_PARTITION NAME 			{ $$=new StmtExpr(sqlimp::EXPR_TOKEN,"PARTITION",*$2); delete($2);
	 //emit("##%s##", $$->toString().c_str());
	 }
   | part_item_values					{ $$=($1); }
   | part_item_storage					{ $$=($1); }
   | part_item_comment					{ $$=($1); }
   | part_item_datadir					{ $$=($1); }
   | part_item_indexdir					{ $$=($1); }
   | part_item_maxrows					{ $$=($1); }
   | part_item_minrows					{ $$=($1); }
   | part_item_tablespace				{ $$=($1); }
   | part_item_nodegroup				{ $$=($1); }
   | part_item_subpartition_list		{ $$=($1); }
   ;
  /*part_item_values: part_item_value         { $$=$1; }
   | part_item_values ',' part_item_value { $$=$1; $$->list_append(EXPR_OP_COMMA_LEAD, $3); }
   */
part_item_values: T_VALUES T_LESS T_THAN part_item_value_lt	 { $$=new StmtExpr();$$->values_prefix($4,true); }
   | T_VALUES T_IN '(' val_list ')'							 { $$=new StmtExpr();$$->values_prefix($4,false); }
   ;
part_item_value_lt: '(' expr ')'							 { $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS, $2); }
   | '(' val_list ')'										 { $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS, $2); }
   //| '(' T_MAXVALUE ')'										 { $$=new StmtExpr(sqlimp::EXPR_TOKEN, "MAXVALUE"); }
   | T_MAXVALUE 											 { $$=new StmtExpr("MAXVALUE"); }
   ;
part_item_storage: T_STORAGE T_ENGINE part_item_opt_name	 { $$=new StmtExpr();$$->storage_engine_prefix($3,true); delete($3); };
   | T_ENGINE part_item_opt_name							 { $$=new StmtExpr();$$->storage_engine_prefix($2,false); delete($2); }
   ;
part_item_comment: T_COMMENT part_item_opt_string            { $$=new StmtExpr();$$->comment_prefix($2); delete($2); };
part_item_datadir: T_DATA T_DIRECTORY part_item_opt_string   { $$=new StmtExpr();$$->directory_prefix($3,true); delete($3); };
part_item_indexdir: T_KEY T_DIRECTORY part_item_opt_string { $$=new StmtExpr();$$->directory_prefix($3,false); delete($3); };
part_item_maxrows: T_MAX T_ROWS part_item_opt_num            { $$=new StmtExpr();$$->minmax_rows_prefix($3,true);  };
part_item_minrows: T_MIN T_ROWS part_item_opt_num            { $$=new StmtExpr();$$->minmax_rows_prefix($3,false);  };
part_item_tablespace: T_TABLESPACE part_item_opt_name        { $$=new StmtExpr();$$->tablespace_prefix($2); delete($2); };
part_item_nodegroup: T_NODEGROUP part_item_opt_num           { $$=new StmtExpr();$$->nodegroup_prefix($2);  };
part_item_subpartition_list: '(' subpartition_list ')'		 { $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS, $2); };

part_item_opt_name: NAME     { $$=$1; }
   | T_COMPARISON NAME       { $$=$2; }
   ;
part_item_opt_string: STRING { $$=$1; }
   | T_COMPARISON STRING     { $$=$2; }
   ;
part_item_opt_num: INTNUM    { $$=$1; }
   | T_COMPARISON INTNUM     { $$=$2; }
   ;
subpartition_list: subpartition_def         { $$=$1; }
   | subpartition_list ',' subpartition_def { $$=$1; $$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $3); }
   ;
subpartition_def: subpartition_def_item		{ $$=$1; }
   | subpartition_def subpartition_def_item	{ $$=$1; $$->list_append($2); }
   ;
subpartition_def_item: NAME					{ $$=new StmtExpr(sqlimp::EXPR_TOKEN,"SUBPARTITION",*$1); delete($1); }
   | T_SUBPARTITION NAME					{ $$=new StmtExpr(sqlimp::EXPR_TOKEN,"SUBPARTITION",*$2); delete($2);}
   | part_item_values						{ $$=($1); }
   | part_item_storage						{ $$=($1); }
   | part_item_comment						{ $$=($1); }
   | part_item_datadir						{ $$=($1); }
   | part_item_indexdir						{ $$=($1); }
   | part_item_maxrows						{ $$=($1); }
   | part_item_minrows						{ $$=($1); }
   | part_item_tablespace					{ $$=($1); }
   | part_item_nodegroup					{ $$=($1); }
   ;

create_col_list: create_definition          { $$ = $1; }
   | create_col_list ',' create_definition  { $$ = $1; $$->merge_rule_items($3, ','); }
   ;

// { /* emit("STARTCOL"); */ }
create_definition: col_name data_type column_atts {
		//emit("COLUMNDEF %d `%s`", $3, $2->c_str());
		$$=new StmtRule(sqlimp::RULE_RULES);

		sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();// debug_point();
		nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$1));
		StmtRule* subRule = new StmtRule(sqlimp::RULE_NAME_LIST, nl);
		subRule->add_sub_rule($2); // $3 is a StmtRule(RULE_COLUMN_DATA_TYPE) pointer
		subRule->add_sub_rule(new StmtRule($3)); //column_atts

		StmtRuleItem* innerItem = $$->get_rule();
		innerItem->set(subRule);
		delete($1);
		}
	| col_name data_type column_def2 {
	    $$=new StmtRule(sqlimp::RULE_RULES);

		sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();// debug_point();
		nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$1));
		StmtRule* subRule = new StmtRule(sqlimp::RULE_NAME_LIST, nl);
		subRule->add_sub_rule($2); // $3 is a StmtRule(RULE_COLUMN_DATA_TYPE) pointer
		subRule->add_sub_rule(new StmtRule($3)); //column_def2

		StmtRuleItem* innerItem = $$->get_rule();
		innerItem->set(subRule);
		delete($1);
      }
    | T_PRIMARY T_KEY '(' index_column_list ')' opt_index_type {
		$$=new StmtRule(NULL,NULL,$4,$6,sqlimp::RULE_INDEX,sqlimp::IK_PK);
		/*debug_point(); emit("PRIKEY %d", $4);*/ }
    | INDEX_KEY opt_index_name '(' index_column_list ')' opt_index_type {
		$$=new StmtRule(NULL, $2, $4,$6,sqlimp::RULE_INDEX,sqlimp::IK_INDEX);
		/*emit("KEY %d", $4);;*/ }
    | CONSTRAINT_symbol T_UNIQUE INDEX_KEY opt_index_name '(' index_column_list ')' opt_index_type {
		$$=new StmtRule(NULL, $4, $6,$8,sqlimp::RULE_INDEX,sqlimp::IK_UK);
		/*emit("--ok--");emit("UNIQUE KEY %d", $5);*/ }
    | FULLTEXT_SPATIAL INDEX_KEY opt_index_name '(' index_column_list ')' opt_index_type {
		$$=new StmtRule(NULL, $3, $5,$7,sqlimp::RULE_INDEX,sqlimp::IK_FULLTEXT);
		/*emit("TEXTINDEX %d", $5);*/ }
    | CONSTRAINT_symbol T_FOREIGN T_KEY opt_index_name '(' index_column_list ')'
      opt_fk_references_definitions {
		$$=new StmtRule($1, $4, $6, $8);
	  }
	| T_CHECK expr { $$=new StmtRule(RULE_CHECK, $2); }
    ;

column_atts: /* nil */				{ $$=new sqlimp::RI_col_att_list(); }
   | column_att						{ $$=$1; }
   | column_atts column_att			{ $$=$1; $$->add($2); }
   ;
column_att
   : T_NOT T_NULL					{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_NOT_NULL)); /*emit("notnull");*/ }
   | T_NULL							{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_NULL)); /*emit("null");*/ }
   | default_clause					{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_DEF_CLAUSE, $1)); }
   | T_AUTO_INCREMENT				{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_AUTOINC)); }
   | T_UNIQUE '(' column_list ')'	{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_UNIQUE,$3)); }
   | T_UNIQUE T_KEY					{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_UK)); }
   | T_UNIQUE 						{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_UK)); }
   | T_PRIMARY T_KEY				{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_PK)); }
   | T_KEY							{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_INDEX)); }
   | T_COMMENT STRING				{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_COMMENT,$2)); delete($2); }
   | T_CHECK expr					{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_CHECK, $2)); }
   | opt_fk_references_definitions	{ $$ = new sqlimp::RI_col_att_list(); $$->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_FK,$1)); }
   /* TODO [COLUMN_FORMAT {FIXED|DYNAMIC|DEFAULT}]
           [STORAGE {DISK|MEMORY|DEFAULT}]
   */
   ;
column_def2: opt_generated_always opt_as_expr opt_virtual_stored
     opt_null_def opt_unique_key opt_primary_key opt_comment {
		if ($2==NULL) { lerror(&yyloc,"expect missed 'AS (expr)'."); }
		$$ = new sqlimp::RI_col_att_list();

		sqlimp::StmtExpr* ex = $2; //new sqlimp::StmtExpr();
		switch($1){
			case 1: ex->set_prefix("GENERATED ALWAYS"); break;
		}
		switch($3){
			case 1: ex->list_append_token_literal(("VIRTUAL")); break;
			case 2: ex->list_append_token_literal(("STORED")); break;
		}
		switch($4){
			case 1: ex->list_append_token_literal(("NULL")); break;
			case 2: ex->list_append_token_literal(("NOT NULL")); break;
		}
		switch($5){
			case 1: ex->list_append_token_literal(("UNIQUE KEY")); break;
		}
		switch($6){
			case 1: ex->list_append_token_literal(("PRIMARY KEY")); break;
		}

		$$->add(new sqlimp::RI_col_att_item(ex));

		if($7){
			sqlimp::RI_col_att_list* cmt = new sqlimp::RI_col_att_list();
			cmt->add(new sqlimp::RI_col_att_item(sqlimp::COL_ATTR_COMMENT,$7));
			delete $7;
		 	$$->add(cmt);
		}
	 }
   ;

default_clause: T_DEFAULT default_literal { $$=$2; };
default_literal: T_NULL				{ /*emit("default null");*/ $$=new StmtExpr(sqlimp::EXPR_NULL); }
   /*| T_NOT T_NULL					{ emit("default not null"); $$=new StmtExpr(EXPR_NOT_NULL); }
   */
   | default_literal_tm				{ $$=$1; }
   | STRING							{ $$=new StmtExpr(*$1); /*emit("default '%s'.",$1->c_str());*/ delete($1); }
   | INTNUM							{ $$=new StmtExpr($1);  /*emit("ATTR DEFAULT NUMBER %d", $1);*/ }
   | APPROXNUM						{ $$=new StmtExpr($1);  /*emit("ATTR DEFAULT FLOAT %g", $1);*/  }
   | BOOL							{ $$=new StmtExpr($1);  /*emit("ATTR DEFAULT BOOL %d", $1);*/ }
   ;
default_literal_tm : T_CURRENT_TIMESTAMP 					{ $$=new StmtExpr(sqlimp::EXPR_CURRENT_TIMESTAMP,1); }
   | T_CURRENT_TIMESTAMP T_ON T_UPDATE T_CURRENT_TIMESTAMP	{ $$=new StmtExpr(sqlimp::EXPR_CURRENT_TIME,2); }
   ;

opt_generated_always: /* nil 8 */	{ $$ = 0; }
   | T_GENERATED_ALWAYS				{ $$ = 1; }
   ;

opt_virtual_stored: /* nil */		{ $$ = 0; }
   | T_VIRTUAL						{ $$ = 1; }
   | T_STORED						{ $$ = 2; }
   ;

opt_null_def: /* nil */				{ $$ = 0; }
   | T_NULL							{ $$ = 1; }
   | T_NOT T_NULL					{ $$ = 2; }
   ;

opt_unique_key: /* nil */			{ $$ = 0; }
   | T_UNIQUE opt_key				{ $$ = 1; }
   ;

opt_key: /* nil */					{ $$ = 0; }
   | T_KEY							{ $$ = 1; }
   ;

opt_primary_key: /* nil */			{ $$ = 0; }
   | T_PRIMARY T_KEY				{ $$ = 1; }
   | T_KEY							{ $$ = 2; }
   ;

opt_comment: /* nil */				{ $$ = NULL; }
   | T_COMMENT STRING				{ $$ = $2; }
   ;

opt_length: /* nil */				{ $$ = 0; }
   | '(' INTNUM ')'					{ $$ = $2; /* 低16位有效 */ }
   | '(' INTNUM ',' INTNUM ')'		{ assert($2<0xffff); assert($4<0x7fff); $$ = $2 + ($4 << 16); }
   ;

opt_binary: /* nil */				{ $$ = false; }
   | T_BINARY						{ $$ = true; }
   ;

opt_uz: /* nil */					{ $$ = 0; }
   | opt_uz T_UNSIGNED				{ $$ = $1 | (1<<31); }
   | opt_uz T_ZEROFILL				{ $$ = $1 | (1<<30); }
   ;

opt_csc: /* nil */								{ $$=NULL; }
   | opt_csc T_CHAR T_SET assign_charset_name	{ $$=new sqlimp::RI_opt_csc(true,*$4);  delete($4); }
   | opt_csc T_COLLATE assign_collation_name	{ $$=new sqlimp::RI_opt_csc(false,*$3); delete($3); }
   ;

assign_charset_name:
     T_COMPARISON charset_name		{ if(!ensure_subtok(&yyloc, $1, sqlimp::COMPARISON_ASSIGN)) YYERROR; $$=$2; }
   | charset_name					{ $$=$1; }
   ;
assign_collation_name:
     T_COMPARISON collation_name	{ if(!ensure_subtok(&yyloc, $1, sqlimp::COMPARISON_ASSIGN)) YYERROR; $$=$2; }
   | collation_name					{ $$=$1; }
   ;
// opt_EQUAL: /* nil */				{ $$=-1; } | T_COMPARISON		{ $$=1; } ;

collation_name: NAME|STRING			{ $$=$1; };
charset_name: NAME|STRING			{ $$=$1; };
tablespace_name: NAME|STRING		{ $$=$1; };


data_type:
     T_BIT opt_length				{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_BIT, $2); }
   | T_TINYINT opt_length opt_uz	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_TINYINT, $2, $3); }
   | T_SMALLINT opt_length opt_uz	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_SMALLINT, $2, $3); }
   | T_MEDIUMINT opt_length opt_uz	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_MEDIUMINT, $2, $3); }
   | T_INT opt_length opt_uz		{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_INT, $2, $3); }
   | T_INTEGER opt_length opt_uz	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_INTEGER, $2, $3); }
   | T_BIGINT opt_length opt_uz		{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_BIGINT, $2, $3); }
   | T_REAL opt_length opt_uz		{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_REAL, $2, $3); }
   | T_DOUBLE opt_length opt_uz		{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_DOUBLE, $2, $3); }
   | T_FLOAT opt_length opt_uz		{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_FLOAT, $2, $3); }
   | T_DECIMAL opt_length opt_uz	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_DECIMAL, $2, $3); }
   | T_DATE { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_DATE); }
   | T_TIME { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_TIME); }
   | T_TIMESTAMP { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_TIMESTAMP); }
   | T_DATETIME { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_DATETIME); }
   | T_YEAR { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_YEAR); }
   | T_CHAR opt_length opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_CHAR, $2, *$3); }
   | T_VARCHAR '(' INTNUM ')' opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_VARCHAR, $3, *$5); }
   | T_BINARY opt_length { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_BINARY, $2); }
   | T_VARBINARY '(' INTNUM ')' { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_VARBINARY, $3); }
   | T_TINYBLOB { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_TINYBLOB); }
   | T_BLOB { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_BLOB); }
   | T_MEDIUMBLOB { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_MEDIUMBLOB); }
   | T_LONGBLOB { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_LONGBLOB); }
   | T_TINYTEXT opt_binary opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_TINYTEXT, $2, *$3); }
   | T_TEXT opt_binary opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_TEXT, $2, *$3); }
   | T_MEDIUMTEXT opt_binary opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_MEDIUMTEXT, $2, *$3); }
   | T_LONGTEXT opt_binary opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_LONGTEXT, $2, *$3); }
   | T_ENUM '(' enum_list ')' opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_ENUM, *$3, *$5); }
   | T_SET '(' enum_list ')' opt_csc { $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_ENUM, *$3, *$5); }
   | spatial_type { $$ = $1; }
   ;

spatial_type: T_GEOMETRY	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_GEOMETRY); }
   | T_POINT				{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_POINT); }
   | T_LINESTRING			{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_LINESTRING); }
   | T_POLYGON				{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_POLYGON); }
   | T_MULTIPOINT			{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_MULTIPOINT); }
   | T_MULTILINESTRING		{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_MULTILINESTRING); }
   | T_MULTIPOLYGON			{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_MULTIPOLYGON); }
   | T_GEOMETRYCOLLECTION	{ $$ = new StmtRule(sqlimp::RULE_COLUMN_DATA_TYPE, sqlimp::DT_GEOMETRYCOLLECTION); }
   ;

enum_list: STRING 			{ $$=new sqlimp::RI_string_list(); $$->add($1); emit("ENUMVAL %s", $1->c_str()); delete($1); }
   | enum_list ',' STRING	{ $$=$1; $$->add($3); emit("ENUMVAL %s", $3->c_str()); delete($3); }
   ;

create_select_statement: opt_ignore_replace opt_as select_stmt {
	 $$=new StmtRule();
	 if ($1>0) $$->add_string($1==1?"IGNORE":"REPLACE");
	 sqlimp::StmtExpr* asExpr = new sqlimp::StmtExpr();
	 asExpr->as_clause($3); delete($3);
	 $$->add_rule_item(new sqlimp::StmtRuleItem(asExpr), ' ');
	 emit("CREATESELECT: opt_ignore_replace=%d, opt_as=%d", $1, $2); }
   ;

opt_ignore_replace: /* nil */				{ $$ = 0; }
   | T_IGNORE								{ $$ = 1; }
   | T_REPLACE								{ $$ = 2; }
   ;

opt_temporary:   /* nil */					{ $$ = 0; }
   | T_TEMPORARY							{ $$ = 1; }
   ;

create_table_opts: /* nil */				{ $$=new sqlimp::RI_opt_name_list(); }
   | create_table_opts cto_create_opt		{ $$=$1; $$->add($2); }
   ;

cto_create_opt:
     cto_autoinc_opt						{ $$=$1; }
   //| AVG_ROW_LENGTH [=] value
   //| [DEFAULT] CHARACTER SET [=] charset_name
   | cto_charset_opt						{ $$=$1; }
   //| CHECKSUM [=] {0 | 1}
   //| [DEFAULT] COLLATE [=] collation_name
   | cto_collate_opt						{ $$=$1; }
   //| COMMENT [=] 'string'
   | cto_comment_opt						{ $$=$1; }
   //| COMPRESSION [=] {'ZLIB'|'LZ4'|'NONE'}
   //| CONNECTION [=] 'connect_string'
   //| {DATA|INDEX} DIRECTORY [=] 'absolute path to directory'
   //| DELAY_KEY_WRITE [=] {0 | 1}
   //| ENCRYPTION [=] {'Y' | 'N'}
   //| ENGINE [=] engine_name
   | cto_engine_opt							{ $$=$1; }
//   | INSERT_METHOD [=] { NO | FIRST | LAST }
//   | KEY_BLOCK_SIZE [=] value
//   | MAX_ROWS [=] value
//   | MIN_ROWS [=] value
//   | PACK_KEYS [=] {0 | 1 | DEFAULT}
//   | PASSWORD [=] 'string'
//   | ROW_FORMAT [=] {DEFAULT|DYNAMIC|FIXED|COMPRESSED|REDUNDANT|COMPACT}
//   | STATS_AUTO_RECALC [=] {DEFAULT|0|1}
//   | STATS_PERSISTENT [=] {DEFAULT|0|1}
//   | STATS_SAMPLE_PAGES [=] value
   //| TABLESPACE tablespace_name [STORAGE {DISK|MEMORY|DEFAULT}]
   | T_TABLESPACE tablespace_name cto_storage_type_opt	{
      $$=new sqlimp::RI_opt_name(sqlimp::ONM_TABLESPACE, *$2, $3);
      delete($2);
   }
//   | UNION [=] (tbl_name[,tbl_name]...)
   ;

cto_storage_type_opt: /* nil */				{ $$=-1; }
   | T_STORAGE cto_storage_type_def			{ $$=$2; }
   ;
cto_storage_type_def: T_DISK				{ $$=1; }
   | T_MEMORY								{ $$=2; }
   | T_NONE									{ $$=0; }
   ;
cto_engine_opt:  T_ENGINE cto_create_db_option_value               { $$=new sqlimp::RI_opt_name(sqlimp::ONM_ENGINE,*$2); delete($2); };
cto_charset_opt:
     T_DEFAULT T_CHAR T_SET cto_create_db_option_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_CHARACTER_SET,*$4); delete($4); }
   | T_CHAR T_SET cto_create_db_option_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_CHARACTER_SET,*$3); delete($3); }
   | T_DEFAULT T_CHAR T_SET cto_create_db_option_string_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_CHARACTER_SET,*$4); delete($4); }
   | T_CHAR T_SET cto_create_db_option_string_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_CHARACTER_SET,*$3); delete($3); }
   | T_DEFAULT T_CHARSET cto_create_db_option_value                { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_CHARACTER_SET,*$3); delete($3); }
   ;
cto_collate_opt:
     T_COLLATE cto_create_db_option_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_COLLATE,*$2); delete($2); }
   | T_DEFAULT T_COLLATE cto_create_db_option_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_COLLATE,*$3); delete($3); }
   | T_COLLATE cto_create_db_option_string_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_COLLATE,*$2); delete($2); }
   | T_DEFAULT T_COLLATE cto_create_db_option_string_value { $$=new sqlimp::RI_opt_name(sqlimp::ONM_DEFAULT_COLLATE,*$3); delete($3); }
   ;
cto_comment_opt: T_COMMENT cto_create_db_option_string_value       { $$=new sqlimp::RI_opt_name(sqlimp::ONM_COMMENT,*$2); delete($2); };
cto_autoinc_opt: T_AUTO_INCREMENT cto_create_db_option_int_value   { $$=new sqlimp::RI_opt_name(sqlimp::ONM_AUTO_INCREMENT,*$2); };

cto_create_db_option_value: NAME			{ $$=$1; }
   | T_COMPARISON NAME						{ if(!ensure_subtok(&yyloc, $1, sqlimp::COMPARISON_ASSIGN)){ lerror(&yyloc,"?? $1=%d", $1); YYERROR; } $$=$2; }
   ;
cto_create_db_option_string_value: STRING	{ $$=$1; }
   | T_COMPARISON STRING					{ if(!ensure_subtok(&yyloc, $1, sqlimp::COMPARISON_ASSIGN)){ lerror(&yyloc,"?? $1=%d", $1); YYERROR; } $$=$2; }
   ;
cto_create_db_option_int_value: INTNUM		{ $$=new std::string(toString($1)); }
   | T_COMPARISON INTNUM					{ if(!ensure_subtok(&yyloc, $1, sqlimp::COMPARISON_ASSIGN)){ lerror(&yyloc,"?? $1=%d", $1); YYERROR; }
											  $$=new std::string(toString($2)); }
   ;





stmt: create_function_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "CREATE FUNCTION STMT -----------------------------------------\n");
	  }
    ;
create_function_stmt: create_function_lead
      opt_create_func_params
      opt_create_func_returns
      opt_create_func_characteristic
      opt_create_func_body {
       sqlimp::SNCreateFunction* curr=(sqlimp::SNCreateFunction*)CURR_STMT;(void)curr;
       $$=curr;
       }
    ;
create_function_lead: T_CREATE opt_REPLACE opt_USER T_FUNCTION NAME
	 { $$=new sqlimp::SNCreateFunction();
	   CURR_STMT=$$;
	   delete($5);
	 }
   ;

opt_REPLACE: /* nil */
   | T_OR T_REPLACE
   ;
opt_USER: /* nil */
   | T_DEFINE T_COMPARISON NAME { delete($3); }
   | T_DEFINE T_COMPARISON T_CURRENT_USER
   ;
opt_create_func_params: '(' opt_create_func_param_list ')';
opt_create_func_param_list: /* nil */
   | opt_create_func_param
   | opt_create_func_param_list ',' opt_create_func_param
   ;
opt_create_func_param: opt_directions NAME data_type { delete($2); };

opt_create_func_returns: T_RETURNS data_type;

opt_create_func_characteristic: /* nil */
   | opt_create_func_ch
   | opt_create_func_characteristic opt_create_func_ch
   ;
opt_create_func_ch: T_COMMENT STRING  { delete($2); }
   | T_LANGUAGE T_SQL
   | T_DETERMINISTIC
   | T_NOT T_DETERMINISTIC
   | opt_create_func_ch_1
   | T_SQL T_SECURITY opt_sql_security_args
   ;
opt_create_func_ch_1: T_CONTAINS T_SQL
   | T_NO T_SQL
   | T_READS T_SQL T_DATA
   | T_MODIFIES T_SQL T_DATA;
opt_sql_security_args: T_DEFINER | T_INVOKER;

opt_create_func_body: stmt_block;

opt_directions: /* nil */ | T_IN | T_OUT | T_INOUT ;



stmt: create_procedure_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "CREATE PROCEDURE STMT -----------------------------------------\n");
	  }
    ;
create_procedure_stmt:
      T_CREATE opt_REPLACE opt_USER T_PROCEDURE NAME { delete($5); }
      opt_create_func_params
      opt_create_func_characteristic
      opt_create_func_body
      { $$=new sqlimp::SNCreateProcedure(); }
    ;





stmt: drop_database_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "DROP DATABASE STMT -----------------------------------------\n");
	  }
    ;
drop_database_stmt:
      T_DROP T_DATABASE opt_if_not_exists NAME
      { $$=new sqlimp::SNDropDatabase($3, $4); delete($4); }
    | T_DROP T_SCHEMA opt_if_not_exists NAME
	  { $$=new sqlimp::SNDropSchema($3, $4); delete($4); }
    ;


stmt: drop_table_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "DROP TABLE STMT -----------------------------------------\n");
	  }
    ;
drop_table_stmt: T_DROP T_TABLE opt_if_not_exists NAME
	  { $$=new sqlimp::SNDropTable($3, $4); delete($4); }
    ;








stmt: alter_table_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "ALTER TABLE STMT -----------------------------------------\n");
	  }
   ;
   /*
   http://dev.mysql.com/doc/refman/5.0/en/alter-table.html
   */
alter_table_stmt: alter_table_lead opt_alter_tbl_sub_clauses { $$=$1; }
   ;

alter_table_lead: T_ALTER opt_IGNORE T_TABLE table_name
		{ sqlimp::SNAlterTable* curr=new sqlimp::SNAlterTable(); CURR_STMT=curr;
		  //emit("-- ALTER TABLE %s", $4->c_str());
		  curr->tableName(*$4);
		  //curr->add($7);
		  $$=CURR_STMT; }
   ;

opt_alter_tbl_sub_clauses:
     opt_alter_tbl_create_table_opts
	 { sqlimp::SNAlterTable* curr=(sqlimp::SNAlterTable*)CURR_STMT;
       curr->add_rule($1);
	   //emit("-- ALTER TABLE with Create Table Options.");
	 }
   | opt_alter_tbl_spec_list
     { sqlimp::SNAlterTable* curr=(sqlimp::SNAlterTable*)CURR_STMT;
       curr->add_rule($1);
	   //emit("-- ALTER TABLE +alter_specification done.");
	 }
   ;

opt_alter_tbl_create_table_opts: create_table_opts  { $$=new StmtRule($1); } ;

opt_alter_tbl_spec_list: alter_tbl_spec_list        { $$=$1; } ;

alter_tbl_spec_list: alter_tbl_spec                 { $$=$1; }
   | alter_tbl_spec_list ',' alter_tbl_spec         { $$=$1; $$->merge_rule_items($3, ','); /*delete($3);*/ }
   ;

alter_tbl_spec:
     T_ADD opt_COLUMN col_name data_type column_atts col_position
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_ADD_COLUMN);
		  //StmtRuleItem* innerItem = $$->get_rule();

		  //sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();
		  //nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3));
		  //StmtRule* subRule = new StmtRule(sqlimp::RULE_ALTER_TABLE_ADD_COLUMN, nl);
		  //subRule->add_sub_rule($4);
		  //subRule->add_sub_rule($5);

		  //innerItem->set(subRule);

		  //首先，将$3, $4, $5, $6包装一个一个StmtRule中的三个items
		  sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();// debug_point();
		  nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3));

		  StmtRule* subRule = new StmtRule(sqlimp::RULE_ALTER_TABLE_ADD_COLUMN, nl);
		  subRule->add_sub_rule($4); // $4 is a StmtRule(RULE_COLUMN_DATA_TYPE) pointer
		  if ($5) subRule->add_sub_rule(new StmtRule($5)); //column_atts
		  if ($6) subRule->add_sub_rule($6); //col_position

		  //然后将该StmtRule嵌入到innerItem中。innerItem是$$规则的首个item
		  StmtRuleItem* innerItem = $$->get_rule();
		  innerItem->set(subRule);
		  delete($3);
		}
   | T_ADD opt_COLUMN '(' create_col_list ')'
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_ADD_COLUMN);
		  $$->add_sub_rule($4);
		}
   | T_ADD INDEX_KEY opt_index_name opt_index_type '(' index_column_list ')' opt_index_type
		{ //$$=new StmtRule(RULE_ALTER_TABLE_ADD_INDEX);
		  $$=new StmtRule(NULL, $3, $6, $8,
		  sqlimp::RULE_ALTER_TABLE_ADD_INDEX, sqlimp::IK_INDEX);
		}
   | T_ADD CONSTRAINT_symbol T_PRIMARY T_KEY opt_index_type '(' index_column_list ')' opt_index_type
		{ //$$=new StmtRule(RULE_ALTER_TABLE_ADD_PK);
		  $$=new StmtRule($2, NULL, $7, $9,
		  sqlimp::RULE_ALTER_TABLE_ADD_PK, sqlimp::IK_PK);
		}
   | T_ADD CONSTRAINT_symbol T_UNIQUE INDEX_KEY opt_index_name opt_index_type '(' index_column_list ')' opt_index_type
		{ //$$=new StmtRule(RULE_ALTER_TABLE_ADD_UK);
		  $$=new StmtRule($2, $5, $8, $10,
		  sqlimp::RULE_ALTER_TABLE_ADD_UK, sqlimp::IK_UK);
		}
   | T_ADD FULLTEXT_SPATIAL INDEX_KEY opt_index_name opt_index_type '(' index_column_list ')' opt_index_type
		{ //
		  $$=new StmtRule(NULL, $4, $7, $9,
		  sqlimp::RULE_ALTER_TABLE_ADD_FULLTEXT, $2==1?sqlimp::IK_FULLTEXT:$2==2?sqlimp::IK_SPATIAL:sqlimp::IK_EMPTY);
		}

	// ADD SPATIAL [INDEX|KEY] [index_name] (index_col_name,...) [index_option] ...

   | T_ADD CONSTRAINT_symbol T_FOREIGN T_KEY opt_index_name '(' index_column_list ')' opt_fk_references_definitions
		{ //RULE_ALTER_TABLE_ADD_FK
		  $$=new StmtRule($2, $5, $7, $9);
		}

	// TODO ALGORITHM [=] {DEFAULT|INPLACE|COPY}

   | T_ALTER opt_COLUMN col_name default_def
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_ALTER_COLUMN);
		  StmtRuleItem* innerItem = $$->get_rule();

		  sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();
		  nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3));
		  StmtRule* subRule = new StmtRule(sqlimp::RULE_ALTER_TABLE_ALTER_COLUMN, nl);
		  subRule->add_sub_rule(new StmtRule($4));
		  //printf("default_def: %d, '%s'.\n", $4->get_value1().type, $4->toString().c_str());

		  innerItem->set(subRule);
		}
   | T_CHANGE opt_COLUMN old_col_name new_col_name data_type column_atts col_position
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_CHANGE_COLUMN);

		  //首先，将$3, $4, $5, $6包装一个一个StmtRule中的三个items
		  sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();// debug_point();
		  nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3));
		  nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$4));

		  StmtRule* subRule = new StmtRule(sqlimp::RULE_ALTER_TABLE_CHANGE_COLUMN, nl);
		  subRule->add_sub_rule($5); // $4 is a StmtRule(RULE_COLUMN_DATA_TYPE) pointer
		  if ($6) subRule->add_sub_rule(new StmtRule($6)); //column_atts
		  if ($7) subRule->add_sub_rule($7); //col_position

		  //然后将该StmtRule嵌入到innerItem中。innerItem是$$规则的首个item
		  StmtRuleItem* innerItem = $$->get_rule();
		  innerItem->set(subRule);
		  delete($3);
		  delete($4);

		  emit("-- change column xx");
		}
   | cdo_charset_opt cdo_collate_opt
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DEF_CHARSET, $1, $2); }
   | T_CONVERT T_TO T_CHAR T_SET charset_name cdo_collate_opt
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_CVT_CHARSET,
		                  new sqlimp::RI_opt_name(sqlimp::ONM_CHARACTER_SET,*$5), $6);
		  delete($5); if ($6) delete($6); }
   | T_DISABLE T_KEYS { $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DISABLE_KEYS); }
   | T_ENABLE T_KEYS  { $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_ENABLE_KEYS); }
   | T_DISCARD T_TABLESPACE { $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DISCARD_TS); }
   | T_IMPORT T_TABLESPACE { $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_IMPORT_TS); }
   | T_DROP opt_COLUMN col_name
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DROP_COLUMN, new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3), NULL); emit("-- drop column %s", $3->c_str()); }
   | T_DROP T_PRIMARY T_KEY
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DROP_PK); }
   | T_DROP INDEX_KEY index_name
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DROP_INDEX, new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3), NULL); }
   | T_DROP T_FOREIGN T_KEY index_name
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_DROP_FK, new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$4), NULL); }

	// TODO FORCE
	// TODO LOCK [=] {DEFAULT|NONE|SHARED|EXCLUSIVE}

   | T_MODIFY opt_COLUMN col_name data_type column_atts col_position
		{ //emit("-- modify column xx");
		  $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_MODIFY_COLUMN);

		  //首先，将$3, $4, $5, $6包装一个一个StmtRule中的三个items
		  sqlimp::RI_opt_name_list* nl=new sqlimp::RI_opt_name_list();// debug_point();
		  nl->add(new sqlimp::RI_opt_name(sqlimp::ONM_NAME,*$3));

		  StmtRule* subRule = new StmtRule(sqlimp::RULE_ALTER_TABLE_MODIFY_COLUMN, nl);
		  subRule->add_sub_rule($4); // $4 is a StmtRule(RULE_COLUMN_DATA_TYPE) pointer
		  if ($5) subRule->add_sub_rule(new StmtRule($5)); //column_atts
		  if ($6) subRule->add_sub_rule($6); //col_position

		  //然后将该StmtRule嵌入到innerItem中。innerItem是$$规则的首个item
		  StmtRuleItem* innerItem = $$->get_rule();
		  innerItem->set(subRule);
		  delete($3);

		  emit("-- modify column xx");
		  }
   | T_ORDER T_BY index_column_list
		{ $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_ORDER_BY, $3); }

	// TODO RENAME {INDEX|KEY} old_index_name TO new_index_name

   | T_RENAME TO_AS new_table_name
		{ std::string toas($2==1?"TO":$2==2?"AS":"");
		  $$=new StmtRule(sqlimp::RULE_ALTER_TABLE_RENAME, &toas, $3); delete($3); }

	// TODO {WITHOUT|WITH} VALIDATION

   | T_ADD T_PARTITION '(' partition_items ')' { /* TODO */$$=new StmtRule(); emit("-- ADD PATITION: %s;", $4->toString().c_str()); }
   | T_DROP T_PARTITION NAME { $$=new StmtRule(); emit("-- DROP PARTITION: '%s'.", $3->c_str()); delete($3); }

	// TODO DISCARD PARTITION {partition_names | ALL} TABLESPACE
	// TODO IMPORT PARTITION {partition_names | ALL} TABLESPACE
	// TODO TRUNCATE PARTITION {partition_names | ALL}
    // TODO COALESCE PARTITION number

   | T_REORGANIZE T_PARTITION name_list T_INTO '(' partition_items ')' { $$=new StmtRule(); emit("-- REORGANIZE PARTITION: '%s'.", $3->toString().c_str()); delete($3); }

	// TODO EXCHANGE PARTITION partition_name WITH TABLE tbl_name [{WITH|WITHOUT} VALIDATION]
	// TODO ANALYZE PARTITION {partition_names | ALL}
	// TODO CHECK PARTITION {partition_names | ALL}
	// TODO OPTIMIZE PARTITION {partition_names | ALL}
	// TODO REBUILD PARTITION {partition_names | ALL}
	// TODO REPAIR PARTITION {partition_names | ALL}
	// TODO REMOVE PARTITIONING
	// TODO UPGRADE PARTITIONING
   ;

//col_def: data_type column_atts
//		{ $$=$1;
//		  //new StmtRule(RULE_COLUMN_DATA_TYPE, $1);
//		}
//   ;

col_position: /* nil */	{ $$=new StmtRule(); }
   | T_FIRST			{ $$=new StmtRule(sqlimp::RULE_FIRST); }
   | T_AFTER col_name	{
		$$=new StmtRule(sqlimp::RULE_AFTER, $2, NULL);
		emit("got T_AFTER col_name: %s", $2->c_str());
		delete($2);
		emit("and whole stmt is: %s", $$->toString().c_str());
	 }
   ;

opt_fk_references_definitions:
     T_REFERENCES table_name '(' column_list ')' opt_constraint_fk_refs
     { $$=new StmtRule(*$2, *$4, $6); }
   ;

opt_constraint_fk_refs: { $$=0; }
   | opt_constraint_fk_refs opt_constraint_fk_ref { $$=($1<<16)+$2; }
   ;
opt_constraint_fk_ref: T_ON T_DELETE opt_ref { $$=0x1000+$3; }
   | T_ON T_UPDATE opt_ref { $$=0x2000+$3; }
   ;
opt_ref: T_SET T_NULL							{ $$=1; }
   | T_CASCADE									{ $$=2; }
   | T_RESTRICT									{ $$=3; }
   | T_NO T_ACTION								{ $$=4; }
   ;

opt_index_type: /* nil */						{ $$=0; }
   | index_type									{ $$=$1; }
   ;
index_type: T_USING opt_index_type_algor		{ $$=$2; }
opt_index_type_algor: T_BTREE					{ $$=1; }
   | T_HASH										{ $$=2; }
   ;
index_opts: /* nil */							{ $$=new StmtExpr(); }
   | index_opt									{ $$=$1; }
   | index_opts index_opt						{ $$=$1; $$->list_append($2); }
   ;
index_opt: index_type							{
		if($1==0)
			$$=new StmtExpr();
		else {
			$$=new StmtExpr(EXPR_TOKEN, " USING");
			$$->set2(EXPR_OP_NONE, new StmtExpr(EXPR_TOKEN, std::string($1==1?"BTREE":"HASH")));
		}
	 }
   | T_KEY_BLOCK_SIZE INTNUM					{ $$=new StmtExpr(EXPR_TOKEN, " KEY_BLOCK_SIZE"); $$->set2(EXPR_OP_ASSIGN, new StmtExpr($2)); }
   | T_KEY_BLOCK_SIZE T_COMPARISON INTNUM		{ $$=new StmtExpr(EXPR_TOKEN, " KEY_BLOCK_SIZE"); $$->set2(EXPR_OP_ASSIGN, new StmtExpr($3)); }
   | T_WITH T_PARSER NAME						{ $$=new StmtExpr(EXPR_TOKEN, " WITH PARSER"); $$->set2(EXPR_OP_NONE, new StmtExpr(EXPR_NAME, *$3)); delete $3;}
   | T_COMMENT STRING							{ $$=new StmtExpr(EXPR_TOKEN, " COMMENT"); $$->set2(EXPR_OP_NONE, new StmtExpr(EXPR_STRING, *$2)); delete $2; }
   ;

default_def: T_SET T_DEFAULT default_literal	{ $$=new StmtExpr($3); $$->set_prefix("SET DEFAULT"); }
   | T_DROP T_DEFAULT 							{ $$=new StmtExpr(); $$->set_prefix("DROP DEFAULT"); }
   ;

opt_COLUMN:  /* nil */ { $$=-1; }       | T_COLUMN   { $$=1; } ;
opt_DEFAULT: /* nil */ { $$=-1; }       | T_DEFAULT  { $$=1; } ;
opt_IGNORE:  /* nil */ { $$=-1; }       | T_IGNORE   { $$=1; } ;
CONSTRAINT_symbol: /* nil */ { $$=NULL;/*CONSTRAINT_symbol*/} | T_CONSTRAINT { $$=new std::string(); } | T_CONSTRAINT symbol { $$=$2; };
/*INDEX_OR_KEY: T_INDEX  { $$=1;}		| T_KEY      { $$=2;} ;*/
INDEX_KEY: T_KEY;
FULLTEXT_SPATIAL: T_FULLTEXT {$$=1;}	| T_SPATIAL  { $$=2; } ;
TO_AS: T_TO { $$=1; }					| T_AS       { $$=2; } ;

opt_index_name: /* nil */ { $$=NULL; }	| index_name { $$=$1; } ;

db_name: NAME				{ $$=$1; /* emit("<DATABASE> %s", $1->c_str()); */ } ;

col_name: NAME				{ $$=$1; /* emit("<COLUMN> %s", $1->c_str()); */ }
   | T_VALUES				{ $$=new std::string("values"); }
   | T_VALUE				{ $$=new std::string("value"); }
   | T_TRAILING				{ $$=new std::string("trailing"); }
   | T_RIGHT				{ $$=new std::string("right"); }
   | T_LEFT					{ $$=new std::string("left"); }
   | T_DATA					{ $$=new std::string("data"); }
   | STRING					{ $$=$1; lerror(&yyloc, "string '%s' found where name required", $1); }
   ;
old_col_name: NAME			{ $$=$1; /* emit("<OLD COLUMN> %s", $1->c_str()); */ };
new_col_name: NAME			{ $$=$1; /* emit("<NEW COLUMN> %s", $1->c_str()); */ };
table_name: NAME			{ $$=$1; };
new_table_name: NAME		{ $$=$1; /* emit("<NEW TABLE> %s", $1->c_str()); */ };
index_name: NAME			{ $$=$1; };
symbol: NAME				{ $$=$1; };




stmt: alter_database_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "ALTER DATABASE STMT -----------------------------------------\n");
	  }
   ;
alter_database_stmt:
     T_ALTER database_or_schema db_name opt_alter_db_sub_clauses
		{ sqlimp::SNAlterDB* curr=new sqlimp::SNAlterDB($2); CURR_STMT=curr;
		  emit("-- ALTER DATABASE %s", $3->c_str());
		  curr->dbName(*$3);
		  curr->add_rule($4);
		  $$=CURR_STMT; }
   ;
database_or_schema:
     T_DATABASE	{ $$=1; }
   | T_SCHEMA	{ $$=2; }
   ;
opt_alter_db_sub_clauses:
     opt_DEFAULT T_CHAR T_SET assign_charset_name
     { $$=new StmtRule(sqlimp::RULE_ALTER_DB_DEF_CHARSET, $1==1?"DEFAULT":"");
       $$->add_string(*$4);
     }
   | opt_DEFAULT T_COLLATE assign_collation_name
     { $$=new StmtRule(sqlimp::RULE_ALTER_DB_DEF_COLLATE, $1==1?"DEFAULT":"");
       $$->add_string(*$3);
     }
   | T_UPGRADE T_DATA T_DIRECTORY_NAME
     { $$=new StmtRule(sqlimp::RULE_ALTER_DB_UPGRADE);
     }
   ;




stmt: create_index_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "START/BEGIN TRANSACTION STMT -----------------------------------------\n");
	  }
   ;
create_index_stmt: T_CREATE opt_create_index INDEX_KEY NAME
     opt_index_type T_ON NAME opt_index_column_list
     index_opts opt_index_algor_opt opt_index_lock_opt
   { $$=new sqlimp::SNCreateIndex($2, $4, $7, $5, $8, $9, $10, $11); }
   ;
opt_index_column_list:/* nil */	{ $$=NULL; }
   | '(' index_column_list ')'	{ $$=$2; }
opt_create_index: /* nil */	{ $$=0; }
   | T_UNIQUE				{ $$=1; }
   | T_FULLTEXT				{ $$=2; }
   | T_SPATIAL				{ $$=3; }
   ;
opt_index_algor_opt: /* nil */						{ $$=0; }
   | index_algor_opt								{ $$=$1; }
   ;
opt_index_lock_opt: /* nil */						{ $$=0; }
   | index_lock_opt									{ $$=$1; }
   ;
index_algor_opt: T_ALGORITHM index_algor_opt_val	{ $$=$2; }
   ;
index_algor_opt_val: index_algor_opt_val_i			{ $$=$1; }
   | T_COMPARISON index_algor_opt_val_i				{ $$=$2; }
   ;
index_algor_opt_val_i: T_DEFAULT					{ $$=1; }
   | T_INPLACE										{ $$=2; }
   | T_COPY											{ $$=3; }
   ;
index_lock_opt: T_LOCK index_lock_opt_i				{ $$=$2; }
   | T_LOCK T_COMPARISON index_lock_opt_i			{ $$=$3; }
   ;
index_lock_opt_i: T_DEFAULT							{ $$=1; }
   | T_NONE											{ $$=2; }
   | T_SHARED										{ $$=3; }
   | T_EXCLUSIVE									{ $$=4; }
   ;





stmt: drop_index_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "START/BEGIN TRANSACTION STMT -----------------------------------------\n");
	  }
   ;
drop_index_stmt: T_DROP INDEX_KEY NAME T_ON NAME drop_index_opts
   { $$=new sqlimp::SNDropIndex($3,$5,$6); }
   ;
drop_index_opts: /* nil */							{ $$=new StmtExpr(); }
   | drop_index_opt									{ $$=new StmtExpr($1); }
   | drop_index_opts drop_index_opt					{ $$=$1; $1->list_append(new StmtExpr($2)); }
   ;
drop_index_opt: index_algor_opt						{ $$=new StmtExpr(EXPR_TOKEN, " ALGORITHM"); $$->set2(EXPR_OP_ASSIGN, new StmtExpr(EXPR_TOKEN, std::string($1==1?"DEFAULT":$1==2?"INPLACE":"COPY"))); }
   | index_lock_opt									{ $$=new StmtExpr(EXPR_TOKEN, " LOCK"); $$->set2(EXPR_OP_ASSIGN, new StmtExpr(EXPR_TOKEN, std::string($1==1?"DEFAULT":$1==2?"NONE":$1==3?"SHARED":"EXCLUSIVE"))); }
   ;





stmt: start_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "START/BEGIN TRANSACTION STMT -----------------------------------------\n");
	  }
   ;
start_stmt: T_START T_TRANSACTION { $$=new sqlimp::SNBeginTrans(); }
   | T_BEGIN T_TRANSACTION { $$=new sqlimp::SNBeginTrans(); }
   | T_BEGIN { $$=new sqlimp::SNBeginTrans(); }
   ;


stmt: commit_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "COMMIT STMT -----------------------------------------\n");
	  }
   ;
commit_stmt: T_COMMIT { $$=new sqlimp::SNCommitTrans(); };


stmt: rollback_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "ROLLBACK STMT -----------------------------------------\n");
	  }
   ;
rollback_stmt: T_ROLLBACK { $$=new sqlimp::SNRollbackTrans(); };


stmt: use_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "USE STMT -----------------------------------------\n");
	  }
   ;
use_stmt: T_USE { /*emit(&yyloc,"'USE'");*/ } NAME
	  { $$=new sqlimp::SNUse(*$3); /*emit("USE `%s`", $3->c_str());*/ delete($3); } ;
	/* use_stmt: T_USE NAME { emit("db name = %s", $2); } */

stmt: lock_tables_stmt { POST_STMT($1,$$);
	  //emit(&yyloc, "LOCK TABLES STMT -----------------------------------------\n");
	  }
   ;
lock_tables_stmt: T_LOCK T_TABLES NAME opt_lock_tables
	  { $$=new sqlimp::SNLockTables(*$3, $4);
	    //emit("LOCK TABLES `%s` %s", $3->c_str(), $4==0?"<>":"<WRITE>");
	    delete($3); } ;

opt_lock_tables: /* nil */ {$$=0;}
   | T_WRITE {$$=1;}
   ;

stmt: unlock_tables_stmt { POST_STMT($1,$$);
	  //emit("UNLOCK TABLES");
	  }
   ;
unlock_tables_stmt: T_UNLOCK T_TABLES { $$=new sqlimp::SNUnlockTables(); } ;




   /**** set user variables ****/

stmt: set_stmt	{ POST_STMT($1,$$);
				 //emit(&yyloc, "END OF SET STMT -----------------------------------------\n");
				}
   ;

set_stmt: 	{ CURR_STMT=new sqlimp::SNSet(); } T_SET set_list
			{ //emit(">> CURR_STMT=new SNSet();");
			  $$=CURR_STMT;
			  $$->add_expr($3);
			}
   ;

set_list: set_expr            { $$=$1; }
   | set_list ',' set_expr    {
		$$=new StmtExpr($1);
		emit("-- set_list: %s | ref: %s", $$->toString().c_str(), $3->toString().c_str());
		$$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $3);
		emit("-- \t-> %s", $$->toString().c_str());
		////emit("set_list: %s | ref: %s", $$->toString().c_str(), $3->toString().c_str());
	  }
   ;

set_expr:
	  USERVAR T_COMPARISON expr {
	    if ($2 != sqlimp::COMPARISON_ASSIGN) { lerror(&yyloc,"bad set to @%s", $1->c_str()); YYERROR; }
		$$ = new StmtExpr();//emit(">> SET @%s=...",$1->c_str());
		$$->set_lfs(sqlimp::LFS_USERVAR, *$1);//emit(">> SET @%s=...",$1->c_str());
		$$->set($3);
		//CURR_STMT->build_sql("SET @%s=%s", $1->c_str(),$3->toString().c_str());
		//emit("//SET @%s=%s", $1->c_str(), $3->toString().c_str());
		delete($1); }
    | USERVAR T_ASSIGN expr {
		$$ = new StmtExpr();
		$$->set_lfs(sqlimp::LFS_USERVAR_A, *$1);
		$$->set($3);
		//CURR_STMT->build_sql("SET %s:=%s", $1->c_str(), $3->toString().c_str());
		//emit("SET %s", $1->c_str());
		delete($1); }
    | NAME T_COMPARISON expr {
		if ($2 != sqlimp::COMPARISON_ASSIGN) { lerror(&yyloc,"bad set to @%s", $1->c_str()); YYERROR; }
		$$ = new StmtExpr();
		$$->set_lfs(sqlimp::LFS_NAME, *$1);
		$$->set($3); //debug_point();
		//CURR_STMT->build_sql("//SET %s=%s", $1->c_str(), $3->toString().c_str());
		//emit("//SET %s=%s", $1->c_str(), $3->toString().c_str());
		delete($1); }
    | NAME expr {
		$$ = new StmtExpr();
		$$->set_lfs(sqlimp::LFS_NAME, *$1);
		$$->set($2);
		//CURR_STMT->build_sql("SET %s %s", $1->c_str(), $2->toString().c_str());
		//emit("SET %s", $1->c_str());
		delete($1); }
    | T_CHAR T_SET expr {
		$$ = new StmtExpr();
		$$->set_lfs(sqlimp::LFS_CHARSET, $3->get_value1().toString());
		//CURR_STMT->build_sql("SET %s", $$->toString().c_str());
    }
    ;

expr: orexpr                       { $$=$1; }
   ;
orexpr: xorexpr                    { $$=$1; }
   | orexpr T_OR xorexpr           { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_OR, $3); }
   ;
xorexpr: andexpr                   { $$=$1; }
   | xorexpr T_XOR andexpr         { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_XOR, $3); }
   ;
andexpr: notexpr                   { $$=$1; }
   | andexpr T_ANDOP notexpr       { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_AND, $3); }
   ;
notexpr: clause_expr               { $$=$1; }
   | T_NOT clause_expr             { $$=$2; $$->not_prefix(); }
   | '!' clause_expr               { $$=$2; $$->not_prefix(); }
   ;
clause_expr: between_clause_expr   { $$=$1; }
   | case_clause_expr              { $$=$1; }
   | comparison_expr               { $$=$1; }
   ;
between_clause_expr: comparison_expr T_BETWEEN comparison_expr T_AND comparison_expr %prec T_BETWEEN
		{ $$=new StmtExpr($1); $$->between_and_op($3,$5); }
   ;
case_clause_expr: T_CASE comparison_expr case_list T_END            { $$=new StmtExpr(EXPR_CLAUSE_CASE, $2);$$->set2(EXPR_OP_CASE_LIST, $3); emit("CASEVAL %d 0", $3); }
   |  T_CASE comparison_expr case_list T_ELSE comparison_expr T_END { $$=new StmtExpr(EXPR_CLAUSE_CASE, $2);$$->set2(EXPR_OP_CASE_LIST, $3, $5); emit("CASEVAL %d 1", $3); }
   |  T_CASE case_list T_END                                        { $$=new StmtExpr(EXPR_CLAUSE_CASE);$$->set2(EXPR_OP_CASE_LIST, $2); emit("CASE %d 0", $2); }
   |  T_CASE case_list T_ELSE comparison_expr T_END                 { $$=new StmtExpr(EXPR_CLAUSE_CASE);$$->set2(EXPR_OP_CASE_LIST, $2, $4); emit("CASE %d 1", $2); }
   ;
case_list: T_WHEN comparison_expr T_THEN comparison_expr            { $$ = new StmtExpr(EXPR_CLAUSE_WHEN_THEN, $2);$$->set2(EXPR_OP_THEN,$4); }
   | case_list T_WHEN comparison_expr T_THEN comparison_expr        { $$=$1; StmtExpr* arg2=new StmtExpr(EXPR_CLAUSE_WHEN_THEN, $3);$$->set2(EXPR_OP_THEN,$5); $$->list_append(arg2); }
   ;
comparison_expr: bitor_expr                                  { $$=$1; }
   | comparison_expr T_COMPARISON bitor_expr                 { $$=$1; $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),$3); /*emit("CMP %d", $2);*/ }
   | comparison_expr T_COMPARISON '(' select_stmt ')'        { $$=$1; $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),$4); /*emit("CMPSELECT %d", $2);*/ delete($4); }
   | comparison_expr T_COMPARISON T_ANY '(' select_stmt ')'  { $$=$1; ((SNSelect*)$5)->prefix=SNSelect::PREFIX_ANY; $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),$5); /*emit("CMPANYSELECT %d", $2);*/ delete($5); }
   | comparison_expr T_COMPARISON T_SOME '(' select_stmt ')' { $$=$1; ((SNSelect*)$5)->prefix=SNSelect::PREFIX_SOME; $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),$5); /*emit("CMPANYSELECT %d", $2);*/ delete($5); }
   | comparison_expr T_COMPARISON T_ALL '(' select_stmt ')'  { $$=$1; ((SNSelect*)$5)->prefix=SNSelect::PREFIX_ALL; $$->set2((EXPR_OPERATOR)((int)EXPR_OP_COMPARISON+$2),$5); /*emit("CMPALLSELECT %d", $2);*/ delete($5); }
   | is_clause_expr                                          { $$=$1; }
   | like_clause_expr                                        { $$=$1; }
   | regexp_clause_expr                                      { $$=$1; }
   | in_clause_expr                                          { $$=$1; }
   ;
is_clause_expr: bitor_expr T_IS T_NULL              { $$=new StmtExpr($1); $$->isnull_op(); }
   | bitor_expr T_IS T_NOT T_NULL                   { $$=new StmtExpr($1); $$->isnull_op(true); }
   | bitor_expr T_IS BOOL                           { $$=new StmtExpr($1); $$->isbool_op($3); }
   | bitor_expr T_IS T_NOT BOOL                     { $$=new StmtExpr($1); $$->isbool_op($4); }
   ;
like_clause_expr: bitor_expr T_LIKE bitor_expr      { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_LIKE, $3); }
   | bitor_expr T_NOT T_LIKE bitor_expr             { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_NOT_LIKE, $4); }
   ;
regexp_clause_expr: bitor_expr T_REGEXP bitor_expr  { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_REGEXP, $3); }
   | bitor_expr T_NOT T_REGEXP bitor_expr           { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_NOT_REGEXP, $4); }
   ;
in_clause_expr: bitor_expr T_IN '(' val_list ')'    {
     StmtExpr* inner=new StmtExpr($4);
     StmtExpr* p=new StmtExpr(sqlimp::EXPR_PARENTHESIS, inner);
	 $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_IN, p); }
   | bitor_expr T_NOT T_IN '(' val_list ')'         {
     StmtExpr* inner=new StmtExpr($5);
     StmtExpr* p=new StmtExpr(sqlimp::EXPR_PARENTHESIS, inner);
     $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_NOT_IN, p); }
   | bitor_expr T_IN '(' select_stmt ')'            {
     StmtExpr* inner=new StmtExpr($4);
     StmtExpr* p=new StmtExpr(sqlimp::EXPR_PARENTHESIS, inner);
     $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_IN, p); }
   | bitor_expr T_NOT T_IN '(' select_stmt ')'      {
     StmtExpr* inner=new StmtExpr($5);
     StmtExpr* p=new StmtExpr(sqlimp::EXPR_PARENTHESIS, inner);
     $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_NOT_IN, p); }
   | T_EXISTS '(' select_stmt ')'                   { //debug_point();
     StmtExpr* inner=new StmtExpr($3);
     StmtExpr* p=new StmtExpr(sqlimp::EXPR_PARENTHESIS, inner);
     //emit("1. %s\n2. %s", inner->toString().c_str(), p->toString().c_str());
     $$=new StmtExpr(  ); $$->set2($1==0?sqlimp::EXPR_OP_EXISTS:sqlimp::EXPR_OP_NOT_EXISTS, p);
     //emit("3. %s", $$->toString().c_str());
     //emit("EXISTS"); if($1) emit("NOT");//delete($3);
     }
   ;
bitor_expr: bitxor_expr                  { $$=$1; }
   | bitor_expr '|' bitxor_expr          { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_BITOR, $3); }
   ;
bitxor_expr: bitand_expr                 { $$=$1; }
   | bitxor_expr '^' bitand_expr         { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_BITXOR, $3); }
   ;
bitand_expr: shift_expr                  { $$=$1; }
   | bitand_expr '&' shift_expr          { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_BITAND, $3); }
   ;
shift_expr: addexpr                      { $$=$1; }
   | shift_expr T_SHIFT addexpr          { $$=new StmtExpr($1); $$->set2($2==1?sqlimp::EXPR_OP_SHIFT_LEFT:sqlimp::EXPR_OP_SHIFT_RIGHT, $3); }
   ;
addexpr: mulexpr                         { $$=$1; }
   | addexpr '+' mulexpr                 { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_ADD, $3); }
   | addexpr '-' mulexpr                 { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_SUB, $3); }
   ;
mulexpr: xxor_expr                       { $$=$1; }
   | mulexpr '*' xxor_expr               { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_MUL, $3); }
   | mulexpr '/' xxor_expr               { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_DIV, $3); }
   | mulexpr T_DIV xxor_expr             { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_DIV, $3); }
   | mulexpr '%' xxor_expr               { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_MOD, $3); }
   | mulexpr T_MOD xxor_expr             { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_MOD, $3); }
   ;
xxor_expr: unaryexpr                     { $$=$1; }
   | xxor_expr '^' unaryexpr             { $$=new StmtExpr($1); $$->set2(sqlimp::EXPR_OP_XOR, $3); }
   ;
unaryexpr: unary2expr                    { $$=$1; }
   | '+' unary2expr                      { $$=$2; }
   | '-' unary2expr                      { $$=$2; $$->neg_prefix(); }
   | '~' unary2expr                      { $$=$2; $$->bitnot_prefix(); }
   ;
unary2expr: '!' binary_collate_expr      { $$=$2; $$->not_prefix(); }
   | binary_collate_expr                 { $$=$1; }
   ;
binary_collate_expr: binary_expr         { $$=$1; }
   | collate_expr                        { $$=$1; }
   | interval_expr                       { $$=$1; }
   ;
binary_expr: T_BINARY interval_expr      { $$=$2; $$->binary_prefix(); }
   ;
collate_expr: T_COLLATE interval_expr    { $$=$2; $$->collate_prefix(); }
   ;
interval_expr: atomexpr                  { $$=$1; }
   | T_INTERVAL expr T_DAY_HOUR          { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_DAY_HOUR); }
   | T_INTERVAL expr T_DAY_MICROSECOND   { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_DAY_MS); }
   | T_INTERVAL expr T_DAY_MINUTE        { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_DAY_MIN); }
   | T_INTERVAL expr T_DAY_SECOND        { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_DAY_SEC); }
   | T_INTERVAL expr T_YEAR_MONTH        { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_YEAR_MONTH); }
   | T_INTERVAL expr T_YEAR              { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_YEAR); }
   | T_INTERVAL expr T_HOUR_MICROSECOND  { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_HOUR_MS); }
   | T_INTERVAL expr T_HOUR_MINUTE       { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_HOUR_MIN); }
   | T_INTERVAL expr T_HOUR_SECOND       { $$=new StmtExpr(sqlimp::EXPR_CLAUSE_INTERVAL, $2);$$->set2(sqlimp::EXPR_OP_INTERVAL_HOUR_SEC); }
   ;
atomexpr: expr_constant      			 { $$=$1; }
   | expr_variable           			 { $$=$1; }
   | '(' expr ')'            			 { $$=new StmtExpr(sqlimp::EXPR_PARENTHESIS, $2); }
   | func_name '(' opt_val_list ')'  	 { $$=new StmtExpr();
     $$->set_func_name($1);
     $$->add_func_arg($3);
     //emit("FUNC CALL: %s(%s)", $1->c_str(), $3->toString().c_str());
     //emit("FUNC CALL: **='%s'.", $$->toString().c_str());
   }
   ;
expr_variable: USERVAR       			 { $$=new StmtExpr(sqlimp::EXPR_USERVAR,*$1); delete($1); }
   | NAME                    			 { $$=new StmtExpr(sqlimp::EXPR_NAME,*$1); delete($1); }
   | NAME '.' NAME           			 { $$=new StmtExpr(sqlimp::EXPR_NAME,*$1,*$3); delete($1);delete($3); }
   | NAME '.' '*'           			 { $$=new StmtExpr(sqlimp::EXPR_NAME,*$1,"*"); delete($1); }
   ;
expr_constant: STRING        			 { $$=new StmtExpr(*$1); delete($1); }
   | INTNUM                  			 { $$=new StmtExpr($1); }
   | APPROXNUM               			 { $$=new StmtExpr($1); }
   | BOOL                    			 { $$=new StmtExpr($1); }
   | T_NULL                  			 { $$=new StmtExpr(  ); }
   | T_CURRENT_TIMESTAMP     			 { $$=new StmtExpr(sqlimp::EXPR_CURRENT_TIMESTAMP,0); }
   | T_CURRENT_DATE          			 { $$=new StmtExpr(sqlimp::EXPR_CURRENT_DATE,0); }
   | T_CURRENT_TIME          			 { $$=new StmtExpr(sqlimp::EXPR_CURRENT_TIME,0); }
   ;

func_name: T_YEAR			 			 { $$=new std::string("YEAR"); }
   | T_LEFT					 			 { $$=new std::string("LEFT"); }
   | T_RIGHT				 			 { $$=new std::string("RIGHT"); }
   | TF_COUNT			 	 			 { $$=new std::string("COUNT"); }
   | TF_TRIM			 	 			 { $$=new std::string("TRIM"); }
   | T_DATE					 			 { $$=new std::string("DATE"); }
   | TF_DATE_ADD			 			 { $$=new std::string("DATE_ADD"); }
   | TF_DATE_SUB			 			 { $$=new std::string("DATE_SUB"); }
   | TF_SUBSTRING			 			 { $$=new std::string("SUBSTRING"); }
   | TF_EXP					 			 { $$=new std::string("EXP"); }
   //| NAME			 		 			 { $$=$1; }
   ;

val_list: expr          				 { $$ = $1;
		//printf("-- *1 val_list: expr: %s\n", $$->toString().c_str());
		}
   | val_list ',' expr  				 { $$ = $1;
		$$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, $3);
		//printf("-- ** val_list: expr: %s\n", $$->toString().c_str());
		}
   | T_MAXVALUE	{ $$=new sqlimp::StmtExpr("MAXVALUE"); }
   | val_list ',' T_MAXVALUE			 { $$ = $1;
			$$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, new sqlimp::StmtExpr("MAXVALUE"));
		}
   | '(' val_list ')' {
		// recursive value list. such as:
		$$ = new StmtExpr(sqlimp::EXPR_PARENTHESIS, $2);
		}
   | val_list ',' '(' val_list ')' {
		// recursive value list. such as:
		// PARTITION p1 VALUES IN( (0,1), (0,2), (0,3), (1,1), (1,2) ),
		$$ = $1;
		$$->list_append(sqlimp::EXPR_OP_COMMA_LEAD, new StmtExpr(sqlimp::EXPR_PARENTHESIS, $4));
		}
   ;

//sub_val_list: expr
//   | sub_val_list ',' expr
//   | T_MAXVALUE
//   | sub_val_list ',' T_MAXVALUE
//   | sub_val_list ',' '(' sub_val_list ')'
//   ;

opt_val_list: /* nil */ { $$ = new StmtExpr();/* make an empty expression object */ }
   | val_list           { $$ = $1; }
   ;

  /* functions with special syntax */

expr
   : TF_COUNT '(' '*' ')' {
    	$$=new StmtExpr(); $$->set_func_name("COUNT"); emit("COUNTALL"); }
   | TF_COUNT '(' expr ')' {
    	$$=new StmtExpr(); $$->set_func_name("COUNT"); $$->add_func_arg($3); emit(" CALL 1 COUNT"); }
   | TF_SUBSTRING '(' val_list ')' {
    	$$=new StmtExpr(); $$->set_func_name("SUBSTRING"); $$->add_func_arg($3); emit("CALL %d SUBSTR", $3);}
   | TF_SUBSTRING '(' expr T_FROM expr ')' {
    	$$=new StmtExpr(); $$->set_func_name("SUBSTRING");
    	StmtExpr* params=new StmtExpr($3);
    	params->list_append($5);
    	$$->add_func_arg(params);
    	emit("CALL 2 SUBSTR");
    	}
   | TF_SUBSTRING '(' expr T_FROM expr T_FOR expr ')' {
    	$$=new StmtExpr(); $$->set_func_name("SUBSTRING");
    	StmtExpr* params=new StmtExpr($3);
    	 params->list_append($5);
    	 params->list_append($7);
    	  $$->add_func_arg(params);
    	emit("CALL 3 SUBSTR");
    	}
   | TF_TRIM '(' val_list ')' {
    	$$=new StmtExpr(); $$->set_func_name("TRIM"); $$->add_func_arg($3);
    	emit("CALL %d TRIM", $3);
    	}
   | TF_TRIM '(' trim_ltb expr T_FROM val_list ')' {
    	$$=new StmtExpr(); $$->set_func_name("TRIM");
    	StmtExpr* params=new StmtExpr($4); params->list_append($6); $$->add_func_arg(params);
    	emit("CALL 3 TRIM");
    	}
   | TF_EXP '(' val_list ')' {
   		$$=new StmtExpr(); $$->set_func_name("EXP"); $$->add_func_arg($3);
   		emit("CALL %d EXP", $3);
   		}
   ;

trim_ltb: T_LEADING { $$=1; emit("INT 1"); }
   | T_TRAILING     { $$=2; emit("INT 2"); }
   | T_BOTH         { $$=3; emit("INT 3"); }
   ;

name_list: NAME 		{ $$=new RI_string_list(); $$->add($1); }
   | name_list ',' NAME	{ $$=$1; $$->add($3); }
   ;



%% /*** Additional Code ***/

void sqlimp::Parser::error(const Parser::location_type& l,
			    const std::string& m)
{
    driver.error(l, m);
}
