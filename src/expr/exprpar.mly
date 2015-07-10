%{
(* Parser specification for the simple expression language. *)

open Absyn
%}

%token <int> CSTINT
%token <string> NAME
%token PLUS MINUS TIMES EQ
%token END IN LET
%token LPAR RPAR
%token EOF

%left MINUS PLUS
%left TIMES

%start main
%type <Absyn.expr> main

%%

main: e = expr EOF { e } ;

expr:
  | s = NAME                                        { Var s }
  | i = CSTINT                                      { CstI i }
  | MINUS i = CSTINT                                { CstI (- i) }
  | LPAR e = expr RPAR                              { e }
  | LET lhs = NAME EQ rhs = expr IN body = expr END { Let(lhs, rhs, body) }
  | lhs = expr TIMES rhs = expr                     { Prim("*", lhs, rhs) }
  | lhs = expr PLUS rhs = expr                      { Prim("+", lhs, rhs) }
  | lhs = expr MINUS rhs = expr                     { Prim("-", lhs, rhs) }
  ;
