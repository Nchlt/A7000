
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TRY
  | THROW
  | THEN
  | STRUCT
  | STAR
  | SET
  | SEMI
  | PRINT
  | POINT
  | PLUS
  | OR
  | OB
  | NEW
  | NEQ
  | MINUS
  | LT
  | LE
  | INT
  | INCR
  | IF
  | IDENT of (string)
  | FOR
  | EQUAL
  | EOF
  | END
  | ELSE
  | DECR
  | CONST_INT of (int)
  | CONST_BOOL of (bool)
  | COMMA
  | CB
  | CATCH
  | BOOL
  | BEGIN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.program)
