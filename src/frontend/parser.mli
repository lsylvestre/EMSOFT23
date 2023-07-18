
(* The type of tokens. *)

type token = 
  | WITH
  | VAR
  | TVAR_IDENT of (string)
  | TIMES
  | THEN
  | STRING_LIT of (string)
  | STATIC
  | SHARP_PIPE_LBRACKET
  | SEMI_SEMI
  | SEMI
  | RPAREN
  | RIGHT_ARROW
  | REGISTER
  | REC
  | RBRACKET
  | PLUS
  | PIPE_RBRACKET
  | PIPE_PIPE
  | PIPE
  | OR
  | NOT
  | NODE
  | NEQ
  | MOD
  | MINUS
  | MATCH
  | LT
  | LPAREN
  | LET
  | LEFT_ARROW
  | LE
  | LBRACKET
  | LAST
  | INT_LIT of (int)
  | IN
  | IMPLY
  | IF
  | IDENT of (string)
  | HAT
  | GT
  | GE
  | FUN
  | FIX
  | EXIT_REPL
  | EXEC
  | EQ_EQ
  | EQ
  | EOF
  | END
  | ELSE
  | DOT_LENGTH
  | DOT
  | DIV
  | DEFAULT
  | COMMA
  | COL
  | BUFFER_MAKE
  | BOOL_LIT of (bool)
  | AND
  | AMP_AMP
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val pi: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Ast.x * Ast.static) list * ((Ast.p * Ast.e) * Prelude.loc) list)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.e)

val decl_opt: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.p * Ast.e) * Prelude.loc) option)
