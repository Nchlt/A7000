{

  open Lexing
  open SourceParser

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "true",     CONST_BOOL(true);
	"false",    CONST_BOOL(false);
	"while",    WHILE;
	"if",       IF;
	"then",     THEN;
	"else",     ELSE;
	"for",      FOR;
	"integer",  INT;
	"boolean",  BOOL;
	"print",    PRINT;
	"var",      VAR;
	"throw",    THROW;
	"try",      TRY;
	"catch",    CATCH;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)

  exception SyntaxError of string
	
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

rule token = parse
  | '\n'
      { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*"
      { comment lexbuf; token lexbuf }
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | "("
      { BEGIN }
  | ")"
      { END }
  | "["
      { OB }
  | "]"
      { CB }
  | ","
      { COMMA }
  | ";"
      { SEMI }
  | ":="
      { SET }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "=="
      { EQUAL }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "++"
      { INCR }
  | "--"
      { DECR }
  | _
      { raise ( SyntaxError ("Unknown character : " ^ (lexeme lexbuf))) }
  | eof
      { EOF }

and comment = parse
  | ['\n']
      { new_line lexbuf; comment lexbuf }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | "*)"
      { () }
  | _
      { comment lexbuf }
  | eof
      { failwith "Unterminated comment" }
