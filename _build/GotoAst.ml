(* Syntaxe abstraite "goto" *)
(* Cette syntaxe abstraite possède les mêmes expressions que la syntaxe
   abstraite non typée, mais abandonne les structures de contrôle. *)
module Symb_Tbl = UntypedAst.Symb_Tbl

type expression      = UntypedAst.expression
type location        = UntypedAst.location
type identifier_info = UntypedAst.identifier_info
type literal         = UntypedAst.literal
type binop           = UntypedAst.binop
type call            = UntypedAst.call
    
type label = string

(* type handler = { mutable previous : handler option; j : label } *)
(* type stack = handler list *)
(* and gest = { mutable first : handler option; l : stack } *)
    
type block = instruction list
and instruction =
  | Set      of location * expression (* Affectation       *)
  | Print    of expression            (* Affichage         *)
  | Label    of label                 (* Point de saut     *)
  | Goto     of label                 (* Saut              *)
  | CondGoto of expression * label    (* Saut conditionnel *)
  | Comment  of string                (* Commentaire       *)
  | ProcCall of call                  (* Appel             *)
  | Throw
  | NewHandler of label              (* Nouveau gestionnaire d'exception *)
  | RmHandler                         (* Abandon du dernier gestionnaire d'exception *)
      
type program = (string * function_info) list
and function_info = {
  locals:  identifier_info Symb_Tbl.t;
  formals: int;
  code:    block;
}

open Printf
      
let rec print_kind : identifier_info -> string = function
  | Local -> "Local"
  | Formal(n) -> sprintf "Formal(%d)" n
  | Return -> "Return"
let print_identifier_info i = print_kind i

let print_symb_tbl tbl =
  Symb_Tbl.fold (fun v i s ->
    (sprintf "  var %s %s;\n" (print_identifier_info i) v) ^ s
  ) tbl ""

let print_literal : literal -> string = function
  | Int i -> sprintf "%d" i
  | Bool b -> if b then "true" else "false"
let print_binop : binop -> string = function
  | Add  -> "+"
  | Mult -> "*"
  | Sub  -> "-"
  | Eq   -> "=="
  | Neq  -> "!="
  | Lt   -> "<"
  | Le   -> "<="
  | And  -> "&&"
  | Or   -> "||"
let rec print_expression : expression -> string= function
  | Literal lit -> print_literal lit
  | Location id -> print_location id
  | Binop(op, e1, e2) -> sprintf "( %s %s %s )" (print_expression e1) (print_binop op) (print_expression e2)
  | FunCall(c) -> print_call c
  | NewArray(e) -> sprintf "[%s]" (print_expression e) 
and print_location : location -> string = function
  | Identifier x -> x
  | ArrayAccess(aa) -> print_a_access aa
and print_a_access (e1, e2) = sprintf "%s[%s]" (print_expression e1) (print_expression e2)
and print_call (id, args) =
  sprintf "%s(%s)" id (print_args args)
and print_args = function
  | []      -> ""
  | [e]     -> print_expression e
  | e::args -> sprintf "%s, %s" (print_expression e) (print_args args)

let offset o = String.make (2*o) ' '
let rec print_block o = function
  | [] -> ""
  | i::b -> (offset o) ^ (print_instruction o i) ^ ";\n" ^ (print_block o b)
and print_instruction o = function
  | Set(id, e) -> sprintf "%s := %s" (print_location id) (print_expression e)
  | Label(l) -> sprintf "%s" l
  | Goto(l) -> sprintf "%s" l
  | CondGoto(e, l) -> sprintf "%s %s" (print_expression e) l
  | Comment(s) -> sprintf "%s" s
  | Print(e) -> sprintf "print(%s)" (print_expression e)
  | ProcCall(c) -> print_call c
  | Throw -> sprintf "throw"
  | NewHandler(s) -> sprintf "New handler %s" s
  | RmHandler -> sprintf "Delete current handler"
    
let print_function f_id f_info =
  sprintf "%s(%s) (\n%s%s)\n"
    f_id (sprintf "%d paramètres" f_info.formals)
    (print_symb_tbl f_info.locals) (print_block 1 f_info.code)
  
let print_program out prog =
  fprintf out "Version Goto\n\n";
  List.iter (fun (f_id, f_info) ->
    fprintf out "%s\n\n" (print_function f_id f_info)
  ) prog
