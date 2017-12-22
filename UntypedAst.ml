(* Syntaxe abstraite non typée *)
(* Cette version est obtenu en retirant tous les indications de typage *)
(* Pour l'instant, les types [expression], [instruction] et associés sont
   redéfinis à l'identique *)
module Symb_Tbl = SourceAst.Symb_Tbl

type identifier_kind = SourceAst.identifier_kind
type identifier_info = identifier_kind

type literal     = SourceAst.literal
type binop       = SourceAst.binop

type expression  =
  | Literal  of literal                         (* Valeur immédiate    *)
  | Location of location                        (* Valeur en mémoire   *)
  | Binop    of binop * expression * expression (* Opération binaire   *)
  | FunCall  of call                            (* Appel de fonction   *)
  | NewArray of expression                      (* Création de tableau *)

and location =
  | Identifier  of string (* Variable en mémoire *)
  | ArrayAccess of access (* Case d'un tableau   *)    
and access = expression * expression

and call = string * expression list
    
type block = instruction list
and instruction =
  | Set   of location   * expression    (* Affectation *)
  | While of expression * block         (* Boucle      *)
  | If    of expression * block * block (* Branchement *)
  | Print of expression                 (* Affichage   *)
  | ProcCall of call                    (* Appel       *)
  | Throw
  | Try of block * block

type program = (string * function_info) list
and function_info = { 
  locals:  identifier_info Symb_Tbl.t;
  formals: int; (* On ne retient plus que le nombre de paramètres formels *)
  code:    block
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
let rec print_expression = function
  | Literal lit -> print_literal lit
  | Location id -> print_location id
  | Binop(op, e1, e2) -> sprintf "( %s %s %s )" (print_expression e1) (print_binop op) (print_expression e2)
  | FunCall(c) -> print_call c
  | NewArray(e) -> sprintf "[%s]" (print_expression e) 
and print_location = function
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
  | While(e, b) ->
    sprintf "while %s (\n%s%s)"
      (print_expression e)
      (print_block (o+1) b) (offset o)
  | If(e, b1, b2) ->
    sprintf "if %s then (\n%s%s) else (\n%s%s)"
      (print_expression e)
      (print_block (o+1) b1) (offset o)
      (print_block (o+1) b2) (offset o)
  | Print(e) -> sprintf "print(%s)" (print_expression e)
  | ProcCall(c) -> print_call c
  | Throw -> sprintf "throw"
  | Try(b1, b2) -> sprintf "try (\n%s%s) catch (\n%s%s)"
     (print_block (o+1) b1) (offset o)
     (print_block (o+1) b2) (offset o)
     
let print_function f_id f_info =
  sprintf "%s(%s) (\n%s%s)\n"
    f_id (sprintf "%d paramètres" f_info.formals)
    (print_symb_tbl f_info.locals) (print_block 1 f_info.code)
  
let print_program out prog =
  fprintf out "Version Untyped\n\n";
  List.iter (fun (f_id, f_info) ->
    fprintf out "%s\n\n" (print_function f_id f_info)
  ) prog

