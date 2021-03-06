(* Syntaxe abstraite annotée *)

module Symb_Tbl = SourceAst.Symb_Tbl

type identifier_kind = SourceAst.identifier_kind
type identifier_info = SourceAst.identifier_info
type typ = SourceAst.typ
type literal = SourceAst.literal
type binop = SourceAst.binop

type program = (string * function_info) list

and function_info = {
  (* Les mentions du type de retour et des types des paramètres seront
     utiles pour la vérification des types *)
  return:  typ option;
  formals: (string * typ) list;
  locals:  identifier_info Symb_Tbl.t;
  code:    block;
}

and block = instruction list

and ('a, 'e) annotated_element = { annot: 'a ;
                                 elt:   'e }

and typed_expression = (typ, expression) annotated_element
and typed_location   = (typ, location)   annotated_element
and typed_call       = (typ option, call)       annotated_element

and instruction =
  | Set   of typed_location   * typed_expression    (* Affectation       *)
  | While of typed_expression * block         (* Boucle            *)
  | If    of typed_expression * block * block (* Branchement       *)
  | Print of typed_expression                 (* Affichage         *)
  | ProcCall of typed_call                    (* Appel de fonction *)
  | Throw
  | Try of block * block

and expression =
  | Literal   of literal                         (* Valeur immédiate    *)
  | Location  of typed_location                        (* Valeur en mémoire   *)
  | Binop     of binop * typed_expression * typed_expression (* Opération binaire   *)
  | FunCall   of typed_call                            (* Appel de fonction   *)
  | NewArray  of typed_expression * typ                (* Création de tableau *)

and call = string * typed_expression list

and location =
  | Identifier  of string   (* Variable en mémoire *)
  | ArrayAccess of a_access (* Case d'un tableau   *)

and a_access = typed_expression * typed_expression


open Printf

let rec print_typ : SourceAst.typ -> string = function
  | TypInteger    -> "integer"
  | TypBoolean    -> "boolean"
  | TypArray(ty)  -> (print_typ ty)^"[]"
let print_identifier_info (i : SourceAst.identifier_info) = print_typ i.typ

let print_symb_tbl tbl =
  Symb_Tbl.fold (fun v i s ->
    (sprintf "  var %s %s;\n" (print_identifier_info i) v) ^ s
  ) tbl ""

let print_literal : SourceAst.literal -> string = function
  | Int i -> sprintf "%d" i
  | Bool b -> if b then "true" else "false"
let print_binop : SourceAst.binop -> string = function
  | Add  -> "+"
  | Mult -> "*"
  | Sub  -> "-"
  | Eq   -> "=="
  | Neq  -> "!="
  | Lt   -> "<"
  | Le   -> "<="
  | And  -> "&&"
  | Or   -> "||"
let rec print_typed_expression typed_expr =
  match typed_expr.elt with
  | Literal lit -> sprintf "%s : %s" (print_typ typed_expr.annot) (print_literal lit)
  | Location id -> sprintf "%s : %s" (print_typ typed_expr.annot) (print_typed_location id)
  | Binop(op, e1, e2) -> sprintf "%s : ( %s %s %s )" (print_typ typed_expr.annot) (print_typed_expression e1) (print_binop op) (print_typed_expression e2)
  | FunCall(c) -> print_typed_call c
  | NewArray(e, ty) -> sprintf "%s : [%s]%s" (print_typ typed_expr.annot) (print_typed_expression e) (print_typ ty)
and print_typed_location loc =
  match loc.elt with
  | Identifier x -> sprintf "%s : %s" (print_typ loc.annot) x
  | ArrayAccess(aa) -> sprintf "%s : %s" (print_typ loc.annot) (print_a_access aa)
and print_a_access (e1, e2) = sprintf "%s[%s]" (print_typed_expression e1) (print_typed_expression e2)
and print_typed_call typed_call =
  let (id, args) = typed_call.elt in
  let t =
    match typed_call.annot with
    | Some t -> print_typ t
    | None -> sprintf "void"
  in sprintf "%s : %s" t (sprintf "%s(%s)" id (print_args args))
and print_args = function
  | []      -> ""
  | [e]     -> print_typed_expression e
  | e::args -> sprintf "%s, %s" (print_typed_expression e) (print_args args)

let offset o = String.make (2*o) ' '
let rec print_block o = function
  | [] -> ""
  | i::b -> (offset o) ^ (print_instruction o i) ^ ";\n" ^ (print_block o b)
and print_instruction o = function
  | Set(id, e) -> sprintf "%s := %s" (print_typed_location id) (print_typed_expression e)
  | While(e, b) ->
    sprintf "while %s (\n%s%s)"
      (print_typed_expression e)
      (print_block (o+1) b) (offset o)
  | If(e, b1, b2) ->
    sprintf "if %s then (\n%s%s) else (\n%s%s)"
      (print_typed_expression e)
      (print_block (o+1) b1) (offset o)
      (print_block (o+1) b2) (offset o)
  | Print(e) -> sprintf "print(%s)" (print_typed_expression e)
  | ProcCall(c) -> print_typed_call c
  | Throw -> sprintf "throw"
  | Try(b1, b2) -> sprintf "try (\n%s%s) catch (\n%s%s)"
     (print_block (o+1) b1) (offset o)
     (print_block (o+1) b2) (offset o)

let rec print_formals = function
  | [] -> ""
  | [(id, ty)] -> sprintf "%s %s" (print_typ ty) id
  | (id,ty)::formals -> sprintf "%s %s, %s" (print_typ ty) id (print_formals formals)

let print_ret_type = function
  | None -> ""
  | Some t -> sprintf "(%s) " (print_typ t)

let print_function f_id f_info =
  sprintf "%s(%s) (\n%s%s)\n"
    f_id (print_formals f_info.formals)
    (print_symb_tbl f_info.locals) (print_block 1 f_info.code)

let print_program out prog =
  fprintf out "Version Typed\n\n";
  List.iter (fun (f_id, f_info) ->
    fprintf out "%s\n\n" (print_function f_id f_info)
  ) prog
