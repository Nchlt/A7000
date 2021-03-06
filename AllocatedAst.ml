(* Syntaxe abstraite de la représentation intermédiaire après allocation *)
(* Le code est inchangé, mais la table des symboles donne l'emplacement de
   chaque registre virtuel. *)

module Symb_Tbl = IrAst.Symb_Tbl

type block       = IrAst.block
type instruction = IrAst.instruction
type literal     = IrAst.literal
type value       = IrAst.value
    
type alloc_info =
  | Reg   of string
  | Stack of int

type program = (string * function_info) list
and function_info = { 
  locals: alloc_info Symb_Tbl.t;
  offset: int;  (* Place à allouer sur la pile pour les variables locales *)
  code:   block
}
open Printf
      
let rec print_block = function
  | []          -> "\n"
  | (l, i) :: b -> sprintf "%s: %s\n%s" l (print_instruction i) (print_block b)

and print_instruction : instruction -> string = function
  | Value(dest, v)   -> sprintf "%s <- %s" dest (print_value v)
  | Binop(dest, op, v1, v2) -> sprintf "%s <- %s %s %s"
    dest (print_value v1) (SourceAst.print_binop op) (print_value v2)
  | Print(v)         -> sprintf "print(%s)" (print_value v)
  | Label(lab)       -> lab
  | Goto(lab)        -> sprintf "goto %s" lab
  | CondGoto(v, lab) -> sprintf "goto %s when %s" lab (print_value v)
  | Comment(c)       -> sprintf "# %s" c
  | FunCall(dest, c) -> sprintf "%s <- %s" dest (print_call c)
  | ProcCall(c)      -> print_call c
  | Load(dest, a)    -> sprintf "%s <- %s" dest (print_access a)
  | Store(a, v)      -> sprintf "%s <- %s" (print_access a) (print_value v)
  | New(dest, v)     -> sprintf "%s <- [%s]" dest (print_value v)
  | Throw            -> sprintf "throw"
  | NewHandler(s)    -> sprintf "New handler %s" s
  | RmHandler        -> sprintf "Delete current handler"
     
    
and print_value = function
  | Literal(lit)   -> SourceAst.print_literal lit
  | Identifier(id) -> id

and print_call (id, args) =
  sprintf "%s(%s)" id (print_args args)
and print_args = function
  | []      -> ""
  | [v]     -> print_value v
  | v::args -> sprintf "%s, %s" (print_value v) (print_args args)

and print_access (v1, v2) =
  sprintf "%s[%s]" (print_value v1) (print_value v2)
      
let print_alloc_info = function
  | Reg(s) -> sprintf "%s" s
  | Stack(i) -> sprintf "stack(%d)" i

let print_symb_tbl tbl =
  Symb_Tbl.fold (fun v i s ->
    (sprintf "  var %s %s;\n" (print_alloc_info i) v) ^ s
  ) tbl ""

let print_function f_id f_info =
  sprintf "%s(%s) (\n%s%s)\n"
    f_id (sprintf "%d offset requis" f_info.offset)
    (print_symb_tbl f_info.locals) (print_block f_info.code)
  
let print_program out prog =
  fprintf out "Version Allocated\n\n";
  List.iter (fun (f_id, f_info) ->
    fprintf out "%s\n\n" (print_function f_id f_info)
  ) prog

