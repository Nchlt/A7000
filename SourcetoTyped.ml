(* Travail de vérification des types 
   + création de structures annotées avec leurs types *)

module S = SourceAst  (* Source de la transformation *)
module T = TypedAst (* Cible de la transformation  *)
module Symb_Tbl = S.Symb_Tbl

(* Rapports d'erreurs *)
exception Type_error of S.typ * S.typ
    
(* comparetype: typ -> typ -> unit
   Lève une exception si les types diffèrent. *)
let comparetype t1 t2 =
  if t1 <> t2
  then raise (Type_error(t1, t2))
    
(* type_literal: literal -> typ *)
let type_literal : S.literal -> S.typ = function
  | Int _  -> TypInteger
  | Bool _ -> TypBoolean
     
(* [type_binop] renvoie le type des opérandes et le type du résultat
   d'un opérateur binaire. *)
(* type_binop: binop -> typ * typ *)
let type_binop : S.binop -> S.typ * S.typ = function
  | Add | Sub | Mult     -> TypInteger, TypInteger
  | Eq  | Neq | Lt  | Le -> TypInteger, TypBoolean
  | And | Or             -> TypBoolean, TypBoolean
    
(* [type_expression] et les fonctions associées prennent en paramètre
   le programme lui-même (pour connaître les types des fonctions) et
   la table de symboles locale (pour connaître les types des variables).
   Leur résultat est de type [typ]. *)
let rec type_expression prog symb_tbl = function
  | S.Literal(lit)  -> type_literal lit
  | S.Location(loc) -> type_location prog symb_tbl loc
  | S.Binop(op, e1, e2) ->
     let ty_op, ty_r = type_binop op in
     comparetype ty_op (type_expression prog symb_tbl e1);
     comparetype ty_op (type_expression prog symb_tbl e2);
     ty_r
  | S.FunCall(c) ->
     (match type_call prog symb_tbl c with
     | None -> failwith "Non-void function expected"
     | Some t -> t)
  | S.NewArray(e, t) ->
     comparetype TypInteger (type_expression prog symb_tbl e);
    TypArray(t)
      
and type_location prog symb_tbl = function
  | S.Identifier(id)  -> (Symb_Tbl.find id symb_tbl).S.typ
  | S.ArrayAccess(aa) -> type_a_access prog symb_tbl aa
     
(* Un accès bien formé demande que [e1] désigne un tableau et [e2] un entier *)
and type_a_access prog symb_tbl (e1, e2) =
  comparetype TypInteger (type_expression prog symb_tbl e2);
  match type_expression prog symb_tbl e1 with
  | TypArray(t) -> t
  | _ -> failwith "Array type expected"
       
(* Un appel bien formé demande que chaque paramètre effectif ait un type 
   compatible avec le paramètre formel correspondant *)
(* Contrairement aux précédentes, [type_call] renvoie un résultat [typ option] *)
and type_call prog symb_tbl (fid, args) =
  let (f : S.function_info) = List.assoc fid prog in
  let expected_types = List.map snd f.formals in
  let arg_types = List.map (type_expression prog symb_tbl) args in
  List.iter2 comparetype expected_types arg_types;
  f.return
    

let annote_function p f =
  
  let symb_tbl = f.S.locals in

  let rec annote_block b = List.map annote_instruction b
    
  and annote_instruction : S.instruction -> T.instruction = function
    | Set(l, e)     -> T.Set(annote_location l, annote_expression e)
    | While(e, b)   -> T.While(annote_expression e, annote_block b)
    | If(e, b1, b2) -> T.If(annote_expression e, annote_block b1, annote_block b2)
    | Print(e)      -> T.Print(annote_expression e)
    | ProcCall(c)   -> T.ProcCall(annote_call c)
    | Throw         -> T.Throw
    | Try(b1, b2)   -> T.Try(annote_block b1, annote_block b2)
      
  and annote_expression expr =
    let annot = type_expression p symb_tbl expr in
    match expr with
    | Literal(l)        -> { annot; elt = T.Literal(l)}
    | Location(l)       -> { annot; elt = T.Location(annote_location l)}
    | Binop(op, e1, e2) -> { annot; elt = T.Binop(op, annote_expression e1, annote_expression e2)}
    | FunCall(c)        -> { annot; elt = T.FunCall(annote_call c)}
    | NewArray(e, t)    -> { annot; elt = T.NewArray(annote_expression e, t)}
       
  and annote_location loc =
    let annot = type_location p symb_tbl loc in
    match loc with
    | Identifier(id)  -> { annot; elt = T.Identifier(id)}
    | ArrayAccess(aa) -> { annot; elt = T.ArrayAccess(annote_a_access aa)}
       
  and annote_a_access (e1, e2) = (annote_expression e1, annote_expression e2)

  and annote_call (id, args) =
    let annot = type_call p symb_tbl (id, args) in
    { annot; elt = (id, List.map annote_expression args) }
    (* (id, List.map annote_expression args) *)
  in

  let code = annote_block f.S.code in  
  { T.return = f.S.return; formals = f.S.formals; locals = f.S.locals; code }    

let annote_program p =
  (* Printf.printf "Affichage de la version Source\n"; *)
  (* Printf.printf "%s\n" (S.print_program p); *)
  List.map (fun (id, f) -> (id, annote_function p f)) p
