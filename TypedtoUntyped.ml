(* Transformation de la syntaxe abstraite annotée
   en syntaxe abstraite non typée. *)

(* À ce stade, la plupart de ces fonctions traduisent le programme
   sans rien modifier. *)

module S = TypedAst  (* Source de la transformation *)
module T = UntypedAst (* Cible de la transformation  *)

 


let erase_function p f =
  (* let structs = p.structs in *)
  
  let rec erase_block b = List.map erase_instruction b

        
  and erase_instruction : S.instruction -> T.instruction = function
    | Set(l, e)     -> T.Set(erase_location l, erase_expression e)
    | While(e, b)   -> T.While(erase_expression e, erase_block b)
    | If(e, b1, b2) -> T.If(erase_expression e, erase_block b1, erase_block b2)
    | Print(e)      -> T.Print(erase_expression e)
    | ProcCall(c)   -> T.ProcCall(erase_call c)
    | Throw         -> T.Throw
    | Try(b1, b2)   -> T.Try(erase_block b1, erase_block b2)
      
  and erase_expression typed_expr =
    match typed_expr.elt with
    | Literal(l)        -> T.Literal(l)
    | Location(l)       -> T.Location(erase_location l)
    | Binop(op, e1, e2) -> T.Binop(op, erase_expression e1, erase_expression e2)
    | FunCall(c)        -> T.FunCall(erase_call c)
    | NewArray(e, _)    -> T.NewArray(erase_expression e)
    | NewRecord(str)    -> T.NewArray(T.Location( T.Identifier(str))) 
  and erase_location typed_loc =
    match typed_loc.elt with
    | Identifier(id)  -> T.Identifier(id)
    | ArrayAccess(aa) -> T.ArrayAccess(erase_a_access aa)
    | FieldAccess(fa) -> failwith "Pas encore implémenté"
       (* Nous y sommes presque mais nous n'arrivons pas à acceder à p.structs pour y faire notre recherche *)
       (* let (te, str) = fa in *)
       (* let s = match te.annot with *)
       (* | TypStruct(st) -> st *)
       (* in *)
       
       (* let s_info = List.assoc s p.S.structs in     *)

       (* let rec get_pos str s count = *)
       (* 	 match s with *)
       (* 	 | [] -> failwith "Champs non existant" *)
       (* 	 | (str,_)::_ -> count *)
       (* 	 | _::tl -> get_pos str tl (count + 1) *)
	      
       (* in *)
       
       (* let pos_int = get_pos str s_info 1 in *)
       (* let pos = T.Literal(Int(pos_int)) in *)
       (* let aa  = (erase_expression te, pos) in  *)
       (* T.ArrayAccess(aa) *)
       
  and erase_a_access (e1, e2) = (erase_expression e1, erase_expression e2)

  and erase_f_access (e,t) = (erase_expression e, t)

  and erase_call typed_c =
    let (id, args) = typed_c.elt in
    let new_id = List.fold_left
      (fun str arg -> str ^ (Printf.sprintf "_%s" (S.print_typ arg.S.annot))) id args      
    in
    (new_id, List.map erase_expression args)
  in
  (* erase_identifier_info: S.identifier_info -> T.identifier_info *)
  let erase_identifier_info (i : S.identifier_info) =
    i.kind in
  let locals =
    S.Symb_Tbl.fold
      (fun id info tbl ->
	T.Symb_Tbl.add id (erase_identifier_info info) tbl)
      f.S.locals
      T.Symb_Tbl.empty
  in
  
  let code = erase_block f.S.code in
  
  { locals; T.formals = List.length f.formals; code }    

let erase_program p =
  (* Printf.printf "\nAffichage de la version Typed \n"; *)
  (* Printf.printf "%s\n" (S.print_program p); *)
  List.map (fun (id, f) ->
    let new_id = List.fold_left
      (fun str arg -> str ^ (Printf.sprintf "_%s" (S.print_typ (snd arg)))) id f.S.formals      
    in
    (new_id, erase_function p f)) p
    
