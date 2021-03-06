open IrAst

(* Création du graphe de flot de contrôle, sous la forme d'une table associant
   à chaque étiquette d'un point de programme les étiquettes de ses successeurs.
     [mk_succ: IrAst.block -> (IrAst.label * IrAst.label) Hashtbl.t]
   
   Note à propos des tables [Hashtbl] de Caml :

   - un appel [Hashtbl.add tbl k v] ajoute à la table [tbl] une association
     entre la clé [k] et la valeur [v], sans effacer définitivement une
     éventuelle association entre [k] et une autre valeur [v']

   - un appel [Hashtbl.find tbl k] renvoie la dernière valeur associée à
     la clé [k] ; lève l'exception [Not_found] s'il n'y a pas de valeur
     associée à [k]

   - un appel [Hashtbl.find_all tbl k] renvoie la liste de toutes les valeurs
     associées à la clé [k] ; renvoie la liste vide s'il n'y a aucune valeur
     associée

   - un appel [Hashtbl.replace tbl k v] ajoute à la table [tbl] une association
     entre la clé [k] et la valeur [v], en effaçant une éventuelle association
     précédente
*)
  
let mk_succ code =
  (* Création avec une capacité arbitraire ; la table sera étendue au besoin *)
  let succ = Hashtbl.create 257 in

  (* Parcours du code du programme et remplissage à la volée de la table *)
  let rec mk_succ = function
    | (lab, Goto(target_lab)) :: code ->
      (* Le seul successeur d'une instruction [Goto] est l'instruction désignée
	 par l'étiquette de saut. *)
      Hashtbl.add succ lab target_lab;
      (* Puis on itère. *)
      mk_succ code

    (* Dans les autres cas, il faut tenir compte de l'étiquette du successeur
       immédiat (en tête de la liste [code]), si ce successeur existe.
       On définit une fonction auxiliaire [add_next] pour cela.
    *)
    | (lab, CondGoto(_, target_lab)) :: code ->
      Hashtbl.add succ lab target_lab; add_next lab code; mk_succ code
    | (lab, _) :: code ->
      add_next lab code; mk_succ code

    (* Dans le cas d'une liste d'instructions vide ne rien faire (c'est la
       fin de l'itération) *)
    | [] -> ()

  (* Fonction auxiliaire : [add_next lab code] ajoute dans les successeurs de
     [lab] l'étiquette de la première instruction de [code] si celle-ci existe.
     Ne fait rien sinon. *)
  and add_next lab = function
    | []                 -> ()
    | (next_lab, _) :: _ -> Hashtbl.add succ lab next_lab
      
  in
  mk_succ code;
  succ


(* Définition des ensembles d'identifiants
   - le type d'un ensemble d'identifiants est [VarSet.t]
   - la constante [VarSet.empty] désigne l'ensemble vide
   - un appel [VarSet.singleton id] renvoie l'ensemble contenant uniquement
     l'identifiant [id]
   - les fonctions [VarSet.union] et [VarSet.diff] prennent en paramètres deux
     ensembles, et renvoient respectivement leur union et leur différence
     ensemblistes.
*)
module VarSet = Set.Make(String)

let mk_lv p =
  (* Création des deux tables destinées à accumuler le résultat, et
     calcul du flot de contrôle. *)
  let code = p.code in
  let lv_in  = Hashtbl.create 257
  and lv_out = Hashtbl.create 257
  and succ = mk_succ code
  in

  (* Initialisation des tables [lv_in] et [lv_out],
     associe [VarSet.empty] à chaque point de programme. *)
  List.iter (fun (lab, _) ->
    Hashtbl.add lv_in  lab VarSet.empty;
    Hashtbl.add lv_out lab VarSet.empty
  ) code;
  

  (* Les fonctions [lv_gen] et [lv_kill] prennent en paramètre une instruction
     et indiquent respectivement l'ensemble des variables vivantes qu'elle
     crée ou tue.
       [lv_gen:  IrAst.instruction -> VarSet.t]
       [lv_kill: IrAst.instruction -> VarSet.t]
  *)
  (* On introduit une fonction auxiliaire [var_use: IrAst.value -> VarSet.t]
     qui renvoie l'ensemble vide ou un singleton selon que la valeur passée
     en argument est un litéral ou un singleton. *)
  let rec var_use = function
    | Literal _     -> VarSet.empty
    | Identifier id -> VarSet.singleton id
  and lv_gen = function
    (* Une instruction utilisant une seule valeur appelle [var_use] *)
    | Value(_, v)
    | Print(v)
    | CondGoto(v, _)      -> var_use v
    (* Une instruction utilisant deux valeurs fait l'union des [var_use] *)
    | Binop(_, _, v1, v2) -> VarSet.union (var_use v1) (var_use v2)
    (* Une instruction n'utilisant pas de valeur de produit rien *)
    | _                   -> VarSet.empty
  and lv_kill = function
    (* Une instruction modifiant un identifiant tue ce seul identifiant *)
    | Value(id, _)
    | Binop(id, _, _, _) -> VarSet.singleton id
    (* Sinon, ne tue rien *)
    | _                  -> VarSet.empty
  in

      
  (* Booléen qu'on met à [true] lorsque les tables [lv_in] et [lv_out] sont
     encore en train de changer. Il est initialisé à [true] car à l'origine il
     y a bien du calcul à faire, mais il sera repassé à [false] avant chaque
     itération, pour n'être remis à [true] que si des changements sont
     observés. *)
  let change = ref true in
  
  (* Un appel [lv_step_instruction (lab, instr)] met à jour les entrées pour
     le point de programme [lab] (comportant l'instruction [instr]) dans les
     tables [lv_in] et [lv_out], en appliquant les équations de flot de données.
     Rappel :
        In[lab]  =  (Out[lab] \ Kill[instr]) ∪ Gen[instr]
       Out[lab]  =  ⋃ᵣ In[r]                                r ∈ Succ[lab]

     Cette fonction doit aussi faire passer le booléen [change] à [true] si
     les valeurs In[lab] et Out[lab] ont été modifiées.
  *)
  let lv_step_instr (lab, instr) =
    (* Récupération de la liste des successeurs *)
    let succs = Hashtbl.find_all succ lab in
    (* Calcul de [lv_out] par union des [lv_in] des successeurs *)
    let new_lv_out = List.fold_left (fun new_lv_out succ_lab ->
      VarSet.union new_lv_out (Hashtbl.find lv_in succ_lab)
    ) VarSet.empty succs
    in
    (* Mémorisation de l'ancien [lv_in] *)
    let old_lv_in = Hashtbl.find lv_in lab in
    (* Calcul du nouveau [lv_in] en fonction du nouveau [lv_out] *)
    let new_lv_in =
      VarSet.union (VarSet.diff new_lv_out (lv_kill instr)) (lv_gen instr)
    in
    (* Mise à jour de la table *)
    Hashtbl.replace lv_out lab new_lv_out;
    Hashtbl.replace lv_in  lab new_lv_in;
    (* Si [lv_in] a changé, alors faire passer le booléen [change] à vrai *)
    if old_lv_in <> new_lv_in
    then change := true
  in

  (* Une passe complète : met à jour une fois chaque instruction *)
  let lv_step_main () =
    List.iter lv_step_instr code
  in
  (* Répéter tant qu'il reste des changements *)
  while !change do
    change := false;
    lv_step_main ();
  done;
  (* Enfin, renvoyer les versions finales des tables *)
  lv_in, lv_out
