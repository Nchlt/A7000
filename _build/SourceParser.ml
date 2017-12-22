
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
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
    | IDENT of (
# 13 "SourceParser.mly"
       (string)
# 31 "SourceParser.ml"
  )
    | FOR
    | EQUAL
    | EOF
    | END
    | ELSE
    | DECR
    | CONST_INT of (
# 8 "SourceParser.mly"
       (int)
# 42 "SourceParser.ml"
  )
    | CONST_BOOL of (
# 10 "SourceParser.mly"
       (bool)
# 47 "SourceParser.ml"
  )
    | COMMA
    | CB
    | BOOL
    | BEGIN
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState119
  | MenhirState114
  | MenhirState109
  | MenhirState107
  | MenhirState103
  | MenhirState102
  | MenhirState99
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState92
  | MenhirState90
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState82
  | MenhirState81
  | MenhirState77
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState50
  | MenhirState47
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState36
  | MenhirState35
  | MenhirState27
  | MenhirState25
  | MenhirState21
  | MenhirState18
  | MenhirState16
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 1 "SourceParser.mly"
  

  open SourceAst
  open Lexing
  

# 132 "SourceParser.ml"

let rec _menhir_reduce16 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.call) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (c : (SourceAst.call))) = _menhir_stack in
    let _v : (SourceAst.expression) = 
# 127 "SourceParser.mly"
                                          ( FunCall(c)        )
# 140 "SourceParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (is : (SourceAst.block))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.block) = 
# 119 "SourceParser.mly"
                                     ( is )
# 163 "SourceParser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState97 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState99 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))), _), _, (b1 : (SourceAst.block))), _, (b2 : (SourceAst.block))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (SourceAst.instruction) = 
# 113 "SourceParser.mly"
                                                    ( If(e, b1, b2) )
# 199 "SourceParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | MenhirState89 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))), _, (b : (SourceAst.block))) = _menhir_stack in
                let _1 = () in
                let _v : (SourceAst.instruction) = 
# 112 "SourceParser.mly"
                                                    ( While(e, b)   )
# 210 "SourceParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (SourceAst.instruction))), _, (is : (SourceAst.block))) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.block) = 
# 107 "SourceParser.mly"
                                       ( i :: is )
# 229 "SourceParser.ml"
         in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (ty : (SourceAst.typ option))), (id : (
# 13 "SourceParser.mly"
       (string)
# 244 "SourceParser.ml"
            ))), _, (args : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list))), _, (vds : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list))), _, (is : (SourceAst.block))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (string * SourceAst.function_info) = 
# 57 "SourceParser.mly"
                                            (
      (* Si la fonction a un type de retour, initialiser la table des
	 symboles avec la variables spéciale "result" *)
      let base_env = match ty with
	| None -> Symb_Tbl.empty
	| Some ty -> Symb_Tbl.singleton "result" { typ=ty; kind=Return }
      in
      (* Calculer et ajouter les informations pour les paramètres formels *)
      let info_params = List.mapi (fun i (id, ty) ->
	id, { typ=ty; kind=Formal (i+1) }
      ) args
      in
      let param_env = List.fold_left (fun acc (id, info) ->
	Symb_Tbl.add id info acc
      ) base_env info_params
      in
      (* Ajouter enfin les variables locales *)
      let local_env = List.fold_left (fun acc (id, ty) ->
	Symb_Tbl.add id { typ=ty; kind=Local } acc
      ) param_env vds
      in
      id, { return=ty; formals=args; locals=local_env; code=is }
    )
# 275 "SourceParser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | IDENT _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | EOF ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs0 : (SourceAst.expression list)) = _v in
    let _v : (SourceAst.expression list) = let args =
      let xs = xs0 in
      
# 206 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 317 "SourceParser.ml"
      
    in
    
# 137 "SourceParser.mly"
                                          ( args              )
# 323 "SourceParser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (id : (
# 13 "SourceParser.mly"
       (string)
# 337 "SourceParser.ml"
        ))), _, (args : (SourceAst.expression list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (SourceAst.call) = 
# 133 "SourceParser.mly"
                                          ( (id, args)        )
# 344 "SourceParser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState40 | MenhirState102 | MenhirState95 | MenhirState92 | MenhirState41 | MenhirState47 | MenhirState82 | MenhirState50 | MenhirState54 | MenhirState58 | MenhirState76 | MenhirState63 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState65 | MenhirState67 | MenhirState60 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState39 | MenhirState90 | MenhirState107 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (c : (SourceAst.call))) = _menhir_stack in
                let _v : (SourceAst.instruction) = 
# 115 "SourceParser.mly"
                                                    ( ProcCall(c)   )
# 362 "SourceParser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | AND | EQUAL | LE | LT | MINUS | NEQ | OB | OR | PLUS | POINT | STAR ->
                _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.literal) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (lit : (SourceAst.literal)) = _v in
    let _v : (SourceAst.expression) = 
# 123 "SourceParser.mly"
                                          ( Literal(lit)      )
# 390 "SourceParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (loc : (SourceAst.location))) = _menhir_stack in
    let _v : (SourceAst.expression) = 
# 124 "SourceParser.mly"
                                          ( Location(loc)     )
# 400 "SourceParser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.block) = 
# 106 "SourceParser.mly"
                                       ( []      )
# 409 "SourceParser.ml"
     in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | CONST_BOOL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | CONST_INT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | IDENT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | CONST_BOOL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | CONST_INT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | IDENT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | IF ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PRINT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | WHILE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | END ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IF ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | PRINT ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | WHILE ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | END ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (SourceAst.expression list)) = _v in
        let _v : (SourceAst.expression list) = 
# 130 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x )
# 577 "SourceParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (SourceAst.expression list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (SourceAst.expression))), _) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.expression list) = 
# 217 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 589 "SourceParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), (id : (
# 13 "SourceParser.mly"
       (string)
# 642 "SourceParser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (SourceAst.expression) = 
# 129 "SourceParser.mly"
                                          ( NewRecord(id)     )
# 650 "SourceParser.ml"
                 in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "SourceParser.mly"
       (string)
# 675 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CONST_BOOL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | CONST_INT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IDENT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | OB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState47 in
            let _v : (SourceAst.expression list) = 
# 128 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 705 "SourceParser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | AND | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OB | OR | PLUS | POINT | SEMI | SET | STAR | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (id : (
# 13 "SourceParser.mly"
       (string)
# 717 "SourceParser.ml"
        ))) = _menhir_stack in
        let _v : (SourceAst.location) = 
# 154 "SourceParser.mly"
                                       ( Identifier(id)      )
# 722 "SourceParser.ml"
         in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "SourceParser.mly"
       (int)
# 735 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 8 "SourceParser.mly"
       (int)
# 743 "SourceParser.ml"
    )) = _v in
    let _v : (SourceAst.literal) = 
# 149 "SourceParser.mly"
                ( Int i  )
# 748 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "SourceParser.mly"
       (bool)
# 755 "SourceParser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 10 "SourceParser.mly"
       (bool)
# 763 "SourceParser.ml"
    )) = _v in
    let _v : (SourceAst.literal) = 
# 150 "SourceParser.mly"
                ( Bool b )
# 768 "SourceParser.ml"
     in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_goto_location : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState102 | MenhirState95 | MenhirState92 | MenhirState40 | MenhirState41 | MenhirState82 | MenhirState47 | MenhirState76 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState60 | MenhirState58 | MenhirState54 | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState39 | MenhirState107 | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | CONST_BOOL _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | CONST_INT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IDENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | OB ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | AND | EQUAL | LE | LT | MINUS | NEQ | OB | OR | PLUS | POINT | STAR ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (id : (
# 13 "SourceParser.mly"
       (string)
# 875 "SourceParser.ml"
        )) = _v in
        let ((_menhir_stack, _menhir_s, (e : (SourceAst.expression))), _) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.location) = 
# 156 "SourceParser.mly"
                                       ( FieldAccess(e,id)   )
# 882 "SourceParser.ml"
         in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | CONST_BOOL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | CONST_INT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IDENT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | OB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (SourceAst.Symb_Tbl.key * SourceAst.typ))) = _menhir_stack in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 187 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1110 "SourceParser.ml"
         in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vds : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list)) = _v in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 91 "SourceParser.mly"
                     ( vds )
# 1120 "SourceParser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | CONST_BOOL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | CONST_INT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | IDENT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | IF ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | OB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PRINT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | WHILE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | END ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_typed_ident__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs0 : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list)) = _v in
    let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = let pl =
      let xs = xs0 in
      
# 206 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1164 "SourceParser.ml"
      
    in
    
# 83 "SourceParser.mly"
                                        ( pl )
# 1170 "SourceParser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | BEGIN | CONST_BOOL _ | CONST_INT _ | END | IDENT _ | IF | NEW | OB | PRINT | WHILE ->
                _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_fun_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * SourceAst.function_info) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (sds : ((string * SourceAst.struct_info) list))), _, (fds : ((string * SourceAst.function_info) list))) = _menhir_stack in
            let _3 = () in
            let _v : (
# 45 "SourceParser.mly"
      (SourceAst.program)
# 1225 "SourceParser.ml"
            ) = 
# 50 "SourceParser.mly"
                                                          ( let p = {functions = fds; structs = sds } in 
       p )
# 1230 "SourceParser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 45 "SourceParser.mly"
      (SourceAst.program)
# 1237 "SourceParser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * SourceAst.function_info))), _, (xs : ((string * SourceAst.function_info) list))) = _menhir_stack in
        let _v : ((string * SourceAst.function_info) list) = 
# 187 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1253 "SourceParser.ml"
         in
        _menhir_goto_list_fun_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState53 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.expression) = 
# 125 "SourceParser.mly"
                                          ( e                 )
# 1281 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OR | PLUS | SEMI | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 161 "SourceParser.mly"
         ( Mult )
# 1326 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1332 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | CB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState59 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (SourceAst.location) = 
# 155 "SourceParser.mly"
                                       ( ArrayAccess(e1, e2) )
# 1357 "SourceParser.ml"
             in
            _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OR | PLUS | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 159 "SourceParser.mly"
         ( Add  )
# 1404 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1410 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | AND | BEGIN | CB | COMMA | END | OR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 167 "SourceParser.mly"
         ( Or   )
# 1449 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1455 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 163 "SourceParser.mly"
         ( Neq  )
# 1486 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1492 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OR | PLUS | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 160 "SourceParser.mly"
         ( Sub  )
# 1519 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1525 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 164 "SourceParser.mly"
         ( Lt   )
# 1556 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1562 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 165 "SourceParser.mly"
         ( Le   )
# 1593 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1599 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 162 "SourceParser.mly"
         ( Eq   )
# 1630 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1636 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | AND | BEGIN | CB | COMMA | END | OR | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (SourceAst.expression))), _), _, (e2 : (SourceAst.expression))) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
              
# 166 "SourceParser.mly"
         ( And  )
# 1675 "SourceParser.ml"
              
            in
            
# 126 "SourceParser.mly"
                                          ( Binop(op, e1, e2) )
# 1681 "SourceParser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState82 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | CONST_BOOL _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | CONST_INT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | IDENT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | OB ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (SourceAst.expression))) = _menhir_stack in
            let _v : (SourceAst.expression list) = 
# 215 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1744 "SourceParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | CB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState86 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BEGIN ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (SourceAst.instruction) = 
# 114 "SourceParser.mly"
                                                    ( Print(e)      )
# 1853 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState96 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (l : (SourceAst.location))), _, (e : (SourceAst.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.instruction) = 
# 111 "SourceParser.mly"
                                                    ( Set(l, e)     )
# 1958 "SourceParser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState39 | MenhirState90 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MINUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | NEQ ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | OB ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | POINT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | _ ->
        _menhir_fail ()

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 185 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2004 "SourceParser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | BOOL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | OB ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_goto_separated_nonempty_list_COMMA_typed_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (SourceAst.Symb_Tbl.key * SourceAst.typ))) = _menhir_stack in
        let _2 = () in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 217 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2039 "SourceParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_ident_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list)) = _v in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 130 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x )
# 2049 "SourceParser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typed_ident__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.typ option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState25 in
                let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 128 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2087 "SourceParser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_typed_ident__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * SourceAst.function_info) list) = 
# 185 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2117 "SourceParser.ml"
     in
    _menhir_goto_list_fun_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.typ option) = 
# 100 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( None )
# 2126 "SourceParser.ml"
     in
    _menhir_goto_option_typ_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_field_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.struct_info) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 13 "SourceParser.mly"
       (string)
# 2146 "SourceParser.ml"
            ))), _, (lfd : (SourceAst.struct_info))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (string * SourceAst.struct_info) = 
# 141 "SourceParser.mly"
                                                             ( (id, lfd) )
# 2154 "SourceParser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | STRUCT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | BEGIN | BOOL | EOF | IDENT _ | INT | OB ->
                _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * SourceAst.typ))), _, (xs : (SourceAst.struct_info))) = _menhir_stack in
        let _v : (SourceAst.struct_info) = 
# 187 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2182 "SourceParser.ml"
         in
        _menhir_goto_list_field_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (ty : (SourceAst.typ))) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (SourceAst.typ) = 
# 101 "SourceParser.mly"
                 ( TypArray(ty)  )
# 2201 "SourceParser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 13 "SourceParser.mly"
       (string)
# 2216 "SourceParser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (SourceAst.typ))) = _menhir_stack in
            let _v : (string * SourceAst.typ) = 
# 145 "SourceParser.mly"
                  ( (id, t) )
# 2222 "SourceParser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | END ->
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (SourceAst.typ))) = _menhir_stack in
        let _v : (SourceAst.typ option) = 
# 102 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 2256 "SourceParser.ml"
         in
        _menhir_goto_option_typ_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 | MenhirState25 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 13 "SourceParser.mly"
       (string)
# 2271 "SourceParser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (SourceAst.typ))) = _menhir_stack in
            let _v : (SourceAst.Symb_Tbl.key * SourceAst.typ) = 
# 87 "SourceParser.mly"
                   ( (id, t) )
# 2277 "SourceParser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState27 | MenhirState25 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                    | BOOL ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                    | INT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                    | OB ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
                | END ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (SourceAst.Symb_Tbl.key * SourceAst.typ))) = _menhir_stack in
                    let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = 
# 215 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2309 "SourceParser.ml"
                     in
                    _menhir_goto_separated_nonempty_list_COMMA_typed_ident_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState36 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMI ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, (vd : (SourceAst.Symb_Tbl.key * SourceAst.typ))) = _menhir_stack in
                    let _3 = () in
                    let _1 = () in
                    let _v : (SourceAst.Symb_Tbl.key * SourceAst.typ) = 
# 95 "SourceParser.mly"
                             ( vd  )
# 2333 "SourceParser.ml"
                     in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | VAR ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                    | BEGIN | CONST_BOOL _ | CONST_INT _ | END | IDENT _ | IF | NEW | OB | PRINT | WHILE ->
                        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (SourceAst.expression))), _), _, (ty : (SourceAst.typ))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (SourceAst.expression) = 
# 128 "SourceParser.mly"
                                          ( NewArray(e, ty)   )
# 2371 "SourceParser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_struct_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * SourceAst.struct_info) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * SourceAst.struct_info))), _, (xs : ((string * SourceAst.struct_info) list))) = _menhir_stack in
        let _v : ((string * SourceAst.struct_info) list) = 
# 187 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2388 "SourceParser.ml"
         in
        _menhir_goto_list_struct_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BOOL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | OB ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | IDENT _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | EOF ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | _ ->
        _menhir_fail ()

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.struct_info) = 
# 185 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2420 "SourceParser.ml"
     in
    _menhir_goto_list_field_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | BOOL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | OB ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (SourceAst.typ) = 
# 99 "SourceParser.mly"
                 ( TypInteger    )
# 2462 "SourceParser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (SourceAst.typ) = 
# 100 "SourceParser.mly"
                 ( TypBoolean    )
# 2474 "SourceParser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 13 "SourceParser.mly"
       (string)
# 2497 "SourceParser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.typ) = 
# 102 "SourceParser.mly"
                         ( TypStruct(id)  )
# 2504 "SourceParser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * SourceAst.struct_info) list) = 
# 185 "/Users/nour/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2744 "SourceParser.ml"
     in
    _menhir_goto_list_struct_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | END ->
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 45 "SourceParser.mly"
      (SourceAst.program)
# 2807 "SourceParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BEGIN | BOOL | EOF | IDENT _ | INT | OB ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/Users/nour/.opam/system/lib/menhir/standard.mly"
  


# 2834 "SourceParser.ml"
