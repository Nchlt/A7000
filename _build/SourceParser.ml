
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | VAR
  | TRY
  | THROW
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
  | IDENT of (string)
  | FOR
  | EQUAL
  | EOF
  | END
  | ELSE
  | DECR
  | CONST_INT of (int)
  | CONST_BOOL of (bool)
  | COMMA
  | CB
  | CATCH
  | BOOL
  | BEGIN
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState116
  | MenhirState111
  | MenhirState106
  | MenhirState104
  | MenhirState100
  | MenhirState99
  | MenhirState96
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState90
  | MenhirState89
  | MenhirState87
  | MenhirState86
  | MenhirState84
  | MenhirState83
  | MenhirState79
  | MenhirState78
  | MenhirState74
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState58
  | MenhirState57
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState47
  | MenhirState44
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState33
  | MenhirState32
  | MenhirState24
  | MenhirState22
  | MenhirState18
  | MenhirState15
  | MenhirState13
  | MenhirState5
  | MenhirState3
  | MenhirState0
  

  open SourceAst
  open Lexing
  

let rec _menhir_reduce16 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.call) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, c) = _menhir_stack in
    let _v : (SourceAst.expression) =                                           ( FunCall(c)        ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, is) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.block) =                                      ( is ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState94 ->
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
                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState96 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, e), _), _, b1), _, b2) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (SourceAst.instruction) =                                                     ( If(e, b1, b2) ) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | MenhirState86 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, e), _, b) = _menhir_stack in
                let _1 = () in
                let _v : (SourceAst.instruction) =                                                     ( While(e, b)   ) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, i), _, is) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.block) =                                        ( i :: is ) in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, ty), id), _, args), _, vds), _, is) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (string * SourceAst.function_info) =                                             (
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
    ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | IDENT _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | EOF ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
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
    let xs0 = _v in
    let _v : (SourceAst.expression list) = let args =
      let xs = xs0 in
          ( xs )
    in
                                              ( args              ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, id), _, args) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (SourceAst.call) =                                           ( (id, args)        ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState37 | MenhirState99 | MenhirState92 | MenhirState89 | MenhirState38 | MenhirState44 | MenhirState79 | MenhirState47 | MenhirState51 | MenhirState53 | MenhirState73 | MenhirState60 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState62 | MenhirState64 | MenhirState57 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState36 | MenhirState87 | MenhirState104 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, c) = _menhir_stack in
                let _v : (SourceAst.instruction) =                                                     ( ProcCall(c)   ) in
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
    let lit = _v in
    let _v : (SourceAst.expression) =                                           ( Literal(lit)      ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, loc) = _menhir_stack in
    let _v : (SourceAst.expression) =                                           ( Location(loc)     ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.block) =                                        ( []      ) in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | CONST_BOOL _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | CONST_INT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | IDENT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | NEW ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | OB ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

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
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | CONST_BOOL _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | CONST_INT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | IDENT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | IF ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NEW ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | OB ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PRINT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | END ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | IF ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PRINT ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | WHILE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | END ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (SourceAst.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                let ((_menhir_stack, _menhir_s), id) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _1 = () in
                let _v : (SourceAst.expression) =                                           ( NewRecord(id)     ) in
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

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
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
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | CONST_BOOL _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | CONST_INT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | IDENT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | NEW ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | OB ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState44 in
            let _v : (SourceAst.expression list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | AND | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OB | OR | PLUS | POINT | SEMI | SET | STAR | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (SourceAst.location) =                                        ( Identifier(id)      ) in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (SourceAst.literal) =                 ( Int i  ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _v : (SourceAst.literal) =                 ( Bool b ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_goto_location : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState99 | MenhirState92 | MenhirState89 | MenhirState37 | MenhirState38 | MenhirState79 | MenhirState44 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState57 | MenhirState53 | MenhirState51 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState36 | MenhirState104 | MenhirState87 ->
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
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | CONST_BOOL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | CONST_INT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IDENT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | NEW ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | OB ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
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

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let id = _v in
        let ((_menhir_stack, _menhir_s, e), _) = _menhir_stack in
        let _2 = () in
        let _v : (SourceAst.location) =                                        ( FieldAccess(e,id)   ) in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | CONST_BOOL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CONST_INT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IDENT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | NEW ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | OB ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =     ( x :: xs ) in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let vds = _v in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =                      ( vds ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | CONST_BOOL _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | CONST_INT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IDENT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IF ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NEW ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | OB ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PRINT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | END ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_typed_ident__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs0 = _v in
    let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) = let pl =
      let xs = xs0 in
          ( xs )
    in
                                            ( pl ) in
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
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | BEGIN | CONST_BOOL _ | CONST_INT _ | END | IDENT _ | IF | NEW | OB | PRINT | WHILE ->
                _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
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
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, sds), _, fds) = _menhir_stack in
            let _3 = () in
            let _v : (SourceAst.program) =                                                           ( let p = {functions = fds; structs = sds } in 
       p ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : ((string * SourceAst.function_info) list) =     ( x :: xs ) in
        _menhir_goto_list_fun_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState50 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (SourceAst.expression) =                                           ( e                 ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OR | PLUS | POINT | SEMI | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Mult )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | CB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (SourceAst.location) =                                        ( ArrayAccess(e1, e2) ) in
            _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OR | PLUS | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Add  )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | AND | BEGIN | CB | COMMA | END | OR | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Or   )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Neq  )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | MINUS | NEQ | OR | PLUS | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Sub  )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Lt   )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Le   )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | AND | BEGIN | CB | COMMA | END | EQUAL | LE | LT | NEQ | OR | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( Eq   )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | AND | BEGIN | CB | COMMA | END | OR | POINT | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1), _), _, e2) = _menhir_stack in
            let _10 = () in
            let _v : (SourceAst.expression) = let op =
              let _1 = _10 in
                       ( And  )
            in
                                                      ( Binop(op, e1, e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState79 | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState78 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | CONST_BOOL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | CONST_INT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | IDENT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | NEW ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | OB ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (SourceAst.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | CB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState83 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BEGIN ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState90 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (SourceAst.instruction) =                                                     ( Print(e)      ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, l), _, e) = _menhir_stack in
            let _2 = () in
            let _v : (SourceAst.instruction) =                                                     ( Set(l, e)     ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState36 | MenhirState87 | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | EQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LT ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OB ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | STAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | _ ->
        _menhir_fail ()

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =     ( [] ) in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | OB ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_goto_separated_nonempty_list_COMMA_typed_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_typed_ident_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =     ( x ) in
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
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState22 in
                let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_typed_ident__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
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
    let _v : ((string * SourceAst.function_info) list) =     ( [] ) in
    _menhir_goto_list_fun_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.typ option) =     ( None ) in
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
            let (((_menhir_stack, _menhir_s), id), _, lfd) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (string * SourceAst.struct_info) =                                                              ( (id, lfd) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | STRUCT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | BOOL | EOF | IDENT _ | INT | OB ->
                _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (SourceAst.struct_info) =     ( x :: xs ) in
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
        let ((_menhir_stack, _menhir_s), _, ty) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (SourceAst.typ) =                ( TypArray(ty)  ) in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let (_menhir_stack, _menhir_s, t) = _menhir_stack in
            let _v : (string * SourceAst.typ) =                   ( (id, t) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | OB ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | END ->
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (SourceAst.typ option) =     ( Some x ) in
        _menhir_goto_option_typ_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState33 | MenhirState22 | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let (_menhir_stack, _menhir_s, t) = _menhir_stack in
            let _v : (SourceAst.Symb_Tbl.key * SourceAst.typ) =                    ( (id, t) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState24 | MenhirState22 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                    | INT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                    | OB ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
                | END ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                    let _v : ((SourceAst.Symb_Tbl.key * SourceAst.typ) list) =     ( [ x ] ) in
                    _menhir_goto_separated_nonempty_list_COMMA_typed_ident_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState33 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMI ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, vd) = _menhir_stack in
                    let _3 = () in
                    let _1 = () in
                    let _v : (SourceAst.Symb_Tbl.key * SourceAst.typ) =                              ( vd  ) in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | VAR ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                    | BEGIN | CONST_BOOL _ | CONST_INT _ | END | IDENT _ | IF | NEW | OB | PRINT | WHILE ->
                        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
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
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, e), _), _, ty) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (SourceAst.expression) =                                           ( NewArray(e, ty)   ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_struct_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * SourceAst.struct_info) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : ((string * SourceAst.struct_info) list) =     ( x :: xs ) in
        _menhir_goto_list_struct_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | OB ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | IDENT _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | EOF ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | _ ->
        _menhir_fail ()

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.struct_info) =     ( [] ) in
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
    let _v : (SourceAst.typ) =                ( TypInteger    ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (SourceAst.typ) =                ( TypBoolean    ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
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
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
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
    let _v : ((string * SourceAst.struct_info) list) =     ( [] ) in
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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.program) =
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
    | BOOL | EOF | IDENT _ | INT | OB ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

