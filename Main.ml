open Format
  
let usage = "usage: ./main.native [options] file.a6m"

let reg_allocation = ref false

let spec =
  [  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".a6m") then
      raise (Arg.Bad "no .a6m extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let mk_dir =  
    try
      Unix.mkdir (Filename.chop_suffix file ".a6m") 0o740
    with
      _ -> ()
let chop = Filename.chop_suffix file ".a6m"
let source_file = chop ^ "/SyntaxeSource"
let typed_file = chop ^ "/SyntaxeTyped"
let untyped_file = chop ^ "/SyntaxeUntyped"
let goto_file = chop ^ "/SyntaxeGoto"
let ir_file = chop ^ "/SyntaxeIr"
let allocated_file = chop ^ "/SyntaxeAllocated"
let output_file = chop ^ ".asm"

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.printf " ligne %d, position %d\n"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  let parse_with_error lexbuf =
    try SourceParser.program SourceLexer.token lexbuf with
    | SourceLexer.SyntaxError msg ->
       Printf.printf "%s at " msg;
      print_position lexbuf;
      exit (-1)
    | SourceParser.Error ->
       Printf.printf "Parsing error at ";
      print_position lexbuf;
      exit (-1)
  in
  parse_with_error lexbuf
  
      
  
let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  (* let p = LocatedSourceParser.program SourceLexer.token lb in *)
  (* let p = SourceParser.program SourceLexer.token lb in *)
  let p = parse lb in
  close_in c;
  (* SourceTypeChecker.typecheck_program p; *)
  let out_source = open_out source_file in
  SourceAst.print_program out_source p;
  close_out out_source;
  
  let p = SourcetoTyped.annote_program p in
  let out_typed = open_out typed_file in
  TypedAst.print_program out_typed p;
  close_out out_typed;
  
  let p = TypedtoUntyped.erase_program p in
  let out_untyped = open_out untyped_file in
  UntypedAst.print_program out_untyped p;
  close_out out_untyped;
  
  (* let p = SourcetoUntyped.erase_program p in *)
  let p = UntypedtoGoto.destructure_program p in
  let out_goto = open_out goto_file in
  GotoAst.print_program out_goto p;
  close_out out_goto;
  
  let p = GototoIr.flatten_program p in
  let out_ir = open_out ir_file in
  IrAst.print_program out_ir p;
  close_out out_ir;
  
  let p = IrtoAllocated.allocate_program !reg_allocation p in
  let out_allocated = open_out allocated_file in
  AllocatedAst.print_program out_allocated p;
  close_out out_allocated;
  
  let asm = AllocatedtoMips.generate_program p in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  (* end; *)
  exit 0
