(* Main function. *)

let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: imp <file>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let c =
    try Parser.prog Lexer.read lexbuf
    with _ ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  ignore (Eval.start_eval filename c)
