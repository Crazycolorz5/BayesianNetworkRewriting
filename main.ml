open Printf
open Graph

let read_all name : string = (* Adapted from https://stackoverflow.com/a/5775024 *)
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  String.concat "" (loop [])

let print_position outx (lexbuf : Lexing.lexbuf) = (* Adapted from https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let main = 
    let lexbuf = Lexing.from_string (read_all "graph.txt") in 
    try 
        let g = Parser.graph Lexer.token lexbuf in
        print_string(string_of_graph g); print_newline ();
        print_string("Ancestors of cancer:\n");
        print_string(string_of_vertices (ancestors g "cancer")); print_newline ();
        print_string("Descendants of smoking:\n");
        print_string(string_of_vertices (descendants g "smoking")); print_newline ();
        ()
    with
      | Parser.Error ->
        fprintf stderr "%a: syntax error\n" print_position lexbuf;
        exit (-1)
