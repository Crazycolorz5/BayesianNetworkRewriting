open Printf
open Graph

let print_position outx (lexbuf : Lexing.lexbuf) = (* Adapted from https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let help_string = 
"Unknown option or incorrect parameters. Valid modes:
./main print <graph_file>
"

let main = 
    try begin match Sys.argv.(1) with
        | "print" -> 
            let fname = Sys.argv.(2) in
            let lexbuf = Lexing.from_channel (open_in fname) in
            Lexing.set_filename lexbuf fname;
            begin try let g = Parser.graph Lexer.token lexbuf in
                print_string(string_of_graph g); print_newline ();
            with
            | Parser.Error -> fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1)
            | Failure s -> fprintf stderr "%s: " s; fprintf stderr "; %a: lexing error\n" print_position lexbuf; exit (-1)
            end
        | _ -> print_string help_string
    end with
    | _ -> print_string help_string

        (*
        print_string(string_of_graph g); print_newline ();
        print_string("Ancestors of cancer:\n");
        print_string(string_of_vertices (ancestors g "cancer")); print_newline ();
        print_string("Descendants of smoking:\n");
        print_string(string_of_vertices (descendants g "smoking")); print_newline ();
        ()
    *)
