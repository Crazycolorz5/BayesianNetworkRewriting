open Printf
open Graph

let flip f x y = f y x

let print_position outx (lexbuf : Lexing.lexbuf) = (* Adapted from https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let help_string = 
"Unknown option or incorrect parameters. Valid modes:
./main print <graph_file>
./main equiv <graph_file1> <graph_file2>
./main pattern <graph_file>
./main enum <graph_file> <output_dir>
./main marginalize <graph_file> <vertex>
"

let graph_of_fname fname = 
    let lexbuf = Lexing.from_channel (open_in fname) in
    Lexing.set_filename lexbuf fname;
    try Parser.graph Lexer.token lexbuf
    with
    | Parser.Error -> fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1)
    | Failure s -> fprintf stderr "%s: " s; fprintf stderr "; %a: lexing error\n" print_position lexbuf; exit (-1)

let main = 
    try begin match Sys.argv.(1) with
        | "print" -> 
            let fname = Sys.argv.(2) in
            print_string @@ string_of_graph @@ graph_of_fname @@ fname;
            print_newline ()
        | "equiv" ->
            let f0, f1 = Sys.argv.(2), Sys.argv.(3) in
            let res = equiv (graph_of_fname f0) (graph_of_fname f1) in
            if res then print_string "Equivalent." else print_string "NOT Equivalent.";
            print_newline ()
        | "pattern" ->
            let fname = Sys.argv.(2) in
            print_string @@ string_of_graph @@ pattern @@ graph_of_fname @@ fname;
            print_newline ()
        | "enum" -> 
            let fname = Sys.argv.(2) in
            let dirname = Sys.argv.(3) in
            let equivs = enumerate_equivalencies @@ graph_of_fname @@ fname in
            begin try Core.Unix.mkdir_p dirname
            with Core.Unix.Unix_error _ -> () end;
            List.iteri (fun i g ->
                let out = open_out @@ dirname ^ "/" ^ fname ^ "_" ^ string_of_int i in
                fprintf out "%s" @@ string_of_graph g; close_out out
            ) equivs
        | "marginalize" ->
            let fname = Sys.argv.(2) in
            let vname = Sys.argv.(3) in
            print_string @@ string_of_graph @@ flip marginalize vname @@ graph_of_fname @@ fname;
            print_newline ()
        | "cliques_n" ->
            let fname = Sys.argv.(2) in
            let n = int_of_string @@ Sys.argv.(3) in
            List.iter (fun vs -> print_string @@ string_of_vertices vs; print_newline (); print_newline ()) @@ flip cliques_n n @@ graph_of_fname @@ fname;
            print_newline ()
        | "infer_n" ->
            let fname = Sys.argv.(2) in
            let n = int_of_string @@ Sys.argv.(3) in
            print_string @@ string_of_graph @@ flip infer_n n @@ graph_of_fname @@ fname;
            print_newline ()
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
