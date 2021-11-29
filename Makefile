all:
	ocamllex -q lexer.mll
	menhir parser.mly
	ocamlfind ocamlc -thread -package core graph.ml parser.mli parser.ml lexer.ml main.ml -linkpkg -o main
