all:
	ocamllex -q lexer.mll
	menhir parser.mly
	ocamlc graph.ml parser.mli parser.ml lexer.ml main.ml
