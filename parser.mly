%{
open Graph

%}

/* Declare your tokens here. */
%token EOF
%token <string> IDENT
%token <int> INT

%token ARROW    /* -> */
%token COMMA
%token SEMICOLON


/* ---------------------------------------------------------------------- */

%start graph
%type <Graph.t> graph
%%

graph:
  | vs=separated_list(COMMA, IDENT) SEMICOLON es=separated_list(COMMA, edge) EOF { Graph(SS.of_list vs, SSS.of_list es) }

edge:
  | from_v=IDENT ARROW to_v=IDENT { (from_v, to_v) }
