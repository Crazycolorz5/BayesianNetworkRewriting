
type t = Graph of string list * (string * string) list

let string_of_graph (g : t) : string = match g with
    | Graph(vs, es) -> (String.concat ",\n" vs) ^ ";\n" ^ String.concat ",\n" (List.map (fun (x, y) -> x ^ " -> " ^ y) es)
