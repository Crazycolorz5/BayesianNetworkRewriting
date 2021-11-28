module SS = Set.Make(String)

module StringPairs = struct
  type t = string * string
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
end

module SSS = Set.Make(StringPairs)

type t = Graph of SS.t * SSS.t

let string_of_graph (g : t) : string = match g with
    | Graph(vs, es) -> let vl = SS.elements vs in let el = SSS.elements es in 
        (String.concat ",\n" vl) ^ ";\n" ^ String.concat ",\n" (List.map (fun (x, y) -> x ^ " -> " ^ y) el)

(* let ancestors (g : t) (v : string) = *)
