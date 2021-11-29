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

let vertices g = match g with Graph (vs, _) -> vs
let edges g = match g with Graph (_, es) -> es

let string_of_vertices (vs : SS.t) : string =
    (String.concat ",\n" (SS.elements vs))

let string_of_edges (es : SSS.t) : string = 
    String.concat ",\n" (List.map (fun (x, y) -> x ^ " -> " ^ y) (SSS.elements es))

let string_of_graph (g : t) : string = match g with
    | Graph(vs, es) -> 
        string_of_vertices vs ^ ";\n" ^ string_of_edges es

let edges_from (g : t) (v : string) : SSS.t = 
    SSS.filter (fun e -> fst e = v) (edges g)
let children = (fun g v -> SSS.fold (fun e vset -> SS.add (snd e) vset) (edges_from g v) SS.empty)

let edges_to (g : t) (v : string) : SSS.t = 
    SSS.filter (fun e -> snd e = v) (edges g)
let parents = (fun g v -> SSS.fold (fun e vset -> SS.add (fst e) vset) (edges_to g v) SS.empty)

let workset_alg f g v = 
    let workset = ref (f g v) in
    let seenset = ref (SS.empty) in
    while not (SS.is_empty !workset) do 
        let elt = SS.choose !workset in
        workset := SS.remove elt !workset;
        if SS.mem elt !seenset
        then ()
        else begin
            seenset := SS.add elt !seenset;
            workset := SS.union !workset (f g elt)
        end
    done;
    !seenset

let ancestors : t -> string -> SS.t = 
    workset_alg parents

let descendants : t -> string -> SS.t = 
    workset_alg children
