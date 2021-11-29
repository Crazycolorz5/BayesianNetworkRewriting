module SS = struct
    include Set.Make(String)
    let bind (vs : t) (f : string -> t) : t =
        fold (fun elt acc -> union acc (f elt)) vs empty
    let (>>=) = bind
end

module StringPairs = struct
  type t = string * string
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
end

let ternary_compare (x0, y0, z0) (x1, y1, z1) = 
    match Stdlib.compare x0 x1 with
        | 0 -> begin match Stdlib.compare y0 y1 with
            | 0 -> Stdlib.compare z0 z1
            | c -> c end
        | c -> c

module SSS = Set.Make(StringPairs)

type t = Graph of SS.t * SSS.t


(*** Convenience functions ***)

let vertices g = match g with Graph (vs, _) -> vs
let edges g = match g with Graph (_, es) -> es

let string_of_vertices (vs : SS.t) : string =
    String.concat ",\n" @@ SS.elements vs

let string_of_edges (es : SSS.t) : string = 
    String.concat ",\n" @@ List.map (fun (x, y) -> x ^ " -> " ^ y) (SSS.elements es)

let string_of_graph (g : t) : string = match g with
    | Graph(vs, es) -> 
        string_of_vertices vs ^ ";\n" ^ string_of_edges es


(*** Standard graph theory utility functions ***)

let edges_from (g : t) (v : string) : SSS.t = 
    SSS.filter (fun e -> fst e = v) (edges g)
let children g v = SSS.fold (fun e vset -> SS.add (snd e) vset) (edges_from g v) SS.empty

let edges_to (g : t) (v : string) : SSS.t = 
    SSS.filter (fun e -> snd e = v) (edges g)
let parents g v = SSS.fold (fun e vset -> SS.add (fst e) vset) (edges_to g v) SS.empty

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
            workset := SS.union !workset @@ f g elt
        end
    done;
    !seenset

(* TODO: May be worth memoizing *)
let ancestors : t -> string -> SS.t = 
    workset_alg parents

let descendants : t -> string -> SS.t = 
    workset_alg children


(*** And now the domain specific functions ***)

(* Definitions and theorems from: "A Transformational Characterization of Equivalent Bayesian Network Structures", Chickering: https://arxiv.org/pdf/1302.4938.pdf *)

(* Original reference: "Equivalence and Synthesis of Causal Models", Verma and Pearl: https://arxiv.org/pdf/1304.1108.pdf *)
let adjacent g x y = let es = edges g in
    SSS.mem (x, y) es || SSS.mem (y, x) es

let is_vstructure g x y z = let es = edges g in
    SSS.mem (x, y) es && SSS.mem (z, y) es && not @@ adjacent g x z

let all_vstructures g = let vs = vertices g in let acc = ref [] in
    SS.iter (fun x -> SS.iter (fun z -> 
        if x < z then SS.iter (fun y -> if is_vstructure g x y z then acc := (x, y, z) :: !acc else ()) vs
        else ()) vs) vs;
    !acc

let skeleton g : SSS.t = 
    (* Erase directionality by transforming all edges into lexically ordered edge *)
    let order (f_v, t_v) = if String.compare f_v t_v > 0 then (t_v, f_v) else (f_v, t_v) in
    SSS.fold (fun edge acc -> SSS.add (order edge) acc) (edges g) SSS.empty

let equiv g0 g1 = 
    let sort_f = List.sort ternary_compare in
    (* "Two dags are equivalent iff they have the same skeletons and the same v-structures" *)
    let skeleton0 = skeleton g0 in
    let skeleton1 = skeleton g1 in
    SSS.equal skeleton0 skeleton1 && begin
    let v0 = sort_f @@ all_vstructures g0 in
    let v1 = sort_f @@ all_vstructures g1 in
    List.equal (fun x y -> x = y) v0 v1
    end

let covered (g : t) (e : string * string) : bool = 
    SS.equal (SS.add (fst e) (ancestors g (fst e))) (ancestors g (snd e))

