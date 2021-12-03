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

let swap (x, y) = (y, x)

let uncurry f (x, y) = f x y

let rec iter_n (vs : SS.t) (n : int) (f : SS.t -> unit) : unit =
    if n = 1 then SS.iter (fun elt -> f @@ SS.singleton elt) vs else
    let g elt lst = if SS.mem elt lst then () else f (SS.add elt lst) in
    SS.iter (fun elt -> iter_n vs (n-1) (g elt)) vs

let new_name g =
    let vs = vertices g in
    let i = ref 0 in
    let name = ref @@ "x" ^ string_of_int !i in
    while SS.mem !name vs do
        i := !i + 1
    done; !name

(*** Standard graph theory utility functions ***)

let adjacent (g : t) x y = let es = edges g in SSS.mem (x, y) es || SSS.mem (y, x) es

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

let topsort g : string list =
    let vs = vertices g in let es = edges g in
    let roots = SS.diff vs (SS.of_list @@ List.map snd @@ SSS.elements es) in
    (* This is just the worklist algorithm, but order matters so we use Lists *)
    let workset = ref @@ SS.elements roots in
    let seenset = ref [] in
    while not (!workset = []) do 
        let elt = List.hd !workset in
        workset := List.tl !workset;
        if List.mem elt !seenset
        then ()
        else begin
            seenset := List.cons elt !seenset;
            workset := !workset @ SS.elements @@ children g elt
        end
    done;
    List.rev !seenset

let inverse g =
    let vs = vertices g in
    let es = edges g in
    Graph (vs, SSS.map swap es)

let cliques_n g n =
    let cliques = ref [] in
    iter_n (vertices g) n (fun elts -> if SS.for_all (fun elt1 -> SS.for_all (fun elt2 -> elt1 = elt2 || adjacent g elt1 elt2) elts) elts then cliques := List.cons elts !cliques else ());
    !cliques

(*** And now the domain specific functions ***)

(* Definitions and theorems from: "A Transformational Characterization of Equivalent Bayesian Network Structures", Chickering: https://arxiv.org/pdf/1302.4938.pdf *)

(* Original reference: "Equivalence and Synthesis of Causal Models", Verma and Pearl: https://arxiv.org/pdf/1304.1108.pdf *)
let adjacent g x y = let es = edges g in
    SSS.mem (x, y) es || SSS.mem (y, x) es

let is_vstructure g x y z = let es = edges g in
    SSS.mem (x, y) es && SSS.mem (z, y) es && not @@ adjacent g x z

let edges_of_vstruct (x, y, z) = SSS.add (z, y) @@ SSS.singleton (x, y)

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

(* Note that this does not produce a DAG, as there will be undirected edges
   (represented as having an edge in both directions) *)
let pattern g =
    let undir_edges = skeleton g in
    let vstructs = all_vstructures g in
    let edges = ref SSS.empty in
    (* We're going to explicitly add in the reverse edges that aren't required by the v-structures. *)
    SSS.iter (fun edge -> 
        List.iter (fun (x, y, z) -> 
            if edge = (x, y) || edge = (z, y)
            then edges := SSS.add edge !edges
            else if edge = (y, x) || edge = (y, z)
            then edges := SSS.add (swap edge) !edges
            else edges := SSS.add edge @@ SSS.add (swap edge) !edges
        ) vstructs
    ) undir_edges;
    Graph (vertices g, !edges)

let enumerate_equivalencies g : t list=
    let undir_edges = skeleton g in
    let vs = vertices g in
    let vstructs = all_vstructures g in
(*    List.iter (fun (x, y, z) -> print_string ("(" ^ x ^ ", " ^ y ^ ", " ^ z ^ "), ")) vstructs; print_newline (); *)
    let rec enum_equiv_rec determined_edges (undet_edges : SSS.t) =
        (* print_string "determined_edges:\n"; print_string @@ string_of_edges determined_edges;
        print_newline ();
        print_string "undet_edges:\n"; print_string @@ string_of_edges undet_edges;
        print_newline ();
        print_newline (); *)
        (* if we've determined all edges, we're done. *)
        if SSS.is_empty undet_edges then [Graph(vs, determined_edges)] else
        (* Pick an edge to be added arbitrarily. *)
        let edge = SSS.choose undet_edges in
        (* If the edge is already one the v-structs dictate, skip *)
        if SSS.mem edge determined_edges || SSS.mem (swap edge) determined_edges
            then enum_equiv_rec determined_edges (SSS.remove edge undet_edges) else
        let undet_edges' = SSS.remove edge undet_edges in
        (* Two possibilities: x -> y (call this the covariant case) or x <- y (contravariant). 
           In each case, check that it does not add a 1) loop, 2) v-structure *)
        let propose_edge e = begin let determined_edges' = SSS.add e determined_edges in
            (* print_string ("proposed_edge: " ^ string_of_edges (SSS.singleton e)); print_newline (); *)
            let g' = Graph (vs, determined_edges') in
            (* 1 - adding the edge causes a loop iff one of the nodes becomes its own parent *)
            (* To save a step on the traversal, just check that the parent is a descendant of its child. *)
            if SS.mem (fst e) @@ descendants g' (snd e)
            then ((* print_string "creates cycle.\n"; *) [])
            (* 2- Adding a new edge creates a new v-struct there is a (non-same) parent that is non-adjacent. *)
            else if SS.cardinal @@ SS.filter (fun par -> not @@ adjacent g' par @@ fst e) @@ parents g' (snd e) != 1
            then ((* print_string "creates new v-struct.\n"; *) [])
            (* else we're safe to recurse *)
            else enum_equiv_rec determined_edges' undet_edges'
            end in
        let covariant = propose_edge edge in
        let contravariant = propose_edge @@ swap edge in
        covariant @ contravariant
    in let determined_edges = (List.fold_left (fun acc e -> SSS.union acc @@ edges_of_vstruct e) SSS.empty vstructs) in
    enum_equiv_rec determined_edges (SSS.filter (fun x -> not (SSS.mem x determined_edges) && not (SSS.mem (swap x) determined_edges)) undir_edges)

let covered (g : t) (e : string * string) : bool = 
    SS.equal (SS.add (fst e) (parents g (fst e))) (parents g (snd e))


(* Marginalizing on (removing a) nodes *)
let marginalize g v =
    let pars = SS.elements @@ parents g v in
    let clds = SS.elements @@ children g v in
    let p_to_c = SSS.of_list @@ List.concat_map (fun par -> List.map (fun cld -> (par, cld)) clds) pars in
    (* create a directed clique among the children respecting topological order *)
    let ts = topsort g in
    let tlevel = Hashtbl.create 10 in
    List.iter (uncurry @@ Hashtbl.add tlevel) @@ List.mapi (fun i e -> (e, i)) ts;
    let c_to_c = SSS.of_list @@ List.concat_map
        (fun c1 -> List.concat_map
        (fun c2 -> if Hashtbl.find tlevel c1 < Hashtbl.find tlevel c2 then [(c1, c2)] else []
        ) clds) clds in
    let filtered_edges = SSS.filter (fun (x, y) -> not (x = v) && not (y = v)) @@ edges g in
    Graph (SS.remove v @@ vertices g, SSS.union p_to_c @@ SSS.union c_to_c filtered_edges)

(* Inverse of marginalize *)
let infer_n g n =
    let cliques = cliques_n g n in
    let cliques_with_parents = List.map (fun clique -> (clique, SS.elements clique |> List.fold_left (fun parent_set child -> SS.inter parent_set (parents g child)) (vertices g))) cliques in
    if cliques_with_parents = [] then g else
    let maximal_clique, pars = List.fold_left (fun (max_so_far, max_pars) (clique, pars) -> if SS.cardinal pars > SS.cardinal max_pars then (clique, pars) else (max_so_far, max_pars)) (List.hd cliques_with_parents) (List.tl cliques_with_parents) in
    let name = new_name g in
    let pruned_edges = SSS.filter (fun edge -> not (SS.mem (fst edge) pars && SS.mem (snd edge) maximal_clique)) @@ edges g in
    let new_edges = SSS.union pruned_edges @@ SSS.union (SS.fold (fun par es -> SSS.add (par, name) es) pars SSS.empty) @@ SS.fold (fun cld es -> SSS.add (name, cld) es) maximal_clique SSS.empty in
    Graph (SS.add name @@ vertices g, new_edges)
