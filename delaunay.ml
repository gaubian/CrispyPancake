module Int =
struct
    type t = int
    let compare = Pervasives.compare
end

module Pair (A : Set.OrderedType) (B : Set.OrderedType) =
struct
    type t = A.t * B.t
    let compare (a,b) (c,d) =
        match A.compare a c with
            | 0 -> B.compare b d
            | x -> x
end

module PairInt = Pair(Int)(Int)
module IPMap = Map.Make(PairInt)
module IMap = Map.Make(Int)

type color = A | B | C | D
type seed = {c : color option; x : int; y : int}
type voronoi = {dim : int * int; seeds : seed array}
type grph = int list array

let (min_abs, max_abs, min_ord, max_ord) = (-10001,10001,-10001,10001)

(* Calcule le déterminant de deux vecteurs *)
let det (w,x) (y,z) = w*z - x*y

(* Calcule le produit scalaire de deux vecteurs *)
let dot (w,x) (y,z) = w*y + x*z

(* Arrondit à l'unité *)
let round x =
    if x -. floor x < 0.5
        then floor x
        else ceil x

(* Calcule la distance euclidienne entre deux points du plan *)
let dist ((a,b),(c,d)) =
   [a-c;b-d]
   |> List.map (fun i -> i*i)
   |> List.fold_left (+) 0
   |> float
   |> sqrt

(* Calcule le déterminant d'une matrice 3*3 *)
let det_three mat =
    let a = mat.(0).(0) and b = mat.(0).(1) and c = mat.(0).(2)
    and d = mat.(1).(0) and e = mat.(1).(1) and f = mat.(1).(2)
    and g = mat.(2).(0) and h = mat.(2).(1) and i = mat.(2).(2) in
        a*e*i + b*f*g + c*d*h - g*e*c - h*f*a - i*d*b

(* Renvoie le signe de x ie si x = 0, cela renvoie 0, sinon x / |x| *)
let sgn x =
    if x > 0
        then 1
        else if x = 0
            then 0
            else -1

(* La triangulation *)
(* Je ne vais pas trop commenter la partie "triangulation de Delaunay" qui est classiquissime.
Remarquons simplement que chaque triangle a un field "emplacement" qui sera utile après la triangulation *)
type vertex_opp = {mutable coord: int*int; mutable opp: triangle ref}
and triangle = {mutable emplacement : int; mutable is_empty: bool;
mutable suiv: triangle ref list; mutable ver: vertex_opp array}

let init_tri (coord_a,opp_a) (coord_b,opp_b) (coord_c,opp_c) =
    {emplacement= -1;is_empty=false; suiv = [];
    ver = [|{coord = coord_a;opp = opp_a};
            {coord = coord_b;opp = opp_b};
            {coord = coord_c;opp = opp_c}|]}

let in_circumscribed (a,b) ((c,d),(e,f),(g,h)) =        
    sgn @@ det (e-c,f-d) (g-c,h-d) * sgn (det_three
    [|[|c-a;e-a;g-a|];
    [|d-b;f-b;h-b|];
    [|c*c-a*a + d*d-b*b;e*e-a*a + f*f-b*b;g*g-a*a+h*h-b*b|]|])

let have_to_be_switched triangle_to_test i = 
    let tri = !triangle_to_test in
    let vois_opp = !(tri.ver.(i).opp) in
        (not vois_opp.is_empty) && (in_circumscribed tri.ver.(i).coord 
        (vois_opp.ver.(0).coord,vois_opp.ver.(1).coord,vois_opp.ver.(2).coord) > 0)

let replace_by_in replaced replacing tri =
    if not tri.is_empty
        then for i = 0 to 2 do
            if tri.ver.(i).opp == replaced
                then tri.ver.(i).opp <- replacing;
             done

let index_opp i tri =
    let tri' = (!tri).ver.(i).opp in
        if (!tri').is_empty
            then -1
            else if (!tri').ver.(0).opp == tri
                then 0
                else if (!tri').ver.(1).opp == tri 
                    then 1
                    else 2

let switch tri i =
    let search_opp coordonnees tri =
        if (!tri).ver.(0).coord = coordonnees
            then (!tri).ver.(0).opp
            else if (!tri).ver.(1).coord = coordonnees
                then (!tri).ver.(1).opp
                else (!tri).ver.(2).opp
    in
    let tri' = (!tri).ver.(i).opp in
    let j = index_opp i tri in
    let (a,b,c,d) = ((!tri).ver.(i).coord ,(!tri).ver.((i+1) mod 3).coord ,(!tri).ver.((i+2) mod 3).coord,(!tri').ver.(j).coord) in
    let (e,f,g,h) = (search_opp b tri',search_opp c tri',search_opp b tri,search_opp c tri) in
    let tri'''  = ref {emplacement= -1;is_empty=false;suiv = [];ver = [|{coord=a;opp=f};{coord=b;opp=tri'};{coord=d;opp=h}|]}
    and tri'''' = ref {emplacement= -1;is_empty=false;suiv = [];ver = [|{coord=a;opp=e};{coord=c;opp=tri};{coord=d;opp=g}|]} in
        (!tri''').ver.(1).opp <- tri'''';
        (!tri'''').ver.(1).opp <- tri''';
        replace_by_in tri tri'''' !g;
        replace_by_in tri' tri'''' !e;
        replace_by_in tri' tri''' !f;
        replace_by_in tri tri''' !h;
        (!tri).suiv <- [tri''';tri''''];
        (!tri').suiv <- [tri''';tri''''];
        [tri]

let split_in_three x tri =
    let (coor_a,coor_b,coor_c,vois_a,vois_b,vois_c) = ((!tri).ver.(0).coord,(!tri).ver.(1).coord,
    (!tri).ver.(2).coord,(!tri).ver.(0).opp,(!tri).ver.(1).opp,(!tri).ver.(2).opp) in
    let tri1 = ref {emplacement= -1;is_empty=false;suiv=[];ver=[||]} and tri2 = ref {emplacement= -1;is_empty=false;suiv=[];ver=[||]}
    and tri3 = ref {emplacement= -1;is_empty=false;suiv=[];ver=[||]} in
        (!tri1).ver <- [|{coord=coor_a;opp=tri3};{coord=coor_b;opp=tri2};{coord=x;opp=vois_c}|];
        (!tri2).ver <- [|{coord=coor_a;opp=tri3};{coord=coor_c;opp=tri1};{coord=x;opp=vois_b}|];
        (!tri3).ver <- [|{coord=coor_c;opp=tri1};{coord=coor_b;opp=tri2};{coord=x;opp=vois_a}|];
        replace_by_in tri tri1 !vois_c;
        replace_by_in tri tri2 !vois_b;
        replace_by_in tri tri3 !vois_a;
        (!tri).suiv <- [tri1;tri2;tri3];
        [tri1;tri2;tri3]

let is_in_triangle x tri =
    let direction (a,b) ((c,d),(e,f)) =
        let determinant = det (a-e,b-f) (c-e,d-f) in
                determinant >= 0, determinant <=0
    in
    let (c,d,e) = (tri.ver.(0).coord,tri.ver.(1).coord,tri.ver.(2).coord) in
    let flag,drap = direction x (c,d) and flag',drap' = direction x (d,e)
    and flag'',drap'' = direction x (e,c) in
        (flag && flag' && flag'') || (drap && drap' && drap'')

let rec in_which_triangle x = function
    |t::q  ->if is_in_triangle x !t
                 then if (!t).suiv = []
                     then t
                     else in_which_triangle x @@ (!t).suiv
                 else in_which_triangle x q
    | _    -> failwith "Empty Triangle List"

let insert x l =
    let rec aux = function
        | [] -> ()
        | tri::q -> begin 
                if (!tri).suiv = []
                    then if have_to_be_switched tri 0
                        then aux @@ (switch tri 0)@q
                        else if have_to_be_switched tri 1
                            then aux @@ (switch tri 1)@q
                            else if have_to_be_switched tri 2
                                then aux @@ (switch tri 2)@q
                                else aux q
                    else aux ((!tri).suiv @ q)
                    end
    in
    let tri = in_which_triangle x l in
    let to_verify = split_in_three x tri in
        aux to_verify

(* La fonction finale pour trianguler, qui ne fait qu'insérer les points les uns après les autres.
L'astuce est de prendre pour cas initial les triangles ((-10001,-10001),(-10001,10001),(10001,10001))
et ((10001,-10001),(10001,10001),(-10001,-10001)), étant donné que l'énoncé contraint les points à être
dans l'un de ces deux triangles *)
let rec delaunay = function
    | [] -> begin
        let nul = ref {emplacement= -1;is_empty=true; suiv = [];ver= [||]} in
        let tri1 = ref @@ init_tri ((min_abs,max_ord),nul) 
    ((min_abs,min_ord),nul) ((max_abs,max_ord),nul)
    and tri2 = ref @@ init_tri ((max_abs,min_ord),nul) 
    ((max_abs,max_ord),nul) ((min_abs,min_ord),nul)
    in (!tri1).ver.(0).opp <- tri2;
            (!tri2).ver.(0).opp <- tri1;
            [tri1;tri2]
        end
    | t::q -> let l = delaunay q in 
        (insert t l;l)

(* Fin de la triangulation *)
(* Etant donnée une liste de points, renvoie le tableau de la triangulation de
Delaunay correspondante, où chaque triangle a été numéroté (numérotation conservée au
niveau de la valeur "emplacement") *)
let pts_to_tab list_points =
    let counter = ref 0 and res = ref [] and l = delaunay list_points 
    in
    let rec aux = function
        | t::q when (!t).emplacement = -1 -> begin
               if (!t).suiv = []
               then (res := t::!res; (!t).emplacement <- !counter; incr counter)
               else ((!t).emplacement <- 0; aux @@ (!t).suiv);
               aux q
                                             end
        | _                               -> ()
    in aux l;
    List.map (!) !res

let add_aux a b grph =
    IMap.add a (b::(try IMap.find a grph with Not_found -> [])) grph

let add a b grph =
	grph := add_aux a b !grph;
	grph := add_aux b a !grph

let main list_points =
	let cpt = ref 0 in
	let itopt = ref IMap.empty in
	let pttoi = List.fold_left (fun x y ->
		let ans = IPMap.add y !cpt x in
			itopt := IMap.add !cpt y !itopt;
			incr cpt;
			ans
		) IPMap.empty list_points
	in
	let tab = pts_to_tab list_points in
	let grph : int list IMap.t ref = ref IMap.empty in
		List.iter (fun tri ->
			let vertices = tri.ver in
				for i = 0 to 1 do
				for j = i + 1 to 2 do
					add (IPMap.find vertices.(i).coord pttoi)
						(IPMap.find vertices.(j).coord pttoi)
						grph 
				done;
				done
		) tab;
		grph

