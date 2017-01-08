open Delaunay

let solve_pos = (700,250)
let (solve_x,solve_y) = solve_pos

let mediat a b c d =
    let (e,f) = (c -. a,d -. b) in
    let (a_n,b_n) = (-. f,  e) in
    let (a_c,b_c) = ((a +. c) /. 2., (b +. d) /. 2.) in
        ((a_c,b_c),(a_c +. a_n, b_c +. b_n))

(*
    ax + c = a'y + c'
    bx + d = b'y + d'

c - ad/b = y (a' - ab'/b) + c' - ad'/b
c - c' + a(d' - d)/b = y (a' - ab' / b)

y connu
*)
let intersect ((a,b),(c,d)) ((a',b'),(c',d')) =
    let m = (b -. d)/.(a -. c)
    and m' = (b' -. d')/.(a' -. c') in
    let p = (1. /. (2. *. m) *. (b +. d) /. (a +. c))
    and p' = (1. /. (2. *. m') *. (b' +. d') /. (a' +. c')) in
    let x = (p' -. p)/.(m -. m') in
    let y = m *. x +. p in
    	(x,y);;

let side (a,b) (c,d) (e,f) =
   let (c',d') = (c -. a,d -. b) in
   let (e',f') = (e -. a,f -. b) in
      compare ((e' *. d') -. (c' *. f')) 0.

let sort (x::l) =
   let xy = List.fold_left min x l in
   let l' = List.filter ((!=) xy) (x::l) in
       xy::List.sort_uniq (side xy) l'

let rec construct_int = function
    | a::b::l -> (intersect a b)::(construct_int (b::l))
    | _ -> []

let ip_to_fp (a,b) = (float a, float b)

let trans grph i itopt =
    let (x,y) = ip_to_fp @@ IMap.find i itopt in
    let l' = sort @@ List.map (fun z -> ip_to_fp @@ IMap.find z itopt) (IMap.find i grph) in
    let coords_med = List.map (fun (a,b) -> mediat a b x y) l' in
    (* CAS PARTICULIERS DES BORDS À GÉRER *)
        coords_med @ [List.hd coords_med]
        |> construct_int
        |> Array.of_list
        |> Array.map (fun (a,b) -> int_of_float a, int_of_float b)

let polygons grph itopt =
    IMap.mapi (fun x _ -> trans grph x itopt) grph

let trace grph itopt =
    let pol = polygons grph itopt in
        Graphics.set_color Graphics.black;
        ignore @@ IMap.map Graphics.draw_poly pol

let fill_col pol i col =
    Graphics.set_color col;
    Graphics.fill_poly (IMap.find i pol)

let init grph itopt pol =
    trace grph itopt;
    Graphics.moveto solve_x solve_y;
    Graphics.draw_string "SOLVE";
    List.iter (fun (i,colo) -> fill_col pol i colo)