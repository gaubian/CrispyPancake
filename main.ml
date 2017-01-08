open Delaunay
open Trace
open Solve
open Examples

exception No_solution
exception Button_not_possible

let color_of_button cur_col =
	function
	| 'r' -> Some Graphics.red
	| 'g' -> Some Graphics.green
	| 'y' -> Some Graphics.yellow
	| 'b' -> Some Graphics.blue
	| _ -> cur_col

let dist (a,b) (c,d) =
	((a - c) * (a - c)) + ((b - d) * (b - d))

let nearest xy (a::l) =
	List.fold_left (fun b c -> min b (dist xy c,c)) (dist xy a, a) l 


let main vor =
	let list_pts = Array.to_list (Array.map (fun a -> (a.x, a.y)) vor.seeds) in
	let (grph, itopt, pttoi) = Delaunay.main list_pts in
	let pol = Trace.polygons grph itopt in
	let itocol = ref (IMap.map (fun _ -> None) pol) in
	let cur_col = ref None in
		Graphics.open_graph "";
		init grph itopt pol list_pts;
		Array.iter (fun a ->
			match a.c with
			| Some colo ->
				begin
					let i = Delaunay.IPMap.find (a.x, a.y) pttoi in
						Trace.fill_col pol i colo;
						itocol := IMap.add i (Some colo) !itocol
				end
			| None -> ()
		) vor.seeds;
		Graphics.loop_at_exit [Graphics.Button_down; Graphics.Key_pressed] @@
			fun stat ->
				if stat.button
				then begin
					let me = (stat.mouse_x, stat.mouse_y) in
					let (d,ne) = nearest me list_pts in
						if d < dist solve_pos me
						then match Solve.solve !itocol grph with
							| None -> raise No_solution
							| Some sol ->
								begin
									itocol := sol;
									IMap.iter (fun i (Some colo) ->
										Trace.fill_col pol i colo) !itocol
								end
						else let ne_i = IPMap.find ne pttoi in
								 if IMap.find ne_i !itocol = None
								 then itocol := IMap.add ne_i !cur_col !itocol
					 end
				else cur_col := color_of_button !cur_col stat.key

let _ = main v4