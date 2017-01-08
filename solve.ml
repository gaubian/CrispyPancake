open Delaunay

let colors = Graphics.[red;green;yellow;blue]

let is_possible col i grph itocol =
	IMap.find i grph
	|> List.map (fun x -> IMap.find x itocol)
	|> List.mem (Some col)
	|> not

let rec aux grph itocol = function
	| [] -> true
	| i::l -> if List.exists (fun col ->
				if is_possible col i grph !itocol
				then (itocol := IMap.add i (Some col) !itocol; aux grph itocol l)
				else false) colors
			  then true
			  else (itocol := IMap.add i None !itocol; false) 

let rec solve aux_itocol grph =
	let itocol = ref aux_itocol in
	let not_colo = IMap.fold (fun i c l -> if c = None then i::l else l) !itocol []
	in
		if aux grph itocol not_colo;
			then Some !itocol
			else None