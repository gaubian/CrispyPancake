open Graphics

let _ =
	open_graph "";
	print_char (wait_next_event [Key_pressed]).key
