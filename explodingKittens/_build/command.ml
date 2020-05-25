type object_phrase = string list

type command = 
  | Play of object_phrase
  | Draw
  | Quit
  | Instruction of object_phrase
  | End

exception Empty
exception Malformed

let rec print_list = function 
  | [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l

let is_empty str =
  if String.equal str "" then false else true

let rec get_object_phrase split_lst : object_phrase = 
  if List.length split_lst < 2 then raise Malformed else
    match split_lst with
    | [] -> []
    | h :: t -> if (String.equal h "play" || String.equal h "instruction")
      then t else get_object_phrase t

let parse str =
  let trimmed_string = String.trim str in 
  if String.length trimmed_string = 0 then raise Empty
  else let split_lst = List.map String.trim 
           (String.split_on_char ' ' trimmed_string) in 

    if (List.length split_lst) < 1 then raise Malformed 
    else 
      let verb = List.hd split_lst in 

      if ((String.equal verb "quit") && (List.length split_lst = 1)) 
      then Quit 

      else if ((String.equal verb "play") && (List.length split_lst > 1)) 
      then Play (get_object_phrase (List.filter is_empty split_lst))

      else if ((String.equal verb "draw") && (List.length split_lst = 1)) 
      then Draw

      else if ((String.equal verb "end") && (List.length split_lst = 1)) 
      then End

      else if ((String.equal verb "instruction") && (List.length split_lst > 1)) 
      then Instruction (get_object_phrase (List.filter is_empty split_lst))

      else raise Malformed