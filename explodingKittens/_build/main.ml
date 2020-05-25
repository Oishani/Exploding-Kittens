open Game
open Command

(* This method is only for testing purposes. *)
(* let rec print_list l =
   match l with
   | [] -> print_string ""
   | h::t -> print_string (h ^ ", "); print_list t *)

(** [spacing lst] returns [lst] with spaces between list items. *)
let rec spacing lst =
  match lst with 
  | [] -> []
  | hd :: tl -> ("'" ^ hd ^ "' ") :: spacing tl

(** [str_list_creator str] returns a the string [str] in the form 
    of a string list of its words. *)
let str_list_creator str =
  String.(str |> trim |> split_on_char ' ')

(** [trimmer lst] returns a list with all words trimmed and 
    no words that are the empty strings. *)
let rec trimmer lst =
  match lst with 
  | [] -> []
  | hd :: tl -> if hd = "" then trimmer tl
    else hd :: trimmer tl

(** [first_word lst] returns the first word of [lst] *)
let first_word lst =
  match lst with
  | [] -> ""
  | hd :: _ -> hd

(** [skip_first_word lst] returns a string without the first word of [lst] *)
let skip_first_word lst =
  match lst with
  | [] -> ""
  | _ :: tl -> String.concat " " tl

(** [card str] returns the card the player wants to play in a play command. *)
let card str =
  str |> str_list_creator |> trimmer |> skip_first_word

(** [cmd str] returns the command the player wants to do. *)
let cmd str =
  str |> str_list_creator |> trimmer |> first_word

(** [check_if_play_nope curr_player state] is true if a Nope card has been
    played, and false otherwise. *)
let check_if_play_nope curr_player state = 

  (* Check if the next player wants to play Nope *)
  let player_next = find_next (curr_player) (get_state_hands state) 
      (get_num_players state) in
  print_endline ("Player number " ^ (string_of_int(player_next)) 
                 ^ ":");
  ANSITerminal.(print_string [cyan] 
                  ("\nYou have been bestowed with a superpower. " ^
                   "You now have the ability to stop this Favor."
                   ^ " Any cards that have been Noped are"
                   ^ " lost. They are left in the Discard Pile.\n\n"));
  print_endline ("Would you like to play a Nope Card, if you have one?\n" ^
                 "Answer 'Yes' or 'No'");
  print_endline ("Pass the computer to the previous player immediately after " 
                 ^"answering. NO PEEKING!!");
  print_string  "> ";

  let input = read_line () in
  if ((String.equal input (String.trim "Yes") || String.equal input 
         (String.trim "yes")) && 
      (check_card "Nope" player_next state)) 
  then (
    ANSITerminal.erase Screen;
    print_endline ("\n\nSorry, you just got NOPED!\n\n"); 
    true
  )
  else if (String.equal input (String.trim "quit")) 
  then (print_endline ("Quitting game"); exit 0 )

  (* Player does not get NOPED *)
  else ( 
    ANSITerminal.erase Screen;
    print_endline ("\n\nSheesh! The Nope card wasn't played.\n");
    false
  )




(** [repl start curr_player display] parses commands input by the players for 
    playing the game *)
let rec repl start curr_player display = 
  try (
    let curr_hand_names = get_hand_names curr_player start in
    print_endline ("\nYou are player number " ^ (string_of_int(curr_player)));

    if display = true then (print_endline ("The cards in your hand are: ")); 
    if display = true then (List.iter print_string (spacing curr_hand_names));

    print_endline "\n";
    print_endline("What action would you like to take?\n");
    print_string  "> ";

    let input = read_line () in
    let command = parse input in 

    begin
      match command with 
      | Play _ -> (

          if String.trim (card input) = "Skip" then 
            (
              let new_state = play (card input) start "1" "" in 
              print_endline ("\nYou played " ^ 
                             (card input) ^ "\n");
              repl new_state (current_p new_state) true
            )


          else if String.trim(card input) = "Favor" then (
            ANSITerminal.erase Screen; 

            print_endline ("\nThe previous player wants to play " ^ 
                           (card input) ^ "\n");
            if (check_if_play_nope curr_player start)
            then
              let nope_state = play_nope start "Favor" 1 in 
              repl nope_state curr_player true

            (* Player does not get NOPED *)

            else ( 
              let new_state = play "Favor" start "1" "" in 
              ANSITerminal.
                (print_string [cyan] "You got a new card as a favor!\n\n");
              print_endline ("The cards in your hand are now: ");
              let new_hand_names = get_hand_names curr_player new_state in 
              (List.iter print_string (spacing new_hand_names));
              repl new_state (current_p new_state) false
            )
          )

          else if String.trim(card input) = "Attack" then (
            ANSITerminal.erase Screen; 
            print_endline ("\nThe previous player wants to play " ^ 
                           (card input) ^ "\n");

            (* Check if attacked player wants to play Nope *)

            if (check_if_play_nope curr_player start)
            then
              let nope_state = play_nope start "Attack" 1 in 
              repl nope_state curr_player true

            (* Player does not get NOPED *)

            else ( 
              print_endline ("Sorry! " ^ 
                             "You have to draw 2 cards. We drew one for you. "^ 
                             "Now you can play your turn as normal.\n\n"); 
              let new_state = play "Attack" start "1" "" in 
              let new_state1 = next_player new_state in  
              let new_state2 = draw new_state1 in
              repl new_state2 (current_p new_state2) true
            )
          )


          else if String.trim(card input) = "Draw from Bottom" then(
            let new_state = play (String.trim(card input)) start "1" "" in 
            print_endline "\nYou must end your turn now.\n";

            repl new_state curr_player true
          )

          else if (String.trim (card input) = "Watermelon Cat" ||
                   String.trim (card input) = "Taco Cat" ||
                   String.trim (card input) = "Beard Cat" ||
                   String.trim (card input) = "Rainbow Cat" ||
                   String.trim (card input) = "Hairy Potato Cat") 
          then( 
            print_endline 
              ("\n" ^ (get_instructions (card input) curr_player start) ^ "\n");
            print_endline ("Would you like to play 2 or 3 cat cards?\n\n");
            ANSITerminal.
              (print_string [yellow] ("Pssst: Even if you make a mistake, we'll" 
                                      ^ " play the right number of cards for " ^
                                      "you! Remember, you can't play less than"
                                      ^ " 2 or more than 3 cat cards.\n\n"));
            ANSITerminal.
              (print_string [cyan] ("You get a free card when playing 2 of a" ^ 
                                    " kind and you get to steal a card of your" 
                                    ^ " choice when playing 3 of a kind!\n"));
            print_string  "> ";
            let input2 = read_line () in
            print_string "\n\n";

            (* if three cat combo *)

            if (String.equal input2 "3") then (
              print_endline ("What card would you like to steal?");
              print_string  "> ";
              let combo_card_name = read_line () in
              let new_state = play (card input) start (String.trim input2) 
                  (String.trim combo_card_name) in
              print_string "\n\nYou played three cat cards!\n";
              print_endline ("\nDraw a card if you wanna end your turn.\n");
              repl new_state (current_p new_state) true
            )

            (* if two cat combo*)

            else if (String.equal input2 "3") then (
              let new_state = play (card input) start (String.trim input2) "" 
              in 
              print_string "\n\nYou played two cat cards!\n";
              print_endline ("\nDraw a card if you wanna end your turn.\n");
              repl new_state (current_p new_state) true
            )

            else print_string 
                "Oops! You did not play a valid number of cat cards!\n";
            repl start curr_player true
          )


          else(
            let new_state = play (card input) start "1" "" in 
            print_endline ("\nYou played " ^ (card input));
            print_endline ("\nDraw a card if you wanna end your turn.\n");

            repl new_state (current_p new_state) true)
        )


      | Draw -> (
          let new_state = draw start in
          print_endline "\nYou must end your turn now. \n\n";

          repl new_state (current_p new_state) true
        )
      | Quit -> print_endline ("Quitting game"); exit 0 
      | Instruction _ -> (
          print_endline 
            ("\n" ^ (get_instructions (card input) curr_player start) ^ "\n");

          repl start curr_player true
        )
      | End -> (ANSITerminal.erase Screen;
                let next_state = next_player start in
                let next_p = current_p next_state in

                repl next_state next_p true
               )
    end
  )
  with 
  | UnknownCard _ -> (print_endline ("\nSorry! That card is not in your hand." 
                                     ^ "\n");

                      repl start curr_player true)
  | Malformed -> (print_endline ("\nSorry! We couldn't recognize your command." 
                                 ^ " Please try again.\n");

                  repl start curr_player false)

  | Empty -> (print_endline ("\nOops! Are you sure you entered something?" 
                             ^ " Why not try again.\n");

              repl start curr_player false)

  | Failure _ -> (print_endline ("\nNo more cards left in the draw pile!" 
                                 ^ " You gotta play with what you have.\n"));

    repl start curr_player false


(** [starting ()] parses the inputs for setting up the game for the players. *)
let rec starting () =
  print_endline "How many of you are about to play this kOoL game?\n";
  print_string  "> ";

  begin
    match read_line () with
    | exception End_of_file -> ()
    | exception Failure _ -> 
      ANSITerminal.
        (print_string [red] 
           ("\nOops! Invalid number. " ^ 
            "Only 2 to 5 of you kool kittens can play this game.\n\n"));

      starting()

    | q when String.trim q = "quit" -> print_endline ("Quitting game"); exit 0 

    | num when (int_of_string num < 6 && int_of_string num > 1) -> 
      ANSITerminal.(print_string [cyan] "\nTime for some explosive fun!\n\n");
      ANSITerminal.(print_string [yellow] 
                      ("Type 'instruction' [space] [card name] " ^ 
                       "to get the instructions for a card.\n\n"));
      ANSITerminal.(print_string [yellow] 
                      ("Remember to type 'end' to end your " ^ 
                       "turn and pass the computer to the next player!\n\n"));

      let start = start_game (int_of_string num) in 
      let curr_player = current_p start in

      repl start curr_player true
    | _ -> 
      ANSITerminal.
        (print_string [red] 
           ("\nOops! Invalid number. " ^
            "Only 2 to 5 of you kool kittens can play this game.\n\n"));

      starting()
  end

(** [main ()] starts the game after a prompt. *)
let rec main () =
  ANSITerminal.(print_string [cyan] "\n\nWelcome to Exploding Kittens!\n");

  ANSITerminal.(print_string [cyan] 
                  "Take a moment to switch to full screen mode.\n\n");

  ANSITerminal.
    (print_string [cyan] 
       ("In the deck of cards are some Exploding Kittens. You play the" ^
        " game by taking turns drawing cards until someone draws an " ^
        "Exploding Kitten. When that happens, that person explodes. " ^
        "They are now dead and out of the game. This process continues " ^
        "until there’s only 1 player left, who wins the game. The more " ^
        "cards you draw, the greater your chances of drawing an " ^
        "Exploding Kitten.\n\nBASICALLY, IF YOU EXPLODE, YOU LOSE. " ^
        "AND YOU ARE FULL OF INCENDIARY LOSER SADSAUCE.\n\nIF YOU " ^
        "DON'T EXPLODE, YOU WIN! AND YOU ARE FULL OF GREATNESS. GOOD " ^
        "JOB, BUDDY.\n\nAND ALL OF THE OTHER CARDS WILL LESSEN YOUR " ^
        "CHANCES OF GETTING EXPLODED BY EXPLODING KITTENS.\n\n"));

  starting()

(* Execute the game engine. *)
let () = main ()