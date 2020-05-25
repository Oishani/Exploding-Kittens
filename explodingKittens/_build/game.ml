open Yojson.Basic.Util

type card_name = string
type instruction = string
type player = int

exception UnknownCard of card_name

type card = {
  name : card_name;
  instruction : instruction;
}

type deck = card list

type hand = card list

type state = {
  draw_pile : card list;
  hands : (player * hand) list;
  current_player : player;
  num_players : int; (* immutable, does not change as players explode. *)
}

(** [to_card card] is the card that [card] represents.
    Requires : [card] is a valid card representation. *)
let to_card card = {
  name = card |> member "name" |> to_string;
  instruction = card |> member "instruction" |> to_string;
}

(** [to_deck deck] is the deck of cards that [deck] represents.
    Requires : [deck] is a valid deck representation. *)
let to_deck deck = 
  deck |> member "cards" |> to_list |> List.map to_card

(** [regular_deck] represents a json of a deck of cards in "cards.json." *)
let regular_deck = 
  Yojson.Basic.from_file "cards.json" |> to_deck

(** [kittens_deck] represents a json of a deck of exploding kittens cards in 
    "kittens.json." *)
let kittens_deck =
  Yojson.Basic.from_file "kittens.json" |> to_deck

(** [defuse_deck] represents a json of a deck of defuse cards in 
    "defuse.json." *)
let defuse_deck =
  Yojson.Basic.from_file "defuse.json" |> to_deck

(** [shuffler l] is a random permutation of [l]. 
    Citation: https://discuss.ocaml.org/t/ *)
let rec shuffler l =
  match l with
  | [] -> []
  | [x] -> [x]
  | list -> 
    let (y, z) = List.partition (fun elt -> Random.bool ()) list in 
    List.rev_append (shuffler y) (shuffler z)

(** [init_hand a b c d defuse] initializes a player's hand with card list 
    [a; b; c; d] and 1 defuse card. *)
let init_hand a b c d defuse = 
  match defuse with 
  | [] -> ([], [])
  | h :: t ->  let y = a :: b :: c :: d :: h :: [] |> shuffler in (y, t)

(** [init_hands num_players deck defuse acc x] initializes players' hands in 
    [acc] and also returns the remaining regular and defuse cards. *)
let rec init_hands num_players deck defuse acc x = 
  if ((List.length acc) < num_players) then
    match deck with
    | a :: b :: c :: d :: t -> 
      let (y, z) = (init_hand a b c d defuse) 
      in init_hands num_players t z ((x, y) :: acc) (x + 1)
    | _ -> (deck, defuse, acc)
  else (deck, defuse, acc)

(** [init_kittens num_players kittens] is n-1 exploding kittens to be added to 
    the drawing player for a game with [num_players]. *)
let rec init_kittens num_players kittens = 
  if (num_players > 1) then
    (List.hd kittens) :: kittens |> init_kittens (num_players - 1) 
  else kittens

let current_p state = state.current_player

(** [get_hand player state] is the hand of [player] in [state]. *)
let get_hand player state = List.assoc player state.hands

let get_state_hands state = state.hands

let get_num_players state = state.num_players

(* For testing purposes only *)
(* let get_draw state = state.draw_pile *)

let get_names cards = List.map (fun x -> x.name) cards

let get_hand_names player state = get_hand player state |> get_names

let get_instructions card player state =
  if (List.mem card (get_hand_names player state))
  then (let the_card = 
          (* [fun: card -> instruction] returns the instruction of a card. *)
          List.find (fun cd -> cd.name = card) (get_hand player state) in
        the_card.instruction) 
  else raise (UnknownCard card)

(* [get_card] is card with name [card_name] in [hand] *)
let rec get_card card_name hand = 
  match hand with 
  | [] -> List.hd hand 
  | h :: t -> if h.name <> card_name then get_card card_name t 
    else h

let check_card card player state = 
  get_hand player state |> get_names |> List.mem card

(* [check_two_card card_name hand bool1] is a boolean of whether two instances 
   of [card_name] exist in the [player]'s hand *)
let rec check_two_card card_name (hand: card_name list ) bool1 = 
  match hand with 
  | [] -> false
  | h :: t -> 
    (* if h is not [card_name] then keep checking using this method *)
    if(h <> card_name) then check_two_card card_name t bool1
    (* if one instance of [card_name] has already been found *)
    else if bool1 then true
    else check_two_card card_name t true

(* [check_three_card] is a boolean of whether three instances of [card_name] 
   exist in the [player]'s hand *)
let rec check_three_card card_name (hand: card_name list ) bool1 = 
  match hand with 
  | [] -> false
  | h :: t -> 
    (* if h is not [card_name] then keep checking using this method *)
    if(h <> card_name) then check_three_card card_name t bool1 
    (* if h is [card_name], then first instance found, now check for two more
       using [check_two_card card_name hand bool1]*)
    else (check_two_card card_name t false)

let rec find_next (player:player) hands num_players = 
  if (player < num_players)
  then 
    if (List.mem_assoc (player + 1) hands) then (player + 1)
    else find_next (player + 1) hands num_players
  else 
  if (List.mem_assoc 1 hands) then 1
  else find_next 1 hands num_players

let next_player state =
  let new_current = 
    find_next state.current_player state.hands state.num_players in 
  {state with current_player = new_current}

(** [play_shuffle state] plays the shuffle card by shuffling the draw_pile in 
    [state] and returning the resultant state. *)
let play_shuffle state = 
  print_endline("\nYou have shuffled the draw pile.");
  let shuffled_deck = shuffler state.draw_pile in
  {state with draw_pile = shuffled_deck}

(** [play_skip state] skips the current_player's turn and returns the resultant
    state. *)
let play_skip state = next_player state

(** [see_the_future state] print the names of the top three cards in the 
    draw_pile of [state]. *)
let see_the_future state =
  print_endline ("\nThe top cards are: ");
  if (List.length state.draw_pile > 3) then 
    match (get_names state.draw_pile) with
    | a :: b :: c :: d -> (print_endline (String.concat ", " 
                                            (a :: b :: c :: []) ^ "\n"))
    | _ -> print_endline ""
  else print_endline (String.concat ", " (get_names state.draw_pile))


(** [del_one x list acc] returns the list of cards [list] without one instance 
    of the card with name [x]. *)
let rec del_one x list acc =
  match list with 
  | [] -> []
  | h :: t -> if (h.name <> x) then (del_one x t (h :: acc)) else (acc @ t)

(** [update_hand player state new_hand] returns an updated value for the hands 
    field of [state] with the hand of [player] being changed to [new_hand]. *)
let update_hand player state new_hand =
  (player, new_hand) :: (List.remove_assoc player (state.hands))

(** [get_random_card hand random_int] returns the card at the [random_int] 
    location in [hand] *)
let rec get_random_card hand random_int = 
  if random_int = 0 then List.hd hand 
  else get_random_card (List.tl hand) (random_int - 1) 

(** [update_play_hand name player state] updates [player]'s hand in [state] 
    when they play a card with name [name]. *)
let update_play_hand card player state =
  let new_hand = del_one card (get_hand player state) [] in
  {state with hands = (update_hand player state new_hand)}

(** [update_play_hand name player state] updates [player]'s hand in [state] 
    when they gain a card with name [name]. *)
let add_to_play_hand card player state =
  let new_hand = card :: (get_hand player state) in
  {state with hands = (update_hand player state new_hand)}

(** [play_favor state] takes one card at random from the next player and returns 
    the resultant state. *)
let play_favor state = 

  let curr_player = (state.current_player) in
  (* Next player and their hand *)
  let player_next = find_next (curr_player) (state.hands) (state.num_players) in
  let player_next_hand = get_hand player_next state in

  match player_next_hand with
  | [] -> state
  | _ -> 
    (* Get a random card from next player's hand *)
    let len = List.length player_next_hand in 
    let rand_num = Random.int len in 
    let random_card = get_random_card player_next_hand rand_num in 

    (* Remove random card from next player's hand and add to curr_player's
       hand*)
    let state_updated1 = update_play_hand (random_card.name) player_next state 
    in add_to_play_hand (random_card) curr_player state_updated1   


let rec play_nope state card_name iter = 
  let curr_player = (state.current_player) in
  let player_next = find_next (curr_player) (state.hands) (state.num_players) in

  if (iter = 1) then 
    let state_updated1 = update_play_hand "Nope" player_next state in 
    let state_updated2 = update_play_hand card_name curr_player state_updated1 
    in state_updated2
  else
    let state_updated2 = update_play_hand card_name curr_player state in 
    play_nope state_updated2 card_name (iter-1)


(** [play_two_cat_card state card_name] first checks whether the current_player
    has two [card_name] cards. It then is the updated state in which both 
    [card_name] cards are removed from the current_player's hand and a random 
    card is removed from the next player's hand and added to the current 
    player's hand *)
let play_two_cat_card card_name state = 
  let curr_player = (state.current_player) in
  let curr_player_hand = get_hand curr_player state in 

  if (check_two_card card_name (curr_player_hand |> get_names) false)
  then
    (*remove the two cat cards*)
    let new_state1 = update_play_hand card_name curr_player state in
    let new_state2 = update_play_hand card_name curr_player new_state1 in 

    (* find next player and hand to generate random card *) 
    let player_next = find_next (curr_player) (state.hands) 
        (state.num_players) in
    let player_next_hand = get_hand player_next state in 
    let len = List.length player_next_hand in 
    match player_next_hand with
    | [] -> state
    | _ -> 
      let rand_num = Random.int len in 
      let random_card = get_random_card player_next_hand rand_num in 

      (* delete [random_card] from [player_next]'s hand and add it to 
         [curr_player]'s hand *)
      let new_state3 = update_play_hand (random_card.name) player_next 
          new_state2 in (add_to_play_hand random_card curr_player new_state3)

  else state

let play_three_cat_card card_name state desired_card_name = 
  let curr_player = (state.current_player) in
  let curr_player_hand = get_hand curr_player state in 

  if (check_three_card card_name (curr_player_hand |> get_names) false)
  then(
    (* remove the three cat cards *)
    let new_state1 = update_play_hand card_name curr_player state in
    let new_state2 = update_play_hand card_name curr_player new_state1 in 
    let new_state3 = update_play_hand card_name curr_player new_state2 in 

    (* find next player and hand to check if card is in their hand *) 
    let player_next = find_next (curr_player) (state.hands) 
        (state.num_players) in
    let player_next_hand = get_hand player_next state in 
    if (check_card desired_card_name player_next new_state3)
    then (
      (* delete [random_card] from [player_next]'s hand and add it to 
         [curr_player]'s hand *)
      let desired_card = get_card desired_card_name player_next_hand in
      let new_state4 = update_play_hand desired_card_name player_next 
          new_state3 in (add_to_play_hand desired_card curr_player new_state4)
    )
    else new_state3
  )
  else state

(* [determine_cat_combo card state cat_combo_num combo_card_name] is the state 
   after a two or three cat card combo occurs*)
let determine_cat_combo card state cat_combo_num combo_card_name = 
  if (String.equal cat_combo_num "3") 
  then play_three_cat_card card state combo_card_name
  else play_two_cat_card card state

(** [remove_player player state] removes [player] from [state]. *)
let remove_player player state = 
  let new_hands = List.remove_assoc player (state.hands) in 
  {state with hands = new_hands}

(** [check_win player state next_player_val] checks if a player has won in case 
    of an explosion. If so, it displays a win message and terminates the game. 
    If not, it displays an explosion message for the player who exploded. *)
let check_win player state next_player_val = 
  if (List.length state.hands = 2) then 
    (print_endline (("\nPlayer " ^ (string_of_int player) ^ " has exploded. ")^
                    ("\nPlayer " ^ (string_of_int next_player_val) ^ 
                     " wins the game!")); exit 0) 
  else print_endline ("\nPlayer " ^ (string_of_int player) ^ " has exploded.")

(** [new_draw_pile pile] is [pile] without its first element. *)
let new_draw_pile pile =
  match pile with
  | [] -> []
  | h :: t -> t

(** [rec insert_kitten_helper card pile index acc] returns the new draw pile 
    with [card] inserted at [index] in [pile] but in reverse order. *)
let rec insert_kitten_helper card pile index acc =
  match pile with 
  | [] -> acc
  | h :: t -> if ((List.length acc) = (index - 1)) 
    then (insert_kitten_helper card t index (h :: card :: acc))
    else (insert_kitten_helper card t index (h :: acc))

(** [new_dfb_pile pile] is [pile] without its last element. *)
let new_dfb_pile pile = 
  List.rev pile |> new_draw_pile |> List.rev

(** [insert_kitten card state] is the new state with [card] inserted at an 
    index input taken from the player. *)
let insert_kitten card dfb state =
  print_endline 
    ("\nYikes! You drew an Exploding Kitten card!" ^ 
     " Defuse the explosion by reinserting the card like a sneak.");
  print_endline 
    "Enter index at which you want to insert kitten (1 for the top, etc.): ";
  print_endline ("\nIf you insert it out of bounds, "^
                 "the card will be placed at the nearest index.\n");
  print_string ">";
  let index = read_line () |> int_of_string in
  let insert_pile = 
    if dfb then new_dfb_pile state.draw_pile
    else new_draw_pile state.draw_pile in
  let new_pile = ((insert_kitten_helper card insert_pile index []) |> 
                  List.rev) 
  in  {state with draw_pile = new_pile}

(** [draw_kitten player state] returns a new state by updating [state] when 
    [player] draws in exploding kitten. *)
let draw_kitten card player state dfb =
  let next_player_val = (next_player state).current_player in
  let killed_player_state = remove_player player state in
  let killed_dp =
    if dfb then new_dfb_pile state.draw_pile 
    else new_draw_pile state.draw_pile in
  let semi_state = 
    if (List.mem "Defuse" (get_hand_names player state))
    then (update_play_hand "Defuse" player state |> insert_kitten card dfb)
    else ((check_win player state next_player_val); 
          {state with draw_pile = killed_dp; 
                      hands = killed_player_state.hands}) 
  in semi_state

(** [draw_other card player state] returns a new state by updating [state] when 
    [player] draws a card with name [card]. *)
let draw_other card player state dfb =
  let new_pile = 
    if dfb then new_dfb_pile state.draw_pile
    else new_draw_pile state.draw_pile in 
  let new_hand = card::(get_hand player state) in
  {state with draw_pile = new_pile; hands = (update_hand player state new_hand)}

let start_game num_players = 
  Random.self_init ();
  let (deck, defuse, hands) = 
    init_hands num_players (shuffler regular_deck) defuse_deck [] 1 in {
    draw_pile = (deck @ defuse @ (init_kittens (num_players - 1) kittens_deck) 
                 |> shuffler);
    hands = hands;
    current_player = 1;
    num_players = num_players;
  }

let draw_from_bottom state =
  let player = current_p state in
  let card = state.draw_pile |> List.rev |> List.hd in
  let name = card.name in
  if (name = "Exploding Kitten") then draw_kitten card player state true
  else draw_other card player state true

let play card state cat_combo_num combo_card_name =
  let player = current_p state in
  if (check_card card player state) then 
    match card with
    | "Shuffle" -> play_shuffle state |> update_play_hand card player
    | "Skip" -> play_skip state |> update_play_hand card player
    | "See the Future" -> see_the_future state; 
      update_play_hand card player state
    | "Favor" -> play_favor state |> update_play_hand card player
    | "Draw from Bottom" -> let new_state = draw_from_bottom state in 
      update_play_hand card player new_state
    | "Watermelon Cat" -> determine_cat_combo card state cat_combo_num 
                            combo_card_name
    | "Beard Cat" -> determine_cat_combo card state cat_combo_num 
                       combo_card_name
    | "Taco Cat" -> determine_cat_combo card state cat_combo_num combo_card_name
    | "Rainbow Cat" -> determine_cat_combo card state cat_combo_num 
                         combo_card_name
    | "Hairy Potato Cat" -> determine_cat_combo card state cat_combo_num 
                              combo_card_name
    | _ -> update_play_hand card player state
  else 
    raise (UnknownCard card)

let draw state =
  let player = current_p state in
  let card = state.draw_pile |> List.hd in
  let name = card.name in
  if (name = "Exploding Kitten") then draw_kitten card player state false
  else draw_other card player state false




