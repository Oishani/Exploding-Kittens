(** Sets up the entire game. *)

(** The type of card names. *)
type card_name = string

(** The type of card instructions. *)
type instruction = string

(** The type of players. *)
type player = int

(** Raised when a card that is not in a player's hand is played. *)
exception UnknownCard of card_name

(** The abstract type representing a card. *)
type card

(** The type of a deck of cards. *)
type deck = card list

(** The type of a player's hand. *)
type hand = card list 

(** The abstract type representing a game state. *)
type state

(** [current_p state] is the current player in [state]. *)
val current_p : state -> int

(* For testing purposes in main.ml only *)
(* val get_draw : state -> card list *)

(** [get_names cards] is a list of names of cards in [cards]. *)
val get_names : card list -> card_name list

(** [get_state_hands state] is the current list of hands in [state]. *)
val get_state_hands: state -> (player * hand) list

(** [get_num_players state] is the number of players in [state]. *)
val get_num_players: state -> int

(** [next_player state] updates the current player field of [state] to the 
    next player. *)
val next_player : state -> state

(** [play_nope state card_name iter] is the updated state in which [card_name] 
    is removed from the current_player's hand [iter] number of times
    and a Nope card is removed from the next player's hand  *)
val play_nope: state -> card_name -> int -> state

(** [get_hand_names player state] is a list of names of the cards in the hand 
    of [player] in [state]. *)
val get_hand_names : player -> state -> card_name list

(** [get_instructions card player state] is the instructions of card [card] of 
    player [player] in state [state]. 
    Raises [UnknownCard card] if [card] is not a card in [player]'s hand. *)
val get_instructions : card_name -> player -> state -> instruction

(** [check_card card player state] is true if [card] is in [player]'s hand in 
    [state]. False otherwise. *)
val check_card: card_name -> player -> state -> bool 

(** [find_next player hands num_players] finds the player after [player]. 
    Requires : at least two players still in the game. *)
val find_next: player -> (int * 'a) list -> int -> player

(** [start_game] is the starting state of the game with a draw pile and 
    players' hands. *)
val start_game : int -> state

(** [play card player state cat_combo_num combo_card_name] plays [card] for 
    [player] in [state] and returns the new state. 
    Raises : Unknown [card] exception if an invalid card is entered or if it 
    is not in the player's hand or if [cat_combo_num] is not "2" or "3". *)
val play : card_name -> state -> string -> card_name -> state

(** [draw player state] returns a new state by updating [state] when [player] 
    draws a card from [state]'s draw pile. *)
val draw : state -> state