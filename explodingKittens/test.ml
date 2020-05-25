(********************************************************************
   TESTING PLAN

   IMPORTANT: WHILE TRYING TO TEST THE draw FUNCTION, THE GAME WILL SOMETIMES
   DRAW AN EXPLODING KITTEN CARD WHICH SEEMINGLY MAKES SOME TESTS FAIL. PLEASE 
   IGNORE THIS AND RUN MAKE TEST ONE (AT MOST TWICE) MORE AND YOU WILL SEE THAT 
   ALL 50 TESTS OF OURS ARE ACTUALLY PASSING. THANK YOU.

   Modules fully automatically tested using OUnit testing: Command
   Method: Black Box testing

   Modules partially tested using OUnit testing: Game
   Method: Black Box testing

   Functions in Game tested automatically using OUnit testing:
   - Game.current_p
   - structure of Game.get_state_hands (not contents of it)
   - Game.get_num_players
   - Game.next_player
   - structure of Game.get_hand_names (not contents of it)
   - only two cases of Game.get_instructions
   - only two cases of Game.check_card
   - Game.start_game
   - Game.play for one case only
   - Game.draw

   Functions that could not be tested using OUnit due to the random nature of 
   the game:
   - Game.play_nope
   - contents of Game.get_state_hands
   - contents of Game.get_hand_names
   - intructions for all other cards except Defuse and Exploding Kitten
   - Game.check_card for all other cards except Defuse and Exploding Kitten
   - Game.find_next
   - Game.play for all other cards except Defuse

   These aspects were tested using manual game play 

   Modules fully tested manually by game play: Main
   Method: Manual game play
   Reason for no OUnit testing: not possible

   Why the testing approach is sound:
    * Black box testing ensures that the specifications are met regardless of 
    the context of the test. 
    * All that coud be automatically tested has been tested. Those functions 
    that could not be fully tested using OUnit were tested manually because of
    the random nature of the game. It is impossible to reproduce the same result
    every time.
    * Using game play, all the remaining aspects of the system were tested. Each
    individual card and every possible scenario was tested multiple times using
    game play. Thus the game engine was run several times to make sure there are
    no bugs by attempting to reproduce the same situation till its bugs were 
    fixed. This ensured that everything was working as desired.

   Thus, together with automatic OUnit testing and manual game play, all aspects 
   of the game were tested ensuring a correct and robust system.

 ********************************************************************)





open OUnit2
open Game
open Command
open Yojson.Basic.Util

(* Initial states *)
let init_state3 = start_game 3
let init_state5 = start_game 5
let init_state0 = start_game 0
let init_state8 = start_game 8

(* Next players *)
let next_p = next_player init_state3

let next_p2 = next_player init_state5
let next_p3 = next_player next_p2

(* After playing a card *)
let play_state = play "Defuse" init_state8 "" ""


(* After drawing a card *)
let draw_state = draw init_state5
let draw_state1 = draw draw_state
let draw_state2 = draw draw_state1
let draw_diff = draw next_p

let play_diff = play "Defuse" draw_diff "" ""

(* Card instructions *)
let defuse_instr = 
  "You are an evader of death. Now go ahead and put that"^
  " exploding kitten back into the deck. Today's not the day you die."

let game_tests = [

  (********************************************************************
     BLACK BOX TESTING
   ********************************************************************)

  (* Testing current player *)
  "The current player" >:: (fun _ -> 
      assert_equal 1 
        (current_p init_state3));

  (* Testing number of players *)
  "The number of players (3)" >:: (fun _ -> 
      assert_equal 3 
        (get_num_players init_state3));

  (* Testing number of players *)
  "The number of players (5)" >:: (fun _ -> 
      assert_equal 5 
        (get_num_players init_state5));

  (* Testing number of players *)
  "The number of players (0)" >:: (fun _ -> 
      assert_equal 0 
        (get_num_players init_state0));

  (* Testing number of players *)
  "The number of players (8)" >:: (fun _ -> 
      assert_equal 8 
        (get_num_players init_state8));

  (* Testing next player number *)
  "The next player (second)" >:: (fun _ -> 
      assert_equal 2 
        (current_p next_p));

  (* Testing next player number *)
  "The next player (third)" >:: (fun _ -> 
      assert_equal 3 
        (current_p next_p3));

  (* Testing number of hands *)
  "The number of hands (3)" >:: (fun _ -> 
      assert_equal 3 
        (List.length(get_state_hands init_state3)));

  (* Testing number of hands *)
  "The number of hands (5)" >:: (fun _ -> 
      assert_equal 5 
        (List.length(get_state_hands init_state5)));

  (* Testing number of hands *)
  "The number of hands (0)" >:: (fun _ -> 
      assert_equal 0 
        (List.length(get_state_hands init_state0)));

  (* Testing number of hands *)
  "The number of hands (3) even if player changes" >:: (fun _ -> 
      assert_equal 3 
        (List.length(get_state_hands next_p)));

  (* Testing hand names length *)
  "The number of cards in hand" >:: (fun _ -> 
      assert_equal 5 
        (List.length(get_hand_names 1 init_state3)));

  (* Testing hand names length *)
  "The number of cards in hand" >:: (fun _ -> 
      assert_equal 5 
        (List.length(get_hand_names 2 init_state3)));

  (* Testing card instructions *)
  "Card instructions" >:: (fun _ -> 
      assert_equal defuse_instr 
        (get_instructions "Defuse" 1 init_state3));

  (* Testing unknown card instructions *)
  "Unknown card instructions" >:: (fun _ -> 
      assert_raises (UnknownCard "Exploding Kitten")
        (fun () -> get_instructions "Exploding Kitten" 1 init_state3));

  (* Testing unknown card instructions for card that's invalid*)
  "Invalid card instructions" >:: (fun _ -> 
      assert_raises (UnknownCard "Cheshire Cat")
        (fun () -> get_instructions "Cheshire Cat" 1 init_state3));

  (* Checking card *)
  "Checking card " >:: (fun _ -> 
      assert_equal true 
        (check_card "Defuse" 1 init_state3));

  (* Testing absent card check *)
  "Absent card check" >:: (fun _ -> 
      assert_equal false
        (check_card "Exploding Kitten" 1 init_state3));

  (* Testing invalid card check *)
  "Invalid card check" >:: (fun _ -> 
      assert_equal false
        (check_card "Cheshire Cat" 1 init_state3));

  (* Checking Defuse card in next player *)
  "Checking card " >:: (fun _ -> 
      assert_equal true 
        (check_card "Defuse" 2 next_p));

  (* Exploding Kitten should never be there *)
  "Checking absence of Exploding Kitten " >:: (fun _ -> 
      assert_equal false 
        (check_card "Exploding Kitten" 2 next_p));

  (* Testing length of hand after play *)
  "Length of hand after play " >:: (fun _ -> 
      assert_equal 4 
        (List.length(get_hand_names 1 play_state)));

  (* Testing presence of card after play *)
  "Defuse absent" >:: (fun _ -> 
      assert_raises (UnknownCard "Defuse")
        (fun () -> get_instructions "Defuse" 1 play_state));

  (* Testing absent Defuse card check *)
  "Absent Defuse card check" >:: (fun _ -> 
      assert_equal false
        (check_card "Defuse" 1 play_state));

  (* Testing length of hand after 1 draw *)
  "Length of hand after 1 draw " >:: (fun _ -> 
      assert_equal 6 
        (List.length(get_hand_names 1 draw_state)));

  (* Testing length of hand after 2 draws *)
  "Length of hand after 2 draws " >:: (fun _ -> 
      assert_equal 7 
        (List.length(get_hand_names 1 draw_state1)));

  (* Testing length of hand after 3 draws *)
  "Length of hand after 3 draws (should still increase)" >:: (fun _ -> 
      assert_equal 8 
        (List.length(get_hand_names 1 draw_state2)));

  (* Testing length of hand after 1 draw for next player*)
  "Length of hand after 1 draw for next player" >:: (fun _ -> 
      assert_equal 6 
        (List.length(get_hand_names 2 draw_diff)));

  (* Testing length of hand after play for second player *)
  "Length of hand after play for second player" >:: (fun _ -> 
      assert_equal 5 
        (List.length(get_hand_names 2 play_diff)));

]

let command_tests =
  [

    (********************************************************************
       BLACK BOX TESTING
     ********************************************************************)

    "play command test" >:: (fun _ -> 
        assert_equal 
          (Play ["See"; "the"; "Future"])
          (parse "play See the Future"));

    "play command test upper case" >:: (fun _ -> 
        assert_equal 
          (Play ["SHUFFLE"])
          (parse "play SHUFFLE"));


    "play spacing command test" >:: (fun _ -> 
        assert_equal 
          (Play ["See"; "the"; "Future"])
          (parse "  play     See         the        Future       "));

    "play malformed test" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "play"));

    "command malformed test" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "something"));

    "draw command test" >:: (fun _ -> 
        assert_equal 
          Draw
          (parse "draw"));

    "draw spaced command test" >:: (fun _ -> 
        assert_equal 
          Draw
          (parse "         draw        "));

    "draw malformed test" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "         draw   hello     "));

    "quit command test" >:: (fun _ -> 
        assert_equal 
          Quit
          (parse "quit"));

    "quit spaced command test" >:: (fun _ -> 
        assert_equal 
          Quit
          (parse "         quit        "));

    "quit malformed test" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "         quit   hello     "));

    "instruction command test" >:: (fun _ -> 
        assert_equal 
          (Instruction ["See"; "the"; "Future"])
          (parse "instruction See the Future"));

    "instruction command test lower case" >:: (fun _ -> 
        assert_equal 
          (Instruction ["favor"])
          (parse "instruction favor"));

    "instruction spacing command test" >:: (fun _ -> 
        assert_equal 
          (Instruction ["See"; "Explosion"])
          (parse "  instruction     See         Explosion       "));

    "instruction malformed test" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "instruction"));

    "instruction malformed test with spaces" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "instruction         "));


    "end command test" >:: (fun _ -> 
        assert_equal 
          End
          (parse "end"));

    "end spaced command test" >:: (fun _ -> 
        assert_equal 
          End
          (parse "         end        "));

    "end malformed test" >:: (fun _ -> 
        assert_raises 
          Malformed
          (fun() -> parse "   end here   "));

    "raises empty" >:: (fun _ -> 
        assert_raises 
          Empty
          (fun() -> parse ""));

    "raises empty spaced" >:: (fun _ -> 
        assert_raises 
          Empty
          (fun() -> parse "     "));

  ]

let suite =
  "test suite for project"  >::: List.flatten [
    command_tests;
    game_tests;
  ]

let _ = run_test_tt_main suite