(** Parses commands issued by the player. *)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["play watermelon cat"], then the object phrase 
      is [["watermelon"; "cat"]].
    - If the player command is ["play watermelon     cat"], then the object 
      phrase is again [["watermelon"; "cat"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  (* | Pass *)
  | Play of object_phrase
  | Draw
  | Quit
  | Instruction of object_phrase
  | End

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [is_empty str] is false if the string provided is empty and false otherwise 
*)
val is_empty: string -> bool


(** [get_object_phrase split_list] are the elements after the first element. If 
    the first element isn't play or instruction or there isn't anything after 
    that word, it is [ [] ] *)
val get_object_phrase: string list -> object_phrase

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    play   watermelon   cat   "] is [Play ["watermelon"; "cat"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "play",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "play" and there is an empty object phrase.

*)
val parse : string -> command