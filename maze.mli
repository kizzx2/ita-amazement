(** Parsing/printing of maze and maze "collision detection" logic *)

type t
type coord

type direction
val directions : direction list

val start      : t -> coord
val goal       : t -> coord

(** Heuristic distance from a to b *)
val distance   : coord -> coord -> int
val go         : t -> coord -> direction -> coord option

val parse_mazes              : string -> t list

(** String of the maze with path overlaid on it. *)
val string_of_maze_with_path : t -> coord list -> string
