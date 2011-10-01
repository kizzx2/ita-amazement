type t
module Coord : sig
    type t
    val compare : t -> t -> int
end

type direction
val directions : direction list

val start      : t -> Coord.t
val goal       : t -> Coord.t

(** Heuristic distance from a to b *)
val distance   : Coord.t -> Coord.t -> int
val go         : t -> Coord.t -> direction -> Coord.t option

val parse_mazes              : string -> t list
val string_of_maze_with_path : t -> Coord.t list -> string