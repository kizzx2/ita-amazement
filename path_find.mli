module type MazeSig =
    sig
        type t
        type direction
        
        module Coord : sig type t val compare : t -> t -> int end
        
        val directions : direction list
        val distance   : Coord.t -> Coord.t -> int
        val go         : t -> Coord.t -> direction -> Coord.t option
    end

module PathFinder : functor (M : MazeSig) ->
    sig
        val find : M.t -> M.Coord.t -> M.Coord.t -> M.Coord.t list
    end
