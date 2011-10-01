module type MazeSig =
    sig
        type t
        type direction
        
        module Coord : Map.OrderedType
        
        val directions : direction list
        val distance   : Coord.t -> Coord.t -> int
        val go         : t -> Coord.t -> direction -> Coord.t option
    end

module PathFinder : functor (M : MazeSig) ->
    sig
        val find : M.t -> M.Coord.t -> M.Coord.t -> M.Coord.t list
    end
