module type MazeSig =
    sig
        type t
        type coord
        type direction
        
        val directions : direction list
        val distance   : coord -> coord -> int
        val go         : t -> coord -> direction -> coord option
    end

module PathFinder : functor (M : MazeSig) ->
    sig
        val find : M.t -> M.coord -> M.coord -> M.coord list
    end
