open Batteries_uni

module type MazeSig = sig
    type t

    module Coord : sig
        type t
        val compare : t -> t -> int
    end
    
    type direction
    val directions : direction list
    
    val distance : Coord.t -> Coord.t -> int
    val go       : t -> Coord.t -> direction -> Coord.t option
end
    
let in_list x l = match List.index_of x l with
    | Some _ -> true
    | None   -> false

module PathFinder (M : MazeSig) = struct
    
    module CoordMap = Map.Make (M.Coord)

    let directions = M.directions
    
    (* A simplified A* algorithm *)
    let find maze start goal =
    
        let current     = ref start in
        let open_list   = ref [M.distance start goal, !current] in
        let closed_list = ref [] in
        let parent_map  = ref CoordMap.empty in
    
        let parent_of coord = CoordMap.find coord !parent_map in
            
        (* Try to add direction to open_list *)
        let try_direction coord dir =
            match M.go maze coord dir with
            | Some dest when not **> in_list dest !closed_list ->
                open_list  := (M.distance dest goal, dest) :: !open_list;
                parent_map := CoordMap.add dest coord !parent_map;
            | None
            | Some _ -> () in
    
        (* Add adjacent paths to open_list *)
        let scan coord = List.iter (try_direction coord) directions in
    
        let retrace_path =
            let rec aux acc = function
                | curr when curr = start -> curr :: acc
                | curr -> aux (curr :: acc) (parent_of curr) in
            aux [] in
    
        let open Return in
        
        label (fun k ->
            while not **> List.is_empty !open_list do
                if !current = goal then return k (retrace_path !current);
                
                (* Choose the lowest score from the open list
                 * and switch it to the closed list, mark it as current *)
                open_list   := List.sort !open_list;
                current     := List.hd !open_list |> snd;
                open_list   := List.drop 1 !open_list;
                closed_list := !current :: !closed_list;
    
                scan !current
            done;
    
            raise Not_found
        )
end
