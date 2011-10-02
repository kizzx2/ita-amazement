open Batteries_uni

module type MazeSig = sig
    type t
    type coord
    type direction

    val directions : direction list
    
    val distance : coord -> coord -> int
    val go       : t -> coord -> direction -> coord option
end
    
let in_list x l = List.index_of x l |> Option.is_some

module PathFinder (M : MazeSig) = struct
    let directions = M.directions
    
    (* A simplified A* algorithm *)
    let find maze start goal =
    
        let current     = ref start in
        let closed_list = ref [] in
        let parent_map  = Hashtbl.create 50 in

        (* a (score * coord) list *)
        let open_list   = ref [M.distance start goal, !current] in
    
        let parent_of coord = Hashtbl.find parent_map coord in

        (* Add adjacent paths to open_list *)
        let scan coord =
            List.map (M.go maze coord) directions
            |> List.filter Option.is_some
            |> List.map Option.get
            |> List.filter (not -| flip in_list !closed_list)
            |> List.iter (fun dest ->
                open_list := (M.distance dest goal, dest) :: !open_list;
                Hashtbl.add parent_map dest coord ) in
    
        let retrace_path from =
            Enum.seq from parent_of ((!=) start)
            |> List.of_enum |> List.rev |> List.cons start in
    
        let open Return in
        
        label (fun k ->
            while not **> List.is_empty !open_list do
                if !current = goal then return k (retrace_path !current);
                
                (* Choose the lowest score from the open list
                 * and switch it to the closed list, mark it as current.
                 *
                 * Could use a heap for better performance.
                 *)
                open_list   := List.sort !open_list;
                current     := List.hd !open_list |> snd;
                open_list   := List.drop 1 !open_list;
                closed_list := !current :: !closed_list;
    
                scan !current
            done;
    
            raise Not_found
        )
end
