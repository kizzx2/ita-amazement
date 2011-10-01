open Batteries_uni

(** Convenience function. Parse all mazes in the input string, solve them 
 *  and then return the string with path overlaid on the mazes *)
let solve input =
    let module P = Path_find.PathFinder (Maze) in
    let mazes    = Maze.parse_mazes input in
    
    let solve_maze maze = 
        let goal  = Maze.goal maze in
        let start = Maze.start maze in
        P.find maze start goal in
    
    let paths = List.map solve_maze mazes in
    let buf   = Buffer.create ((List.length mazes) * 10) in
    
    let add_solution maze path =
        Maze.string_of_maze_with_path maze path ^ "\n"
        |> Buffer.add_string buf in
        
    List.iter2 add_solution mazes paths;
    Buffer.contents buf
