open Batteries_uni

type grid      = Space | HWall | VWall | Path
type direction = Up | Down | Left | Right
type coord     = int * int
type t         = grid array array

module Coord = struct
    type t      = coord
    let compare = compare
end

let grid_of_char = function
    | ' ' -> Space
    | '_' -> HWall
    | '|' -> VWall
    | _   -> invalid_arg "grid"

let char_of_grid = function
    | Space -> ' '
    | HWall -> '_'
    | VWall -> '|'
    | Path -> 'X'

let directions = [Up; Down; Left; Right]

let go maze (row, col) = function
    | Up    -> if maze.(row-1).(col) = Space then Some (row-2,col) else None
    | Down  -> if maze.(row+1).(col) = Space then Some (row+2,col) else None
    | Left  -> if maze.(row).(col-1) = Space then Some (row,col-3) else None
    | Right -> if maze.(row).(col+2) = Space then Some (row,col+3) else None

let distance (r1, c1) (r2, c2) =
    let sqrt_int = int_of_float -| sqrt -| float_of_int in
    let sq a     = a * a in
    sqrt_int ( sq (r1-r2) + sq (c1-c2) )

let start maze = let height = Array.length maze in (height - 2, 1)
let goal maze  = let width  = Array.length maze.(0) in (1, width - 2)

(* Simple hand-rolled parser. Sufficient for our simple need *)
let parse_maze =
    let is_grid_char c = c = '|' || c = '_' || c = ' ' in

    let parse_line str =
        let len = String.length str in
        let sanitized =
            String.sub str 5 (len-5) |> String.filter is_grid_char in
        String.explode sanitized |> List.map grid_of_char in

    Array.of_list
    -| List.map (Array.of_list -| parse_line)
    -| Str.split (Str.regexp_string "\n") 

let parse_mazes =
    List.map parse_maze
    -| Str.split (Str.regexp_string "\n\n")
    -| Str.global_substitute (Str.regexp_string "\r\n") (const "\n")

let string_of_maze maze =
    let padding = "     " in
    let string_of_line =
        String.implode -| Array.to_list -| Array.map char_of_grid in
    let num_lines = Array.length maze in
    let buf = Buffer.create (num_lines * 80) in

    padding ^ string_of_line maze.(0) ^ "ITA\n" |> Buffer.add_string buf;
    flip Enum.iter (1--(num_lines - 2))
	    ( fun i ->
          Buffer.add_string buf padding
        ; Buffer.add_string buf (string_of_line maze.(i))
        ; Buffer.add_char buf '\n'
        );
    "Start" ^ string_of_line maze.(num_lines - 1) ^ "\n"
        |> Buffer.add_string buf;
    Buffer.contents buf

let string_of_maze_with_path maze path =
    let maze' = Array.copy maze in
    let overlay_coord (row,col) =
        maze'.(row).(col)   <- Path;
        maze'.(row).(col+1) <- Path in

    List.iter overlay_coord path;
    string_of_maze maze'
