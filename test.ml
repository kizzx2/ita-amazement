open OUnit
open Batteries_uni

let test_maze in_file out_file () =
    let pipe_thru f file =
        File.with_file_in file (String.trim -| f -| IO.read_all) in
    let output    = pipe_thru Solve.solve in_file in
    let reference = pipe_thru identity out_file in
    assert_equal reference output

let make_test_case in_name ref_name =
    let filename_of name = "mazes/" ^ name ^ ".txt" in
    in_name >:: test_maze (filename_of in_name) (filename_of ref_name)

let suite = "amazement" >:::
    (List.map (uncurry make_test_case)
	    [ "sample-mazes", "sample-maze-solutions"
        ; "input1", "output1"
        ; "input2", "output2"
        ; "input3", "output3"
        ; "input4", "output4"
        ; "input5", "output5"
        ; "input6", "output6"
	    ])

let _ = run_test_tt_main suite
