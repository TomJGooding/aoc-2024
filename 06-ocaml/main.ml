let load_map_matrix (file: string) : char array array =
    let lines = In_channel.with_open_text file In_channel.input_lines in
    let num_cols = String.length (List.hd lines) in

    let map_matrix = Array.of_list (List.map (fun line ->
        Array.init num_cols (String.get line)
    ) lines) in

    map_matrix

let find_initial_position (map_matrix: char array array) : int * int =
    let result =
        Array.find_mapi (fun row_index row ->
            match Array.find_index ((=) '^') row with
            | Some col_index -> Some (row_index, col_index)
            | None -> None
        ) map_matrix
    in
    match result with
    | Some position -> position
    | None -> failwith "Invalid input, expected '^' in map"

let cycle_directions () =
    let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)] in
    let position = ref 0 in
    fun () ->
        let direction = List.nth directions !position in
        position := (!position + 1) mod (List.length directions);
        direction

let next_direction = cycle_directions ()

module PositionSet = Set.Make(struct
    type t = int * int
    let compare (a, b) (c, d) =
        if a = c then
            compare b d
        else
            compare a c
end)

let solve_part_one (map_matrix: char array array) : int =
    (* This isn't idiomatic OCaml but I'm just not smart enough to figure out *)
    (* how to solve this the functional way! *)
    let guard_position = find_initial_position map_matrix in
    let curr_row = ref (fst guard_position) in
    let curr_col = ref (snd guard_position) in

    let direction = ref (next_direction ()) in

    let visited = ref PositionSet.empty in

    let leaving_map = ref false in
    while not !leaving_map do
        let new_row = !curr_row + fst !direction in
        let new_col = !curr_col + snd !direction in
        if new_row < 0 || new_row >= Array.length map_matrix ||
            new_col < 0 || new_col >= Array.length map_matrix.(0) then
                leaving_map := true
        else if map_matrix.(new_row).(new_col) = '#' then
            direction := next_direction ()
        else (
            curr_row := new_row;
            curr_col := new_col;
            visited := PositionSet.add (!curr_row, !curr_col) !visited;
        )
    done;

    PositionSet.cardinal !visited

let main () =
    if Array.length Sys.argv < 2 then
        failwith "Input file not provided";

    let input_file = Sys.argv.(1) in
    let map_matrix = load_map_matrix input_file in

    print_endline "--- Day 6: Guard Gallivant ---";
    Printf.printf "Answer for part 1: %d\n" (solve_part_one map_matrix)

let () = main()
