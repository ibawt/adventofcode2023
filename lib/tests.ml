let read_lines s =
  In_channel.with_open_text s In_channel.input_all
  |> String.split_on_char '\n' |> List.filter (fun x -> String.length x > 0 )

let is_digit = function '0' .. '9' -> true | _ -> false

let int_of_char c =
  Char.code c - (Char.code '0')

let print_list nums =
  let () = match nums with
    | [] -> Format.printf "empty list" ;
    | xs -> List.iter (fun n -> Format.printf "%d" n ) xs  in
  Format.printf "\n"


let sum_ends nums =
  let left = List.hd nums * 10 in
  let rec right = function
    | [n] -> n
    | [] -> failwith "invalid list"
    | _::xs -> right xs in
  left + right nums

let test_numbers = [ "1abc2" ; "pqr3stu8vwx" ; "a1b2c3d4e5f" ; "treb7uchet" ]

let test_one_part1 () =
  let extract_numbers s =
    List.rev @@ String.fold_left (fun acc c ->
        if is_digit c then
          (int_of_char c)::acc
        else
          acc
      ) [] s in

  let test_input = read_lines "test1.txt" in
  let nums = List.map extract_numbers test_input in
  let num = List.fold_left (fun acc x ->
                acc + sum_ends x ) 0 nums in

  Format.printf "%d\n" num ;
  ()



