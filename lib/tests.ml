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
  let sum =
    let left = (List.hd nums) * 10 in
    let rec right = function
      | [n] -> n
      | [] -> failwith "invalid list"
      | _::xs -> right xs in
    left + right nums in
  Format.printf "sum_ends\n";
  print_list nums ;
  Format.printf "sum : %d\n" sum ;
  sum

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

let nums = Array.of_list [ "one" ; "two" ; "three" ; "four" ; "five" ;
                           "six" ; "seven" ; "eight" ; "nine" ]

let test_one_part2 () =
  let matches_word_number s n n_s  =
    match String.starts_with s ~prefix:n_s with
    | true -> ( String.sub s 1 ((String.length s) - 1), Some (n+1) )
    | false -> ( String.sub s 1 ((String.length s)-1), None )
  in

  let rec extract_numbers acc s =
    Format.printf "extract_numbers: %s\n" s;
    print_list acc;
    if String.length s = 0 then
      acc
    else
      let ch = String.get s 0 in
      if is_digit ch then
        if String.length s = 1 then
          (int_of_char ch)::acc
        else
          extract_numbers ((int_of_char ch)::acc) (String.sub s 1 ((String.length s) -1))
      else
        let rec find_word_number n =
          match n with
          | 9 -> extract_numbers acc (String.sub s 1 ((String.length s) -1))
          | _ -> (
            let n_s = Array.get nums n in
            match matches_word_number s n n_s with
            | s, (Some n) -> extract_numbers (n::acc) s
            | _, None -> find_word_number (n + 1) ) in
        find_word_number 0
  in

  let _test_numbers = [
      "eighthree"]
  in

  let _test_numbers = [
      "two1nine" ;
        "eightwothree" ;
        "abcone2threexyz" ;
        "xtwone3four" ;
        "4nineeightseven2" ;
        "zoneight234" ;
        "7pqrstsixteen"
    ] in

  let test_input = read_lines "test1.txt" in
  let nums = List.map (fun line ->
                 let nums = List.rev @@ extract_numbers [] line in
                 nums
               ) test_input in
  let num = List.fold_left (fun acc x ->
                let sum = sum_ends x in
                Format.printf "sum: %d\n" sum ;
                acc + sum) 0 nums in

  Format.printf "%d\n" num ;

