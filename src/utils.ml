let starts_with str substr =
  String.(length str >= length substr && sub str 0 (length substr) = substr)

let ends_with str substr =
  String.(length str >= length substr && sub str (length str - length substr) (length substr) = substr)

let to_pascal_case str =
  String.split_on_char '_' str
  |> List.tl
  |> List.map String.lowercase_ascii
  |> List.map String.capitalize_ascii
  |> String.concat ""

let contains_substring haystack needle =
  let open String in
  let hlen, nlen = length haystack, length needle in
  let rec aux i =
    if i + nlen > hlen
    then false
    else if sub haystack i nlen = needle
    then true
    else aux (i + 1)
  in
  aux 0

let replace_char rep chr str =
  String.(init (length str) (fun i -> if str.[i] = rep then chr else str.[i]))

let is_weakly_ordered cmp l =
  let rec ascend = function
    | hd1 :: hd2 :: _ when cmp hd1 hd2 > 0 -> false
    | _ :: tl -> ascend tl
    | [] -> true
  in
  let rec descend = function
    | hd1 :: hd2 :: _ when cmp hd1 hd2 < 0 -> false
    | _ :: tl -> descend tl
    | [] -> true
  in
  let rec undecided = function
    | hd1 :: (hd2 :: _ as tl) when cmp hd1 hd2 > 0 -> descend tl
    | hd1 :: (hd2 :: _ as tl) when cmp hd1 hd2 < 0 -> ascend tl
    | _ :: tl -> undecided tl
    | [] -> true
  in
  undecided l

let replace_if_reserved_caml_word = function
  | "type" -> "kind"
  | "end" -> "finish"
  | "val" -> "value"
  | s -> s

let replace_if_reserved_c_word = function
  | "value" -> "ml_value" (* not a reserved word, but collides with the type name of OCaml values. *)
  | s -> s

let remove_gl_prefix str =
  String.(uncapitalize_ascii (sub str 2 (length str - 2)))
