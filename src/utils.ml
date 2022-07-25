module String =
  struct
    include String

    let contains_string haystack needle =
      let nlen = length needle in
      let last_index = length haystack - nlen in
      let rec aux hi ni =
        if ni = nlen
        then true
        else if haystack.[hi] <> needle.[ni]
        then false
        else aux (hi + 1) (ni + 1)
      in
      let rec loop i =
        if i > last_index
        then false
        else if aux i 0
        then true
        else loop (i + 1)
      in
      loop 0
  end

let to_pascal_case str =
  String.split_on_char '_' str
  |> List.tl
  |> List.map String.lowercase_ascii
  |> List.map String.capitalize_ascii
  |> String.concat ""

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
