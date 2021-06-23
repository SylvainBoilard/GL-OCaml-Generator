type enum = {
    name: string;
    value: string;
    groups: string list;
  }

type param = {
    name: string;
    c_type: string;
    pgroup: string;
    pclass: string;
  }

let null_param : param = Obj.magic 0

type command = {
    proto: param;
    params: param list;
    alias: string;
  }

type api = GL_API | GLES1_API | GLES2_API | GLSC2_API

let api_of_string = function
  | "gl" -> GL_API
  | "gles1" -> GLES1_API
  | "gles2" -> GLES2_API
  | "glsc2" -> GLSC2_API
  | _ -> failwith "api_of_string"

let string_of_api = function
  | GL_API -> "GL"
  | GLES1_API -> "GLES1"
  | GLES2_API -> "GLES2"
  | GLSC2_API -> "GLSC2"

type profile = Common | Core | Compatibility

let string_of_profile = function
  | Common -> "common"
  | Core -> "core"
  | Compatibility -> "compatibility"

type feature_list = {
    api: api;
    version: string;
    profile: profile;
    required_enums: string list;
    required_commands: string list;
    removed_enums: string list;
    removed_commands: string list;
  }

let rec attrs_assoc key = function
  | [] -> raise Not_found
  | ((_, name), value) :: _ when name = key -> value
  | _ :: tl -> attrs_assoc key tl

let rec attrs_assoc_opt key = function
  | [] -> None
  | ((_, name), value) :: _ when name = key -> Some value
  | _ :: tl -> attrs_assoc_opt key tl

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
  String.init (String.length str) (fun i -> if str.[i] = rep then chr else str.[i])

let pretty_enum_name str =
  String.split_on_char '_' str
  |> List.tl
  |> List.map String.lowercase_ascii
  |> List.map String.capitalize_ascii
  |> String.concat ""

let read_xml xml_input =
  let enums_by_name = Hashtbl.create 4096 in
  let commands_by_name = Hashtbl.create 4096 in
  let features = ref [] in
  let rec drop () = match Xmlm.input xml_input with
    | `El_start _ -> drop (); drop ()
    | `El_end -> ()
    | `Data _ | `Dtd _ -> drop ()
  in
  let read_enum attrs =
    drop ();
    try
      let name = attrs_assoc "name" attrs in
      let value = attrs_assoc "value" attrs in
      let groups = Option.fold ~none:[] ~some:(String.split_on_char ',') (attrs_assoc_opt "group" attrs) in
      if Hashtbl.mem enums_by_name name then (
        let previous = Hashtbl.find enums_by_name name in
        Printf.eprintf "Found duplicated enum \"%s\" (current value: \"%s\", new value: \"%s\"); keeping current value.\n%!"
          name previous.value value;
      ) else Hashtbl.add enums_by_name name { name; value; groups }
    with
    | Not_found ->
       Printf.eprintf "Found enum with missing attributes:\n%!";
       List.iter (fun ((_, name), value) -> Printf.eprintf "  %s = %s\n%!" name value) attrs
  in
  let read_param attrs =
    let pgroup = Option.value (attrs_assoc_opt "group" attrs) ~default:"" in
    let pclass = Option.value (attrs_assoc_opt "class" attrs) ~default:"" in
    let rec aux depth c_type name is_name = match Xmlm.input xml_input with
      | `El_start ((_, "name"), _) -> aux (depth + 1) c_type name true
      | `El_start ((_, "ptype"), _) -> aux (depth + 1) c_type name false
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at param scope.\n%!" name;
         drop (); aux depth  c_type name is_name
      | `El_end when depth > 1 -> aux (depth - 1) c_type name false
      | `El_end ->
         if pclass <> "" && not (contains_substring c_type "GLuint" || contains_substring c_type "GLsync") then
           Printf.eprintf "Found parameter \"%s\" with non-empty class \"%s\" and unexpected type \"%s\".\n%!"
             name pclass c_type;
         { name; c_type; pgroup; pclass }
      | `Data str when is_name && name <> "" ->
         Printf.eprintf "Found parameter with several names (current: \"%s\", new: \"%s\"); keeping current name.\n%!" name str;
         aux depth c_type name is_name
      | `Data str when is_name -> aux depth c_type str true
      | `Data str -> aux depth (if c_type = "" then str else String.concat " " [c_type; str]) name false
      | `Dtd _ -> aux depth c_type name is_name
    in
    aux 1 "" "" false
  in
  let rec enums_scope () = match Xmlm.input xml_input with
    | `El_start ((_, "unused"), _) -> drop (); enums_scope ()
    | `El_start ((_, "enum"), attrs) -> read_enum attrs; enums_scope ()
    | `El_start ((_, name), _) ->
       Printf.eprintf "Don’t know what to do with element %s at enums scope.\n%!" name;
       drop (); enums_scope ()
    | `El_end -> ()
    | `Data _ ->
       Printf.eprintf "Don’t know what to do with data at enums scope.\n%!";
       enums_scope ()
    | `Dtd _ -> enums_scope ()
  in
  let command_scope () =
    let rec aux proto params alias = match Xmlm.input xml_input with
      | `El_start ((_, "glx"), _)
      | `El_start ((_, "vecequiv"), _) -> drop (); aux proto params alias
      | `El_start ((_, "alias"), attrs) when alias <> "" ->
         Printf.eprintf "Command %s aliases several commands (current alias: %s; new alias: %s); keeping current alias.\n%!"
           proto.name alias (attrs_assoc "name" attrs);
         drop (); aux proto params alias
      | `El_start ((_, "alias"), attrs) ->
         let alias = attrs_assoc "name" attrs in
         drop (); aux proto params alias
      | `El_start ((_, "param"), attrs) ->
         let param = read_param attrs in
         aux proto (param :: params) alias
      | `El_start ((_, "proto"), _) when proto != null_param ->
         Printf.eprintf "Command %s declares several prototypes; keeping current prototype.\n%!" proto.name;
         drop (); aux proto params alias
      | `El_start ((_, "proto"), attrs) ->
         let proto = read_param attrs in
         aux proto params alias
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at command scope.\n%!" name;
         drop (); aux proto params alias
      | `El_end when proto == null_param ->
         Printf.eprintf "Found command without prototype.\n%!"
      | `El_end when Hashtbl.mem commands_by_name proto.name ->
         Printf.eprintf "Found duplicated command %s; keeping current command.\n%!" proto.name
      | `El_end -> Hashtbl.add commands_by_name proto.name { proto; params = List.rev params; alias }
      | `Data _ ->
         Printf.eprintf "Don’t know what to do with data at command scope.\n%!";
         aux proto params alias
      | `Dtd _ -> aux proto params alias
    in
    aux null_param [] ""
  in
  let rec commands_scope () = match Xmlm.input xml_input with
    | `El_start ((_, "command"), _) -> command_scope (); commands_scope ()
    | `El_start ((_, name), _) ->
       Printf.eprintf "Don’t know what to do with element %s at commands scope.\n%!" name;
       drop (); commands_scope ()
    | `El_end -> ()
    | `Data _ ->
       Printf.eprintf "Don’t know what to do with data at commands scope.\n%!";
       commands_scope ()
    | `Dtd _ -> commands_scope ()
  in
  let feature_scope attrs =
    let api = api_of_string (attrs_assoc "api" attrs) in
    let version = attrs_assoc "number" attrs in
    let rec aux1 depth enums commands = match Xmlm.input xml_input with
      | `El_start ((_, "type"), _) -> drop (); aux1 depth enums commands
      | `El_start ((_, "enum"), attrs) -> aux1 (depth + 1) (attrs_assoc "name" attrs :: enums) commands
      | `El_start ((_, "command"), attrs) -> aux1 (depth + 1) enums (attrs_assoc "name" attrs :: commands)
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at feature scope.\n%!" name;
         drop (); aux1 depth enums commands
      | `El_end when depth > 1 -> aux1 (depth - 1) enums commands
      | `El_end -> enums, commands
      | `Data _ ->
         Printf.eprintf "Don’t know what to do with data at require/remove scope.\n%!";
         aux1 depth enums commands
      | `Dtd _ -> aux1 depth enums commands
    in
    let required_enums = Array.make 3 [] in
    let required_commands = Array.make 3 [] in
    let removed_enums = Array.make 3 [] in
    let removed_commands = Array.make 3 [] in
    let int_of_attrs_profile attrs = match attrs_assoc_opt "profile" attrs with
      | Some "core" -> 1
      | Some "compatibility" -> 2
      | _ -> 0
    in
    let profile_of_int = function
      | 0 -> Common
      | 1 -> Core
      | 2 -> Compatibility
      | _ -> failwith "profile_of_int"
    in
    let rec aux2 () = match Xmlm.input xml_input with
      | `El_start ((_, "require"), attrs) ->
         let i = int_of_attrs_profile attrs in
         let enums, commands = aux1 1 required_enums.(i) required_commands.(i) in
         required_enums.(i) <- enums;
         required_commands.(i) <- commands;
         aux2 ()
      | `El_start ((_, "remove"), attrs) ->
         let i = int_of_attrs_profile attrs in
         let enums, commands = aux1 1 removed_enums.(i) removed_commands.(i) in
         removed_enums.(i) <- enums;
         removed_commands.(i) <- commands;
         aux2 ()
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at feature scope.\n%!" name;
         drop (); aux2 ()
      | `El_end ->
         for i = 0 to 2 do
           let required_enums = required_enums.(i) in
           let required_commands = required_commands.(i) in
           let removed_enums = removed_enums.(i) in
           let removed_commands = removed_commands.(i) in
           let profile = profile_of_int i in
           if required_enums <> [] || required_commands <> [] || removed_enums <> [] || removed_commands <> []
           then features := { api; version; profile; required_enums; required_commands; removed_enums; removed_commands } :: !features
         done
      | `Data _ ->
         Printf.eprintf "Don’t know what to do with data at feature scope.\n%!";
         aux2 ()
      | `Dtd _ -> aux2 ()
    in
    aux2 ()
  in
  let rec registry_scope () = match Xmlm.input xml_input with
    | `El_start ((_, "comment"), _)
    | `El_start ((_, "extensions"), _)
    | `El_start ((_, "types"), _) -> drop (); registry_scope ()
    | `El_start ((_, "commands"), _) -> commands_scope (); registry_scope ()
    | `El_start ((_, "enums"), _) -> enums_scope (); registry_scope ()
    | `El_start ((_, "feature"), attrs) -> feature_scope attrs; registry_scope ()
    | `El_start ((_, name), _) ->
       Printf.eprintf "Don’t know what to do with element %s at registry scope.\n%!" name;
       drop (); registry_scope ()
    | `El_end -> ()
    | `Data _ ->
       Printf.eprintf "Don’t know what to do with data at registry scope.\n%!";
       registry_scope ()
    | `Dtd _ -> registry_scope ()
  in
  let rec global_scope () =
    begin match Xmlm.input xml_input with
    | `El_start ((_, "registry"), _) -> registry_scope ()
    | `El_start ((_, name), _) ->
       Printf.eprintf "Don’t know what to do with element %s at global scope.\n%!" name;
       drop ()
    | `El_end -> assert false
    | `Data _ -> assert false
    | `Dtd _ -> ()
    end;
    if not (Xmlm.eoi xml_input) then global_scope ()
  in
  global_scope ();
  enums_by_name, commands_by_name, List.rev !features

let print_enums enums_by_name =
  Hashtbl.iter (fun _ (enum : enum) ->
      Printf.printf "Enum %s\n  Value: %s\n" enum.name enum.value;
      if enum.groups <> [] then (
        Printf.printf "  Groups:\n";
        List.iter (fun group -> Printf.printf "    %s\n" group) enum.groups
      )
    ) enums_by_name;
  Printf.printf "%!"

let print_groups enums_by_group =
  Hashtbl.iter (fun group enums ->
      Printf.printf "Group %s\n  Enums:\n" group;
      List.iter (fun (enum : enum) -> Printf.printf "    %s\n" enum.name) enums
    ) enums_by_group;
  Printf.printf "%!"

let print_commands commands_by_name =
  Hashtbl.iter (fun _ command ->
      Printf.printf "Command %s:\n" command.proto.name;
      if command.alias <> "" then Printf.printf "  Alias of %s\n" command.alias;
      Printf.printf "  Return type: \"%s\"" command.proto.c_type;
      if command.proto.pgroup <> "" || command.proto.pclass <> "" then (
        let prefix = ref " (" in
        if command.proto.pgroup <> "" then (
          Printf.printf "%sgroup: \"%s\"" !prefix command.proto.pgroup;
          prefix := "; "
        );
        if command.proto.pclass <> "" then (
          Printf.printf "%sclass: \"%s\"" !prefix command.proto.pclass;
          prefix := "; "
        );
        Printf.printf ")"
      );
      Printf.printf "\n  Parameters:\n";
      if command.params <> [] then
        List.iter (fun param ->
            Printf.printf "    %s (type: \"%s\"" param.name param.c_type;
            if param.pgroup <> "" then Printf.printf "; group: \"%s\"" param.pgroup;
            if param.pclass <> "" then Printf.printf "; class: \"%s\"" param.pclass;
            Printf.printf ")\n"
          ) command.params
    ) commands_by_name;
  Printf.printf "%!"

let print_features features =
  List.iter (fun feature ->
      Printf.printf "%s api, version %s\n" (string_of_api feature.api) feature.version;
      if feature.required_enums <> [] then (
        Printf.printf "  Required enums:\n";
        List.iter (Printf.printf "    %s\n") feature.required_enums
      );
      if feature.required_commands <> [] then (
        Printf.printf "  Required commands:\n";
        List.iter (Printf.printf "    %s\n") feature.required_commands
      );
      if feature.removed_enums <> [] then (
        Printf.printf "  Removed enums:\n";
        List.iter (Printf.printf "    %s\n") feature.removed_enums
      );
      if feature.removed_commands <> [] then (
        Printf.printf "  Removed commands:\n";
        List.iter (Printf.printf "    %s\n") feature.removed_commands
      )
    ) features;
  Printf.printf "%!"

let hash_enums_by_group enums_by_name =
  let enums_by_group = Hashtbl.create 4096 in
  Hashtbl.iter (fun _ enum ->
      List.iter (fun group ->
          let enums = Option.value (Hashtbl.find_opt enums_by_group group) ~default:[] in
          Hashtbl.replace enums_by_group group (enum :: enums)
        ) enum.groups
    ) enums_by_name;
  enums_by_group

let print_identical_groups enums_by_group =
  let rec aux = function
    | (g1, e1) :: ((g2, e2) :: _ as tl) ->
       if e1 = e2 then Printf.printf "Groups \"%s\" and \"%s\" are identical.\n%!" g1 g2;
       aux tl
    | _ -> ()
  in
  let open List in
  of_seq (Hashtbl.to_seq enums_by_group)
  |> rev_map (fun (group, enums) -> group, sort (fun (e1 : enum) e2 -> String.compare e1.name e2.name) enums)
  |> sort (fun (_, (e1 : enum list)) (_, e2) -> Stdlib.compare e1 e2)
  |> aux

let print_groups_partition enums_by_group =
  let module GroupSet = Set.Make(
    struct
      type t = string * enum list
      let compare (g1, _) (g2, _) = String.compare g1 g2
    end)
  in
  let prefix = ref "" in
  let rec aux (group, enums) rem_groups =
    let pulled_groups =
      List.fold_left (fun acc enum ->
          List.fold_left (fun acc group ->
              if List.mem group acc then acc else group :: acc
            ) acc enum.groups
        ) [] enums
    in
    Printf.printf "%s%s" !prefix group;
    prefix := ", ";
    List.fold_left (fun rem_groups group ->
        try
          let elt = GroupSet.find (group, []) rem_groups in
          aux elt (GroupSet.remove elt rem_groups)
        with Not_found -> rem_groups
      ) rem_groups pulled_groups
  in
  let rec loop groups =
    if not (GroupSet.is_empty groups) then (
      let elt = GroupSet.choose groups in
      let rem_groups = aux elt (GroupSet.remove elt groups) in
      Printf.printf "\n%!";
      prefix := "\n";
      loop rem_groups
    )
  in
  loop (GroupSet.of_seq (Hashtbl.to_seq enums_by_group))

let filter_by_feature enums_by_name commands_by_name features =
  let filtered_enums_by_name = Hashtbl.create 4096 in
  let filtered_commands_by_name = Hashtbl.create 4096 in
  let rec loop = function
    | [] -> ()
    | hd :: tl ->
       List.iter (Hashtbl.remove filtered_enums_by_name) hd.removed_enums;
       List.iter (Hashtbl.remove filtered_commands_by_name) hd.removed_commands;
       List.iter Hashtbl.(fun name -> replace filtered_enums_by_name name (find enums_by_name name)) hd.required_enums;
       List.iter Hashtbl.(fun name -> replace filtered_commands_by_name name (find commands_by_name name)) hd.required_commands;
       loop tl
  in
  loop features;
  let accessible_groups =
    Hashtbl.fold (fun _ command acc ->
        List.fold_left (fun acc param ->
            if List.mem param.pgroup acc then acc else param.pgroup :: acc
          ) acc (command.proto :: command.params)
      ) filtered_commands_by_name []
  in
  Hashtbl.filter_map_inplace (fun _ enum ->
      let groups = List.filter (ListLabels.mem ~set:accessible_groups) enum.groups in
      if groups = [] then (
        Printf.eprintf "Enum \"%s\" does not belong to any useable group; removed.\n%!" enum.name;
        None
      ) else Some {enum with groups}
    ) filtered_enums_by_name;
  filtered_enums_by_name, filtered_commands_by_name

let () =
  let ml_out, c_out =
    if Sys.argv.(0) <> "./generator.exe"
    then open_out "GL.ml", open_out "GL_stubs.c"
    else open_out "GL_preview.ml", open_out "GL_stubs_preview.c"
  in
  let opengl_registry_in = open_in "gl.xml" in
  let xml_input = Xmlm.make_input ~strip:true (`Channel opengl_registry_in) in
  try
    let enums_by_name, commands_by_name, features = read_xml xml_input in
    let features = List.filter (fun f -> f.api = GL_API && f.version <= "3.2" && f.profile <> Compatibility) features in
    let enums_by_name, commands_by_name = filter_by_feature enums_by_name commands_by_name features in
    let classes =
      Hashtbl.fold (fun _ command acc ->
          List.fold_left (fun acc param ->
              if param.pclass = "" || List.mem param.pclass acc then acc else param.pclass :: acc
            ) acc (command.proto :: command.params)
        ) commands_by_name []
    in
    let enums_by_group = hash_enums_by_group enums_by_name in
    (* DEBUG *)
    List.iter (fun feature ->
        Printf.printf "%s api, version %s, %s profile\n"
          (string_of_api feature.api) feature.version (string_of_profile feature.profile)
      ) features;
    Printf.printf "\n%!";
    print_enums enums_by_name;
    Printf.printf "\n%!";
    print_commands commands_by_name;
    Printf.printf "\n%!";
    print_groups enums_by_group;
    Printf.printf "\n%!";
    Printf.printf "Groups with no enum:\n  From parameters with no class:\n";
    Hashtbl.fold (fun _ command acc ->
        List.fold_left (fun acc param ->
            if param.pgroup = "" || param.pclass <> "" || Hashtbl.mem enums_by_group param.pgroup || List.mem param.pgroup acc
            then acc
            else param.pgroup :: acc
          ) acc (command.proto :: command.params)
      ) commands_by_name []
    |> List.iter (Printf.printf "    %s\n");
    Printf.printf "  From parameters with a class:\n";
    Hashtbl.fold (fun _ command acc ->
        List.fold_left (fun acc param ->
            if param.pgroup = "" || param.pclass = "" || Hashtbl.mem enums_by_group param.pgroup || List.mem param.pgroup acc
            then acc
            else param.pgroup :: acc
          ) acc (command.proto :: command.params)
      ) commands_by_name []
    |> List.iter (Printf.printf "    %s\n");
    Printf.printf "\n%!";
    Printf.printf "Classes:\n";
    List.iter (Printf.printf "  \"%s\"\n") classes;
    (* /DEBUG *)
    let open Printf in
    List.iter (fun pclass -> fprintf ml_out "type %s [@@immediate]\n" (replace_char ' ' '_' pclass)) classes;
    fprintf ml_out "\ntype 'k enum =\n";
    Hashtbl.iter (fun _ (enum : enum) ->
        let prefix = ref "<" in
        fprintf ml_out "  | %s : [" (pretty_enum_name enum.name);
        List.iter (fun group ->
            fprintf ml_out "%s`%s" !prefix group;
            prefix := "|"
          ) enum.groups;
        fprintf ml_out "] enum\n"
      ) enums_by_name;
    close_out ml_out;
    close_out c_out
  with
  | Xmlm.Error ((l, c), error) ->
     Printf.eprintf "Xmlm.Error: %s at (%d, %d)\n%!" (Xmlm.error_message error) l c
