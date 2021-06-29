open Registry

let starts_with str substr =
  String.(length str >= length substr && sub str 0 (length substr) = substr)

let ends_with str substr =
  String.(length str >= length substr && sub str (length str - length substr) (length substr) = substr)

let replace_if_reserved_caml_word = function
  | "type" -> "kind"
  | "end" -> "finish"
  | "val" -> "value"
  | s -> s

let pretty_enum_name str =
  String.split_on_char '_' str
  |> List.tl
  |> List.map String.lowercase_ascii
  |> List.map String.capitalize_ascii
  |> String.concat ""

let caml_type_of_param param = match param.ptype with
  | _ when param.pclass <> "" && ends_with param.ptype "*" -> param.pclass ^ " array"
  | _ when param.pclass <> "" -> param.pclass
  | "void" -> "unit"
  | "GLenum" when param.pgroup = "ShaderBinaryFormat" || param.pgroup = "ProgramBinaryFormat" -> "int"
  | "GLenum *" when param.pgroup = "ShaderBinaryFormat" || param.pgroup = "ProgramBinaryFormat" && param.plength = "1" -> "int"
  | "GLenum" -> Printf.sprintf "[`%s] enum" param.pgroup
  | "GLenum *" when param.plength = "1" -> Printf.sprintf "[`%s] enum" param.pgroup
  | "GLsync" -> "sync"
  | "GLboolean" -> "bool"
  | "GLboolean *" when param.plength = "1" -> "bool"
  | "GLboolean *" -> "bool array"
  | "const GLchar *" -> "string"
  | "const GLchar *const*" -> "string array"
  | "GLchar *" -> "bytes"
  | "GLfloat" | "GLdouble" -> "float"
  | "GLfloat *" when param.plength = "1" -> "float"
  | "const GLfloat *" | "GLfloat *" -> "float array"
  | "GLint" | "GLuint" | "GLsizei" | "GLintptr" | "GLsizeiptr" -> "int"
  | "const GLint *" | "const GLuint *" | "const GLsizei *" -> "int array"
  | "GLint *" | "GLuint *" | "GLsizei *" when param.plength = "1" -> "int"
  | "GLint *" | "GLuint *" | "GLsizei *" -> "int array"
  | "GLbitfield" when param.pgroup = "" -> "int"
  | "GLbitfield" -> Printf.sprintf "[`%s] enum list" param.pgroup
  | _ -> "'z"

let print_enums enums_by_name =
  Hashtbl.iter (fun _ enum ->
      Printf.eprintf "Enum %s\n  Value: %s\n" enum.name enum.value;
      if enum.groups <> [] then (
        Printf.eprintf "  Groups:\n";
        List.iter (fun group -> Printf.eprintf "    %s\n" group) enum.groups
      )
    ) enums_by_name

let print_groups enums_by_group =
  Hashtbl.iter (fun group enums ->
      Printf.eprintf "Group %s\n  Enums:\n" group;
      List.iter (fun enum -> Printf.eprintf "    %s\n" enum.name) enums
    ) enums_by_group

let print_commands commands_by_name =
  Hashtbl.iter (fun _ command ->
      Printf.eprintf "Command %s:\n" command.proto.pname;
      if command.alias <> "" then Printf.eprintf "  Alias of %s\n" command.alias;
      Printf.eprintf "  Return type: \"%s\"" command.proto.ptype;
      if command.proto.pgroup <> "" || command.proto.pclass <> "" then (
        let prefix = ref " (" in
        if command.proto.pgroup <> "" then (
          Printf.eprintf "%sgroup: \"%s\"" !prefix command.proto.pgroup;
          prefix := "; "
        );
        if command.proto.pclass <> "" then (
          Printf.eprintf "%sclass: \"%s\"" !prefix command.proto.pclass;
          prefix := "; "
        );
        Printf.eprintf ")"
      );
      Printf.eprintf "\n  Parameters:\n";
      if command.params <> [] then
        List.iter (fun param ->
            Printf.eprintf "    %s (type: \"%s\"" param.pname param.ptype;
            if param.pgroup <> "" then Printf.eprintf "; group: \"%s\"" param.pgroup;
            if param.pclass <> "" then Printf.eprintf "; class: \"%s\"" param.pclass;
            if param.plength <> "" then Printf.eprintf "; length: \"%s\"" param.plength;
            Printf.eprintf ")\n"
          ) command.params
    ) commands_by_name

let print_features features =
  List.iter (fun feature ->
      Printf.eprintf "%s api, version %s\n" (string_of_api feature.api) feature.version;
      if feature.required_enums <> [] then (
        Printf.eprintf "  Required enums:\n";
        List.iter (Printf.eprintf "    %s\n") feature.required_enums
      );
      if feature.required_commands <> [] then (
        Printf.eprintf "  Required commands:\n";
        List.iter (Printf.eprintf "    %s\n") feature.required_commands
      );
      if feature.removed_enums <> [] then (
        Printf.eprintf "  Removed enums:\n";
        List.iter (Printf.eprintf "    %s\n") feature.removed_enums
      );
      if feature.removed_commands <> [] then (
        Printf.eprintf "  Removed commands:\n";
        List.iter (Printf.eprintf "    %s\n") feature.removed_commands
      )
    ) features

let print_identical_groups enums_by_group =
  let rec aux = function
    | (g1, e1) :: ((g2, e2) :: _ as tl) ->
       if e1 = e2 then Printf.eprintf "Groups \"%s\" and \"%s\" are identical.\n" g1 g2;
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
    Printf.eprintf "%s%s" !prefix group;
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
      Printf.eprintf "\n";
      prefix := "\n";
      loop rem_groups
    )
  in
  Printf.eprintf "Groups partition:\n\n";
  loop (GroupSet.of_seq (Hashtbl.to_seq enums_by_group))

let print_features_short features =
  List.iter (fun feature ->
      Printf.eprintf "%s api, version %s, %s profile\n"
        (string_of_api feature.api) feature.version (string_of_profile feature.profile)
    ) features

let print_empty_groups commands_by_name enums_by_group =
  Printf.eprintf "Groups with no enum:\n  From parameters with no class:\n";
  Hashtbl.fold (fun _ command acc ->
      List.fold_left (fun acc param ->
          if param.pgroup = "" || param.pclass <> "" || Hashtbl.mem enums_by_group param.pgroup || List.mem param.pgroup acc
          then acc
          else param.pgroup :: acc
        ) acc (command.proto :: command.params)
    ) commands_by_name []
  |> List.iter (Printf.eprintf "    %s\n");
  Printf.eprintf "  From parameters with a class:\n";
  Hashtbl.fold (fun _ command acc ->
      List.fold_left (fun acc param ->
          if param.pgroup = "" || param.pclass = "" || Hashtbl.mem enums_by_group param.pgroup || List.mem param.pgroup acc
          then acc
          else param.pgroup :: acc
        ) acc (command.proto :: command.params)
    ) commands_by_name []
  |> List.iter (Printf.eprintf "    %s\n")

let print_classes classes =
  Printf.eprintf "Classes:\n";
  List.iter (Printf.eprintf "  \"%s\"\n") classes

let filter_by_feature enums_by_name commands_by_name features =
  let filtered_enums_by_name = Hashtbl.create 4096 in
  let filtered_commands_by_name = Hashtbl.create 4096 in
  let rec loop = function
    | [] -> ()
    | hd :: tl ->
       List.iter (Hashtbl.remove filtered_enums_by_name) hd.removed_enums;
       List.iter (Hashtbl.remove filtered_commands_by_name) hd.removed_commands;
       List.iter Hashtbl.(fun name ->
         try replace filtered_enums_by_name name (find enums_by_name name)
         with Not_found -> Printf.eprintf "Required enum \"%s\" not found.\n%!" name
       ) hd.required_enums;
       List.iter Hashtbl.(fun name ->
         try replace filtered_commands_by_name name (find commands_by_name name)
         with Not_found -> Printf.eprintf "Required command \"%s\" not found.\n%!" name
       ) hd.required_commands;
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

let hash_enums_by_group enums_by_name =
  let enums_by_group = Hashtbl.create 4096 in
  Hashtbl.iter (fun _ enum ->
      List.iter (fun group ->
          let enums = Option.value (Hashtbl.find_opt enums_by_group group) ~default:[] in
          Hashtbl.replace enums_by_group group (enum :: enums)
        ) enum.groups
    ) enums_by_name;
  enums_by_group

let partition_params_in_out command =
  let rec aux pin pout = function
    | [] ->
       let pin = List.filter (fun p -> List.for_all (fun p' -> p.pname <> p'.plength) pin) pin in
       let pout = List.filter (fun p -> List.for_all (fun p' -> p.pname <> p'.plength) pout) pout in
       List.rev pin, List.rev pout
    | hd :: tl ->
       if ends_with hd.ptype "*" && not (starts_with hd.ptype "const")
       then aux pin (hd :: pout) tl
       else aux (hd :: pin) pout tl
  in
  let pout = if command.proto.ptype = "void" then [] else [command.proto] in
  aux [] pout command.params

let () =
  let ml_out, c_out =
    if Sys.argv.(0) <> "./generator.exe"
    then open_out "GL.ml", open_out "GL_stubs.c"
    else open_out "GL_preview.ml", open_out "GL_stubs_preview.c"
  in
  try
    let raw_enums_by_name, raw_commands_by_name, features = Registry.load "gl.xml" in
    let features = List.filter (fun f -> f.api = GLES2_API) features in
    let enums_by_name, commands_by_name = filter_by_feature raw_enums_by_name raw_commands_by_name features in
    let classes =
      Hashtbl.fold (fun _ command acc ->
          List.fold_left (fun acc param ->
              if param.pclass = "" || List.mem param.pclass acc then acc else param.pclass :: acc
            ) acc (command.proto :: command.params)
        ) commands_by_name []
    in
    (* DEBUG *)
(*
    print_features_short features;
    Printf.eprintf "\n%!";
    print_enums enums_by_name;
    Printf.eprintf "\n%!";
    print_commands commands_by_name;
    Printf.eprintf "\n%!";
    print_classes classes;
    Printf.eprintf "\n%!";
    let enums_by_group = hash_enums_by_group enums_by_name in
    print_groups enums_by_group;
    Printf.eprintf "\n%!";
    print_empty_groups commands_by_name enums_by_group;
    Printf.eprintf "\n%!";
    print_identical_groups enums_by_group;
    Printf.eprintf "\n%!";
    print_groups_partition enums_by_group;
    Printf.eprintf "\n%!";
 *)
    (* /DEBUG *)
    let open Printf in
    List.iter (fprintf ml_out "type %s [@@immediate]\n") classes;
    fprintf c_out "#include <GLES3/gl32.h>\n\n"; (* FIXME: use the header that corresponds to the API we’re generating for. *)
    fprintf ml_out "\ntype 'k enum =\n";
    fprintf c_out "GLenum enums[] = {\n";
    Hashtbl.to_seq_values enums_by_name
    |> List.of_seq
    |> List.sort (fun e1 e2 -> String.compare e1.value e2.value)
    |> List.iter (fun enum ->
           let prefix = ref "<" in
           fprintf ml_out "  | %s : [" (pretty_enum_name enum.name);
           List.iter (fun group ->
               fprintf ml_out "%s`%s" !prefix group;
               prefix := "|"
             ) enum.groups;
           fprintf ml_out "] enum\n";
           fprintf c_out "    %s,\n" enum.name;
         );
    fprintf ml_out "\n";
    fprintf c_out "};\n\n";
    Hashtbl.iter (fun _ command ->
        let pin, pout = partition_params_in_out command in
        fprintf ml_out "external %s :" command.proto.pname;
        if pin = []
        then fprintf ml_out " unit"
        else (
          let prefix = ref " " in
          List.iter (fun param ->
              let ml_ptype = caml_type_of_param param in
              if ml_ptype = "'z" then
                Printf.eprintf "No OCaml replacement for parameter \"%s\" of type \"%s\" in command %s.\n%!" param.pname param.ptype command.proto.pname;
              fprintf ml_out "%s%s:%s" !prefix (replace_if_reserved_caml_word param.pname) ml_ptype;
              prefix := " -> "
            ) pin
        );
        let stub_name =
          if command.alias <> "" && Hashtbl.mem commands_by_name command.alias
          then command.alias
          else command.proto.pname
        in
        let result_type =
          List.fold_left (fun str param ->
              if str = "unit"
              then caml_type_of_param param
              else Printf.sprintf "%s * %s" str (caml_type_of_param param)
            ) "unit" pout
        in
        if List.length command.params > 5
        then fprintf ml_out " -> %s = \"caml_%s_byte\" \"caml_%s\"\n" result_type stub_name stub_name
        else fprintf ml_out " -> %s = \"caml_%s\"\n" result_type stub_name
      ) commands_by_name;
    close_out ml_out;
    close_out c_out
  with
  | Xmlm.Error ((l, c), error) ->
     Printf.eprintf "Xmlm.Error: %s at (%d, %d)\n%!" (Xmlm.error_message error) l c
