open Utils

type enum = {
    ename: string;
    value: string;
    groups: string list;
    value_group: string;
  }

type param = {
    pname: string;
    gl_type: string;
    gl_group: string;
    gl_class: string;
    gl_kind: string;
    length: string;
    length2: string;
    value_for: string;
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

let propagate_groups_by_value enums =
  let enums =
    if is_weakly_ordered (fun e1 e2 -> String.compare e1.value e2.value) enums
    then enums
    else List.sort (fun e1 e2 -> String.compare e1.value e2.value) enums
  in
  let rec aux acc staged value groups = function
    | [] ->
       List.fold_left (fun acc enum ->
           (if enum.groups = groups then enum else {enum with groups}) :: acc
         ) acc staged
    | hd :: tl when hd.value = value ->
       let groups =
         List.fold_left (fun groups g ->
             if List.mem g groups then groups else g :: groups
           ) groups hd.groups
       in
       aux acc (hd :: staged) value groups tl
    | hd :: tl ->
       let acc =
         List.fold_left (fun acc enum ->
             (if enum.groups = groups then enum else {enum with groups}) :: acc
           ) acc staged
       in
       aux acc [hd] hd.value hd.groups tl
  in
  aux [] [] "" [] enums

let load filename =
  let opengl_registry_in = open_in filename in
  let xml_input = Xmlm.make_input ~strip:true (`Channel opengl_registry_in) in
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
      let ename = attrs_assoc "name" attrs in
      let value = attrs_assoc "value" attrs in
      let groups = Option.fold ~none:[] ~some:(String.split_on_char ',') (attrs_assoc_opt "group" attrs) in
      let value_group = Option.value (attrs_assoc_opt "value_group" attrs) ~default:"" in
      { ename; value; groups; value_group }
    with
    | Not_found ->
       let prefix = ref "" in
       Printf.eprintf "Found enum with missing name or value (";
       List.iter (fun ((_, name), value) ->
           Printf.eprintf "%s%s = \"%s\"" !prefix name value;
           prefix := ", "
         ) attrs;
       Printf.eprintf ")\n%!";
       raise Not_found
  in
  let read_param attrs =
    let gl_group, gl_class = match attrs_assoc_opt "group" attrs, attrs_assoc_opt "class" attrs with
      (* Although the parameters expecting values from the {Shader,Program}BinaryFormat
         groups are of type GLenum, no enums are actually defined to be in these groups.
         Turning those two groups into classes has the effect of making related values be
         of an abstract type in the context of this binding and allows actually using them. *)
      | Some "ShaderBinaryFormat", None -> "", "shader_binary_format"
      | Some "ProgramBinaryFormat", None -> "", "program_binary_format"
      | g, p -> Option.(value g ~default:"", fold ~none:"" ~some:(String.map (function ' ' -> '_' | c -> c)) p)
    in
    let gl_kind = Option.value (attrs_assoc_opt "kind" attrs) ~default:"" in
    let length = Option.value (attrs_assoc_opt "len" attrs) ~default:"" in
    let length2 = Option.value (attrs_assoc_opt "len2" attrs) ~default:"" in
    let value_for = Option.value (attrs_assoc_opt "value_for" attrs) ~default:"" in
    let rec aux depth gl_type pname is_name = match Xmlm.input xml_input with
      | `El_start ((_, "name"), _) -> aux (depth + 1) gl_type pname true
      | `El_start ((_, "ptype"), _) -> aux (depth + 1) gl_type pname false
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at param scope.\n%!" name;
         drop (); aux depth gl_type pname is_name
      | `El_end when depth > 1 -> aux (depth - 1) gl_type pname false
      | `El_end ->
         if gl_class <> "" && not (List.exists (String.contains_string gl_type) ["GLuint"; "GLsync"; "GLenum"; "const void *"]) then
           Printf.eprintf "Found parameter \"%s\" with non-empty class \"%s\" and unexpected type \"%s\".\n%!"
             pname gl_class gl_type;
         if List.(length (filter ((<>) "") [gl_group; gl_class; gl_kind])) > 1 then
           Printf.eprintf "Found parameter \"%s\" with more than one non-empty attribute among group, class and kind (\"%s\", \"%s\", \"%s\").\n%!"
             pname gl_group gl_class gl_kind;
         { pname; gl_type; gl_group; gl_class; gl_kind; length; length2; value_for }
      | `Data str when is_name && pname = "" -> aux depth gl_type str true
      | `Data str when is_name ->
         Printf.eprintf "Found parameter with several names (current: \"%s\", new: \"%s\"); keeping current name.\n%!" pname str;
         aux depth gl_type pname is_name
      | `Data str -> aux depth (if gl_type = "" then str else Printf.sprintf "%s %s" gl_type str) pname false
      | `Dtd _ -> aux depth gl_type pname is_name
    in
    aux 1 "" "" false
  in
  let enums_scope propagate_groups =
    let rec aux acc = match Xmlm.input xml_input with
      | `El_start ((_, "unused"), _) -> drop (); aux acc
      | `El_start ((_, "enum"), attrs) -> begin
          try aux (read_enum attrs :: acc)
          with Not_found -> aux acc
        end
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at enums scope.\n%!" name;
         drop (); aux acc
      | `El_end ->
         List.iter (fun enum ->
             match Hashtbl.find_opt enums_by_name enum.ename with
             | None -> Hashtbl.add enums_by_name enum.ename enum
             | Some current ->
                Printf.eprintf "Found duplicated enum \"%s\" (current value: \"%s\", new value: \"%s\"); keeping current value.\n%!"
                  enum.ename current.value enum.value
           ) (if propagate_groups then propagate_groups_by_value acc else acc)
      | `Data _ ->
         Printf.eprintf "Don’t know what to do with data at enums scope.\n%!";
         aux acc
      | `Dtd _ -> aux acc
    in
    aux []
  in
  let command_scope () =
    let rec aux proto params alias = match Xmlm.input xml_input with
      | `El_start ((_, "glx"), _)
      | `El_start ((_, "vecequiv"), _) -> drop (); aux proto params alias
      | `El_start ((_, "alias"), attrs) when alias <> "" ->
         Printf.eprintf "Command %s aliases several commands (current alias: %s, new alias: %s); keeping current alias.\n%!"
           proto.pname alias (attrs_assoc "name" attrs);
         drop (); aux proto params alias
      | `El_start ((_, "alias"), attrs) ->
         let alias = attrs_assoc "name" attrs in
         drop (); aux proto params alias
      | `El_start ((_, "param"), attrs) ->
         let param = read_param attrs in
         aux proto (param :: params) alias
      | `El_start ((_, "proto"), _) when proto != null_param ->
         Printf.eprintf "Command %s declares several prototypes; keeping current prototype.\n%!" proto.pname;
         drop (); aux proto params alias
      | `El_start ((_, "proto"), attrs) ->
         let proto = read_param attrs in
         aux proto params alias
      | `El_start ((_, name), _) ->
         Printf.eprintf "Don’t know what to do with element %s at command scope.\n%!" name;
         drop (); aux proto params alias
      | `El_end when proto == null_param ->
         Printf.eprintf "Found command without prototype.\n%!"
      | `El_end when Hashtbl.mem commands_by_name proto.pname ->
         Printf.eprintf "Found duplicated command %s; keeping current command.\n%!" proto.pname
      | `El_end -> Hashtbl.add commands_by_name proto.pname { proto; params = List.rev params; alias }
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
    | `El_start ((_, "types"), _)
    | `El_start ((_, "kinds"), _)
    | `El_start ((_, "extensions"), _) -> drop (); registry_scope ()
    | `El_start ((_, "commands"), _) -> commands_scope (); registry_scope ()
    | `El_start ((_, "enums"), attrs) ->
       enums_scope (attrs_assoc_opt "group" attrs <> Some "SpecialNumbers");
       registry_scope ()
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
    | `El_end | `Data _ -> assert false
    | `Dtd _ -> ()
    end;
    if not (Xmlm.eoi xml_input) then global_scope ()
  in
  global_scope ();
  close_in_noerr opengl_registry_in;
  enums_by_name, commands_by_name, List.rev !features
