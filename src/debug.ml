open Registry

let print_enums enums_by_name =
  Hashtbl.iter (fun _ enum ->
      Printf.eprintf "Enum %s\n  Value: %s\n" enum.ename enum.value;
      if enum.groups <> [] then (
        Printf.eprintf "  Groups:\n";
        List.iter (fun group -> Printf.eprintf "    %s\n" group) enum.groups
      )
    ) enums_by_name

let print_groups enums_by_group =
  Hashtbl.iter (fun group enums ->
      Printf.eprintf "Group %s\n  Enums:\n" group;
      List.iter (fun enum -> Printf.eprintf "    %s\n" enum.ename) enums
    ) enums_by_group

let print_commands commands_by_name =
  Hashtbl.iter (fun _ command ->
      Printf.eprintf "Command %s:\n" command.proto.pname;
      if command.alias <> "" then Printf.eprintf "  Alias of %s\n" command.alias;
      Printf.eprintf "  Return type: \"%s\"" command.proto.gl_type;
      if command.proto.gl_group <> "" || command.proto.gl_class <> "" then (
        let prefix = ref " (" in
        if command.proto.gl_group <> "" then (
          Printf.eprintf "%sgroup: \"%s\"" !prefix command.proto.gl_group;
          prefix := "; "
        );
        if command.proto.gl_class <> "" then (
          Printf.eprintf "%sclass: \"%s\"" !prefix command.proto.gl_class;
          prefix := "; "
        );
        Printf.eprintf ")"
      );
      Printf.eprintf "\n  Parameters:\n";
      if command.params <> [] then
        List.iter (fun param ->
            Printf.eprintf "    %s (type: \"%s\"" param.pname param.gl_type;
            if param.gl_group <> "" then Printf.eprintf "; group: \"%s\"" param.gl_group;
            if param.gl_class <> "" then Printf.eprintf "; class: \"%s\"" param.gl_class;
            if param.length <> "" then Printf.eprintf "; length: \"%s\"" param.length;
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
  |> rev_map (fun (group, enums) -> group, sort (fun (e1 : enum) e2 -> String.compare e1.ename e2.ename) enums)
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
          if param.gl_group = "" || param.gl_class <> "" || Hashtbl.mem enums_by_group param.gl_group || List.mem param.gl_group acc
          then acc
          else param.gl_group :: acc
        ) acc (command.proto :: command.params)
    ) commands_by_name []
  |> List.iter (Printf.eprintf "    %s\n");
  Printf.eprintf "  From parameters with a class:\n";
  Hashtbl.fold (fun _ command acc ->
      List.fold_left (fun acc param ->
          if param.gl_group = "" || param.gl_class = "" || Hashtbl.mem enums_by_group param.gl_group || List.mem param.gl_group acc
          then acc
          else param.gl_group :: acc
        ) acc (command.proto :: command.params)
    ) commands_by_name []
  |> List.iter (Printf.eprintf "    %s\n")

let print_classes classes =
  Printf.eprintf "Classes:\n";
  List.iter (Printf.eprintf "  \"%s\"\n") classes
