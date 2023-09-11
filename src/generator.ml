open Utils
open Registry
open Printf

let get_gl_include_file_name api version = match api, version with
  | GL_API, _ -> "GL/gl.h"
  | GLES1_API, ("1.0"|"1.1") -> "GLES/gl.h"
  | GLES2_API, "2.0" -> "GLES2/gl2.h"
  | GLES2_API, "3.0" -> "GLES3/gl3.h"
  | GLES2_API, "3.1" -> "GLES3/gl31.h"
  | GLES2_API, "3.2" -> "GLES3/gl32.h"
  | _ -> failwith "get_gl_include_file_name"

let is_caml_type_block = function
  | Type "pointer" -> true
  | Unit | Int | Bool | Type _ | Enum _ | Unimplemented -> false
  | _ -> true

let rec string_of_caml_type = function
  | Unit -> "unit"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Int64 -> "int64"
  | Type "pointer" -> "('a, 'b) pointer"
  | Type gl_class -> gl_class
  | Enum gl_group -> sprintf "[`%s] enum" gl_group
  | List ml_type -> string_of_caml_type ml_type ^ " list"
  | Array ml_type -> string_of_caml_type ml_type ^ " array"
  | Bigarray (t, elt) -> sprintf "(%s, %s, c_layout) Genarray.t" t elt
  | Unimplemented -> "'z"

let c_value_of_caml_value value = function
  | Unit -> failwith "c_value_of_caml_value: unit value"
  | Type "pointer" -> sprintf "Pointer_val(%s)" value
  | Int | Type _ -> sprintf "Int_val(%s)" value
  | Bool -> sprintf "Bool_val(%s)" value
  | Float -> sprintf "Double_val(%s)" value
  | String -> sprintf "String_val(%s)" value
  | Int64 -> sprintf "Int64_val(%s)" value
  | Enum _ -> sprintf "gl_enums[Int_val(%s)]" value
  | List _ -> sprintf "glbitfield_of_enum_list(gl_enums, %s)" value
  | Array _ -> sprintf "%s_array" value
  | Bigarray _ -> sprintf "Caml_ba_data_val(%s)" value
  | Unimplemented -> failwith "c_value_of_caml_value: unimplemented value"

let caml_value_of_c_value value = function
  | Unit -> "Val_unit"
  | Type "pointer" -> failwith "caml_value_of_c_value: pointer value"
  | Int | Type _ -> sprintf "Val_int(%s)" value
  | Bool -> sprintf "Val_bool(%s)" value
  | Float -> sprintf "caml_copy_double(%s)" value
  | String -> sprintf "caml_copy_string(%s)" value
  | Int64 -> sprintf "caml_copy_int64(%s)" value
  | Enum _ -> sprintf "Val_int(find_enum_offset(gl_enums, sizeof(gl_enums) / sizeof(*gl_enums), %s))" value
  | List _ -> failwith "caml_value_of_c_value: list value"
  | Array _ -> sprintf "%s_array" value
  | Bigarray _ -> failwith "caml_value_of_c_value: bigarray value"
  | Unimplemented -> failwith "caml_value_of_c_value: unimplemented value"

let c_array_of_caml_array param buffer =
  let gl_type =
    let open String in
    if starts_with ~prefix:"const " param.gl_type && ends_with ~suffix:" *" param.gl_type
    then sub param.gl_type 6 (length param.gl_type - 8)
    else if ends_with ~suffix:"const*" param.gl_type
    then sub param.gl_type 0 (length param.gl_type - 6)
    else failwith "c_array_of_caml_array: could not deduce element type"
  in
  let pname = replace_if_reserved_c_word param.pname in
  let element = match param.caml_type with
    | Array inner_caml_type -> c_value_of_caml_value (sprintf "Field(%s, i)" pname) inner_caml_type
    | _ -> failwith "c_array_of_caml_array: not an array"
  in
  bprintf buffer "    const GLsizei %s_length = Wosize_val(%s);\n" pname pname;
  bprintf buffer "    %s %s_array[%s_length];\n" gl_type pname pname;
  bprintf buffer "    for (int i = 0; i < %s_length; ++i)\n" pname;
  bprintf buffer "        %s_array[i] = %s;\n" pname element

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
         with Not_found -> eprintf "Required enum \"%s\" not found.\n%!" name
       ) hd.required_enums;
       List.iter Hashtbl.(fun name ->
         try replace filtered_commands_by_name name (find commands_by_name name)
         with Not_found -> eprintf "Required command \"%s\" not found.\n%!" name
       ) hd.required_commands;
       loop tl
  in
  loop features;
  let accessible_groups =
    Hashtbl.fold (fun _ command acc ->
        List.fold_left (fun acc param ->
            match param.caml_type with
            | Enum e | Array (Enum e) | List (Enum e) when not (List.mem e acc) -> e :: acc
            | _ -> acc
          ) acc (command.proto :: command.params)
      ) filtered_commands_by_name []
    |> Hashtbl.fold (fun _ enum acc ->
           match enum.value_group with
           | Some value_group when not (List.mem value_group acc) -> value_group :: acc
           | _ -> acc
         ) filtered_enums_by_name
  in
  Hashtbl.filter_map_inplace (fun _ enum ->
      let groups = List.filter (ListLabels.mem ~set:accessible_groups) enum.groups in
      if groups = [] then (
        eprintf "Enum \"%s\" required but does not belong to any useable group; removed.\n%!" enum.ename;
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

let emit_caml_header caml_out api version =
  fprintf caml_out "(* Generated by GL-OCaml-Generator for API %s v%s. *)

open Bigarray

" (string_of_api api) version

let emit_c_header c_out api version =
  fprintf c_out "/* Generated by GL-OCaml-Generator for API %s v%s. */

#define GL_GLEXT_PROTOTYPES
#include <%s>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define CAMLvoid CAMLunused_start value unit CAMLunused_end

#ifndef Val_none
# define Val_none Val_int(0)
#endif

#define Pointer_val(v) (Tag_val(v) == 0 ? Caml_ba_data_val(Field((v), 0)) : (void*)(intnat)Int_val(Field((v), 0)))

" (string_of_api api) version (get_gl_include_file_name api version)

let emit_caml_types caml_out buffer types enums_sorted_by_value =
  Buffer.clear buffer;
  bprintf buffer "type ('a, 'b) pointer =
  | Memory of ('a, 'b, c_layout) Genarray.t
  | Offset of int

";
  List.iter (bprintf buffer "type %s [@@immediate]\n") types;
  bprintf buffer "\ntype 'k enum =\n";
  List.iter (fun enum ->
      let variant_type = sprintf "[<`%s]" (String.concat "|`" enum.groups) in
      match enum.value_group with
      | None -> bprintf buffer "  | %s : %s enum\n" (to_pascal_case enum.ename) variant_type
      | Some value_group -> bprintf buffer "  | %s : (%s * [`%s] enum) enum\n" (to_pascal_case enum.ename) variant_type value_group
    ) enums_sorted_by_value;
  bprintf buffer "\n";
  Buffer.output_buffer caml_out buffer

let emit_c_glenums_translation_table c_out buffer enums_sorted_by_value =
  Buffer.clear buffer;
  bprintf buffer "const GLenum gl_enums[] = {\n";
  List.iter (fun enum -> bprintf buffer "    %s, // %s\n" enum.ename enum.value) enums_sorted_by_value;
  bprintf buffer "};\n\n";
  Buffer.output_buffer c_out buffer

let emit_c_static_functions c_out =
  fprintf c_out "static size_t find_enum_offset(const GLenum gl_enums[], size_t length, GLenum gl_enum)
{
    size_t min = 0, max = length;
    while (min < max)
    {
        size_t mean = (min + max) / 2;
        if (gl_enums[mean] < gl_enum)
            min = mean + 1;
        else
            max = mean;
    }
    return min;
}

static GLbitfield glbitfield_of_enum_list(const GLenum gl_enums[], value list)
{
    GLbitfield ret = 0;
    while (list != Val_emptylist)
    {
        ret |= gl_enums[Int_val(Field(list, 0))];
        list = Field(list, 1);
    }
    return ret;
}

"

let emit_caml_function
      caml_out command explicit_input_parameters all_explicit_outputs
      needs_byte_version needs_block_allocation =
  let function_type = match explicit_input_parameters with
    | [] -> "unit"
    | _ ->
       List.map (fun param ->
           if param.caml_type = Unimplemented then
             eprintf "No OCaml replacement for parameter \"%s\" of type \"%s\" in command %s.\n%!"
               param.pname param.gl_type command.proto.pname;
           if param.value_for <> None
           (* FIXME: if a function has several parameters with different "value_for"
              attributes, we need to emit 'a, then 'b, then 'c and so on. *)
           then sprintf "%s:'a" (replace_if_reserved_caml_word param.pname)
           else if List.exists (fun other -> other.value_for = Some param.pname) explicit_input_parameters
           then (
             let[@warning "-8"] Enum gl_group = param.caml_type in
             sprintf "%s:([`%s] * 'a) enum" (replace_if_reserved_caml_word param.pname) gl_group
           ) else sprintf "%s:%s" (replace_if_reserved_caml_word param.pname) (string_of_caml_type param.caml_type)
         ) explicit_input_parameters
       |> String.concat " -> "
  in
  let result_type = match all_explicit_outputs with
    | [] -> "unit"
    | _ ->
       List.map (fun param ->
           if param.caml_type = Unimplemented then
             eprintf "No OCaml replacement for output \"%s\" of type \"%s\" in command %s.\n%!"
               param.pname param.gl_type command.proto.pname;
           string_of_caml_type param.caml_type
         ) all_explicit_outputs
       |> String.concat " * "
  in
  let stub_names = match needs_byte_version with
    | true -> sprintf "\"caml_%s_byte\" \"caml_%s\"" command.proto.pname command.proto.pname
    | false -> sprintf "\"caml_%s\"" command.proto.pname
  in
  let attributes = match needs_block_allocation with
    | true -> ""
    | false -> " [@@noalloc]"
  in
  fprintf caml_out "external %s : %s -> %s = %s%s\n"
    (remove_gl_prefix command.proto.pname) function_type result_type stub_names attributes

let emit_c_function
      c_out buffer command input_parameters explicit_input_parameters
      implicit_input_parameters explicit_output_parameters all_explicit_outputs
      needs_byte_version needs_block_allocation =
  let stub_params = match input_parameters with
    | [] -> "CAMLvoid"
    | _ ->
       List.map (fun param ->
           sprintf "value %s" (replace_if_reserved_c_word param.pname)
         ) explicit_input_parameters
       |> String.concat ", "
  in
  fprintf c_out "CAMLprim value caml_%s(%s)\n{\n" command.proto.pname stub_params;
  Buffer.clear buffer;
  begin try
    if needs_block_allocation then
      failwith "block allocation unimplemented";
    List.iter (fun param ->
        match param.caml_type with
        | Array _ -> c_array_of_caml_array param buffer
        | _ -> ()
      ) input_parameters;
    if explicit_output_parameters == all_explicit_outputs
    then bprintf buffer "    %s(" command.proto.pname
    else bprintf buffer "    %s result = %s(" command.proto.gl_type command.proto.pname;
    let call_params =
      List.map (fun param ->
          let pname = replace_if_reserved_c_word param.pname in
          if List.memq param implicit_input_parameters then
            match param.caml_type with
            | Array _ -> c_value_of_caml_value pname param.caml_type
            | _ ->
               let other_param = List.find (fun other -> other.length = Some param.pname) explicit_input_parameters in
               let other_pname = replace_if_reserved_c_word other_param.pname in
               match other_param.caml_type with
               | Array _ -> sprintf "%s_length" other_pname
               | Bigarray _ -> sprintf "caml_ba_byte_size(Caml_ba_array_val(%s))" other_pname
               | _ -> failwith "could not deduce implicit parameter"
          else if param.value_for <> None then
            (* TODO: properly figure out the type of the value before using it.
               At the moment this is used for glTexParameteri, which is the only function definition
               where we added a "value_for" attribute to a parameter, and it only takes enums. *)
            c_value_of_caml_value pname (Enum "#DUMMY#")
          else
            c_value_of_caml_value pname param.caml_type
        ) command.params
      |> String.concat ", "
    in
    bprintf buffer "%s);\n" call_params;
    bprintf buffer "    return %s;\n" (caml_value_of_c_value "result" command.proto.caml_type)
  with Failure error ->
    Buffer.clear buffer;
    List.iter (fun param -> bprintf buffer "    (void)%s;\n" (replace_if_reserved_c_word param.pname)) explicit_input_parameters;
    bprintf buffer "    caml_failwith(\"GL.%s: %s\");\n" (remove_gl_prefix command.proto.pname) error
  end;
  bprintf buffer "}\n\n";
  if needs_byte_version then (
    let call_params =
      List.mapi (fun i _ -> sprintf "val_array[%d]" i) explicit_input_parameters
      |> String.concat ", "
    in
    bprintf buffer "CAMLprim value caml_%s_byte(value* val_array, int val_count)\n{\n    (void)val_count;\n    return caml_%s(%s);\n}\n\n"
      command.proto.pname command.proto.pname call_params
  );
  Buffer.output_buffer c_out buffer

let emit_functions caml_out c_out buffer commands_by_name =
  Hashtbl.to_seq_values commands_by_name
  |> List.of_seq
  |> List.sort (fun c1 c2 -> String.compare c1.proto.pname c2.proto.pname)
  |> List.iter (fun command ->
         let input_parameters, output_parameters =
           List.partition (fun param ->
               not (String.ends_with ~suffix:"*" param.gl_type) || (String.starts_with ~prefix:"const" param.gl_type)
             ) command.params
         in
         let explicit_input_parameters, implicit_input_parameters =
           List.partition (fun param ->
               List.for_all (fun other -> other.length <> Some param.pname) input_parameters
             ) input_parameters
         in
         let explicit_output_parameters, _implicit_output_parameters =
           List.partition (fun param ->
               List.for_all (fun other -> other.length <> Some param.pname) output_parameters
             ) output_parameters
         in
         let all_explicit_outputs =
           if command.proto.gl_type = "void"
           then explicit_output_parameters
           else command.proto :: explicit_output_parameters
         in
         let needs_byte_version = List.compare_length_with explicit_input_parameters 5 > 0 in
         let needs_block_allocation =
           List.compare_length_with all_explicit_outputs 1 > 0
           || List.exists (fun param -> is_caml_type_block param.caml_type) all_explicit_outputs
         in
         Debug.emit_C_prototype_comment command caml_out c_out;
         let override_caml_filename = sprintf "overrides/%s.ml" command.proto.pname in
         if Sys.file_exists override_caml_filename then
           output_file caml_out override_caml_filename
         else
           emit_caml_function caml_out command explicit_input_parameters
             all_explicit_outputs needs_byte_version needs_block_allocation;
         let override_c_filename = sprintf "overrides/%s.c" command.proto.pname in
         if Sys.file_exists override_c_filename then
           output_file c_out override_c_filename
         else
           emit_c_function c_out buffer command input_parameters explicit_input_parameters
             implicit_input_parameters explicit_output_parameters all_explicit_outputs
             needs_byte_version needs_block_allocation
       )

let () =
  let caml_out, c_out = open_out "GL.ml", open_out "GL_stubs.c" in
  let raw_enums_by_name, raw_commands_by_name, features =
    try Registry.load "gl.xml"
    with Xmlm.Error ((l, c), error) ->
      eprintf "Xmlm.Error: %s at line %d, character %d.\n%!" (Xmlm.error_message error) l c;
      exit 1
  in
  let api, version = GLES2_API, "2.0" in
  let features = List.filter (fun f -> f.api = api && f.version <= version) features in
  let enums_by_name, commands_by_name = filter_by_feature raw_enums_by_name raw_commands_by_name features in
  let types =
    Hashtbl.fold (fun _ command acc ->
        List.fold_left (fun acc param ->
            match param.caml_type with
            | Type t when t <> "pointer" && not (List.mem t acc) -> t :: acc
            | _ -> acc
          ) acc (command.proto :: command.params)
      ) commands_by_name []
  in
  (* DEBUG *)
(*
  Debug.print_features_short features;
  eprintf "\n%!";
  let enums_by_group = hash_enums_by_group enums_by_name in
  Debug.print_groups enums_by_group;
  eprintf "\n%!";
  Debug.print_empty_groups commands_by_name enums_by_group;
  eprintf "\n%!";
  Debug.print_identical_groups enums_by_group;
  eprintf "\n%!";
  Debug.print_groups_partition enums_by_group;
  eprintf "\n%!";
 *)
  (* /DEBUG *)
  let buffer = Buffer.create 4087 in
  emit_caml_header caml_out api version;
  emit_c_header c_out api version;
  let () =
    let enums_sorted_by_value =
      Hashtbl.to_seq_values enums_by_name
      |> List.of_seq
      |> List.sort (fun e1 e2 -> String.compare e1.value e2.value)
    in
    emit_caml_types caml_out buffer types enums_sorted_by_value;
    emit_c_glenums_translation_table c_out buffer enums_sorted_by_value
  in
  Buffer.reset buffer;
  emit_c_static_functions c_out;
  emit_functions caml_out c_out buffer commands_by_name;
  close_out caml_out;
  close_out c_out;
  eprintf "Generated in %.6f seconds (CPU time).\n%!" (Sys.time ())
