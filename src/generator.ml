open Utils
open Registry

let get_gl_include_file_name api version = match api, version with
  | GL_API, _ -> "GL/gl.h"
  | GLES1_API, ("1.0"|"1.1") -> "GLES/gl.h"
  | GLES2_API, "2.0" -> "GLES2/gl2.h"
  | GLES2_API, "3.0" -> "GLES3/gl3.h"
  | GLES2_API, "3.1" -> "GLES3/gl31.h"
  | GLES2_API, "3.2" -> "GLES3/gl32.h"
  | _ -> failwith "get_gl_include_file_name"

type caml_type =
  | Unit
  | Int
  | Bool
  | Float
  | String
  | Int64
  | Type of string
  | Enum of string
  | List of caml_type
  | Array of caml_type
  | Bigarray of string * string
  | Unimplemented

let is_caml_type_block = function
  | Unit | Int | Bool | Type _ | Enum _ | Unimplemented -> false
  | _ -> true

let caml_type_of_param param =
  match param.gl_type with
  | _ when param.gl_class = "pointer" -> Type param.gl_class
  | _ when param.gl_class <> "" && String.ends_with ~suffix:"*" param.gl_type && param.length <> "1" -> Array (Type param.gl_class)
  | _ when param.gl_class <> "" -> Type param.gl_class
  | "void" -> Unit
  | "GLenum" -> Enum param.gl_group
  | "GLenum *" when param.length = "1" -> Enum param.gl_group
  | "const GLenum *" -> Array (Enum param.gl_group)
  | "GLboolean" -> Bool
  | "GLboolean *" when param.length = "1" -> Bool
  | "GLboolean *" -> Array Bool
  | "GLchar *" | "const GLchar *" | "const GLubyte *" -> String
  | "const GLchar *const*" -> Array (Bigarray ("char", "int8_unsigned_elt"))
  | "GLfloat" | "GLdouble" -> Float
  | "GLfloat *" when param.length = "1" -> Float
  | "GLfloat *" -> Array Float
  | "const GLfloat *" -> Bigarray ("float", "float32_elt")
  | "GLint" | "GLuint" | "GLsizei" | "GLintptr" | "GLsizeiptr" -> Int
  | "GLuint64" -> Int64
  | "GLint *" | "GLuint *" | "GLsizei *" when param.length = "1" -> Int
  | "GLint *" | "GLuint *" | "GLsizei *" -> Array Int
  | "const GLint *" when param.pname = "length" -> Array Int (* FIXME: ugly kludge to make glShaderSource work. *)
  | "const GLint *" | "const GLuint *" -> Bigarray ("int32", "int32_elt")
  | "const GLsizei *" -> Bigarray ("nativeint", "nativeint_elt")
  | "const void *" -> Bigarray ("'a", "'b")
  | "GLbitfield" when param.gl_group = "" -> List Int
  | "GLbitfield" -> List (Enum param.gl_group)
  | _ -> Unimplemented

let rec string_of_caml_type = function
  | Unit -> "unit"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Int64 -> "int64"
  | Type "pointer" -> "('a, 'b) pointer"
  | Type gl_class -> gl_class
  | Enum gl_group -> Printf.sprintf "[`%s] enum" gl_group
  | List ml_type -> string_of_caml_type ml_type ^ " list"
  | Array ml_type -> string_of_caml_type ml_type ^ " array"
  | Bigarray (t, elt) -> Printf.sprintf "(%s, %s, c_layout) Genarray.t" t elt
  | Unimplemented -> "'z"

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
            if List.mem param.gl_group acc then acc else param.gl_group :: acc
          ) acc (command.proto :: command.params)
      ) filtered_commands_by_name []
    |> Hashtbl.fold (fun _ enum acc ->
           if enum.value_group = "" || List.mem enum.value_group acc then acc else enum.value_group :: acc
         ) filtered_enums_by_name
  in
  Hashtbl.filter_map_inplace (fun _ enum ->
      let groups = List.filter (ListLabels.mem ~set:accessible_groups) enum.groups in
      if groups = [] then (
        Printf.eprintf "Enum \"%s\" required but does not belong to any useable group; removed.\n%!" enum.ename;
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

let () =
  let ml_out, c_out = open_out "GL.ml", open_out "GL_stubs.c" in
  try
    let raw_enums_by_name, raw_commands_by_name, features = Registry.load "gl.xml" in
    let api, version = GLES2_API, "2.0" in
    let features = List.filter (fun f -> f.api = api && f.version <= version) features in
    let enums_by_name, commands_by_name = filter_by_feature raw_enums_by_name raw_commands_by_name features in
    let classes =
      Hashtbl.fold (fun _ command acc ->
          List.fold_left (fun acc param ->
              if param.gl_class = "" || param.gl_class = "pointer" || List.mem param.gl_class acc then acc else param.gl_class :: acc
            ) acc (command.proto :: command.params)
        ) commands_by_name []
    in
    (* DEBUG *)
(*
    Debug.print_features_short features;
    Printf.eprintf "\n%!";
    Debug.print_enums enums_by_name;
    Printf.eprintf "\n%!";
    Debug.print_commands commands_by_name;
    Printf.eprintf "\n%!";
    Debug.print_classes classes;
    Printf.eprintf "\n%!";
    let enums_by_group = hash_enums_by_group enums_by_name in
    Debug.print_groups enums_by_group;
    Printf.eprintf "\n%!";
    Debug.print_empty_groups commands_by_name enums_by_group;
    Printf.eprintf "\n%!";
    Debug.print_identical_groups enums_by_group;
    Printf.eprintf "\n%!";
    Debug.print_groups_partition enums_by_group;
    Printf.eprintf "\n%!";
 *)
    (* /DEBUG *)
    let open Printf in
    (* OCaml header *)
    fprintf ml_out "(* Generated by GL-OCaml-Generator for API %s v%s. *)

open Bigarray

type ('a, 'b) pointer =
  | Memory of ('a, 'b, c_layout) Genarray.t
  | Offset of int

" (string_of_api api) version;
    (* C header *)
    fprintf c_out "/* Generated by GL-OCaml-Generator for API %s v%s. */

#define GL_GLEXT_PROTOTYPES
#include <%s>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#undef string_length

#define CAMLvoid CAMLunused_start value unit CAMLunused_end

#define Val_none Val_int(0)

static size_t find_enum_offset(const GLenum gl_enums[], size_t length, GLenum gl_enum)
{
    size_t min = 0, max = length;
    while (min < max)
    {
        size_t mean = (min + max) / 2;
        if (gl_enums[mean] < gl_enum)
            min = mean + 1;
        else
            max = mean + 1;
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

" (string_of_api api) version (get_gl_include_file_name api version);
    (* Types and enums. *)
    List.iter (fprintf ml_out "type %s [@@immediate]\n") classes;
    fprintf ml_out "\ntype 'k enum =\n";
    fprintf c_out "const GLenum gl_enums[] = {\n";
    Hashtbl.to_seq_values enums_by_name
    |> List.of_seq
    |> List.sort (fun e1 e2 -> String.compare e1.value e2.value)
    |> List.iter (fun enum ->
           let variant_type = sprintf "[<`%s]" (String.concat "|`" enum.groups) in
           if enum.value_group = ""
           then fprintf ml_out "  | %s : %s enum\n" (to_pascal_case enum.ename) variant_type
           else fprintf ml_out "  | %s : (%s * [`%s] enum) enum\n" (to_pascal_case enum.ename) variant_type enum.value_group;
           fprintf c_out "    %s, // %s\n" enum.ename enum.value;
         );
    fprintf ml_out "\n";
    fprintf c_out "};\n\n";
    (* Functions. *)
    Hashtbl.to_seq_values commands_by_name
    |> List.of_seq
    |> List.sort (fun c1 c2 -> String.compare c1.proto.pname c2.proto.pname)
    |> List.iter (fun command ->
           (* DEBUG: add a copy of the C prototypes in the generated files. *)
           let gl_params =
             List.map (fun param ->
                 sprintf "%s %s" param.gl_type param.pname;
               ) command.params
             |> String.concat ", "
           in
           fprintf ml_out "(* %s %s(%s) *)\n" command.proto.gl_type command.proto.pname gl_params;
           fprintf c_out "/* %s %s(%s) */\n" command.proto.gl_type command.proto.pname gl_params;
           (* /DEBUG *)
           let input_parameters, output_parameters =
             List.partition (fun param ->
                 not (String.ends_with ~suffix:"*" param.gl_type) || (String.starts_with ~prefix:"const" param.gl_type)
               ) command.params
           in
           let explicit_input_parameters, implicit_input_parameters =
             List.partition (fun p ->
                 List.for_all (fun p' -> p.pname <> p'.length && p.pname <> p'.length2) input_parameters
               ) input_parameters
           in
           let explicit_output_parameters, _implicit_output_parameters =
             List.partition (fun p ->
                 List.for_all (fun p' -> p.pname <> p'.length) output_parameters
               ) output_parameters
           in
           let all_explicit_outputs = if command.proto.gl_type = "void" then explicit_output_parameters else command.proto :: explicit_output_parameters in
           let needs_byte_version = List.compare_length_with explicit_input_parameters 5 > 0 in
           let needs_block_allocation =
             List.compare_length_with all_explicit_outputs 1 > 0
             || List.exists (fun param -> is_caml_type_block (caml_type_of_param param)) all_explicit_outputs
           in
           (* OCaml *)
           let function_type = match explicit_input_parameters with
             | [] -> "unit"
             | _ ->
                List.map (fun param ->
                    let ml_type = caml_type_of_param param in
                    if ml_type = Unimplemented then
                      Printf.eprintf "No OCaml replacement for parameter \"%s\" of type \"%s\" in command %s.\n%!"
                        param.pname param.gl_type command.proto.pname;
                    (* FIXME: naively emitting 'a will not work if a function has several parameters with different "value_for" attributes. *)
                    if param.value_for <> ""
                    then sprintf "%s:'a" (replace_if_reserved_caml_word param.pname)
                    else if List.exists (fun param' -> param.pname = param'.value_for) explicit_input_parameters
                    then sprintf "%s:([`%s] * 'a) enum" (replace_if_reserved_caml_word param.pname) param.gl_group
                    else sprintf "%s:%s" (replace_if_reserved_caml_word param.pname) (string_of_caml_type ml_type)
                  ) explicit_input_parameters
                |> String.concat " -> "
           in
           let result_type = match all_explicit_outputs with
             | [] -> "unit"
             | _ ->
                List.map (fun param ->
                    let ml_type = caml_type_of_param param in
                    if ml_type = Unimplemented then
                      Printf.eprintf "No OCaml replacement for output \"%s\" of type \"%s\" in command %s.\n%!"
                        param.pname param.gl_type command.proto.pname;
                    string_of_caml_type ml_type
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
           fprintf ml_out "external %s : %s -> %s = %s%s\n"
             (remove_gl_prefix command.proto.pname) function_type result_type stub_names attributes;
           (* C *)
           let stub_params = match input_parameters with
             | [] -> "CAMLvoid"
             | _ ->
                List.map (fun param ->
                    sprintf "value %s" (replace_if_reserved_c_word param.pname)
                  ) explicit_input_parameters
                |> String.concat ", "
           in
           fprintf c_out "CAMLprim value caml_%s(%s)\n{\n" command.proto.pname stub_params;
           if not needs_block_allocation then (
             List.iter (fun param ->
                 match caml_type_of_param param with
                 | Array ml_type ->
                    let gl_type =
                      let open String in
                      if starts_with ~prefix:"const " param.gl_type && ends_with ~suffix:" *" param.gl_type
                      then sub param.gl_type 6 (length param.gl_type - 8)
                      else if ends_with ~suffix:"const*" param.gl_type
                      then sub param.gl_type 0 (length param.gl_type - 6)
                      else "void"
                    in
                    let len2_of = List.find_opt (fun p -> p.length2 = param.pname) input_parameters in
                    let pname = replace_if_reserved_c_word param.pname in
                    let length_pname = match len2_of with
                      | None -> pname
                      | Some param' -> replace_if_reserved_c_word param'.pname
                    in
                    let element = match ml_type, len2_of with
                      | Int, Some param' ->
                         let pname' = replace_if_reserved_c_word param'.pname in
                         sprintf "caml_ba_byte_size(Caml_ba_array_val(Field(%s, i)))" pname'
                      | (Int | Type _), _ -> sprintf "Int_val(Field(%s, i))" pname
                      | Bool, _ -> sprintf "Bool_val(Field(%s, i))" pname
                      | Float, _ -> sprintf "Double_val(Field(%s, i))" pname
                      | String, _ -> sprintf "String_val(Field(%s, i))" pname
                      | Int64, _ -> sprintf "Int64_val(Field(%s, i))" pname
                      | Enum _, _ -> sprintf "gl_enums[Int_val(Field(%s, i))]" pname
                      | List _, _ -> sprintf "glbitfield_of_enum_list(gl_enums, Field(%s, i))" pname
                      | Bigarray _, _ -> sprintf "Caml_ba_data_val(Field(%s, i))" pname
                      | _ -> "NULL"
                    in
                    fprintf c_out "    const GLsizei %s_length = Wosize_val(%s);\n" pname length_pname;
                    fprintf c_out "    %s %s_array[%s_length];\n" gl_type pname pname;
                    fprintf c_out "    for (int i = 0; i < %s_length; ++i)\n" pname;
                    fprintf c_out "        %s_array[i] = %s;\n" pname element
                 | _ -> ()
               ) input_parameters;
             if explicit_output_parameters == all_explicit_outputs
             then fprintf c_out "    %s(" command.proto.pname
             else fprintf c_out "    %s result = %s(" command.proto.gl_type command.proto.pname;
             let call_params =
               List.map (fun param ->
                   if List.memq param implicit_input_parameters then
                     match caml_type_of_param param with
                     | Array _ -> sprintf "%s_array" (replace_if_reserved_c_word param.pname)
                     | _ ->
                        let param = List.find (fun p -> p.length = param.pname) explicit_input_parameters in
                        let pname = replace_if_reserved_c_word param.pname in
                        match caml_type_of_param param with
                        | Array _ -> sprintf "%s_length" pname
                        | Bigarray _ -> sprintf "caml_ba_byte_size(Caml_ba_array_val(%s))" pname
                        | _ -> "NULL"
                   else
                     let pname = replace_if_reserved_c_word param.pname in
                     match caml_type_of_param param with
                     | Type "pointer" -> sprintf "(Tag_val(%s) == 0 ? Caml_ba_data_val(Field(%s, 0)) : (void*)(intnat)Int_val(Field(%s, 0)))" pname pname pname
                     | _ when param.value_for <> "" ->
                        (* TODO: properly figure out the type of the value before using it.
                           At the moment this is used for glTexParameteri, which is the only function definition
                           where we added a "value_for" attribute to a parameter, and it only takes enums. *)
                        sprintf "gl_enums[Int_val(%s)]" pname
                     | Int | Type _ -> sprintf "Int_val(%s)" pname
                     | Bool -> sprintf "Bool_val(%s)" pname
                     | Float -> sprintf "Double_val(%s)" pname
                     | String -> sprintf "String_val(%s)" pname
                     | Int64 -> sprintf "Int64_val(%s)" pname
                     | Enum _ -> sprintf "gl_enums[Int_val(%s)]" pname
                     | List _ -> sprintf "glbitfield_of_enum_list(gl_enums, %s)" pname
                     | Array _ -> sprintf "%s_array" pname
                     | Bigarray _ -> sprintf "Caml_ba_data_val(%s)" pname
                     | _ -> "NULL"
                 ) command.params
               |> String.concat ", "
             in
             fprintf c_out "%s);\n" call_params;
             match caml_type_of_param command.proto with
             | Int | Type _ -> fprintf c_out "    return Val_int(result);\n"
             | Bool -> fprintf c_out "    return Val_bool(result);\n"
             | Enum _ -> fprintf c_out "    return Val_int(find_enum_offset(gl_enums, sizeof(gl_enums) / sizeof(*gl_enums), result));\n"
             | _ -> fprintf c_out "    return Val_unit;\n"
           ) else (
             List.iter (fun param -> fprintf c_out "    (void)%s;\n" (replace_if_reserved_c_word param.pname)) explicit_input_parameters;
             fprintf c_out "    caml_failwith(\"GL.%s: unimplemented\");\n" (remove_gl_prefix command.proto.pname)
           );
           fprintf c_out "}\n\n";
           if needs_byte_version then (
             let call_params =
               List.mapi (fun i _ -> sprintf "val_array[%d]" i) explicit_input_parameters
               |> String.concat ", "
             in
             fprintf c_out "CAMLprim value caml_%s_byte(value* val_array, int val_count)\n{\n    (void)val_count;\n    return caml_%s(%s);\n}\n\n"
               command.proto.pname command.proto.pname call_params
           )
         );
    close_out ml_out;
    close_out c_out
  with
  | Xmlm.Error ((l, c), error) ->
     Printf.eprintf "Xmlm.Error: %s at (%d, %d)\n%!" (Xmlm.error_message error) l c
