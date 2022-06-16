open Utils
open Registry

type ml_type =
  | Unit
  | Int
  | Bool
  | Float
  | String
  | Int64
  | Type of string
  | Enum of string
  | List of ml_type
  | Array of ml_type
  | Bigarray of string * string
  | Unimplemented

let caml_type_of_param param =
  match param.gl_type with
  | _ when param.gl_class <> "" && ends_with param.gl_type "*" && param.length <> "1" -> Array (Type param.gl_class)
  | _ when param.gl_class <> "" -> Type param.gl_class
  | "void" -> Unit
  | "GLenum" -> Enum param.gl_group
  | "GLenum *" when param.length = "1" -> Enum param.gl_group
  | "const GLenum *" -> Array (Enum param.gl_group)
  | "GLboolean" -> Bool
  | "GLboolean *" when param.length = "1" -> Bool
  | "GLboolean *" -> Array Bool
  | "GLchar *" | "const GLchar *" | "const GLubyte *" -> String
  | "const GLchar *const*" -> Array String
  | "GLfloat" | "GLdouble" -> Float
  | "GLfloat *" when param.length = "1" -> Float
  | "GLfloat *" -> Array Float
  | "const GLfloat *" -> Bigarray ("float", "float32_elt")
  | "GLint" | "GLuint" | "GLsizei" | "GLintptr" | "GLsizeiptr" -> Int
  | "GLuint64" -> Int64
  | "GLint *" | "GLuint *" | "GLsizei *" when param.length = "1" -> Int
  | "GLint *" | "GLuint *" | "GLsizei *" -> Array Int
  | "const GLint *" | "const GLuint *" -> Bigarray ("int32", "int32_elt")
  | "const GLsizei *" -> Bigarray ("nativeint", "nativeint_elt")
  | "const void *" -> Bigarray ("'a", "'b")
  | "GLbitfield" when param.gl_group = "" -> Int
  | "GLbitfield" -> List (Enum param.gl_group)
  | _ -> Unimplemented

let rec string_of_caml_type = function
  | Unit -> "unit"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Int64 -> "int64"
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

let partition_params_in_out command =
  let rec aux pin pout = function
    | [] ->
       let pin = List.filter (fun p -> List.for_all (fun p' -> p.pname <> p'.length) pin) pin in
       let pout = List.filter (fun p -> List.for_all (fun p' -> p.pname <> p'.length) pout) pout in
       List.rev pin, List.rev pout
    | hd :: tl ->
       if ends_with hd.gl_type "*" && not (starts_with hd.gl_type "const")
       then aux pin (hd :: pout) tl
       else aux (hd :: pin) pout tl
  in
  let pout = if command.proto.gl_type = "void" then [] else [command.proto] in
  aux [] pout command.params

let () =
  let ml_out, c_out =
    if Sys.argv.(0) <> "./generator.exe"
    then open_out "GL.ml", open_out "GL_stubs.c"
    else open_out "GL_preview.ml", open_out "GL_stubs_preview.c"
  in
  try
    let raw_enums_by_name, raw_commands_by_name, features = Registry.load "gl.xml" in
    let api, version = GLES2_API, "2.0" in
    let features = List.filter (fun f -> f.api = api && f.version <= version) features in
    let enums_by_name, commands_by_name = filter_by_feature raw_enums_by_name raw_commands_by_name features in
    let classes =
      Hashtbl.fold (fun _ command acc ->
          List.fold_left (fun acc param ->
              if param.gl_class = "" || List.mem param.gl_class acc then acc else param.gl_class :: acc
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
 *)
    Debug.print_classes classes;
    Printf.eprintf "\n%!";
    let enums_by_group = hash_enums_by_group enums_by_name in
(*
    Debug.print_groups enums_by_group;
    Printf.eprintf "\n%!";
 *)
    Debug.print_empty_groups commands_by_name enums_by_group;
    Printf.eprintf "\n%!";
(*
    Debug.print_identical_groups enums_by_group;
    Printf.eprintf "\n%!";
    Debug.print_groups_partition enums_by_group;
    Printf.eprintf "\n%!";
 *)
    (* /DEBUG *)
    let open Printf in
    (* OCaml: static header *)
    fprintf ml_out "(* Generated by GL-OCaml for API %s v%s. *)

open Bigarray

" (string_of_api api) version;
    (* C: static header *)
    fprintf c_out "/* Generated by GL-OCaml for API %s v%s. */

#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#define CAMLvoid CAMLunused_start value unit CAMLunused_end

#define Val_none Val_int(0)

" (string_of_api api) version;
    (* Types and enums. *)
    List.iter (fprintf ml_out "type %s [@@immediate]\n") classes;
    fprintf ml_out "\ntype 'k enum =\n";
    fprintf c_out "GLenum enums[] = {\n";
    Hashtbl.to_seq_values enums_by_name
    |> List.of_seq
    |> List.sort (fun e1 e2 -> String.compare e1.value e2.value)
    |> List.iter (fun enum ->
           let prefix = ref "<" in
           fprintf ml_out "  | %s : [" (to_pascal_case enum.ename);
           List.iter (fun group ->
               fprintf ml_out "%s`%s" !prefix group;
               prefix := "|"
             ) enum.groups;
           fprintf ml_out "] enum\n";
           fprintf c_out "    %s,\n" enum.ename;
         );
    fprintf ml_out "\n";
    fprintf c_out "};\n\n";
    (* Functions. *)
    Hashtbl.to_seq_values commands_by_name
    |> List.of_seq
    |> List.sort (fun c1 c2 -> String.compare c1.proto.pname c2.proto.pname)
    |> List.iter (fun command ->
           (* DEBUG: copy the C prototypes. *)
           fprintf ml_out "(* %s %s(" command.proto.gl_type command.proto.pname;
           fprintf c_out "/* %s %s(" command.proto.gl_type command.proto.pname;
           let prefix = ref "" in
           List.iter (fun param ->
               fprintf ml_out "%s%s %s" !prefix param.gl_type param.pname;
               fprintf c_out "%s%s %s" !prefix param.gl_type param.pname;
               prefix := ", "
             ) command.params;
           fprintf ml_out ") *)\n";
           fprintf c_out ") */\n";
           (* /DEBUG *)
           let pin, pout = partition_params_in_out command in
           let stub_name =
             if command.alias <> "" && Hashtbl.mem commands_by_name command.alias
             then command.alias
             else command.proto.pname
           in
           (* OCaml *)
           fprintf ml_out "external %s :" (remove_gl_prefix command.proto.pname);
           if pin = []
           then fprintf ml_out " unit"
           else (
             let prefix = ref " " in
             List.iter (fun param ->
                 let ml_type = caml_type_of_param param in
                 if ml_type = Unimplemented then
                   Printf.eprintf "No OCaml replacement for parameter \"%s\" of type \"%s\" in command %s.\n%!"
                     param.pname param.gl_type command.proto.pname;
                 fprintf ml_out "%s%s:%s" !prefix (replace_if_reserved_caml_word param.pname) (string_of_caml_type ml_type);
                 prefix := " -> "
               ) pin
           );
           let result_type =
             List.fold_left (fun str param ->
                 let ml_type = caml_type_of_param param in
                 if ml_type = Unimplemented then
                   Printf.eprintf "No OCaml replacement for output \"%s\" of type \"%s\" in command %s.\n%!"
                     param.pname param.gl_type command.proto.pname;
                 if str = "unit"
                 then string_of_caml_type ml_type
                 else Printf.sprintf "%s * %s" str (string_of_caml_type ml_type)
               ) "unit" pout
           in
           if List.length pin > 5
           then fprintf ml_out " -> %s = \"caml_%s_byte\" \"caml_%s\"\n" result_type stub_name stub_name
           else fprintf ml_out " -> %s = \"caml_%s\"\n" result_type stub_name;
           (* C *)
           fprintf c_out "CAMLprim value caml_%s(" stub_name;
           if pin = []
           then fprintf c_out "CAMLvoid"
           else (
             let prefix = ref "" in
             List.iter (fun param ->
                 fprintf c_out "%svalue %s" !prefix (replace_if_reserved_c_word param.pname);
                 prefix := ", "
               ) pin
           );
           fprintf c_out ")\n{\n";
           List.iter (fun param -> fprintf c_out "    (void)%s;\n" (replace_if_reserved_c_word param.pname)) pin;
           fprintf c_out "    caml_failwith(\"Unimplemented\");\n}\n\n";
           if List.length pin > 5 then (
             fprintf c_out "CAMLprim value caml_%s_byte(value* val_array, int val_count)\n{\n    (void)val_count;\n    return caml_%s(" stub_name stub_name;
             let prefix = ref "" in
             List.iteri (fun i _ -> fprintf c_out "%sval_array[%d]" !prefix i; prefix := ", ") pin;
             fprintf c_out ");\n}\n\n";
           )
         );
    close_out ml_out;
    close_out c_out
  with
  | Xmlm.Error ((l, c), error) ->
     Printf.eprintf "Xmlm.Error: %s at (%d, %d)\n%!" (Xmlm.error_message error) l c
