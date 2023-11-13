CAMLprim value caml_glGetActiveUniform(value program, value index)
{
    CAMLparam0();
    GLsizei length;
    GLint size;
    GLenum type;
    GLint bufSize;
    glGetProgramiv(Int_val(program), GL_ACTIVE_UNIFORM_MAX_LENGTH, &bufSize);
    GLchar name[bufSize];
    glGetActiveUniform(Int_val(program), Int_val(index), bufSize, &length, &size, &type, name);
    CAMLlocal1(name_string);
    name_string = caml_alloc_initialized_string(length, name);
    CAMLlocal1(result_tuple);
    result_tuple = caml_alloc_small(0, 3);
    Field(result_tuple, 0) = Val_int(size);
    Field(result_tuple, 1) = Val_int(find_enum_offset(gl_enums, sizeof(gl_enums) / sizeof(*gl_enums), type));
    Field(result_tuple, 2) = name_string;
    CAMLreturn(result_tuple);
}

