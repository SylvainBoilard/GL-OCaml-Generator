CAMLprim value caml_glGetProgramInfoLog(value program)
{
    CAMLparam0();
    GLint length;
    glGetProgramiv(Int_val(program), GL_INFO_LOG_LENGTH, &length);
    CAMLlocal1(infoLog_string);
    infoLog_string = caml_alloc_string(length > 0 ? length - 1 : 0);
    glGetProgramInfoLog(Int_val(program), length, NULL, Bytes_val(infoLog_string));
    CAMLreturn(infoLog_string);
}

