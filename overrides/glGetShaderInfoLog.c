CAMLprim value caml_glGetShaderInfoLog(value shader)
{
    CAMLparam0();
    GLint length;
    glGetShaderiv(Int_val(shader), GL_INFO_LOG_LENGTH, &length);
    CAMLlocal1(infoLog_string);
    infoLog_string = caml_alloc_string(length > 0 ? length - 1 : 0);
    glGetShaderInfoLog(Int_val(shader), length, NULL, Bytes_val(infoLog_string));
    CAMLreturn(infoLog_string);
}

