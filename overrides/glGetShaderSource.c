CAMLprim value caml_glGetShaderSource(value shader)
{
    CAMLparam0();
    GLint length;
    glGetShaderiv(Int_val(shader), GL_SHADER_SOURCE_LENGTH, &length);
    CAMLlocal1(source_string);
    source_string = caml_alloc_string(length > 0 ? length - 1 : 0);
    glGetShaderSource(Int_val(shader), length, NULL, Bytes_val(source_string));
    CAMLreturn(source_string);
}

