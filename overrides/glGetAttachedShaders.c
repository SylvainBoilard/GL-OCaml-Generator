CAMLprim value caml_glGetAttachedShaders(value program)
{
    CAMLparam0();
    GLint shaders_length;
    glGetProgramiv(Int_val(program), GL_ATTACHED_SHADERS, &shaders_length);
    GLuint shaders_array[shaders_length];
    glGetAttachedShaders(Int_val(program), shaders_length, NULL, shaders_array);
    CAMLlocal1(caml_shaders_array);
    caml_shaders_array = caml_alloc_small(0, shaders_length);
    for (int i = 0; i < shaders_length; ++i)
        Field(caml_shaders_array, i) = Val_int(shaders_array[i]);
    CAMLreturn(caml_shaders_array);
}

