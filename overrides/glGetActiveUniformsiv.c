CAMLprim value caml_glGetActiveUniformsiv(value program, value uniformIndices, value pname)
{
    CAMLparam0();
    const GLsizei uniformCount = Wosize_val(uniformIndices);
    GLint params_array[uniformCount];
    glGetActiveUniformsiv(Int_val(program), uniformCount, Caml_ba_data_val(uniformIndices), gl_enums[Int_val(pname)], params_array);
    CAMLlocal1(caml_params_array);
    caml_params_array = caml_alloc_small(0, uniformCount);
    for (int i = 0; i < uniformCount; ++i)
        Field(caml_params_array, i) = Val_int(params_array[i]);
    CAMLreturn(caml_params_array);
}
