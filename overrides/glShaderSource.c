CAMLprim value caml_glShaderSource(value shader, value string)
{
    const GLsizei string_count = Wosize_val(string);
    const GLchar* string_array[string_count];
    GLint length_array[string_count];
    for (int i = 0; i < string_count; ++i)
    {
        string_array[i] = Caml_ba_data_val(Field(string, i));
        length_array[i] = caml_ba_byte_size(Caml_ba_array_val(Field(string, i)));
    }
    glShaderSource(Int_val(shader), string_count, string_array, length_array);
    return Val_unit;
}

