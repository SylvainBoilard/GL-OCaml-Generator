CAMLprim value caml_glGetProgramBinary(value program)
{
    CAMLparam0();
    GLint length;
    glGetProgramiv(Int_val(program), GL_PROGRAM_BINARY_LENGTH, &length);
    GLenum binaryFormat;
    CAMLlocal2(binary, result);
    binary = caml_alloc_string(length);
    glGetProgramBinary(Int_val(program), length, NULL, &binaryFormat, Bytes_val(binary));
    result = caml_alloc_small(0, 2);
    Field(result, 0) = Val_int(binaryFormat);
    Field(result, 1) = binary;
    CAMLreturn(result);
}
