CAMLprim value caml_glGetProgramBinary(value program, value bufSize)
{
    CAMLparam0();
    GLsizei length;
    GLenum binaryFormat;
    CAMLlocal2(binary, result);
    binary = caml_alloc_string(Int_val(bufSize));
    glGetProgramBinary(Int_val(program), Int_val(bufSize), &length, &binaryFormat, Bytes_val(binary));
    result = caml_alloc_small(0, 3);
    Field(result, 0) = Val_int(length);
    Field(result, 1) = Val_int(binaryFormat);
    Field(result, 2) = binary;
    CAMLreturn(result);
}
