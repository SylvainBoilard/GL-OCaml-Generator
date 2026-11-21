CAMLprim value caml_glGetProgramBinary(value program)
{
    CAMLparam0();
    GLint binaryLength;
    glGetProgramiv(Int_val(program), GL_PROGRAM_BINARY_LENGTH, &binaryLength);
    GLsizei length;
    GLenum binaryFormat;
    CAMLlocal2(binary, result);
    binary = caml_alloc_string(binaryLength > 0 ? binaryLength : 0);
    glGetProgramBinary(Int_val(program), binaryLength, &length, &binaryFormat, Bytes_val(binary));
    result = caml_alloc_small(0, 3);
    Field(result, 0) = Val_int(length);
    Field(result, 1) = Val_int(binaryFormat);
    Field(result, 2) = binary;
    CAMLreturn(result);
}
