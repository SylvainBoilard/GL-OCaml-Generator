CAMLprim value caml_glMapBufferRange(value target, value offset, value length, value access)
{
    CAMLparam0();
    void* pointer = glMapBufferRange(gl_enums[Int_val(target)], (GLintptr)Int_val(offset), (GLsizeiptr)Int_val(length), caml_convert_flag_list(access, gl_enums));
    CAMLlocal1(result);
    result = caml_alloc_small(1, 1);
    Field(result, 0) = Val_int(pointer);
    CAMLreturn(result);
}
