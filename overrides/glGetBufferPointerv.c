CAMLprim value caml_glGetBufferPointerv(value target, value pname)
{
    CAMLparam0();
    void* pointer;
    glGetBufferPointerv(gl_enums[Int_val(target)], gl_enums[Int_val(pname)], &pointer);
    CAMLlocal1(result);
    result = caml_alloc_small(1, 1);
    Field(result, 0) = Val_int(pointer);
    CAMLreturn(result);
}
