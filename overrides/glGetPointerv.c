CAMLprim value caml_glGetPointerv(value pname)
{
    CAMLparam0();
    void* pointer;
    glGetPointerv(gl_enums[Int_val(pname)], &pointer);
    CAMLlocal1(result);
    result = caml_alloc_small(1, 1);
    Field(result, 0) = Val_int(pointer);
    CAMLreturn(result);
}
