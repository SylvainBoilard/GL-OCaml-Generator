CAMLprim value caml_glGetVertexAttribPointerv(value index, value pname)
{
    CAMLparam0();

#if !(defined(GL_ES_VERSION_3_0) || defined(GL_VERSION_3_0))
    GLint buffer;
    glGetIntegerv(GL_ARRAY_BUFFER_BINDING, &buffer);
    if (buffer == 0)
        caml_failwith("GL.getVertexAttribPointerv: result is an unsafe pointer (no array buffer bound).");
#endif // !(defined(GL_ES_VERSION_3_0) || defined(GL_VERSION_3_0))

    void* pointer;
    glGetVertexAttribPointerv(Int_val(index), gl_enums[Int_val(pname)], &pointer);
    CAMLlocal1(result);
    result = caml_alloc_small(1, 1);
    Field(result, 0) = Val_int(pointer);
    CAMLreturn(result);
}
