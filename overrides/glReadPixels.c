CAMLprim value caml_glReadPixels(value x, value y, value width, value height, value format, value type)
{
    // TODO: make this work for any GL version. GLES 2.0 only right now.
    CAMLparam0();
    int old_pack_alignment;
    CAMLlocal1(pixels);
    int kind = CAML_BA_UINT8;
    int elems = 1;
    switch (gl_enums[Int_val(type)])
    {
    case GL_UNSIGNED_BYTE:
        switch (gl_enums[Int_val(format)])
        {
        case GL_RGB:
            elems = 3;
            break;
        case GL_RGBA:
            elems = 4;
        }
        break;
    case GL_UNSIGNED_SHORT_5_6_5:
    case GL_UNSIGNED_SHORT_4_4_4_4:
    case GL_UNSIGNED_SHORT_5_5_5_1:
        kind = CAML_BA_UINT16;
    }
    pixels = caml_ba_alloc_dims(kind | CAML_BA_C_LAYOUT, 2, NULL, Nativeint_val(width) * elems, Nativeint_val(height));
    glGetIntegerv(GL_PACK_ALIGNMENT, &old_pack_alignment);
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadPixels(Int_val(x), Int_val(y), Int_val(width), Int_val(height), gl_enums[Int_val(format)], gl_enums[Int_val(type)], Caml_ba_data_val(pixels));
    glPixelStorei(GL_PACK_ALIGNMENT, old_pack_alignment);
    CAMLreturn(pixels);
}

CAMLprim value caml_glReadPixels_byte(value* val_array, int val_count)
{
    (void)val_count;
    return caml_glReadPixels(val_array[0], val_array[1], val_array[2], val_array[3], val_array[4], val_array[5]);
}

