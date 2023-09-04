/* SPDX-License-Identifier: MIT */

// Common macros to simplify glue
#define M5_NIF_v_v(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        UNUSED(argv);                                     \
        M5.module.api_func();                             \
        return OK_ATOM;                                   \
    }

#define M5_NIF_v_3b(name, module, api_func)                                                                                   \
    static term name(Context* ctx, int argc, term argv[])                                                                     \
    {                                                                                                                         \
        UNUSED(argc);                                                                                                         \
        UNUSED(argv);                                                                                                         \
        float x, y, z;                                                                                                        \
        bool r = M5.module.api_func(&x, &y, &z);                                                                              \
        if (memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(3) + 3 * FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) { \
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);                                                                                  \
        }                                                                                                                     \
        term point = term_alloc_tuple(3, &ctx->heap);                                                                         \
        term result = term_alloc_tuple(2, &ctx->heap);                                                                        \
        term_put_tuple_element(result, 0, r ? TRUE_ATOM : FALSE_ATOM);                                                        \
        term_put_tuple_element(result, 1, point);                                                                             \
        term_put_tuple_element(point, 0, term_from_float(x, &ctx->heap));                                                     \
        term_put_tuple_element(point, 1, term_from_float(y, &ctx->heap));                                                     \
        term_put_tuple_element(point, 2, term_from_float(z, &ctx->heap));                                                     \
        return result;                                                                                                        \
    }

#define M5_NIF_v_b(name, module, api_func)                    \
    static term name(Context* ctx, int argc, term argv[])     \
    {                                                         \
        UNUSED(argc);                                         \
        UNUSED(argv);                                         \
        return M5.module.api_func() ? TRUE_ATOM : FALSE_ATOM; \
    }

#define M5_NIF_v_i(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        return term_from_int(M5.module.api_func());       \
    }

#define M5_NIF_b_v(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_atom);            \
        M5.module.api_func(                               \
            term_to_int(argv[0]) == TRUE_ATOM);           \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i_b(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_integer);         \
        bool r = M5.module.api_func(                      \
            term_to_int(argv[0]));                        \
        return r ? TRUE_ATOM : FALSE_ATOM;                \
    }

#define M5_NIF_i_v(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_integer);         \
        M5.module.api_func(                               \
            term_to_int(argv[0]));                        \
        return OK_ATOM;                                   \
    }

#define M5_NIF_u_v(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_integer);         \
        M5.module.api_func(                               \
            (uint32_t)term_to_int(argv[0]));              \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i2_v(name, module, api_func)               \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_integer);         \
        VALIDATE_VALUE(argv[1], term_is_integer);         \
        M5.module.api_func(                               \
            term_to_int(argv[0]),                         \
            term_to_int(argv[1]));                        \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i3_v(name, module, api_func)               \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_integer);         \
        VALIDATE_VALUE(argv[1], term_is_integer);         \
        VALIDATE_VALUE(argv[2], term_is_integer);         \
        M5.module.api_func(                               \
            term_to_int(argv[0]),                         \
            term_to_int(argv[1]),                         \
            term_to_int(argv[2]));                        \
        return OK_ATOM;                                   \
    }

#define M5_NIF_ici2_v(name, module, api_func, t)          \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[0], term_is_integer);         \
        VALIDATE_VALUE(argv[1], term_is_integer);         \
        VALIDATE_VALUE(argv[2], term_is_integer);         \
        M5.module.api_func(                               \
            (t)term_to_int(argv[0]),                      \
            term_to_int(argv[1]),                         \
            term_to_int(argv[2]));                        \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i4_v(name, module, api_func)               \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_integer);     \
        }                                                 \
        M5.module.api_func(                               \
            term_to_int(argv[0]),                         \
            term_to_int(argv[1]),                         \
            term_to_int(argv[2]),                         \
            term_to_int(argv[3]));                        \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i32o4_v(name, module, api_func)                                               \
    static term name(Context* ctx, int argc, term argv[])                                    \
    {                                                                                        \
        UNUSED(argc);                                                                        \
        UNUSED(argv);                                                                        \
        int32_t x, y, w, h;                                                                  \
        M5.module.api_func(&x, &y, &w, &h);                                                  \
        if (memory_ensure_free_opt(ctx, TUPLE_SIZE(4), MEMORY_CAN_SHRINK) != MEMORY_GC_OK) { \
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);                                                 \
        }                                                                                    \
        term result = term_alloc_tuple(4, &ctx->heap);                                       \
        term_put_tuple_element(result, 0, term_from_int(x));                                 \
        term_put_tuple_element(result, 1, term_from_int(y));                                 \
        term_put_tuple_element(result, 2, term_from_int(w));                                 \
        term_put_tuple_element(result, 3, term_from_int(h));                                 \
        return result;                                                                       \
    }

#define M5_NIF_si2_i(name, module, api_func)              \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        VALIDATE_VALUE(argv[1], term_is_integer);         \
        VALIDATE_VALUE(argv[2], term_is_integer);         \
        size_t slen;                                      \
        switch (interop_iolist_size(argv[0], &slen)) {    \
        case InteropOk:                                   \
            break;                                        \
        case InteropMemoryAllocFail:                      \
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);              \
        case InteropBadArg:                               \
            RAISE_ERROR(BADARG_ATOM);                     \
        }                                                 \
        char* sbuf = (char*)malloc(slen + 1);             \
        if (IS_NULL_PTR(sbuf)) {                          \
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);              \
        }                                                 \
        switch (interop_write_iolist(argv[0], sbuf)) {    \
        case InteropOk:                                   \
            break;                                        \
        case InteropMemoryAllocFail:                      \
            free(sbuf);                                   \
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);              \
        case InteropBadArg:                               \
            free(sbuf);                                   \
            RAISE_ERROR(BADARG_ATOM);                     \
        }                                                 \
        sbuf[slen] = 0;                                   \
        int r = M5.module.api_func(                       \
            sbuf,                                         \
            term_to_int(argv[1]),                         \
            term_to_int(argv[2]));                        \
        free(sbuf);                                       \
        return term_from_int(r);                          \
    }

#define M5_NIF_v_u_v(name, module, api_func)              \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        if (argc == 0) {                                  \
            M5.module.api_func();                         \
        } else {                                          \
            VALIDATE_VALUE(argv[0], term_is_integer);     \
            M5.module.api_func(                           \
                (uint32_t)term_to_int(argv[0]));          \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_f_f2_v(name, module, api_func)             \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_number);      \
        }                                                 \
        if (argc == 1) {                                  \
            M5.module.api_func(                           \
                term_conv_to_float(argv[0]));             \
        } else {                                          \
            M5.module.api_func(                           \
                term_conv_to_float(argv[0]),              \
                term_conv_to_float(argv[1]));             \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i2_i2u_v(name, module, api_func)           \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_integer);     \
        }                                                 \
        if (argc == 2) {                                  \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]));                    \
        } else {                                          \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                (uint32_t)term_to_int(argv[2]));          \
        }                                                 \
        return OK_ATOM;                                   \
    }
#define M5_NIF_i3_i3u_v(name, module, api_func)           \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_integer);     \
        }                                                 \
        if (argc == 3) {                                  \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]));                    \
        } else {                                          \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                (uint32_t)term_to_int(argv[3]));          \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i4_i4u_v(name, module, api_func)           \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_integer);     \
        }                                                 \
        if (argc == 4) {                                  \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                term_to_int(argv[3]));                    \
        } else {                                          \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                term_to_int(argv[3]),                     \
                (uint32_t)term_to_int(argv[4]));          \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i5_i5u_v(name, module, api_func)           \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_integer);     \
        }                                                 \
        if (argc == 5) {                                  \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                term_to_int(argv[3]),                     \
                term_to_int(argv[4]));                    \
        } else {                                          \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                term_to_int(argv[3]),                     \
                term_to_int(argv[4]),                     \
                (uint32_t)term_to_int(argv[5]));          \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i6_i6u_v(name, module, api_func)           \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        for (int i = 0; i < argc; i++) {                  \
            VALIDATE_VALUE(argv[i], term_is_integer);     \
        }                                                 \
        if (argc == 6) {                                  \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                term_to_int(argv[3]),                     \
                term_to_int(argv[4]),                     \
                term_to_int(argv[5]));                    \
        } else {                                          \
            M5.module.api_func(                           \
                term_to_int(argv[0]),                     \
                term_to_int(argv[1]),                     \
                term_to_int(argv[2]),                     \
                term_to_int(argv[3]),                     \
                term_to_int(argv[4]),                     \
                term_to_int(argv[5]),                     \
                (uint32_t)term_to_int(argv[6]));          \
        }                                                 \
        return OK_ATOM;                                   \
    }
