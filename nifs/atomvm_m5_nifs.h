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

#define M5_NIF_v_i(name, module, api_func)                \
    static term name(Context* ctx, int argc, term argv[]) \
    {                                                     \
        UNUSED(argc);                                     \
        return term_from_int(M5.module.api_func());       \
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

#define M5_NIF_i2_i3_v(name, module, api_func)            \
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
                term_to_int(argv[2]));                    \
        }                                                 \
        return OK_ATOM;                                   \
    }
#define M5_NIF_i3_i4_v(name, module, api_func)            \
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
                term_to_int(argv[3]));                    \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i4_i5_v(name, module, api_func)            \
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
                term_to_int(argv[4]));                    \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i5_i6_v(name, module, api_func)            \
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
                term_to_int(argv[5]));                    \
        }                                                 \
        return OK_ATOM;                                   \
    }

#define M5_NIF_i6_i7_v(name, module, api_func)            \
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
                term_to_int(argv[6]));                    \
        }                                                 \
        return OK_ATOM;                                   \
    }
