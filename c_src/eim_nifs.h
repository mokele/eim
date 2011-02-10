#include "eim_image.cpp"

static ErlNifResourceType* EIM_IMAGE_RESOURCE;

typedef struct
{
    eim_image* image;
} ihandle;

extern "C"
{
    #include "erl_nif.h"
    
    
    ERL_NIF_TERM load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM derive_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    
    int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
    
    static ErlNifFunc nif_funcs[] = {
        {"load", 1, load_nif},
        {"do_derive", 2, derive_nif}
    };
    ERL_NIF_INIT(eim, nif_funcs, &on_load, NULL, NULL, NULL);
}