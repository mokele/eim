
extern "C"
{
    #include "erl_nif.h"
    ERL_NIF_TERM resize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    
    static ErlNifFunc nif_funcs[] = {
        {"resize", 3, resize_nif}
    };
    ERL_NIF_INIT(eim, nif_funcs, NULL, NULL, NULL, NULL);
}