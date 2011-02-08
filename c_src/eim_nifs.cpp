#include "eim_nifs.h"
#include "eim.cpp"
#include "erl_nif_compat.h"
#include <string.h>

ERL_NIF_TERM resize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    int width, height;
    if(enif_inspect_binary(env, argv[0], &data)
        && enif_get_int(env, argv[1], &width)
        && enif_get_int(env, argv[2], &height))
    {
        int ret;
        ret = 69;
        
        ErlNifBinary new_binary;
        
        size_t new_length;
        unsigned char *new_blob;

        new_blob = eim::resize(data.data, data.size, width, height, &new_length);
        enif_alloc_binary_compat(env, new_length, &new_binary);
        memcpy(new_binary.data, new_blob, new_length);
        return enif_make_binary(env, &new_binary);
    }
    else
    {
        return enif_make_badarg(env);
    }
}
