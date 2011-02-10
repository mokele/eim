#include "eim_nifs.h"
#include "erl_nif_compat.h"
#include <string.h>

ERL_NIF_TERM load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    if(enif_inspect_binary(env, argv[0], &data))
    {
        ihandle* handle = (ihandle*)enif_alloc_resource_compat(env, EIM_IMAGE_RESOURCE, sizeof(ihandle));
        handle->image = new eim_image(data.data, data.size);
        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource_compat(env, handle);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}
ERL_NIF_TERM derive_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ihandle* handle;
    if (enif_get_resource(env, argv[0], EIM_IMAGE_RESOURCE, (void**)&handle))
    {
        ErlNifBinary new_binary;
        size_t new_length;
        unsigned char *new_blob;
        
        new_blob = handle->image->resize(200, 200, &new_length);
        enif_alloc_binary_compat(env, new_length, &new_binary);
        memcpy(new_binary.data, new_blob, new_length);
        return enif_make_binary(env, &new_binary);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    EIM_IMAGE_RESOURCE = enif_open_resource_type_compat(env, "eim_image_resource", NULL, flags, 0);
    return 0;
}


