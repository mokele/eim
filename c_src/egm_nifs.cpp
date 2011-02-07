#include "egm_nifs.h"
#include "egm.cpp"

ERL_NIF_TERM resize_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int ret;
    ret = 69;
    egm::resize();
    return enif_make_int(env, ret);
}