#include "erl_nif.h"
#include <stdlib.h>

#define BUF_SIZE 2048

static ERL_NIF_TERM ATOM_BADPATH;

static ERL_NIF_TERM realpath_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char dir[BUF_SIZE];
    char resolvedname[BUF_SIZE];

    if (argc != 1)
    {
        return enif_make_badarg(env);
    }

    if (! enif_get_string(env, argv[0], dir, BUF_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    if (! realpath(dir, resolvedname))
    {
        return ATOM_BADPATH;
    }
    else
    {
        return enif_make_string(env, resolvedname, ERL_NIF_LATIN1);
    }
}

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ATOM_BADPATH = enif_make_atom(env, "badpath");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"realpath_nif", 1, realpath_nif}
};

ERL_NIF_INIT(handyman_nifs, nif_funcs, &on_load, NULL, NULL, NULL);

