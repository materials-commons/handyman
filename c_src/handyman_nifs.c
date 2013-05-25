#include "erl_nif.h"
#include <stdlib.h>
#include <pwd.h>
#include <string.h>
#include <stdio.h>

#define BUF_SIZE 2048

static ERL_NIF_TERM ATOM_BADPATH;
static ERL_NIF_TERM ATOM_BADUSER;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;

static char *user_home(char *username);
static struct passwd *getpwentry();

#define STREQL(a,b) (strcmp(a,b) == 0)
#define MAKE_SUCCESS_TUPLE(env, Item) enif_make_tuple2(env, ATOM_OK, Item)
#define MAKE_SUCCESS_TUPLE_STR(env, str) MAKE_SUCCESS_TUPLE(env, enif_make_string(env, str, ERL_NIF_LATIN1))
#define MAKE_ERROR_TUPLE(env, Item) enif_make_tuple2(env, ATOM_ERROR, Item)

static ERL_NIF_TERM realpath_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char dir[BUF_SIZE];
    char resolvedname[BUF_SIZE];

    if (argc != 1 || ! enif_get_string(env, argv[0], dir, BUF_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    return realpath(dir, resolvedname) ?
            MAKE_SUCCESS_TUPLE_STR(env, resolvedname) : MAKE_ERROR_TUPLE(env, ATOM_BADPATH);
                /*enif_make_string(env, resolvedname, ERL_NIF_LATIN1) : ATOM_BADPATH;*/
}

static ERL_NIF_TERM user_home_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char username[32];
    char *dir;

    if (argc != 1 || ! enif_get_string(env, argv[0], username, 32, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    return (dir = user_home(username)) ?
            MAKE_SUCCESS_TUPLE_STR(env, dir) : MAKE_ERROR_TUPLE(env, ATOM_BADUSER);
            /*enif_make_string(env, dir, ERL_NIF_LATIN1) : ATOM_BADUSER;*/
}

static char *user_home(char *username)
{
    struct passwd *pw;
    int found = 0;

    setpwent();

    while((pw = getpwentry()) != NULL)
    {
        if (STREQL(username, pw->pw_name))
        {
            found = 1;
            break;
        }
    }

    endpwent();

    return found ? pw->pw_dir : NULL;
}

static struct passwd *getpwentry()
{
#ifdef __MACH__
    return getpwent();
#elif __linux__
    char buf[BUF_SIZE];
    struct passwd pw, *pwp;

    if (getpwent_r(&pw, buf, BUF_SIZE, &pwp) == 0)
    {
        return pwp;
    }
    else
    {
        return NULL;
    }
#endif
}

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ATOM_BADPATH = enif_make_atom(env, "badpath");
    ATOM_BADUSER = enif_make_atom(env, "baduser");
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"realpath_nif", 1, realpath_nif},
    {"user_home_nif", 1, user_home_nif}
};

ERL_NIF_INIT(handyman_nifs, nif_funcs, &on_load, NULL, NULL, NULL);

