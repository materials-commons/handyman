#ifdef _WIN32

#include "erl_nif.h"

#include <windows.h>
#include <lm.h>
#include <sddl.h>
#include <unistd.h>
#include <stdlib.h>

static ERL_NIF_TERM ATOM_BADPATH;
static ERL_NIF_TERM ATOM_BADUSER;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_PASSWD;

#define BUF_SIZE 2048
#define STREQL(a,b) (strcmp(a,b) == 0)
#define MAKE_SUCCESS_TUPLE(env, Item) enif_make_tuple2(env, ATOM_OK, Item)
#define MAKE_SUCCESS_TUPLE_STR(env, str) MAKE_SUCCESS_TUPLE(env, enif_make_string(env, str, ERL_NIF_LATIN1))
#define MAKE_ERROR_TUPLE(env, Item) enif_make_tuple2(env, ATOM_ERROR, Item)

static char *realpath(const char *name, char *resolvedname);
static LPUSER_INFO_1 find_userentry(char *username);
static ERL_NIF_TERM make_passwd_record(ErlNifEnv *env, LPUSER_INFO_1 pw);

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
}

static char *realpath(const char *name, char *resolvedname)
{
    if (name == NULL)
    {
        return NULL;
    }
    else if (access(name, R_OK) != 0)
    {
        return NULL;
    }
    else
    {
        resolvedname = _fullpath(resolvedname, name, BUF_SIZE);
        return resolvedname;
    }
}

static ERL_NIF_TERM getpwent_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char username[32];
    LPUSER_INFO_1 pw;

    if (argc != 1 || ! enif_get_string(env, argv[0], username, 32, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    return (pw = find_userentry(username)) ? MAKE_SUCCESS_TUPLE(env, make_passwd_record(env, pw)) :
            MAKE_ERROR_TUPLE(env, ATOM_BADUSER);
}

static LPUSER_INFO_1 find_userentry(char *username)
{
    LPUSER_INFO_1 buf;

    if (NetUserGetInfo(NULL, username, (DWORD) 1, (LPBYTE *) &buf) == NERR_Success)
    {
        return buf;
    }
    else
    {
        return NULL;
    }
}

static ERL_NIF_TERM make_passwd_record(ErlNifEnv *env, LPUSER_INFO_1 pw)
{
    return enif_make_tuple3(env,
                ATOM_PASSWD,
                enif_make_string(env, pw->usri1_home_dir, ERL_NIF_LATIN1),
                enif_make_string(env, pw->usri1_name, ERL_NIF_LATIN1));
}

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ATOM_BADPATH = enif_make_atom(env, "badpath");
    ATOM_BADUSER = enif_make_atom(env, "baduser");
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_PASSWD = enif_make_atom(env, "passwd");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"realpath_nif", 1, realpath_nif},
    {"getpwent_nif", 1, getpwent_nif}
};

ERL_NIF_INIT(handyman_nifs, nif_funcs, &on_load, NULL, NULL, NULL);


#endif
