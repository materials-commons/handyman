
#include "erl_nif.h"
#include "os_if.h"

static ERL_NIF_TERM ATOM_BADPATH;
static ERL_NIF_TERM ATOM_BADUSER;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_HANDY_USER;

#define MAKE_SUCCESS_TUPLE(env, Item) enif_make_tuple2(env, ATOM_OK, Item)
#define MAKE_SUCCESS_TUPLE_STR(env, str) MAKE_SUCCESS_TUPLE(env, enif_make_string(env, str, ERL_NIF_LATIN1))
#define MAKE_ERROR_TUPLE(env, Item) enif_make_tuple2(env, ATOM_ERROR, Item)

static ERL_NIF_TERM make_handy_user_record(ErlNifEnv *env, struct handy_user *user);
void free_user(struct handy_user *user);

static ERL_NIF_TERM realpath_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char dir[MAX_PATH_SIZE];
    char resolvedname[MAX_PATH_SIZE];

    if (argc != 1 || ! enif_get_string(env, argv[0], dir, MAX_PATH_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    return realpath(dir, resolvedname) ?
            MAKE_SUCCESS_TUPLE_STR(env, resolvedname) : MAKE_ERROR_TUPLE(env, ATOM_BADPATH);
}

static ERL_NIF_TERM getuser_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char username[32];
    struct handy_user *user;

    if (argc != 1 || ! enif_get_string(env, argv[0], username, 32, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    return (user = find_user_entry(username)) ? MAKE_SUCCESS_TUPLE(env, make_handy_user_record(env, user)) :
            MAKE_ERROR_TUPLE(env, ATOM_BADUSER);
}

static ERL_NIF_TERM username_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char *usernamebuf;

    if (argc != 0)
    {
        return enif_make_badarg(env);
    }

    return (usernamebuf = username()) ?
            MAKE_SUCCESS_TUPLE_STR(env, usernamebuf) : MAKE_ERROR_TUPLE(env, ATOM_BADUSER);
}

static ERL_NIF_TERM tmpdir_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char *tmpdirbuf;

    if (argc != 0)
    {
        return enif_make_badarg(env);
    }

    return (tmpdirbuf = tmpdir()) ? MAKE_SUCCESS_TUPLE_STR(env, tmpdirbuf) :
            MAKE_ERROR_TUPLE(env, ATOM_BADPATH);
}

static ERL_NIF_TERM make_handy_user_record(ErlNifEnv *env, struct handy_user *user)
{
    ERL_NIF_TERM pwentry = enif_make_tuple3(env,
                			ATOM_HANDY_USER,
                			enif_make_string(env, user->homedir, ERL_NIF_LATIN1),
                			enif_make_string(env, user->username, ERL_NIF_LATIN1));
    free_user(user);
    return pwentry;
}

void free_user(struct handy_user *user)
{
	free(user->homedir);
    free(user->username);
    free(user);
}

static int on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ATOM_BADPATH = enif_make_atom(env, "badpath");
    ATOM_BADUSER = enif_make_atom(env, "baduser");
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_HANDY_USER = enif_make_atom(env, "handy_user");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"realpath_nif", 1, realpath_nif},
    {"getuser_nif", 1, getuser_nif},
    {"username_nif", 0, username_nif},
    {"tmpdir_nif", 0, tmpdir_nif}
};

ERL_NIF_INIT(handyman_nifs, nif_funcs, &on_load, NULL, NULL, NULL);