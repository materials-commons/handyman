#ifdef _WIN32

#include "os_if.h"

#include <windows.h>
#include <lm.h>
#include <sddl.h>
#include <unistd.h>
#include <stdlib.h>

static LPUSER_INFO_1 find_windows_user_entry(char *username);
static struct handy_user *make_handy_user(LPUSER_INFO_1 lpuser);

char *realpath(const char *name, char *resolvedname)
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
        resolvedname = _fullpath(resolvedname, name, MAX_PATH_SIZE);
        return resolvedname;
    }
}

struct handy_user *find_user_entry(char *username)
{
	LPUSER_INFO_1 lpuser;

    return (lpuser = find_windows_user_entry(username)) ?
            make_handy_user(lpuser) : NULL;
}

static LPUSER_INFO_1 find_windows_user_entry(char *username)
{
	LPUSER_INFO_1 buf;
    wchar_t username_wide[32];

    mbstowcs(username_wide, username, 32);
    return (NetUserGetInfo(NULL, username_wide, (DWORD) 1,
                            (LPBYTE *) &buf) == NERR_Success) ? buf : NULL;
}

static struct handy_user *make_handy_user(LPUSER_INFO_1 lpuser)
{
    char buf[512];
    wcstombs(buf, lpuser->usri1_name, 512);
	struct handy_user *user = (struct handy_user *) malloc(sizeof(struct handy_user));
	user->username = malloc(strlen(buf) + 1);
	strcpy(user->username, buf);

    wcstombs(buf, lpuser->usri1_home_dir, 512);
	user->homedir = malloc(strlen(buf) + 1);
	strcpy(user->homedir, buf);
	return user;
}

char *username()
{
    char usernamebuf[MAX_USER_NAME_SIZE];
    DWORD size = MAX_USER_NAME_SIZE;

    /*
    ** Unlike Unix If GetUserName() returns 0 then it failed
    */
    return (GetUserName(usernamebuf, &size) != 0) ? usernamebuf : NULL;
}

char *tmpdir()
{
    char tmpdirbuf[MAX_PATH_SIZE];

    /*
    ** GetTempPath returns 0 on failure
    */
    return (GetTempPath(MAX_PATH_SIZE, tmpdirbuf) != 0) ? tmpdirbuf : NULL;
}

#endif
