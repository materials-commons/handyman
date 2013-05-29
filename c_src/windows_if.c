#ifdef _WIN32

#include "os_if.h"

#include <windows.h>
#include <lm.h>
#include <sddl.h>
#include <unistd.h>
#include <stdlib.h>

static LPUSER_INFO_1 find_windows_userentry(char *username);
static handy_user *make_handy_user(LPUSER_INFO_1 lpuser);

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
        resolvedname = _fullpath(resolvedname, name, BUF_SIZE);
        return resolvedname;
    }
}

struct handy_user *find_user_entry(char *username)
{
	LPUSER_INFO_1 lpuser;
	struct handy_user user;

	if ((lpuser = find_windws_userentry(username)))
	{
		return make_handy_user(lpuser);
	}
	else
	{
		return NULL;
	}
}

static LPUSER_INFO_1 find_windows_userentry(char *username)
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

static handy_user *make_handy_user(LPUSER_INFO_1 lpuser)
{
	struct handy_user *user = (struct handy_user *) malloc(sizeof(struct handy_user));
	user->username = malloc(strlen(lpuser->usri1_name) + 1);
	strcpy(user->username, lpuser->user1_name);
	user->homedir = malloc(strlen(lpuser->usri1_home_dir) + 1);
	strcpy(user->homedir, lpuser->usri1_home_dir);
	return user;
}

#endif