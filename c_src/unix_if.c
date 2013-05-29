#if defined __MACH__ || defined __linux__

#include "os_if.h"

#include <stdlib.h>
#include <pwd.h>
#include <stdio.h>
#include <unistd.h>

static struct passwd *find_pwentry(char *username);
static struct passwd *getpwentry();
static struct handy_user *make_handy_user(struct passwd *pw);

struct handy_user *find_user_entry(char *username)
{
	struct passwd *pw = find_pwentry(username);
	return pw ? make_handy_user(pw) : NULL;
}

static struct passwd *find_pwentry(char *username)
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

    return found ? pw : NULL;
}

static struct passwd *getpwentry()
{
#ifdef __MACH__
    return getpwent();
#elif __linux__

#define BUF_SIZE 2048

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

static struct handy_user *make_handy_user(struct passwd *pw)
{
	struct handy_user *user = (struct handy_user *) malloc(sizeof(struct handy_user));
	user->username = malloc(strlen(pw->pw_name) + 1);
	strcpy(user->username, pw->pw_name);
	user->homedir = malloc(strlen(pw->pw_dir) + 1);
	strcpy(user->homedir, pw->pw_dir);
	return user;
}

char *username()
{
#ifdef __MACH__
    return getlogin();
#elif __linux__
    char usernamebuf[MAX_USER_NAME_SIZE];
    return (getlogin_r(usernamebuf, MAX_USER_NAME_SIZE) == 0) ? usernamebuf : NULL;
#endif
}

char *tmpdir()
{
    return "/tmp";
}

#endif
