#ifndef __OS_IF__H
#define __OS_IF__H

#include <string.h>

#define MAX_PATH_SIZE 512
#define MAX_USER_NAME_SIZE 64

#define STREQL(a,b) (strcmp(a,b) == 0)

struct handy_user {
	char *username;
	char *homedir;
};

struct handy_user *find_user_entry(char *username);
char *username();
char *tmpdir();

#ifdef _WIN32
	char *realpath(const char *name, char *resolvedname);
#endif

#endif /* __OS_IF__H */
