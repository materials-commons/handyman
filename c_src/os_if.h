
#define MAX_PATH_SIZE 512

struct handy_user {
	char *username;
	char *homedir;
};

struct handy_user *find_user_entry(char *username);

#ifdef _WIN32
	char *realpath(const char *name, char *resolvedname);
#endif