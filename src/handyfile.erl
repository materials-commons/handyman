-module(handyfile).
-export([file_exists/1]).

file_exists(Filepath) ->
    case file:read_file_info(Filepath) of
        {ok, _FileInfo} -> true;
        {error, enoent} -> false;
        _ -> false
    end.
