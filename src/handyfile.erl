%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc file utilities.
%%%
%%% @copyright Copyright (c) 2013, Regents of the University of Michigan.
%%% All rights reserved.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% ===================================================================

-module(handyfile).
-export([file_exists/1, is_symlink/1, realpath/1, tmpdir/0]).

-include_lib("kernel/include/file.hrl").

%% @doc Check if file exists (is readable).
-spec file_exists(string()) -> boolean().
file_exists(Filepath) ->
    case file:read_file_info(Filepath) of
        {ok, _FileInfo} -> true;
        {error, enoent} -> false;
        _ -> false
    end.

-spec is_symlink(string()) -> boolean().
is_symlink(FilePath) ->
    case file:read_link_info(FilePath) of
        {ok, #file_info{type = symlink}} -> true;
        _ -> false
    end.

-spec realpath(string()) -> {ok, string()} | {error, badpath}.
realpath(FilePath) ->
    handyman_nifs:realpath_nif(FilePath).

-spec tmpdir() -> {ok, string()} | {error, badpath}.
tmpdir() -> handyman_nifs:tmpdir_nif().
