%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc OS independent user information.
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

-module(handyuser).
-export([user_home/1, username/0]).

user_home(Username) -> user_home(Username, os:type()).

user_home(Username, {unix, _Os}) ->
    Cmd = "cd ~" ++ Username ++ "; pwd",
    Dir = binary_to_list(handystring:trim_trailing(os:cmd(Cmd), "\n")),
    case string:str(Dir, "cd:") of
        0 -> Dir;
        _ -> false %% Command failed
    end;
user_home(_Username, {win32, _Os}) -> throw(notimplemented).

username() -> username(os:type()).

username({unix, _Os}) -> os:getenv("USER");
username({win32, _Os}) -> throw(notimplemented).