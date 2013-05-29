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
-module(handyman_nifs).

%% API
-export([realpath_nif/1, getuser_nif/1, username_nif/0]).

%% On module load
-on_load(init/0).

%%%===================================================================
%%% API
%%%===================================================================

realpath_nif(_) ->
    not_loaded(?LINE).

getuser_nif(_) ->
	not_loaded(?LINE).

username_nif() ->
    not_loaded(?LINE).

%%%===================================================================
%%% Local
%%%===================================================================

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%%===================================================================
%%% on_load
%%%===================================================================

init() ->
    Library = handyman_library(code:priv_dir(handyman)),
    erlang:load_nif(Library, 0).

handyman_library({error, bad_name}) ->
    from_code_location(code:which(?MODULE));
handyman_library(Dir) -> filename:join(Dir, "handyman").

from_code_location(Filename) when is_list(Filename) ->
    filename:join([filename:dirname(Filename), "..", "priv", "handyman"]);
from_code_location(_) ->
    filename:join(["..", "priv", "handyman"]).
