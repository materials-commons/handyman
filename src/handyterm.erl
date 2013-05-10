%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Utilities for translating Erlang terms to strings, or strings
%%%      back to Erlang terms.
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

-module(handyterm).
-export([term_to_string/1, string_to_term/1]).

%% @doc convert erlang term to string_2_term
-spec term_to_string(term()) -> string().
term_to_string(Term) ->
    lists:flatten(io_lib:format("~p.", [Term])).

%% @doc convert string to erlang term.
-spec string_to_term(string()) -> term().
string_to_term(String) ->
    {ok, Token, _} = erl_scan:string(terminate_term_string(String)),
    {ok, Item} = erl_parse:parse_term(Token),
    Item.

%% Make sure the string is a properly formed term by appending a '.' if
%% one doesn't exist.
terminate_term_string(String) ->
    case lists:last(String) =:= $. of
        true -> String;
        false -> String ++ "."
    end.
