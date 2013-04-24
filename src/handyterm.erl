-module(handyterm).
-export([term_to_string/1, string_to_term/1]).

%% @doc convert erlang term to string_2_term
term_to_string(Term) ->
    lists:flatten(io_lib:format("~p.", [Term])).

%% @doc convert string to erlang term.
string_to_term(String) ->
    {ok, Token, _} = erl_scan:string(terminate_term_string(String)),
    {ok, Item} = erl_parse:parse_term(Token),
    Item.

terminate_term_string(String) ->
    case lists:last(String) =:= $. of
        true -> String;
        false -> String ++ "."
    end.
