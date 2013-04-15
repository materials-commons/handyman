-module(handyconfig).

-export([get_env_default/3]).

get_env_default(App, What, Default) ->
    case application:get_env(App, What) of
        {ok, Value} -> Value;
        undefined -> Default
    end.