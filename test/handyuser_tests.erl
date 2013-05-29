-module(handyuser_tests).
-include_lib("eunit/include/eunit.hrl").

username_test() ->
    ?assert(handyuser:username() =/= {error, baduser}).

user_home_test() ->
    ?assertEqual({error, baduser}, handyuser:user_home("doesnotexist")),
    {ok, Username} = handyuser:username(),
    ?assert(handyuser:user_home(Username) =/= {error, baduser}).
