-module(handyuser_tests).
-include_lib("eunit/include/eunit.hrl").

user_home_test() ->
    ?assertEqual(baduser, handyuser:user_home("doesnotexist")),
    %% Assume that username is working
    ?assert(handyuser:user_home(handyuser:username()) =/= baduser).
