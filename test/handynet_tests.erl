-module(handynet_tests).
-include_lib("eunit/include/eunit.hrl").

no_local_addrs_test() ->
    Addrs = handynet:get_external_addrs(),
    lists:foreach(fun(Item) -> check_for_local(Item) end, Addrs).

check_for_local({_, "127.0.0.1"}) ->
    throw("Local Address Found");
check_for_local({_Interface, _Address}) ->
    ok.