-module(handyqueue_tests).
-include_lib("eunit/include/eunit.hrl").

get_queue_test() ->
	Addr = handynet:get_address(handynet:get_external_addrs(), 1),
	"/queue/" ++ Addr =:= handyqueue:get_queue_for_host("/queue/", 1).
	
