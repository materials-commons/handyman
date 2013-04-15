-module(handyqueue).

-export([get_queue_for_host/2]).

get_queue_for_host(QueueNameBase, Interface) ->
    {ok, Address} = handynet:get_address(handynet:get_external_addrs(), Interface),
    string:concat(QueueNameBase, Address).