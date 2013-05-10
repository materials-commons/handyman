%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Network utilities.
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

-module(handynet).

-export([get_external_addrs/0, get_address/2]).

%% @doc Get external IPv4 addresses as a list of interface string tuples.
-spec get_external_addrs() -> [{atom(), string()}] | [].
get_external_addrs() ->
    {ok, Interfaces} = inet:getifaddrs(),
    InterfacesAndV4Addrs = get_if_v4_addrs(Interfaces),
    NonLocalAddresses = lists:filter(fun is_external_addr/1, InterfacesAndV4Addrs),
    lists:map(fun addr_tuple_to_string/1, NonLocalAddresses).

get_if_v4_addrs(Interfaces) ->
    lists:map(
        fun({Interface, Values}) ->
                Addrs = lists:filter(fun is_v4_address/1, Values),
               {Interface, Addrs}
         end, Interfaces).

 is_v4_address({Key, Value}) ->
    Key =:= addr andalso is_tuple(Value)
        andalso tuple_size(Value) =:= 4.

is_external_addr({_IName, Values}) ->
    case lists:keyfind(addr, 1, Values) of
        {addr, {127,0,0,1}} -> false;
        false -> false;
        _ -> true
    end.

addr_tuple_to_string({Interface, Values}) ->
    {addr, {Oct1, Oct2, Oct3, Oct4}} = lists:keyfind(addr, 1, Values),
    {Interface, lists:flatten(io_lib:format("~b.~b.~b.~b", [Oct1, Oct2, Oct3, Oct4]))}.

%% @doc Given a list of address and an interface (other number or atom),
%%      return address
-spec get_address([{atom(), string()}] | [], atom() | integer()) -> string().
get_address([], _Interface) ->
    {error, no_interfaces};
get_address(Interfaces, InterfaceIndex) when is_integer(InterfaceIndex) ->
    {_InterfaceName, Address} = lists:nth(InterfaceIndex, Interfaces),
    {ok, Address};
get_address(Interfaces, InterfaceName) ->
    {_IName, Address} = lists:keyfind(InterfaceName, 1, Interfaces),
    {ok, Address}.

