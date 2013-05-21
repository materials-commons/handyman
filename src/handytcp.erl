%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc SSL Utilities.
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

-module(handytcp).
-export([ssl_recv_all/2, ssl_recv_rest/3]).

ssl_recv_all(Socket, {active, true}) ->
    ssl:setopts(Socket, [{active, false}]),
    AllData = ssl_recv_all(Socket),
    ssl:setopts(Socket, [{active, true}]),
    AllData;
ssl_recv_all(Socket, {active, false}) ->
    ssl_recv_all(Socket).

ssl_recv_all(Socket) ->
    {ok, Data} = ssl:recv(Socket, 0),
    ssl_recv_rest(Socket, Data).

ssl_recv_rest(Socket, Data, {active, true}) ->
    ssl:setopts(Socket, [{active, false}]),
    AllData = ssl_recv_rest(Socket, Data),
    ssl:setopts(Socket, [{active, true}]),
    AllData;
ssl_recv_rest(Socket, Data, {active, false}) ->
    ssl_recv_rest(Socket, Data).

ssl_recv_rest(Socket, AlreadyReceived) ->
    case ssl:recv(Socket, 0, 100) of
        {ok, Data} -> ssl_recv_rest(Socket, <<AlreadyReceived/binary, Data/binary>>);
        {error, timeout} -> {ok, AlreadyReceived};
        _ -> error
    end.
