-module(handyfile_tests).
-include_lib("eunit/include/eunit.hrl").

tmpdir_test() ->
    ?assert(handyfile:tmpdir() =/= {error, badpath}).