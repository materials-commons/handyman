-module(handyterm_tests).
-include_lib("eunit/include/eunit.hrl").

-record(r, {item1, item2}).

%% Test non-terminated
non_terminated_test() ->
    [] = handyterm:string_to_term("[]").

terminated_test() ->
    [] = handyterm:string_to_term("[].").

conversion_to_string_test() ->
    "[]." = handyterm:term_to_string([]),
    "{r,a,b}." = handyterm:term_to_string(#r{item1 = a, item2 = b}),
    "[{r,a,b},{r,c,d}]." = handyterm:term_to_string([#r{item1=a,item2=b}, #r{item1=c,item2=d}]).

conversion_from_string_test() ->
    #r{item1=a,item2=b} = handyterm:string_to_term("{r,a,b}"),
    [#r{item1=a,item2=b}] = handyterm:string_to_term("[{r,a,b}]"),
    [#r{item1=a,item2=b}] = handyterm:string_to_term(
                    handyterm:term_to_string([#r{item1 = a, item2 = b}])).