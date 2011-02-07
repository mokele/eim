-module(egm).
-export([resize/0]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    ok = erlang:load_nif("../priv/egm_nifs", 0).

resize() ->
    exit(nif_library_not_loaded).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

resize_test() ->
    69 = resize().

-endif.