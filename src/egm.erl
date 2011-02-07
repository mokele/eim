-module(egm).
-export([resize/3]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    ok = erlang:load_nif("../priv/egm_nifs", 0).

resize(_Bin, _W, _H) ->
    exit(nif_library_not_loaded).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

resize_test() ->
    {ok, Binary} = file:read_file("../priv/board.png"),
    ResizedBinary = resize(Binary, 200, 150),
    ok = file:write_file("../priv/board_thumb.jpg", ResizedBinary).
-endif.