-module(eim).
-export([load/1, derive/2]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    ok = erlang:load_nif("../priv/eim_nifs", 0).

load(_Bin) ->
    exit(nif_library_not_loaded).

derive(Image, Deriv) when is_tuple(Deriv)->
    derive(Image, [Deriv]);
derive(Image, Deriv) ->
    do_derive(Image, Deriv).

do_derive(_Image, _Deriv) ->
    exit(nif_library_not_loaded).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

all_test_() ->
    {timeout, 60, fun() ->
        %% not a test, just for debugging's sake
        {ok, Image} = load(element(2,file:read_file("../priv/board.png"))),
        ok = file:write_file("../priv/out.jpg", derive(Image, {fit, 100, 310}))
        %{ok, Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087),
        %Bucket = <<"images">>,
        %Obj1 = riakc_obj:new(Bucket, <<"10.jpg">>, derive(Resource, {fit, 100, 310}), "image/jpg"),
        %ok = riakc_pb_socket:put(Riak, Obj1)
    end}.

-endif.