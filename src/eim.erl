-module(eim).
-export([resize/3]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    ok = erlang:load_nif("../priv/eim_nifs", 0).

resize(_Bin, _W, _H) ->
    exit(nif_library_not_loaded).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

resize_test_() ->
    {timeout, 60, fun() ->
        {ok, Binary} = file:read_file("../priv/board.png"),
        {ok, Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087),
        Bucket = <<"images">>,
        ok = riakc_pb_socket:put(Riak, riakc_obj:new(Bucket, <<"1.jpg">>, resize(Binary, 200, 150), "image/jpg")),
        ok = riakc_pb_socket:put(Riak, riakc_obj:new(Bucket, <<"2.jpg">>, resize(Binary, 300, 250), "image/jpg")),
        ok = riakc_pb_socket:put(Riak, riakc_obj:new(Bucket, <<"3.jpg">>, resize(Binary, 400, 350), "image/jpg")),
        ok = riakc_pb_socket:put(Riak, riakc_obj:new(Bucket, <<"4.jpg">>, resize(Binary, 500, 450), "image/jpg")),
        ok = riakc_pb_socket:put(Riak, riakc_obj:new(Bucket, <<"5.jpg">>, resize(Binary, 600, 550), "image/jpg")),
        ok
    end}.
-endif.