-module(eim).
-export([load/1, derive/3]).
-export([go/0]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    SoName = case code:priv_dir(eim) of
        {error, bad_name} ->
            filename:join("../priv", "eim_nifs");
        Dir ->
            filename:join(Dir, "eim_nifs")
    end,
    erlang:load_nif(SoName, 0).

-spec load(binary()) -> {ok, reference()} | error.
load(_Bin) ->
    exit(nif_library_not_loaded).

-spec derive(reference(), image_format(), deriv()) -> binary() | error.
-type image_format() :: jpg | gif | png.
-type deriv() :: deriv_opt() | [deriv_opt()].
-type deriv_opt() :: {crop, integer(), integer(), integer(), integer()}
                   | {scale, dimension(), integer()}
                   | {max, dimension(), integer()}
                   | {box, integer(), integer()}
                   | {box, integer(), integer(), h_float(), v_float()}
                   | {fit, integer(), integer()}.
-type dimension() :: width | height.
-type h_float() :: left | center | right.
-type v_float() :: top | center | bottom.
derive(Image, Fmt, Deriv) when is_tuple(Deriv)->
    derive(Image, Fmt, [Deriv]);
derive(Image, Fmt, Deriv) ->
    do_derive(Image, Fmt, Deriv).

do_derive(_Image, _Fmt, _Deriv) ->
    exit(nif_library_not_loaded).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

all_test() ->
    error = load(<<>>).
    %% not a test, just for debugging's sake below
    %io:format("Res: ~p~n", [Res])
    %{ok, Image} = load(element(2,file:read_file("../priv/images/09052008181.jpg"))),
    %ok = file:write_file("../priv/out.jpg", derive(Image, jpg, {box,100,100,left,center})).
    %{ok, Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    %Bucket = <<"images">>,
    %Obj1 = riakc_obj:new(Bucket, <<"10.jpg">>, derive(Resource, {fit, 100, 310}), "image/jpg"),
    %ok = riakc_pb_socket:put(Riak, Obj1)

-endif.


go() ->
    lists:foreach(fun img/1, filelib:wildcard("../priv/images/*.{jpg,jpeg,gif,png}")),
    go().%forever

img(File) ->
    case eim:load(element(2,file:read_file(File))) of
        {ok, Image} ->
            ThumbFile = "../priv/thumbs/"++filename:basename(File),
            file:write_file(ThumbFile, derive(Image, png, {box,100,100,center,center}));
        error ->
            io:format("Error: ~p~n", [File])
    end.


