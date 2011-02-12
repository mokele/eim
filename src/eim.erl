%%
%% Copyright (C) 2011 by Steven Gravell
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%

-module(eim).
-author('Steven Gravell <steve@mokele.co.uk>').
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
-spec derive(reference(), image_format(), deriv()) -> binary() | error.
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


