-module(transit_writer).
-export([write/2]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

write(Obj, Config) -> write_(Obj, canonicalize(Config)).

-ifndef(maps_support).
write_(Obj, [{format,Format},{handler,Handler}]) ->
  Rep = transit_marshaler:marshal_top(marshaler(Format), Obj, {Format, Handler}),
  pack(Format, Rep).
-endif.

-ifdef(maps_support).
write_(Obj, #{ format := Format, handler := Handler }) ->
  Rep = transit_marshaler:marshal_top(marshaler(Format), Obj, {Format, Handler}),
  pack(Format, Rep).
-endif.

marshaler(json_verbose) -> transit_json_verbose_marshaler;
marshaler(json) -> transit_json_marshaler;
marshaler(msgpack) -> transit_json_marshaler.

pack(msgpack, Rep) -> msgpack:pack(Rep, [{format, jsx}]);
pack(json, Rep) -> jsx:encode(Rep);
pack(json_verbose, Rep) -> jsx:encode(Rep).

-ifndef(maps_support).
canonicalize([{format,_F},{handler,_H}] = M) -> M;
canonicalize([{format,_F}|_] = M) -> 
    M1 = proplists:delete(handler, M),
    canonicalize(M1 ++ [{handler,?MODULE}]);
canonicalize([{handler,_H}|_] = M) ->
    M1 = proplists:delete(format, M),
    canonicalize([{format,json}] ++ M1);
canonicalize([]) -> [{format,json},{handler,?MODULE}].
-endif.

-ifdef(maps_support).
canonicalize(#{ format := _F, handler := _H } = M) -> M;
canonicalize(#{ format := _F } = M) -> canonicalize(M#{ handler => ?MODULE });
canonicalize(#{ handler := _H } = M) -> canonicalize(M#{ format => json });
canonicalize(#{}) -> #{ format => json, handler => ?MODULE }.
-endif.