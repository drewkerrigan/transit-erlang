-module(transit_writer).
-export([write/2]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-ifdef(maps_support).
write(Obj, Config) when is_map(Config) -> 
  #{ format := Format, handler := Handler } = canonicalize(Config),
  Rep = transit_marshaler:marshal_top(marshaler(Format), Obj, {Format, Handler}),
  pack(Format, Rep);
write(Obj, Config) -> write_(Obj, canonicalize(Config)).
-endif.

-ifndef(maps_support).
write(Obj, Config) -> write_(Obj, canonicalize(Config)).
-endif.

write_(Obj, Config) when is_list(Config) ->
  Format = proplists:get_value(format, Config),
  Handler = proplists:get_value(handler, Config),
  Rep = transit_marshaler:marshal_top(marshaler(Format), Obj, {Format, Handler}),
  pack(Format, Rep).

marshaler(json_verbose) -> transit_json_verbose_marshaler;
marshaler(json) -> transit_json_marshaler;
marshaler(msgpack) -> transit_json_marshaler.

pack(msgpack, Rep) -> msgpack:pack(Rep, [{format, jsx}]);
pack(json, Rep) -> jsx:encode(Rep);
pack(json_verbose, Rep) -> jsx:encode(Rep).

-ifdef(maps_support).
canonicalize(#{ format := _F, handler := _H } = M) -> M;
canonicalize(#{ format := _F } = M) -> canonicalize(M#{ handler => ?MODULE });
canonicalize(#{ handler := _H } = M) -> canonicalize(M#{ format => json });
canonicalize(#{}) -> #{ format => json, handler => ?MODULE };
canonicalize(Config) -> canonicalize_(Config).
-endif.
-ifndef(maps_support).
canonicalize(Config) -> canonicalize_(Config).
-endif.

canonicalize_(Config) ->
  F = proplists:get_value(format, Config, json),
  H = proplists:get_value(handler, Config, ?MODULE),
  [{format,F},{handler,H}].
