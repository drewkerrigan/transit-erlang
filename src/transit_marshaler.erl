-module(transit_marshaler).
-include("transit_format.hrl").
-include_lib("transit.hrl").

-export([flatten_map/1, quote_string/1, escape/1, context/1, force_context/2]).
-export([marshal_top/3, marshal/3, new_env/0, new_env/1, cache/1]).

-callback emit_null(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::#env{}.

-callback emit_boolean(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::#env{}.

-callback emit_int(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::#env{}.

-callback emit_float(Rep, Env) ->
  {Rep, Env}
    when Rep::bitstring(), Env::#env{}.

-callback emit_tagged(Rep, Env) ->
  {Resp, Env}
    when Rep::#tagged_value{}, Resp::bitstring(), Env::#env{}.

-callback emit_object(Rep, Env) ->
  {Resp, Env}
    when Rep::term(), Resp::bitstring(), Env::#env{}.

-callback emit_encoded(Tag, Rep, Env) ->
  {Rep, Env}
    when Tag::bitstring(), Rep::bitstring(), Env::#env{}.

-callback emit_array(Rep, Env) ->
  {Resp, Env}
    when Rep::term(), Resp::bitstring(), Env::#env{}.

-callback emit_map(Rep, Env) ->
  {Rep, Env}
    when Rep::term(), Env::#env{}.

-callback emit_cmap(Rep, Env) ->
  {Rep, Env}
    when Rep::term(), Env::#env{}.

-callback emit_string(Tag, Rep, Env) ->
  {Rep, Env}
    when Tag::bitstring(), Rep::bitstring(), Env::#env{}.

-callback handler(Obj) ->
  Handler when Obj::term(), Handler::transit_write_handlers:writer_handler().

-ifndef(maps_support).
% flatten_map(M) when is_map(M) ->
%   maps:fold(fun(K, V, In) ->  [K, V| In] end, [], M);
flatten_map([{}]) -> [];
flatten_map([{K,V}|Tail]) -> [K,V|flatten_map(Tail)];
flatten_map([]) -> [].
-endif.

-ifdef(maps_support).
%%% flatten R17 map
flatten_map(M) when is_map(M) ->
  maps:fold(fun(K, V, In) ->  [K, V| In] end, [], M);
flatten_map([{}]) -> [];
flatten_map([{K,V}|Tail]) -> [K,V|flatten_map(Tail)];
flatten_map([]) -> [].
-endif.
quote_string(Str) ->
  EscapeSlash = re:replace(Str, "\\\\", "\\\\"),
  EscapeQuote = re:replace(EscapeSlash, "\\\"", "\\\""),
  <<"\"", EscapeQuote/bitstring, "\"">>.

-spec escape(binary()) -> binary().
escape(?MAP_AS_ARR) -> ?MAP_AS_ARR;
escape(<<$^, _/binary>> = S) -> <<?ESC/binary, S/binary>>;
escape(<<$~, _/binary>> = S) -> <<?ESC/binary, S/binary>>;
escape(<<$`, _/binary>> = S) -> <<?ESC/binary, S/binary>>;
escape(S) -> S.

-spec context(#env{}) ->  boolean().
context(#env{context = K}) -> K.

-spec cache(#env{}) -> pid().
cache( #env{ cache = C }) -> C.

force_context(Kind, Env) ->
  Env#env{ context = Kind}.

find_handler(Mod, Obj, Env) ->
  case Mod:handler(Obj) of
    undefined ->
      find_handler_default_handlers(Obj, Env);
    Handler -> Handler
  end.

find_handler_default_handlers(Obj, #env { custom_handler = CHandler }) ->
  case transit_write_handlers:handler(Obj) of
    undefined -> CHandler:handler(Obj);
    Handler -> Handler
  end.

marshal_top(Mod, Object, Conf) ->
  Env = new_env(Conf),
  #write_handler { tag = TagFun } = find_handler(Mod, Object, Env),
  {Ret, _Env} = marshal_top_output(Mod, Object, Env, TagFun(Object)),
  Ret.
  
marshal_top_output(Mod, Object, Env, Tag) when byte_size(Tag) == 1 ->
  Mod:emit_tagged(#tagged_value { tag = ?QUOTE, rep = Object}, Env);
marshal_top_output(Mod, Object, Env, _) ->
  marshal(Mod, Object, Env).

-spec marshal(module(), any(), S) -> {bitstring(), S}
    when S :: #env{}.
marshal(Mod, Obj, #env { context = Kind } = S) ->
  #write_handler { tag = TagFun,
                   string_rep = StringRep,
                   rep = Repr } = find_handler(Mod, Obj, S),
  Rep = case Kind of
          key -> StringRep(Obj);
          value -> Repr(Obj)
        end,
  case emit_ground(Mod, Rep, S, TagFun(Obj)) of
    {extension, Tag} when is_list(Rep), byte_size(Tag) == 1 ->
      Bin = list_to_binary(Rep),
      Mod:emit_string(<<?ESC/binary, Tag/binary>>, Bin, S);
    {extension, Tag} when byte_size(Tag) == 1 ->
      Mod:emit_string(<<?ESC/binary, Tag/binary>>, StringRep(Obj), S);
    {extension, Tag} ->
      Mod:emit_encoded(Tag, Rep, S);
    V -> V
  end.
  
emit_ground(Mod, Rep, S, ?NULL) -> Mod:emit_null(Rep, S);
emit_ground(Mod, Rep, S, ?BOOLEAN) -> Mod:emit_boolean(Rep, S);
emit_ground(Mod, Rep, S, ?INT) -> Mod:emit_int(Rep, S);
emit_ground(Mod, Rep, S, ?FLOAT) -> Mod:emit_float(Rep, S);
emit_ground(Mod, Rep, S, ?ARRAY) -> Mod:emit_array(Rep, S);
emit_ground(Mod, Rep, S, ?STRING) when is_binary(Rep) -> Mod:emit_string(<<>>, Rep, S);
emit_ground(Mod, Rep, S, ?MAP) ->
  case stringable_keys(Rep) of
    true -> Mod:emit_map(Rep, S);
    false -> Mod:emit_cmap(Rep, S)
  end;
emit_ground(_Mod, _Rep, _S, T) -> {extension, T}.

new_env() ->
  #env{cache=transit_rolling_cache:empty(json)}.

-spec new_env({atom(), module()}) -> #env{}.
new_env({Format, CustomHandler}) ->
  Env = new_env(),
  Cache = transit_rolling_cache:empty(Format),
  Env#env{custom_handler=CustomHandler, cache=Cache}.

-ifndef(maps_support).
% stringable_keys(#{} = Rep) -> 
%   lists:all(fun(X) -> byte_size(transit_write_handlers:tag(X)) =:= 1 end,
%             maps:keys(Rep));
stringable_keys([{}]) -> true;
stringable_keys([]) -> true;
stringable_keys([{K, _}|T]) ->
  case byte_size(transit_write_handlers:tag(K)) =:= 1 of
    true -> stringable_keys(T);
    false -> false
  end;
stringable_keys(_) -> false.
-endif.

-ifdef(maps_support).
stringable_keys(#{} = Rep) -> 
  lists:all(fun(X) -> byte_size(transit_write_handlers:tag(X)) =:= 1 end,
            maps:keys(Rep));
stringable_keys([{}]) -> true;
stringable_keys([]) -> true;
stringable_keys([{K, _}|T]) ->
  case byte_size(transit_write_handlers:tag(K)) =:= 1 of
    true -> stringable_keys(T);
    false -> false
  end;
stringable_keys(_) -> false.
-endif.