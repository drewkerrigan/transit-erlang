-module(transit).
-export([read/1, read/2, write/1, write/2]).

-ifdef(maps_support).
read(Rep) -> read(Rep, #{ format => json }).
write(Rep) -> write(Rep, #{ format => json }).
-endif.
-ifndef(maps_support).
read(Rep) -> read(Rep, [{ format, json }]).
write(Rep) -> write(Rep, [{ format, json }]).
-endif.

read(Rep, Opts) ->
  transit_reader:read(Rep, Opts).

write(Rep, Opts) ->
  transit_writer:write(Rep, Opts).
