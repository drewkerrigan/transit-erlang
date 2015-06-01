-module(transit).
-export([read/1, read/2, write/1, write/2]).

-ifndef(maps_support).
read(Rep) ->
	read(Rep, [{format,json}]).
-endif.

-ifdef(maps_support).
read(Rep) ->
    read(Rep, #{ format => json }).
-endif.

read(Rep, Opts) ->
  transit_reader:read(Rep, Opts).

-ifndef(maps_support).
write(Rep) ->
	write(Rep, [{format,json}]).
-endif.

-ifdef(maps_support).
write(Rep) ->
    write(Rep, #{ format => json }).
-endif.

write(Rep, Opts) ->
  transit_writer:write(Rep, Opts).
