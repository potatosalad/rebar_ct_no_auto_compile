-module(example_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).

%% Tests.
-export([example/1]).

%% ct.

all() ->
    [example].

example(_) ->
    ok = application:start(example),
    ok = application:stop(example).
