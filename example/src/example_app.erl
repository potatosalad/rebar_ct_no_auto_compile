-module(example_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

%%====================================================================
%% API functions
%%====================================================================

start(_StartType, _StartArgs) ->
    example_sup:start_link().

stop(_State) ->
    ok.
