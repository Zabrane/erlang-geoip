-module(geoip_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  application:ensure_started(inets),
  geoip_sup:start_link().

stop(_State) ->
  ok.
