-module(geoip_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  BaseFile = application:get_env(geoip, base, "GeoIP.dat"),
  Worker = ?CHILD(geoip_server, worker, [BaseFile, 1, binary]),
  Updater = ?CHILD(geoip_updater, worker, [BaseFile]),
  {ok, { {one_for_one, 5, 10}, [Updater, Worker]} }.
