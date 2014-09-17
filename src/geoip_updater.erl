-module(geoip_updater).
-behaviour(gen_server).

%% API
-export([
        start_link/1,
        update/0
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {file, update_date}).

-define(GEOIP_URL,
        "http://geolite.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz").

-define(DEF_TIMEOUT, 7*24*60*60*1000).          % one week

-spec start_link(string()) -> {ok, pid()} | ignore | {error, term()}.
start_link(File) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [File], []).

update() ->
  erlang:whereis(?SERVER) ! update.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([DatFile]) ->
  Timeout = application:get_env(geoip, update_timeout, ?DEF_TIMEOUT),
  case filelib:is_regular(DatFile) of
    true ->
      erlang:send_after(Timeout, self(), update),
      {ok, #state{file=DatFile}};
    false ->
      case update_geoip_base() of
        {ok, LastModified, Data} ->
          file:write_file(DatFile, Data),
          erlang:send_after(Timeout, self(), update),
          {ok, #state{file=DatFile, update_date=LastModified}};
        {error, E} ->
          {stop, E}
      end
  end.

handle_call(_, _From, State) ->
    {reply, reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update, #state{update_date=Upd, file=File}=State) ->
    Timeout = application:get_env(geoip, update_timeout, ?DEF_TIMEOUT),
    case update_geoip_base(Upd) of
      {ok, Date, Data} ->
        file:write_file(File, Data),
        geoip_server:reload_data(File),
        erlang:send_after(Timeout, self(), update),
        error_logger:info_msg("Geoip updated: ~p", [Date]),
        {noreply, State#state{update_date=Date}};
      {ok, nothing} ->
        error_logger:info_msg("Geoip is up to date: ~p", [Upd]),
        erlang:send_after(trunc(Timeout/2), self(), update),
        {noreply, State};
      {error, _E} ->
        erlang:send_after(trunc(Timeout/2), self(), update),
        {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

-spec update_geoip_base(string()) -> {ok, string(), binary()} | {ok, nothing} | {error, any()}.
update_geoip_base(LastUpdated) ->
  case httpc:request(head, {?GEOIP_URL, []}, [{timeout, 5000}], []) of
    {ok, {{_, 200, _}, Headers, _}} ->
      LastModified = proplists:get_value("last-modified", Headers),
      if LastUpdated /= LastModified ->
          update_geoip_base();
         true ->
          {ok, nothing}
      end;
    {ok, {{_, Status, _}, _, Data}} ->
      error_logger:error_msg("geoip update failed: ~p, ~ts", [Status, Data]),
      {error, Status};
    {error, E} ->
      error_logger:error_msg("geoip update failed: ~p", [E]),
      {error, E}
  end.

-spec update_geoip_base() -> {ok, string(), binary()} | {error, any()}.
update_geoip_base() ->
  case httpc:request(get, {?GEOIP_URL, []}, [{timeout, 5000}], []) of
    {ok, {{_, 200, _}, Headers, GZData}} ->
      LastModified = proplists:get_value("last-modified", Headers),
      Data = zlib:gunzip(GZData),
      {ok, LastModified, Data};
    {ok, {{_, Status, _}, _, Data}} ->
      error_logger:error_msg("geoip update failed: ~p, ~ts", [Status, Data]),
      {error, Status};
    {error, E} ->
      {error, E}
  end.
