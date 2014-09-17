-module(geoip_server).
-behaviour(gen_server).

%% API
-export([
        start_link/1,
        start_link/2,
        start_link/3,
        get_country_name_by_ip/1,
        get_country_code_by_ip/1,
        get_country_code3_by_ip/1,
        reload_data/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {geoip :: port(),
                file :: string(),
                mode :: integer(),
                format :: string | binary}).


-spec start_link(string()) -> {ok, pid()} | ignore | {error, term()}.
start_link(GeoIPData) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeoIPData], []).
start_link(GeoIPData, Type) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeoIPData, Type], []).
start_link(GeoIPData, Type, ReturnType) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GeoIPData, Type,
                                                      ReturnType], []).


get_country_name_by_ip(Ip) ->
    gen_server:call(?SERVER, {get_country_name_by_ip, Ip}).
get_country_code_by_ip(Ip) ->
    gen_server:call(?SERVER, {get_country_code_by_ip, Ip}).
get_country_code3_by_ip(Ip) ->
    gen_server:call(?SERVER, {get_country_code3_by_ip, Ip}).

reload_data(File) ->
  gen_server:call(?SERVER, {reload_base, File}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([File]) ->
  do_init(File, 1, binary);

init([File, Mode]) ->
  do_init(File, Mode, binary);

init([File, Mode, Format]) ->
  do_init(File, Mode, Format).

handle_call({get_country_name_by_ip, Ip}, _From, State) ->
    Result = geoip:get_country_name_by_ip(State#state.geoip, Ip),
    {reply, Result, State};

handle_call({get_country_code_by_ip, Ip}, _From, State) ->
    Result = geoip:get_country_code_by_ip(State#state.geoip, Ip),
    {reply, Result, State};

handle_call({get_country_code3_by_ip, Ip}, _From, State) ->
    Result = geoip:get_country_code3_by_ip(State#state.geoip, Ip),
    {reply, Result, State};

handle_call({reload_base, FileName}, _From,
            #state{mode=M, format=F}=State) ->
    geoip:delete(State#state.geoip),
    case do_init(FileName, M, F) of
      {ok, State2} ->
        {reply, reloaded, State2};
      {stop, Error} ->
        {stop, Error}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    geoip:delete(State#state.geoip).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_init(File, Mode, Format) ->
  case geoip:new(File, Mode, Format) of
    {ok, Pid} ->
      {ok, #state{geoip=Pid, file=File, mode=Mode, format=Format}};
    {error, Error} ->
      {stop, Error}
  end.
