%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc EDBI Application
%% @end
%%%-------------------------------------------------------------------
-module(edbi_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            true;
        {error, {already_started, App}} ->
            true;
        Else -> 
            error_logger:error_msg("Couldn't start ~p: ~p", [App, Else])
    end.
        
%% @spec start() -> ok
%% @doc Start the vhreg server.
start() ->
    application:load(edbi),
    {ok, Deps} = application:get_key(edbi, applications),
    true = lists:all(fun ensure_started/1, Deps),
    application:start(edbi).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case edbi_sup:start_link(StartArgs) of
        {ok, Pid} ->
            load_pools(),
            {ok, Pid};
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

load_pools() ->
    lists:foreach(fun load_pool/1, pools()).

load_pool({Id, Driver, Options, Count}) ->
    case edbi_sup:start_pool(Id, [Driver, Options, Count]) of
        {ok, _} -> ok;
        {error, Reason} ->
            erlang:error({couldnt_start_pool, Reason})
    end.

pools() ->
    {ok, Pools} = application:get_env(edbi, pools),
    Pools.


%%====================================================================
%% Internal functions
%%====================================================================
