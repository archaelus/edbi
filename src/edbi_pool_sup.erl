%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc A database connection pool.
%% @end
%%%-------------------------------------------------------------------
-module(edbi_pool_sup).

-behaviour(supervisor).


%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("pool.hrl").

-define(RESTART_SECONDS, 2). % seconds
%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(#edbi_pool{} = P) ->
    supervisor:start_link(?MODULE, [P]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([Pool]) ->
    {ok,
     {{one_for_one,1,?RESTART_SECONDS},
      [ {pool_queue,
         {edbi_pool_queue, start_link, [Pool]},
         permanent, timer:seconds(?RESTART_SECONDS), worker, [edbi_pool_queue]}
       ,{pool_conn_sup,
         {edbi_pool_conn_sup, start_link, [Pool]},
         permanent, timer:seconds(?RESTART_SECONDS), worker, [edbi_pool_queue]}
      ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
