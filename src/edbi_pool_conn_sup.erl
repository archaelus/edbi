%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Pool driver connection supervisor
%% @end
%%%-------------------------------------------------------------------
-module(edbi_pool_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/1
         ,children/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("pool.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(#edbi_pool{} = P) ->
    supervisor:start_link({local, edbi_pool:conn_sup_name(P)}, ?MODULE, [P]).

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
init([P = #edbi_pool{pool_size=S}]) ->
    {ok,{{one_for_one,S,1000},
         [child(P, N) || N <- lists:seq(1,S)]}}.

children(#edbi_pool{name=N}) ->
    children(N);
children(N) when is_atom(N) ->
    supervisor:which_children(edbi_pool:conn_sup_name(N)).

%%====================================================================
%% Internal functions
%%====================================================================

child(Pool = #edbi_pool{driver={Driver, DriverOptions}}, Number) ->
    {child_id(Driver, Number),
     edbi_driver:connect_mfa(Driver, Pool, DriverOptions),
     transient, timer:seconds(2), worker,
     [Driver]}.

child_id(Driver, Number) ->
    atom_to_list(Driver) ++ " " ++ integer_to_list(Number).
