%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc The pool supervisor (EDBI toplevel supervisor)
%% @end
%%%-------------------------------------------------------------------
-module(edbi_sup).

-behaviour(supervisor).

%% API
-export([start_link/1
         ,start_pool/2
         ,pool/1
         ,connection/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Args::any()) -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_pool(Id, Args) ->
    CSpec = {Id,
             {edbi_pool,start_link, Args},
             transient,2000,worker,
             [edbi_pool]},
    supervisor:start_child(?SERVER, CSpec).

pool(Id) ->
    case lists:keysearch(Id, 1, supervisor:which_children(?SERVER)) of
        false ->
            {error, {no_such_pool, Id}};
        {value, {_Id, Pid, _, _}} when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            {error, {not_running, Id}}
    end.

connection({ok, Pid}) ->
    edbi_pool:connection(Pid);
connection({error, E}) ->
    {error, E};
connection(Pool) ->
    connection(pool(Pool)).

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
init([]) ->
    {ok,{{one_for_one,0,60},[]}}.
