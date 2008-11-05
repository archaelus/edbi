%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc A database connection pool.
%% @end
%%%-------------------------------------------------------------------
-module(edbi_pool).

-behaviour(supervisor).

%% API
-export([start_link/3
         ,connection/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(RESTARTTIME, timer:minutes(10) div timer:seconds(1)).
%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(Driver, DriverOptions, Count) ->
    Children = [ child(Driver, DriverOptions, N)
                 || N <- lists:seq(1,Count) ],
    supervisor:start_link(?MODULE, [Children]).

connection(Pool) ->
    Pids = [Child
            || {_Id, Child, _Type, _Modules} <- supervisor:which_children(Pool),
               is_pid(Child) ],
    Which = random:uniform(length(Pids)),
    lists:nth(Which, Pids).

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
init([Children]) ->
    {ok,{{one_for_one,1,?RESTARTTIME}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

child(Driver, DriverOptions, Number) ->
    {child_id(Driver, Number),
     edbi_driver:connect_mfa(Driver, DriverOptions),
     transient, timer:seconds(2), worker,
     [Driver]}.

child_id(Driver, Number) ->
    atom_to_list(Driver) ++ " " ++ integer_to_list(Number).
