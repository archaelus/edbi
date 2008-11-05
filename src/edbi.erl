%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc EDBI Public API
%% @end
%%%-------------------------------------------------------------------
-module(edbi).

%% API
-export([pool/1
         ,connection/1]).

%%====================================================================
%% API
%%====================================================================

pools() ->
    [{ID, Pid} 
     || {ID, Pid, _, _} <- supervisor:which_children(edbi_sup)].

%% @spec pool(PoolID::term()) -> {ok, Pid::pid()} | {error, Reason::term()}
%% @doc Returns the Pid of the pool supervisor for pool PoolID.
%% @end 
pool(Id) ->
    case lists:keysearch(Id, 1, pools()) of
        false ->
            {error, {no_such_pool, Id}};
        {value, {_Id, Pid}} when is_pid(Pid) ->
            {ok, Pid}
    end.

%% @spec pool(PoolID::term()) -> {ok, Pid::pid()} | {error, Reason::term()}
%% @doc Returns the Pid of the pool supervisor for pool PoolID.
%% @end 
connection({ok, Pid}) ->
    edbi_pool:connection(Pid);
connection({error, E}) ->
    {error, E};
connection(Pool) ->
    connection(pool(Pool)).

%%====================================================================
%% Internal functions
%%====================================================================
