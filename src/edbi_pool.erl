%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc A database connection pool.
%% @end
%%%-------------------------------------------------------------------
-module(edbi_pool).

%% API
-export([start/4
         ,start/1
         ,queue_name/1
         ,conn_sup_name/1
         ,dispatch_query/2
        ]).

-export([notify_idle/1
         ,request_notify/1
         ,reply_query/2
        ]).

-export([test_query_time/3]).

-include_lib("pool.hrl").

%%====================================================================
%% API functions
%%====================================================================

start(Name, Driver, DriverArgs, PoolSize) ->
    start(#edbi_pool{name=Name,
                     driver={Driver,DriverArgs},
                     pool_size=PoolSize}).

start(#edbi_pool{name=N,
                 driver={D,A},
                 pool_size=S
                } = P)
  when is_atom(N), is_atom(D), is_list(A), is_integer(S) ->
    edbi_sup:start_pool(P).

queue_name(#edbi_pool{name=N}) ->
    queue_name(N);
queue_name(PoolName) when is_atom(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_queue").

conn_sup_name(#edbi_pool{name=N}) ->
    conn_sup_name(N);
conn_sup_name(PoolName) when is_atom(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_conn_sup").

notify_idle(Name) ->
    error_logger:info_msg("About to notify ~p that ~p is idle",
                          [queue_name(Name), self()]),
    queue_name(Name) ! {conn, self(), idle},
    ok.

request_notify(Pid) when is_pid(Pid) ->
    Pid ! request_notify.

dispatch_query(Pool, Query) ->
    gen:call(queue_name(Pool), 'query', Query).

reply_query({_From, _Ref} = Caller, Reply) ->
    gen:reply(Caller, Reply).

test_query_time(Pool, Query, Count) ->
    pmap(fun (N) ->
                 timer:tc(?MODULE, dispatch_query,
                          [Pool, Query ++ " " ++ integer_to_list(N)])
         end,
         lists:seq(1,Count)).

%%% Pmap from http://lukego.livejournal.com/6753.html -- Luke Gorrie
pmap(F,List) ->
    [wait_result(Worker)
     || Worker <- [spawn_worker(self(),F,E)
                   || E <- List] ].

spawn_worker(Parent, F, E) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), F(E)} end).

wait_result({Pid,Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid,Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.
