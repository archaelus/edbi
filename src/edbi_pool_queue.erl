%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(edbi_pool_queue).

-behaviour(plain_fsm).
-include_lib("plain_fsm.hrl").

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("pool.hrl").

%% API
-export([start_link/1
        ]).

-export([code_change/3]).


-record(state, {queries = [],
                conns = [],
                pool
               }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(#edbi_pool{} = P) ->
    plain_fsm:start_opt(?MODULE,
                        fun () ->
                                true = erlang:register(edbi_pool:queue_name(P), self()),
                                error_logger:info_msg("~p/~p starting up as ~p~n",
                                                      [?MODULE, self(), edbi_pool:queue_name(P)]),
                                {reply,{ok, self()},
                                 fun () ->
                                         init(#state{pool=P})
                                 end}
                        end,
                        timer:seconds(2),
                        [link]).

%%====================================================================
%% FSM States
%%====================================================================

init(#state{pool=P} = S) ->
    case whereis(edbi_pool:conn_sup_name(P)) of
        undefined ->
            %% We started before the conn sup, we'll be notified.
            ok;
        _ ->
            %% Conn sup is already up, request notify
            [edbi_pool:request_notify(Conn)
             || {_, Conn, _, _} <- edbi_pool_conn_sup:children(P)]
    end,
    idle(S).

idle(#state{queries=Qs,conns=[]} = S) when Qs =/= [] ->
    need_conn(S);
idle(#state{queries=Qs,conns=Cs} = S) ->
    plain_fsm:extended_receive(
      receive
          {'query', {From, Ref}, Query} when Cs =/= [] ->
              dispatch({{From,Ref}, Query}, hd(Cs), S#state{conns=tl(Cs)});
          {'query', {From, Ref}, Query} when Cs =:= [] ->
              need_conn(S#state{queries=[{{From,Ref}, Query}|Qs]});
          {conn, Pid, idle} ->
              case lists:member(Pid, Cs) of
                  true ->
                      ?WARN("Duplicate connection ~p", [Pid]),
                      idle(S);
                  false ->
                      ?INFO("Added a new connection: ~p", [Pid]),
                      idle(S#state{conns=[Pid|Cs]})
              end
      end).

need_conn(State = #state{queries=[Q|Rest], conns=[]}) ->
    ?INFO("Overloaded: need a connection to dispatch ~p, "
          "Message Queue size: ~p", [Q, element(2, process_info(self(), message_queue_len))]),
    receive
        {conn, Pid, idle} ->
            dispatch(Q, Pid, State#state{queries=Rest})
    end.

dispatch(Q, S = #state{conns=[Pid|Rest]}) ->
    dispatch(Q, Pid, S#state{conns=Rest}).

dispatch(Q, Pid, State) ->
    ?INFO("Dispatching ~p to ~p.", [Q, Pid]),
    Pid ! Q,
    idle(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State, []}.
