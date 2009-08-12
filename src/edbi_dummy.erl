%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Dummy EDBI Driver
%% @end
-module(edbi_dummy).

-behaviour(plain_fsm).
-include_lib("plain_fsm.hrl").

-export([connect_link/2
         ,init/1
         ,code_change/3
        ]).

-record(state, {parent
                ,opts = []
                ,pool
               }).

connect_link(Pool, Options) when is_list(Options) ->
    Parent = self(),
    plain_fsm:start_opt(?MODULE,
                        fun () ->
                                {reply,{ok, self()},
                                 fun () ->
                                         init(#state{parent=Parent,
                                                     opts=Options,
                                                     pool=Pool})
                                 end}
                        end,
                        timer:seconds(2),
                        [link]).    

init(S = #state{parent=Parent,opts=Options}) ->
    error_logger:info_msg("EDBI Dummy connection ~p: Starting up~nParent ~p~nOptions:~p~n",
                          [self(), Parent, Options]),
    notify(S).

notify(S = #state{pool=P}) ->
    edbi_pool:notify_idle(P),
    wait_query(S).

wait_query(S = #state{}) ->
    plain_fsm:extended_receive(
      receive
          request_notify -> notify(S);
          {{_From, _Ref} = Caller, Query} ->
              error_logger:info_msg("EDBI Dummy: replying to ~p re query ~p~n",
                                    [Caller, Query]),
              timer:sleep(random:uniform(timer:seconds(2))),
              edbi_pool:reply_query(Caller, ok),
              notify(S);
          Msg ->
              error_logger:info_msg("EDBI Dummy wait_query: Got msg ~p~n",
                                    [Msg]),
              wait_query(S)
      end).

code_change(_OldVsn, State, _Extra) ->
    {ok, State, []}.
