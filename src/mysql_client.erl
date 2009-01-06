%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc MySQL client FSM
%% @end
-module(mysql_client).
-behaviour(plain_fsm).

-export([spawn_link/5
         ,squery/2
        ]).

-export([data_vsn/0, code_change/3]).
-include("plain_fsm.hrl").

-record(state, {sock,
                buf = <<>>,
                seq = 0,
                conf}).
-record(conf, {host, port, username, password, options}).

-define(INFO(Format, Args),
        error_logger:info_msg("(~p ~p:~p) " ++ Format,
                              [self(), ?MODULE, ?LINE | Args])).
-define(WARN(Format, Args),
        error_logger:warning_msg("(~p ~p:~p) " ++ Format,
                                 [self(), ?MODULE, ?LINE | Args])).
-define(ERR(Format, Args),
        error_logger:error_msg("(~p ~p:~p) " ++ Format,
                               [self(), ?MODULE, ?LINE | Args])).

-define(CALL_TAG, '$mysql_call').

spawn_link(Host, Port, Username, Password, Options) ->
    crypto:start(),
    Conf = #conf{host=Host,
                 port=Port,
                 username=Username,
                 password=Password,
                 options=Options},
    plain_fsm:spawn_link(?MODULE,
                         fun() ->
                                 connect(#state{conf=Conf})
                         end).

call(Pid, Message) ->
    gen:call(Pid, ?CALL_TAG, Message).

reply(From, Reply) ->
    gen:reply(From, Reply).

squery(Pid, Query) ->
    call(Pid, {squery, Query}).

%%====================================================================
%% FSM States
%%====================================================================

connect(S = #state{sock=undefined,
                   conf=#conf{host=Host,
                              port=Port}}) ->
    case gen_tcp:connect(Host, Port, 
                         [{active, once},
                          {packet, raw},
                          binary]) of
        {ok, Sock} ->
            wait_handshake(S#state{sock=Sock});
        {error, Reason} ->
            erlang:exit({connect_failed, Reason})
    end.

wait_handshake(S = #state{conf=#conf{username=Username,
                                     password=Password,
                                     options=Opts},
                          seq=Seq}) ->
    {Bytes, S1} = read_packet(S),
    {packet, Seq, {server_handshake, Hsk}, <<>>} =
        mysql_proto:decode(server_handshake, Bytes),
    S2 = S1#state{seq=Seq+1},
    ?INFO("Got handshake, ~p~n", [Hsk]),
    SBuf = proplists:get_value(scramble_buff, Hsk),
    Resp = mysql_proto:client_handshake(Username, Password
                                        ,[{scramble_buff, SBuf}
                                          ,{client_flags, [long_password,
                                                           long_flags,
                                                           protocol_41,
                                                           transactions,
                                                           secure_connection,
                                                           connect_with_db]}
                                          ,{dbname, proplists:get_value(dbname, Opts)}
                                         ]),
    ?INFO("Sending ~P~n", [Resp, 10000]),
    S3 = send(Resp, S2#state{seq=S1#state.seq+1}),
    wait_handshake_response(S3).

wait_handshake_response(S = #state{seq=Seq}) ->
    {Bytes, _S1} = read_packet(S),
    {packet, Seq, Resp, <<>>} = mysql_proto:decode(response, Bytes),
    case Resp of 
        {response, ok, _} ->
            connected(S#state{seq=0});
        {response, error, _, Msg} ->
            erlang:exit({error, Msg})
    end.

connected(S = #state{}) ->
    plain_fsm:extended_receive(
      receive
          {?CALL_TAG, From, Msg} ->
              handle_call(Msg, From, S);
          {tcp_closed, _Sock} ->
              erlang:exit(socket_closed);
          {tcp_data, _Sock, Data} ->
              erlang:exit({unexpected_tcp_data, Data});
          Message ->
              erlang:exit({unexpected_message, Message})
      end).

handle_call({squery, Q}, From, S) ->
    State = send({command, 'query', [{sql, Q}]}, reset_seq(S)),
    sent_query(From, State).

sent_query(From, State) ->
    {Bytes, NewState} = read_packet(State),
    case catch mysql_proto:decode(result_set_header, Bytes) of
        {packet, _Seq, {result_set_header, FieldCount, _Extra}, _Rest} ->
            read_fields(FieldCount, From, NewState);
        {'EXIT', {function_clause, _}} ->
            case mysql_proto:decode(response, Bytes) of
                {packet, _Seq,
                 {response, {error, Type, _DBState, Message}},
                 _Rest} ->
                    reply(From, {query_error, Type, Message}),
                    connected(NewState)
            end
    end.

read_fields(FieldCount, From, State) ->
    read_fields(FieldCount, From, [], State).
    
read_fields(FieldsLeft, From, Acc, State) when FieldsLeft > 0 ->
    {Bytes, NewState} = read_packet(State),
    case mysql_proto:decode(field, Bytes) of
        {packet, _Seq, {field, Info}, _Rest} ->
            read_fields(FieldsLeft - 1,
                        From,
                        [ {proplists:get_value(name, Info), Info} | Acc],
                        NewState)
    end;
read_fields(0, From, FieldAcc, State) ->
    {Bytes, NewState} = read_packet(State),
    case mysql_proto:decode(field, Bytes) of
        {packet, _Seq, {end_of_fields, _Info}, _Rest} ->
            read_rows([], FieldAcc, From, NewState)
    end.

read_rows(Acc, FieldInfo, From, State) ->
    {Bytes, NewState} = read_packet(State),
    case mysql_proto:decode(row, Bytes) of
        {packet, _Seq, {row, Columns}, _Rest} ->
            read_rows([ list_to_tuple(Columns) | Acc],
                      FieldInfo, From, NewState);
        {packet, _Seq, {row_eof, _Info}, _Rest} ->
            reply(From, {query_result, FieldInfo, lists:reverse(Acc)}),
            connected(State)
    end.
%%====================================================================
%% Internal Functions
%%====================================================================

send(Message, State = #state{sock=Sock, seq=Seq}) ->
    MsgBytes = mysql_proto:encode(Message),
    Bytes = mysql_proto:encode_packet(Seq, MsgBytes),
    ?INFO("Sent ~P~n", [Bytes, 10000]),
    case gen_tcp:send(Sock, Bytes) of
        ok -> State#state{seq=Seq+1};
        Err -> erlang:exit({send_error, Err})
    end.

read_packet(S = #state{buf=Buf}) ->
    case mysql_proto:decode(packet, Buf) of
        {packet, Bytes, Rest} ->
            {Bytes, S#state{buf=Rest}};
        {incomplete, Rest} ->
            read_wire_packet(S#state{buf=Rest});
        {incomplete, _SizeNeeded, Rest} ->
            read_wire_packet(S#state{buf=Rest})
    end.

read_wire_packet(S = #state{sock=Sock, buf=Buf}) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive         
        {tcp_closed, _Sock} ->
            erlang:exit(normal);
        {tcp, Sock, Data} ->
            ?INFO("Received ~P~n", [Data, 10000]),
            NewBuf = iolist_to_binary([Buf, Data]),
            read_packet(S#state{buf=NewBuf})
    end.

reset_seq(S = #state{}) ->
    S#state{seq=0}.

incr_seq(S = #state{seq=Seq}) ->
    S#state{seq=Seq + 1}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, {newstate, data_vsn()}}.

