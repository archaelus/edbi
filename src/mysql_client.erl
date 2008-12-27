%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc MySQL client FSM
%% @end
-module(mysql_client).
-behaviour(plain_fsm).

-export([spawn_link/5]).
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

%%====================================================================
%% FSM States
%%====================================================================

connect(S = #state{sock=undefined,
                   conf=#conf{host=Host,
                              port=Port}}) ->
    case gen_tcp:connect(Host, Port, 
                         [{active, true}, {packet, raw}, binary]) of
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
    ?INFO("Got response, ~p~n", [Resp]),
    erlang:exit(nyi).

%%====================================================================
%% Internal Functions
%%====================================================================

send(Response, State = #state{sock=Sock, seq=Seq}) ->
    RespBytes = mysql_proto:encode(Response),
    Bytes = mysql_proto:encode_packet(Seq, RespBytes),
    ?INFO("Sent ~P~n", [Bytes, 10000]),
    case gen_tcp:send(Sock, Bytes) of
        ok -> State#state{seq=Seq+1};
        Err -> erlang:exit({send_error, Err})
    end.

read_packet(S = #state{buf=Buf}) ->
    receive         
        {tcp_closed, _Sock} ->
            erlang:exit(normal);
        {tcp, _Sock, Data} ->
            ?INFO("Received ~P~n", [Data, 10000]),
            NewBuf = iolist_to_binary([Buf, Data]),
            case mysql_proto:decode(packet, NewBuf) of
                {packet, Bytes, Rest} ->
                    {Bytes, S#state{buf=Rest}};
                {incomplete, Rest} ->
                    read_packet(S#state{buf=Rest});
                {incomplete, _SizeNeeded, Rest} ->
                    read_packet(S#state{buf=Rest})
            end
    end.

code_change(_OldVsn, _State, _Extra) ->
    {ok, {newstate, data_vsn()}}.

