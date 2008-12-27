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
                conf}).
-record(conf, {host, port, username, password, options}).

-define(INFO(Format, Args),
        error_logger:info_report([{where, lists:flatten(io_lib:format("(~p:~p ~p)", [?MODULE, ?LINE, self()]))},
                                  lists:flatten(io_lib:format(Format, Args))])).
-define(WARN(Format, Args),
        error_logger:warning_report([{where, lists:flatten(io_lib:format("(~p:~p ~p)", [?MODULE, ?LINE, self()]))},
                                     lists:flatten(io_lib:format(Format, Args))])).
-define(ERR(Format, Args),
        error_logger:error_report([{where, lists:flatten(io_lib:format("(~p:~p ~p)", [?MODULE, ?LINE, self()]))},
                                   lists:flatten(io_lib:format(Format, Args))])).

spawn_link(Host, Port, Username, Password, Options) ->
    Conf = #conf{host=Host,
                 port=Port,
                 username=Username,
                 password=Password,
                 options=Options},
    plain_fsm:spawn_link(?MODULE,
                         fun() ->
                                 connect(#state{conf=Conf})
                         end).

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

wait_handshake(S = #state{}) ->
    {packet, Bytes, <<>>} = read_packet(S),
    Hsk = mysql_proto:decode(server_handshake, Bytes),
    ?INFO("Got handshake, ~p~n", [Hsk]),
    erlang:exit(finished_for_now).


read_packet(S = #state{buf=Buf}) ->
    receive         
        {tcp_closed, _Sock} ->
            erlang:exit(normal);
        {tcp, _Sock, Data} ->
            NewBuf = iolist_to_binary([Buf, Data]),
            case mysql_proto:decode(packet, NewBuf) of
                P = {packet, _Bytes, _Rest} ->
                    P;
                {incomplete, Rest} ->
                    read_packet(S#state{buf=Rest});
                {incomplete, _SizeNeeded, Rest} ->
                    read_packet(S#state{buf=Rest})
            end
    end.

code_change(_OldVsn, _State, _Extra) ->
    {ok, {newstate, data_vsn()}}.

