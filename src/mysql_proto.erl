%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc MySQL Protocol encoding and decoding.
%% @end
%%%-------------------------------------------------------------------
-module(mysql_proto).

%% API
-export([decode/2,
         encode/1]).
-export([test_client/0]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(MYSQL_VERSION_10, 10).

%%====================================================================
%% API
%%====================================================================

decode(Type, <<Length:24/little, SeqNo:8/little, Packet:Length/binary, Rest/binary>>) ->
    {packet, decode_packet(Type, [{seqno, SeqNo}], Packet), Rest};
decode(_Type, Rest) ->
    {incomplete, Rest}.

decode_packet(server_handshake, Pkt, <<?MYSQL_VERSION_10, Rest/binary>>) ->
    case decode_nullterm_string(Rest) of
        {ServerVersion,
         <<ThreadId:32/little,
          Scramble1:8/binary, 0,
          Capabilities:16/bitstring,
          Lang:8/little,
          Status:16/little,
          _Filler:13/binary,
          Scramble2:13/binary>>} ->
            {server_handshake, [{vsn, ?MYSQL_VERSION_10},
                         {thread_id, ThreadId},
                         {server_vsn, ServerVersion},
                         {scramble_buff, iolist_to_binary([Scramble1, Scramble2])},
                         {server_capabilities, Capabilities},
                         {language, Lang},
                         {server_status, Status} | Pkt]}
    end.

decode_nullterm_string(Bin) ->
    decode_nullterm_string(Bin, 1).

decode_nullterm_string(Bin, Idx) ->
    case Bin of
        <<String:Idx/binary, 0, Rest/binary>> ->
            {String, Rest};
        _ when byte_size(Bin) > Idx ->
            decode_nullterm_string(Bin, Idx + 1)
    end.

encode({server_handshake, Values}) ->
    Seq = proplists:get_value(seqno, Values),
    <<Scramble1:8/binary,Scramble2/binary>> = proplists:get_value(scramble_buff,Values),
    encode_packet(Seq,
                  [<<?MYSQL_VERSION_10>>,
                   encode_nullterm_string(proplists:get_value(server_vsn,Values)),
                   <<(proplists:get_value(thread_id,Values)):32/little>>,
                   Scramble1, 
                   0,
                   proplists:get_value(server_capabilities,Values),
                   <<(proplists:get_value(language,Values)):8/little>>,
                   <<(proplists:get_value(server_status,Values)):16/little>>,
                   << 0:(8*13) >>,
                   Scramble2]).

encode_nullterm_string(Str) ->
    iolist_to_binary([Str, 0]).

encode_packet(Seq, IoList) when is_list(IoList) ->
    encode_packet(Seq, iolist_to_binary(IoList));
encode_packet(Seq, Bin) when is_binary(Bin) ->
    <<(byte_size(Bin)):24/little, Seq:8/little, Bin/binary>>.

%%====================================================================
%% Internal functions
%%====================================================================

test_client() ->
    {ok, Sock} = gen_tcp:connect("192.168.0.17", 3306,
                                 [{active, false}, {packet, raw}, binary]),
    timer:sleep(100),
    Rcv = gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock),
    Rcv.

example_mysql_server_handshake() ->
    <<52,0,0,0,10,53,46,48,46,52,53,0,44,0,0,0,120,42,98,51,
     116,111,83,49,0,44,162,8,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     56,100,114,50,61,124,124,119,96,93,50,125,0>>.

server_handshake_test() ->
    Bytes = example_mysql_server_handshake(),
    ?assertMatch({packet, {server_handshake, _Values}, <<>>},
                 decode(server_handshake, Bytes)),
    {packet, Pkt, <<>>} = decode(server_handshake, example_mysql_server_handshake()),
    ?assertMatch(Bytes, encode(Pkt)).

