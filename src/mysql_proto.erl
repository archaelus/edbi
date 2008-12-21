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
-define(MYSQL_16MEG_PKT, 16777216).
-define(MYSQL_DEFAULT_CHARSET, 8).

%%====================================================================
%% API
%%====================================================================

decode(Type, <<Length:24/little, SeqNo:8/little, Packet:Length/binary, Rest/binary>>) ->
    {packet, SeqNo, decode_packet(Type, [], Packet), Rest};
decode(_Type, Rest) ->
    {incomplete, Rest}.

encode({server_handshake, Values}) ->
    <<Scramble1:8/binary,Scramble2/binary>> = proplists:get_value(scramble_buff,Values),
    [<<?MYSQL_VERSION_10>>,
     encode_nullterm_string(proplists:get_value(server_vsn,Values)),
     <<(proplists:get_value(thread_id,Values)):32/little>>,
     Scramble1, 
     0,
     proplists:get_value(server_capabilities,Values),
     <<(proplists:get_value(language,Values)):8/little>>,
     <<(proplists:get_value(server_status,Values)):16/little>>,
     << 0:(8*13) >>,
     Scramble2];
encode({client_handshake, Values}) ->
    [<<(proplists:get_value(client_flags,Values)):32/little>>,
     <<(proplists:get_value(max_packet_size,Values)):32/little>>,
     <<(proplists:get_value(charset_no,Values)):8/little>>,
     << 0:(8*23) >>,
     encode_nullterm_string(proplists:get_value(username,Values)),
     encode_lcb(proplists:get_value(scrambled_pass,Values))
     |
     case proplists:get_value(dbname,Values) of
         undefined -> [];
         V -> [0, encode_nullterm_string(V)]
     end].

client_handshake(Username, Password, Options) when is_list(Username) ->
    client_handshake(iolist_to_binary(Username), Password, Options);
client_handshake(Username, Password, Options) when is_list(Password) ->
    client_handshake(Username, iolist_to_binary(Password), Options);
client_handshake(Username, Password, Options) when is_binary(Username), is_binary(Password) ->
    ScrambleBuff = proplists:get_value(scramble_buff, Options),
    {client_handshake,
     [{client_flags, proplists:get_value(client_flags, Options, 41637)},
      {max_packet_size, proplists:get_value(max_packet_size, Options, ?MYSQL_16MEG_PKT)},
      {charset_no, proplists:get_value(charset_no, Options, ?MYSQL_DEFAULT_CHARSET)},
      {username, Username},
      {scrambled_pass, scramble_password(ScrambleBuff,Password)}
     ]}.


encode_packet(Seq, IoList) when is_list(IoList) ->
    encode_packet(Seq, iolist_to_binary(IoList));
encode_packet(Seq, Bin) when is_binary(Bin) ->
    <<(byte_size(Bin)):24/little, Seq:8/little, Bin/binary>>.


%%====================================================================
%% Internal functions
%%====================================================================


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
            {server_handshake,
             [{vsn, ?MYSQL_VERSION_10},
              {thread_id, ThreadId},
              {server_vsn, ServerVersion},
              {scramble_buff, iolist_to_binary([Scramble1, Scramble2])},
              {server_capabilities, Capabilities},
              {language, Lang},
              {server_status, Status} | Pkt]}
    end;
decode_packet(client_handshake, Pkt,
              <<ClientFlags:32/little,
               MaxPktSize:32/little,
               CharsetNo:8/little,
               _Filler:23/binary,
               Rest1/binary>>) ->
    Opts = Pkt ++ [{client_flags, ClientFlags},
                   {max_packet_size, MaxPktSize},
                   {charset_no, CharsetNo}],
    case decode_nullterm_string(Rest1) of
        {UserName, <<>>} ->
            {client_handshake,
             Opts ++ [{username, UserName}]};
        {UserName, Rest2} ->
            case decode_lcb(Rest2) of
                {ScrambleBuff,<<>>} ->
                    {client_handshake,
                     Opts ++ [{username, UserName},
                              {scramble_buff, ScrambleBuff}]};
                {ScrambleBuff, <<0, Rest3>>} ->
                    case decode_nullterm_string(Rest3) of
                        {DbName, <<>>} ->
                            {client_handshake,
                             Opts ++ [{username, UserName},
                                      {scramble_buff, ScrambleBuff},
                                      {db_name, DbName}]}
                    end
            end
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


encode_nullterm_string(Str) ->
    iolist_to_binary([Str, 0]).

encode_lcb(null) ->
    <<251:8/little>>;
encode_lcb(Bin) when byte_size(Bin) =< 250 ->
    <<(byte_size(Bin)):8/little, Bin/binary>>;
encode_lcb(Bin) when byte_size(Bin) =< 65535 ->
    <<252:8/little, (byte_size(Bin)):16/little, Bin/binary>>;
encode_lcb(Bin) when byte_size(Bin) =< 16777215 ->
    <<253:8/little, (byte_size(Bin)):24/little, Bin/binary>>;
encode_lcb(Bin) when byte_size(Bin) > 16777215 ->
    <<254:8/little, (byte_size(Bin)):64/little, Bin/binary>>.

decode_lcb(<<251:8/little, Rest/binary>>) ->
    {null, Rest};
decode_lcb(<<Len:8/little, Data:Len/binary, Rest/binary>>) when Len =< 250 ->
    {Data, Rest};
decode_lcb(<<252, Len:16/little, Data:Len/binary, Rest/binary>>) when Len =< 65535 ->
    {Data, Rest};
decode_lcb(<<253, Len:24/little, Data:Len/binary, Rest/binary>>) when Len =< 16777215 ->
    {Data, Rest};
decode_lcb(<<254, Len:64/little, Data:Len/binary, Rest/binary>>) when Len > 16777215 ->
    {Data, Rest}.

scramble_password(<<ScrambleBuff:20/binary, 0>>, Password) when is_binary(Password) ->
    Stage1 = crypto:sha(Password),
    Stage2 = crypto:sha(Stage1),
    crypto:exor(Stage1, crypto:sha(<<ScrambleBuff/binary, Stage2/binary>>)).

%%====================================================================
%% Unit testing
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

example_scramble_buff() ->
    <<119,94,80,87,66,36,43,75,121,63,111,49,68,69,106,101,62,
     67,119,80,0>>.

example_scrambled_pass() ->
    <<250,250,28,1,178,179,188,240,224,6,40,213,32,38,142,14,
     69,236,160,21>>.

example_client_handshake() ->
    <<62,0,0,1,165,162,0,0,0,0,0,64,8,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0,0,101,106,97,98,98,101,114,100,0,20,
     250,250,28,1,178,179,188,240,224,6,40,213,32,38,142,14,
     69,236,160,21>>.

scramble_test() ->
    Bytes = example_scrambled_pass(),
    ?assertMatch(Bytes,
                 scramble_password(example_scramble_buff(), <<"ejabberd">>)).

client_handshake_test() ->
    Hsk = client_handshake("ejabberd", "ejabberd", [{scramble_buff, example_scramble_buff()},
                                                    {max_packet_size, 1073741824}]),
    Pkt = encode(Hsk),
    Bytes = example_client_handshake(),
    ?assertMatch(Bytes, encode_packet(1, Pkt)).

server_handshake_test() ->
    Bytes = example_mysql_server_handshake(),
    ?assertMatch({packet, 0, {server_handshake, _Values}, <<>>},
                 decode(server_handshake, Bytes)),
    {packet, 0, Pkt, <<>>} = decode(server_handshake, example_mysql_server_handshake()),
    ?assertMatch(Bytes, encode_packet(0,encode(Pkt))).

lcb_test() ->
    crypto:start(),
    lists:foreach(fun (Len) ->
                          Bytes = crypto:rand_bytes(Len),
                          Enc = encode_lcb(Bytes),
                          ?assertMatch({Bytes, <<>>}, decode_lcb(Enc))
                  end,
                  [1,2,249,250,251,65534,65535,65536]).
                   %%,16777215,16777216,16777217]).
