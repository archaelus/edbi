%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Protocol constants
%% @end
%%%-------------------------------------------------------------------
-module(mysql_proto_constants).

%% API
-export([command_code/1,
         command/1]).

%%====================================================================
%% API
%%====================================================================
 
command(16#00) -> com_sleep;
command(16#01) -> com_quit;
command(16#02) -> com_init_db;
command(16#03) -> com_query;
command(16#04) -> com_field_list;
command(16#05) -> com_create_db;
command(16#06) -> com_drop_db;
command(16#07) -> com_refresh;
command(16#08) -> com_shutdown;
command(16#09) -> com_statistics;
command(16#0a) -> com_process_info;
command(16#0b) -> com_connect;
command(16#0c) -> com_process_kill;
command(16#0d) -> com_debug;
command(16#0e) -> com_ping;
command(16#0f) -> com_time;
command(16#10) -> com_delayed_insert;
command(16#11) -> com_change_user;
command(16#12) -> com_binlog_dump;
command(16#13) -> com_table_dump;
command(16#14) -> com_connect_out;
command(16#15) -> com_register_slave;
command(16#16) -> com_stmt_prepare;
command(16#17) -> com_stmt_execute;
command(16#18) -> com_stmt_send_long_data;
command(16#19) -> com_stmt_close;
command(16#1a) -> com_stmt_reset;
command(16#1b) -> com_set_option;
command(16#1c) -> com_stmt_fetch.

command_code(com_sleep) -> 16#00;
command_code(com_quit) -> 16#01;
command_code(com_init_db) -> 16#02;
command_code(com_query) -> 16#03;
command_code(com_field_list) -> 16#04;
command_code(com_create_db) -> 16#05;
command_code(com_drop_db) -> 16#06;
command_code(com_refresh) -> 16#07;
command_code(com_shutdown) -> 16#08;
command_code(com_statistics) -> 16#09;
command_code(com_process_info) -> 16#0a;
command_code(com_connect) -> 16#0b;
command_code(com_process_kill) -> 16#0c;
command_code(com_debug) -> 16#0d;
command_code(com_ping) -> 16#0e;
command_code(com_time) -> 16#0f;
command_code(com_delayed_insert) -> 16#10;
command_code(com_change_user) -> 16#11;
command_code(com_binlog_dump) -> 16#12;
command_code(com_table_dump) -> 16#13;
command_code(com_connect_out) -> 16#14;
command_code(com_register_slave) -> 16#15;
command_code(com_stmt_prepare) -> 16#16;
command_code(com_stmt_execute) -> 16#17;
command_code(com_stmt_send_long_data) -> 16#18;
command_code(com_stmt_close) -> 16#19;
command_code(com_stmt_reset) -> 16#1a;
command_code(com_set_option) -> 16#1b;
command_code(com_stmt_fetch) -> 16#1c.

%%====================================================================
%% Internal functions
%%====================================================================
