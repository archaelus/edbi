%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc MySQL Protocol constant conversion functions
%% @end
%%%-------------------------------------------------------------------
-module(mysql_proto_constants).

%% API
-export([command/1
         ,command_code/1
         ,commands/0
         ,error/1
         ,error_code/1
         ,errors/0
        ]).

%%====================================================================
%% API
%%====================================================================
 
command(16#00) -> sleep;
command(16#01) -> quit;
command(16#02) -> init_db;
command(16#03) -> 'query';
command(16#04) -> field_list;
command(16#05) -> create_db;
command(16#06) -> drop_db;
command(16#07) -> refresh;
command(16#08) -> shutdown;
command(16#09) -> statistics;
command(16#0a) -> process_info;
command(16#0b) -> connect;
command(16#0c) -> process_kill;
command(16#0d) -> debug;
command(16#0e) -> ping;
command(16#0f) -> time;
command(16#10) -> delayed_insert;
command(16#11) -> change_user;
command(16#12) -> binlog_dump;
command(16#13) -> table_dump;
command(16#14) -> connect_out;
command(16#15) -> register_slave;
command(16#16) -> stmt_prepare;
command(16#17) -> stmt_execute;
command(16#18) -> stmt_send_long_data;
command(16#19) -> stmt_close;
command(16#1a) -> stmt_reset;
command(16#1b) -> set_option;
command(16#1c) -> stmt_fetch.

command_code(sleep) -> 16#00;
command_code(quit) -> 16#01;
command_code(init_db) -> 16#02;
command_code('query') -> 16#03;
command_code(field_list) -> 16#04;
command_code(create_db) -> 16#05;
command_code(drop_db) -> 16#06;
command_code(refresh) -> 16#07;
command_code(shutdown) -> 16#08;
command_code(statistics) -> 16#09;
command_code(process_info) -> 16#0a;
command_code(connect) -> 16#0b;
command_code(process_kill) -> 16#0c;
command_code(debug) -> 16#0d;
command_code(ping) -> 16#0e;
command_code(time) -> 16#0f;
command_code(delayed_insert) -> 16#10;
command_code(change_user) -> 16#11;
command_code(binlog_dump) -> 16#12;
command_code(table_dump) -> 16#13;
command_code(connect_out) -> 16#14;
command_code(register_slave) -> 16#15;
command_code(stmt_prepare) -> 16#16;
command_code(stmt_execute) -> 16#17;
command_code(stmt_send_long_data) -> 16#18;
command_code(stmt_close) -> 16#19;
command_code(stmt_reset) -> 16#1a;
command_code(set_option) -> 16#1b;
command_code(stmt_fetch) -> 16#1c.
commands() ->
    [sleep
     ,quit
     ,init_db
     ,'query'
     ,field_list
     ,create_db
     ,drop_db
     ,refresh
     ,shutdown
     ,statistics
     ,process_info
     ,connect
     ,process_kill
     ,debug
     ,ping
     ,time
     ,delayed_insert
     ,change_user
     ,binlog_dump
     ,table_dump
     ,connect_out
     ,register_slave
     ,stmt_prepare
     ,stmt_execute
     ,stmt_send_long_data
     ,stmt_close
     ,stmt_reset
     ,set_option].

error(1000) -> hashchk;
error(1001) -> nisamchk;
error(1002) -> no;
error(1003) -> yes;
error(1004) -> cant_create_file;
error(1005) -> cant_create_table;
error(1006) -> cant_create_db;
error(1007) -> db_create_exists;
error(1008) -> db_drop_exists;
error(1009) -> db_drop_delete;
error(1010) -> db_drop_rmdir;
error(1011) -> cant_delete_file;
error(1012) -> cant_find_system_rec;
error(1013) -> cant_get_stat;
error(1014) -> cant_get_wd;
error(1015) -> cant_lock;
error(1016) -> cant_open_file;
error(1017) -> file_not_found;
error(1018) -> cant_read_dir;
error(1019) -> cant_set_wd;
error(1020) -> checkread;
error(1021) -> disk_full;
error(1022) -> dup_key;
error(1023) -> error_on_close;
error(1024) -> error_on_read;
error(1025) -> error_on_rename;
error(1026) -> error_on_write;
error(1027) -> file_used;
error(1028) -> filsort_abort;
error(1029) -> form_not_found;
error(1030) -> get_errno;
error(1031) -> illegal_ha;
error(1032) -> key_not_found;
error(1033) -> not_form_file;
error(1034) -> not_keyfile;
error(1035) -> old_keyfile;
error(1036) -> open_as_readonly;
error(1037) -> outofmemory;
error(1038) -> out_of_sortmemory;
error(1039) -> unexpected_eof;
error(1040) -> con_count_error;
error(1041) -> out_of_resources;
error(1042) -> bad_host_error;
error(1043) -> handshake_error;
error(1044) -> dbaccess_denied_error;
error(1045) -> access_denied_error;
error(1046) -> no_db_error;
error(1047) -> unknown_com_error;
error(1048) -> bad_null_error;
error(1049) -> bad_db_error;
error(1050) -> table_exists_error;
error(1051) -> bad_table_error;
error(1052) -> non_uniq_error;
error(1053) -> server_shutdown;
error(1054) -> bad_field_error;
error(1055) -> wrong_field_with_group;
error(1056) -> wrong_group_field;
error(1057) -> wrong_sum_select;
error(1058) -> wrong_value_count;
error(1059) -> too_long_ident;
error(1060) -> dup_fieldname;
error(1061) -> dup_keyname;
error(1062) -> dup_entry;
error(1063) -> wrong_field_spec;
error(1064) -> parse_error;
error(1065) -> empty_query;
error(1066) -> nonuniq_table;
error(1067) -> invalid_default;
error(1068) -> multiple_pri_key;
error(1069) -> too_many_keys;
error(1070) -> too_many_key_parts;
error(1071) -> too_long_key;
error(1072) -> key_column_does_not_exits;
error(1073) -> blob_used_as_key;
error(1074) -> too_big_fieldlength;
error(1075) -> wrong_auto_key;
error(1076) -> ready;
error(1077) -> normal_shutdown;
error(1078) -> got_signal;
error(1079) -> shutdown_complete;
error(1080) -> forcing_close;
error(1081) -> ipsock_error;
error(1082) -> no_such_index;
error(1083) -> wrong_field_terminators;
error(1084) -> blobs_and_no_terminated;
error(1085) -> textfile_not_readable;
error(1086) -> file_exists_error;
error(1087) -> load_info;
error(1088) -> alter_info;
error(1089) -> wrong_sub_key;
error(1090) -> cant_remove_all_fields;
error(1091) -> cant_drop_field_or_key;
error(1092) -> insert_info;
error(1093) -> update_table_used;
error(1094) -> no_such_thread;
error(1095) -> kill_denied_error;
error(1096) -> no_tables_used;
error(1097) -> too_big_set;
error(1098) -> no_unique_logfile;
error(1099) -> table_not_locked_for_write;
error(1100) -> table_not_locked;
error(1101) -> blob_cant_have_default;
error(1102) -> wrong_db_name;
error(1103) -> wrong_table_name;
error(1104) -> too_big_select;
error(1105) -> unknown_error;
error(1106) -> unknown_procedure;
error(1107) -> wrong_paramcount_to_procedure;
error(1108) -> wrong_parameters_to_procedure;
error(1109) -> unknown_table;
error(1110) -> field_specified_twice;
error(1111) -> invalid_group_func_use;
error(1112) -> unsupported_extension;
error(1113) -> table_must_have_columns;
error(1114) -> record_file_full;
error(1115) -> unknown_character_set;
error(1116) -> too_many_tables;
error(1117) -> too_many_fields;
error(1118) -> too_big_rowsize;
error(1119) -> stack_overrun;
error(1120) -> wrong_outer_join;
error(1121) -> null_column_in_index;
error(1122) -> cant_find_udf;
error(1123) -> cant_initialize_udf;
error(1124) -> udf_no_paths;
error(1125) -> udf_exists;
error(1126) -> cant_open_library;
error(1127) -> cant_find_dl_entry;
error(1128) -> function_not_defined;
error(1129) -> host_is_blocked;
error(1130) -> host_not_privileged;
error(1131) -> password_anonymous_user;
error(1132) -> password_not_allowed;
error(1133) -> password_no_match;
error(1134) -> update_info;
error(1135) -> cant_create_thread;
error(1136) -> wrong_value_count_on_row;
error(1137) -> cant_reopen_table;
error(1138) -> invalid_use_of_null;
error(1139) -> regexp_error;
error(1140) -> mix_of_group_func_and_fields;
error(1141) -> nonexisting_grant;
error(1142) -> tableaccess_denied_error;
error(1143) -> columnaccess_denied_error;
error(1144) -> illegal_grant_for_table;
error(1145) -> grant_wrong_host_or_user;
error(1146) -> no_such_table;
error(1147) -> nonexisting_table_grant;
error(1148) -> not_allowed_command;
error(1149) -> syntax_error;
error(1150) -> delayed_cant_change_lock;
error(1151) -> too_many_delayed_threads;
error(1152) -> aborting_connection;
error(1153) -> net_packet_too_large;
error(1154) -> net_read_error_from_pipe;
error(1155) -> net_fcntl_error;
error(1156) -> net_packets_out_of_order;
error(1157) -> net_uncompress_error;
error(1158) -> net_read_error;
error(1159) -> net_read_interrupted;
error(1160) -> net_error_on_write;
error(1161) -> net_write_interrupted;
error(1162) -> too_long_string;
error(1163) -> table_cant_handle_blob;
error(1164) -> table_cant_handle_auto_increment;
error(1165) -> delayed_insert_table_locked;
error(1166) -> wrong_column_name;
error(1167) -> wrong_key_column;
error(1168) -> wrong_mrg_table;
error(1169) -> dup_unique;
error(1170) -> blob_key_without_length;
error(1171) -> primary_cant_have_null;
error(1172) -> too_many_rows;
error(1173) -> requires_primary_key;
error(1174) -> no_raid_compiled;
error(1175) -> update_without_key_in_safe_mode;
error(1176) -> key_does_not_exits;
error(1177) -> check_no_such_table;
error(1178) -> check_not_implemented;
error(1179) -> cant_do_this_during_an_transaction;
error(1180) -> error_during_commit;
error(1181) -> error_during_rollback;
error(1182) -> error_during_flush_logs;
error(1183) -> error_during_checkpoint;
error(1184) -> new_aborting_connection;
error(1185) -> dump_not_implemented;
error(1186) -> flush_master_binlog_closed;
error(1187) -> index_rebuild;
error(1188) -> master;
error(1189) -> master_net_read;
error(1190) -> master_net_write;
error(1191) -> ft_matching_key_not_found;
error(1192) -> lock_or_active_transaction;
error(1193) -> unknown_system_variable;
error(1194) -> crashed_on_usage;
error(1195) -> crashed_on_repair;
error(1196) -> warning_not_complete_rollback;
error(1197) -> trans_cache_full;
error(1198) -> slave_must_stop;
error(1199) -> slave_not_running;
error(1200) -> bad_slave;
error(1201) -> master_info;
error(1202) -> slave_thread;
error(1203) -> too_many_user_connections;
error(1204) -> set_constants_only;
error(1205) -> lock_wait_timeout;
error(1206) -> lock_table_full;
error(1207) -> read_only_transaction;
error(1208) -> drop_db_with_read_lock;
error(1209) -> create_db_with_read_lock;
error(1210) -> wrong_arguments;
error(1211) -> no_permission_to_create_user;
error(1212) -> union_tables_in_different_dir;
error(1213) -> lock_deadlock;
error(1214) -> table_cant_handle_ft;
error(1215) -> cannot_add_foreign;
error(1216) -> no_referenced_row;
error(1217) -> row_is_referenced;
error(1218) -> connect_to_master;
error(1219) -> query_on_master;
error(1220) -> error_when_executing_command;
error(1221) -> wrong_usage;
error(1222) -> wrong_number_of_columns_in_select;
error(1223) -> cant_update_with_readlock;
error(1224) -> mixing_not_allowed;
error(1225) -> dup_argument;
error(1226) -> user_limit_reached;
error(1227) -> specific_access_denied_error;
error(1228) -> local_variable;
error(1229) -> global_variable;
error(1230) -> no_default;
error(1231) -> wrong_value_for_var;
error(1232) -> wrong_type_for_var;
error(1233) -> var_cant_be_read;
error(1234) -> cant_use_option_here;
error(1235) -> not_supported_yet;
error(1236) -> master_fatal_error_reading_binlog;
error(1237) -> slave_ignored_table;
error(1238) -> incorrect_global_local_var;
error(1239) -> wrong_fk_def;
error(1240) -> key_ref_do_not_match_table_ref;
error(1241) -> operand_columns;
error(1242) -> subquery_no_1_row;
error(1243) -> unknown_stmt_handler;
error(1244) -> corrupt_help_db;
error(1245) -> cyclic_reference;
error(1246) -> auto_convert;
error(1247) -> illegal_reference;
error(1248) -> derived_must_have_alias;
error(1249) -> select_reduced;
error(1250) -> tablename_not_allowed_here;
error(1251) -> not_supported_auth_mode;
error(1252) -> spatial_cant_have_null;
error(1253) -> collation_charset_mismatch;
error(1254) -> slave_was_running;
error(1255) -> slave_was_not_running;
error(1256) -> too_big_for_uncompress;
error(1257) -> zlib_z_mem_error;
error(1258) -> zlib_z_buf_error;
error(1259) -> zlib_z_data_error;
error(1260) -> cut_value_group_concat;
error(1261) -> warn_too_few_records;
error(1262) -> warn_too_many_records;
error(1263) -> warn_null_to_notnull;
error(1264) -> warn_data_out_of_range;
error(1265) -> n_data_truncated;
error(1266) -> warn_using_other_handler;
error(1267) -> cant_aggregate_2collations;
error(1268) -> drop_user;
error(1269) -> revoke_grants;
error(1270) -> cant_aggregate_3collations;
error(1271) -> cant_aggregate_ncollations;
error(1272) -> variable_is_not_struct;
error(1273) -> unknown_collation;
error(1274) -> slave_ignored_ssl_params;
error(1275) -> server_is_in_secure_auth_mode;
error(1276) -> warn_field_resolved;
error(1277) -> bad_slave_until_cond;
error(1278) -> missing_skip_slave;
error(1279) -> until_cond_ignored;
error(1280) -> wrong_name_for_index;
error(1281) -> wrong_name_for_catalog;
error(1282) -> warn_qc_resize;
error(1283) -> bad_ft_column;
error(1284) -> unknown_key_cache;
error(1285) -> warn_hostname_wont_work;
error(1286) -> unknown_storage_engine;
error(1287) -> unused_1;
error(1288) -> non_updatable_table;
error(1289) -> feature_disabled;
error(1290) -> option_prevents_statement;
error(1291) -> duplicated_value_in_type;
error(1292) -> truncated_wrong_value;
error(1293) -> too_much_auto_timestamp_cols;
error(1294) -> invalid_on_update;
error(1295) -> unsupported_ps;
error(1296) -> get_errmsg;
error(1297) -> get_temporary_errmsg;
error(1298) -> unknown_time_zone;
error(1299) -> warn_invalid_timestamp;
error(1300) -> invalid_character_string;
error(1301) -> warn_allowed_packet_overflowed;
error(1302) -> conflicting_declarations;
error(1303) -> sp_no_recursive_create;
error(1304) -> sp_already_exists;
error(1305) -> sp_does_not_exist;
error(1306) -> sp_drop_failed;
error(1307) -> sp_store_failed;
error(1308) -> sp_lilabel_mismatch;
error(1309) -> sp_label_redefine;
error(1310) -> sp_label_mismatch;
error(1311) -> sp_uninit_var;
error(1312) -> sp_badselect;
error(1313) -> sp_badreturn;
error(1314) -> sp_badstatement;
error(1315) -> update_log_deprecated_ignored;
error(1316) -> update_log_deprecated_translated;
error(1317) -> query_interrupted;
error(1318) -> sp_wrong_no_of_args;
error(1319) -> sp_cond_mismatch;
error(1320) -> sp_noreturn;
error(1321) -> sp_noreturnend;
error(1322) -> sp_bad_cursor_query;
error(1323) -> sp_bad_cursor_select;
error(1324) -> sp_cursor_mismatch;
error(1325) -> sp_cursor_already_open;
error(1326) -> sp_cursor_not_open;
error(1327) -> sp_undeclared_var;
error(1328) -> sp_wrong_no_of_fetch_args;
error(1329) -> sp_fetch_no_data;
error(1330) -> sp_dup_param;
error(1331) -> sp_dup_var;
error(1332) -> sp_dup_cond;
error(1333) -> sp_dup_curs;
error(1334) -> sp_cant_alter;
error(1335) -> sp_subselect_nyi;
error(1336) -> stmt_not_allowed_in_sf_or_trg;
error(1337) -> sp_varcond_after_curshndlr;
error(1338) -> sp_cursor_after_handler;
error(1339) -> sp_case_not_found;
error(1340) -> fparser_too_big_file;
error(1341) -> fparser_bad_header;
error(1342) -> fparser_eof_in_comment;
error(1343) -> fparser_error_in_parameter;
error(1344) -> fparser_eof_in_unknown_parameter;
error(1345) -> view_no_explain;
error(1346) -> frm_unknown_type;
error(1347) -> wrong_object;
error(1348) -> nonupdateable_column;
error(1349) -> view_select_derived;
error(1350) -> view_select_clause;
error(1351) -> view_select_variable;
error(1352) -> view_select_tmptable;
error(1353) -> view_wrong_list;
error(1354) -> warn_view_merge;
error(1355) -> warn_view_without_key;
error(1356) -> view_invalid;
error(1357) -> sp_no_drop_sp;
error(1358) -> sp_goto_in_hndlr;
error(1359) -> trg_already_exists;
error(1360) -> trg_does_not_exist;
error(1361) -> trg_on_view_or_temp_table;
error(1362) -> trg_cant_change_row;
error(1363) -> trg_no_such_row_in_trg;
error(1364) -> no_default_for_field;
error(1365) -> division_by_zero;
error(1366) -> truncated_wrong_value_for_field;
error(1367) -> illegal_value_for_type;
error(1368) -> view_nonupd_check;
error(1369) -> view_check_failed;
error(1370) -> procaccess_denied_error;
error(1371) -> relay_log_fail;
error(1372) -> passwd_length;
error(1373) -> unknown_target_binlog;
error(1374) -> io_err_log_index_read;
error(1375) -> binlog_purge_prohibited;
error(1376) -> fseek_fail;
error(1377) -> binlog_purge_fatal_err;
error(1378) -> log_in_use;
error(1379) -> log_purge_unknown_err;
error(1380) -> relay_log_init;
error(1381) -> no_binary_logging;
error(1382) -> reserved_syntax;
error(1383) -> wsas_failed;
error(1384) -> diff_groups_proc;
error(1385) -> no_group_for_proc;
error(1386) -> order_with_proc;
error(1387) -> logging_prohibit_changing_of;
error(1388) -> no_file_mapping;
error(1389) -> wrong_magic;
error(1390) -> ps_many_param;
error(1391) -> key_part_0;
error(1392) -> view_checksum;
error(1393) -> view_multiupdate;
error(1394) -> view_no_insert_field_list;
error(1395) -> view_delete_merge_view;
error(1396) -> cannot_user;
error(1397) -> xaer_nota;
error(1398) -> xaer_inval;
error(1399) -> xaer_rmfail;
error(1400) -> xaer_outside;
error(1401) -> xaer_rmerr;
error(1402) -> xa_rbrollback;
error(1403) -> nonexisting_proc_grant;
error(1404) -> proc_auto_grant_fail;
error(1405) -> proc_auto_revoke_fail;
error(1406) -> data_too_long;
error(1407) -> sp_bad_sqlstate;
error(1408) -> startup;
error(1409) -> load_from_fixed_size_rows_to_var;
error(1410) -> cant_create_user_with_grant;
error(1411) -> wrong_value_for_type;
error(1412) -> table_def_changed;
error(1413) -> sp_dup_handler;
error(1414) -> sp_not_var_arg;
error(1415) -> sp_no_retset;
error(1416) -> cant_create_geometry_object;
error(1417) -> failed_routine_break_binlog;
error(1418) -> binlog_unsafe_routine;
error(1419) -> binlog_create_routine_need_super;
error(1420) -> exec_stmt_with_open_cursor;
error(1421) -> stmt_has_no_open_cursor;
error(1422) -> commit_not_allowed_in_sf_or_trg;
error(1423) -> no_default_for_view_field;
error(1424) -> sp_no_recursion;
error(1425) -> too_big_scale;
error(1426) -> too_big_precision;
error(1427) -> m_bigger_than_d;
error(1428) -> wrong_lock_of_system_table;
error(1429) -> connect_to_foreign_data_source;
error(1430) -> query_on_foreign_data_source;
error(1431) -> foreign_data_source_doesnt_exist;
error(1432) -> foreign_data_string_invalid_cant_create;
error(1433) -> foreign_data_string_invalid;
error(1434) -> cant_create_federated_table;
error(1435) -> trg_in_wrong_schema;
error(1436) -> stack_overrun_need_more;
error(1437) -> too_long_body;
error(1438) -> warn_cant_drop_default_keycache;
error(1439) -> too_big_displaywidth;
error(1440) -> xaer_dupid;
error(1441) -> datetime_function_overflow;
error(1442) -> cant_update_used_table_in_sf_or_trg;
error(1443) -> view_prevent_update;
error(1444) -> ps_no_recursion;
error(1445) -> sp_cant_set_autocommit;
error(1446) -> malformed_definer;
error(1447) -> view_frm_no_user;
error(1448) -> view_other_user;
error(1449) -> no_such_user;
error(1450) -> forbid_schema_change;
error(1451) -> row_is_referenced_2;
error(1452) -> no_referenced_row_2;
error(1453) -> sp_bad_var_shadow;
error(1454) -> trg_no_definer;
error(1455) -> old_file_format;
error(1456) -> sp_recursion_limit;
error(1457) -> sp_proc_table_corrupt;
error(1458) -> sp_wrong_name;
error(1459) -> table_needs_upgrade;
error(1460) -> sp_no_aggregate;
error(1461) -> max_prepared_stmt_count_reached;
error(1462) -> view_recursive;
error(1463) -> non_grouping_field_used;
error(1464) -> table_cant_handle_spkeys;
error(1465) -> illegal_ha_create_option;
error(1466) -> partition_requires_values_error;
error(1467) -> partition_wrong_values_error;
error(1468) -> partition_maxvalue_error;
error(1469) -> partition_subpartition_error;
error(1470) -> partition_subpart_mix_error;
error(1471) -> partition_wrong_no_part_error;
error(1472) -> partition_wrong_no_subpart_error;
error(1473) -> const_expr_in_partition_func_error;
error(1474) -> no_const_expr_in_range_or_list_error;
error(1475) -> field_not_found_part_error;
error(1476) -> list_of_fields_only_in_hash_error;
error(1477) -> inconsistent_partition_info_error;
error(1478) -> partition_func_not_allowed_error;
error(1479) -> partitions_must_be_defined_error;
error(1480) -> range_not_increasing_error;
error(1481) -> inconsistent_type_of_functions_error;
error(1482) -> multiple_def_const_in_list_part_error;
error(1483) -> partition_entry_error;
error(1484) -> mix_handler_error;
error(1485) -> partition_not_defined_error;
error(1486) -> too_many_partitions_error;
error(1487) -> subpartition_error;
error(1488) -> cant_create_handler_file;
error(1489) -> blob_field_in_part_func_error;
error(1490) -> unique_key_need_all_fields_in_pf;
error(1491) -> no_parts_error;
error(1492) -> partition_mgmt_on_nonpartitioned;
error(1493) -> foreign_key_on_partitioned;
error(1494) -> drop_partition_non_existent;
error(1495) -> drop_last_partition;
error(1496) -> coalesce_only_on_hash_partition;
error(1497) -> reorg_hash_only_on_same_no;
error(1498) -> reorg_no_param_error;
error(1499) -> only_on_range_list_partition;
error(1500) -> add_partition_subpart_error;
error(1501) -> add_partition_no_new_partition;
error(1502) -> coalesce_partition_no_partition;
error(1503) -> reorg_partition_not_exist;
error(1504) -> same_name_partition;
error(1505) -> no_binlog_error;
error(1506) -> consecutive_reorg_partitions;
error(1507) -> reorg_outside_range;
error(1508) -> partition_function_failure;
error(1509) -> part_state_error;
error(1510) -> limited_part_range;
error(1511) -> plugin_is_not_loaded;
error(1512) -> wrong_value;
error(1513) -> no_partition_for_given_value;
error(1514) -> filegroup_option_only_once;
error(1515) -> create_filegroup_failed;
error(1516) -> drop_filegroup_failed;
error(1517) -> tablespace_auto_extend_error;
error(1518) -> wrong_size_number;
error(1519) -> size_overflow_error;
error(1520) -> alter_filegroup_failed;
error(1521) -> binlog_row_logging_failed;
error(1522) -> binlog_row_wrong_table_def;
error(1523) -> binlog_row_rbr_to_sbr;
error(1524) -> event_already_exists;
error(1525) -> event_store_failed;
error(1526) -> event_does_not_exist;
error(1527) -> event_cant_alter;
error(1528) -> event_drop_failed;
error(1529) -> event_interval_not_positive_or_too_big;
error(1530) -> event_ends_before_starts;
error(1531) -> event_exec_time_in_the_past;
error(1532) -> event_open_table_failed;
error(1533) -> event_neither_m_expr_nor_m_at;
error(1534) -> col_count_doesnt_match_corrupted;
error(1535) -> cannot_load_from_table;
error(1536) -> event_cannot_delete;
error(1537) -> event_compile_error;
error(1538) -> event_same_name;
error(1539) -> event_data_too_long;
error(1540) -> drop_index_fk;
error(1541) -> warn_deprecated_syntax;
error(1542) -> cant_write_lock_log_table;
error(1543) -> cant_read_lock_log_table;
error(1544) -> foreign_duplicate_key;
error(1545) -> col_count_doesnt_match_please_update;
error(1546) -> removed_spaces;
error(1547) -> temp_table_prevents_switch_out_of_rbr;
error(1548) -> stored_function_prevents_switch_binlog_format;
error(1549) -> ndb_cant_switch_binlog_format;
error(1550) -> partition_no_temporary;
error(1551) -> partition_const_domain_error;
error(1552) -> partition_function_is_not_allowed;
error(1553) -> ddl_log_error;
error(1554) -> null_in_values_less_than;
error(1555) -> wrong_partition_name;
error(1556) -> cant_change_tx_isolation;
error(1557) -> dup_entry_autoincrement_case;
error(1558) -> event_modify_queue_error;
error(1559) -> event_set_var_error;
error(1560) -> partition_merge_error;
error(1561) -> cant_activate_log;
error(1562) -> rbr_not_available;
error(1563) -> no_triggers_on_system_schema;
error(1564) -> cant_alter_log_table;
error(1565) -> bad_log_engine;
error(1566) -> cant_drop_log_table.

error_code(hashchk) -> 1000;
error_code(nisamchk) -> 1001;
error_code(no) -> 1002;
error_code(yes) -> 1003;
error_code(cant_create_file) -> 1004;
error_code(cant_create_table) -> 1005;
error_code(cant_create_db) -> 1006;
error_code(db_create_exists) -> 1007;
error_code(db_drop_exists) -> 1008;
error_code(db_drop_delete) -> 1009;
error_code(db_drop_rmdir) -> 1010;
error_code(cant_delete_file) -> 1011;
error_code(cant_find_system_rec) -> 1012;
error_code(cant_get_stat) -> 1013;
error_code(cant_get_wd) -> 1014;
error_code(cant_lock) -> 1015;
error_code(cant_open_file) -> 1016;
error_code(file_not_found) -> 1017;
error_code(cant_read_dir) -> 1018;
error_code(cant_set_wd) -> 1019;
error_code(checkread) -> 1020;
error_code(disk_full) -> 1021;
error_code(dup_key) -> 1022;
error_code(error_on_close) -> 1023;
error_code(error_on_read) -> 1024;
error_code(error_on_rename) -> 1025;
error_code(error_on_write) -> 1026;
error_code(file_used) -> 1027;
error_code(filsort_abort) -> 1028;
error_code(form_not_found) -> 1029;
error_code(get_errno) -> 1030;
error_code(illegal_ha) -> 1031;
error_code(key_not_found) -> 1032;
error_code(not_form_file) -> 1033;
error_code(not_keyfile) -> 1034;
error_code(old_keyfile) -> 1035;
error_code(open_as_readonly) -> 1036;
error_code(outofmemory) -> 1037;
error_code(out_of_sortmemory) -> 1038;
error_code(unexpected_eof) -> 1039;
error_code(con_count_error) -> 1040;
error_code(out_of_resources) -> 1041;
error_code(bad_host_error) -> 1042;
error_code(handshake_error) -> 1043;
error_code(dbaccess_denied_error) -> 1044;
error_code(access_denied_error) -> 1045;
error_code(no_db_error) -> 1046;
error_code(unknown_com_error) -> 1047;
error_code(bad_null_error) -> 1048;
error_code(bad_db_error) -> 1049;
error_code(table_exists_error) -> 1050;
error_code(bad_table_error) -> 1051;
error_code(non_uniq_error) -> 1052;
error_code(server_shutdown) -> 1053;
error_code(bad_field_error) -> 1054;
error_code(wrong_field_with_group) -> 1055;
error_code(wrong_group_field) -> 1056;
error_code(wrong_sum_select) -> 1057;
error_code(wrong_value_count) -> 1058;
error_code(too_long_ident) -> 1059;
error_code(dup_fieldname) -> 1060;
error_code(dup_keyname) -> 1061;
error_code(dup_entry) -> 1062;
error_code(wrong_field_spec) -> 1063;
error_code(parse_error) -> 1064;
error_code(empty_query) -> 1065;
error_code(nonuniq_table) -> 1066;
error_code(invalid_default) -> 1067;
error_code(multiple_pri_key) -> 1068;
error_code(too_many_keys) -> 1069;
error_code(too_many_key_parts) -> 1070;
error_code(too_long_key) -> 1071;
error_code(key_column_does_not_exits) -> 1072;
error_code(blob_used_as_key) -> 1073;
error_code(too_big_fieldlength) -> 1074;
error_code(wrong_auto_key) -> 1075;
error_code(ready) -> 1076;
error_code(normal_shutdown) -> 1077;
error_code(got_signal) -> 1078;
error_code(shutdown_complete) -> 1079;
error_code(forcing_close) -> 1080;
error_code(ipsock_error) -> 1081;
error_code(no_such_index) -> 1082;
error_code(wrong_field_terminators) -> 1083;
error_code(blobs_and_no_terminated) -> 1084;
error_code(textfile_not_readable) -> 1085;
error_code(file_exists_error) -> 1086;
error_code(load_info) -> 1087;
error_code(alter_info) -> 1088;
error_code(wrong_sub_key) -> 1089;
error_code(cant_remove_all_fields) -> 1090;
error_code(cant_drop_field_or_key) -> 1091;
error_code(insert_info) -> 1092;
error_code(update_table_used) -> 1093;
error_code(no_such_thread) -> 1094;
error_code(kill_denied_error) -> 1095;
error_code(no_tables_used) -> 1096;
error_code(too_big_set) -> 1097;
error_code(no_unique_logfile) -> 1098;
error_code(table_not_locked_for_write) -> 1099;
error_code(table_not_locked) -> 1100;
error_code(blob_cant_have_default) -> 1101;
error_code(wrong_db_name) -> 1102;
error_code(wrong_table_name) -> 1103;
error_code(too_big_select) -> 1104;
error_code(unknown_error) -> 1105;
error_code(unknown_procedure) -> 1106;
error_code(wrong_paramcount_to_procedure) -> 1107;
error_code(wrong_parameters_to_procedure) -> 1108;
error_code(unknown_table) -> 1109;
error_code(field_specified_twice) -> 1110;
error_code(invalid_group_func_use) -> 1111;
error_code(unsupported_extension) -> 1112;
error_code(table_must_have_columns) -> 1113;
error_code(record_file_full) -> 1114;
error_code(unknown_character_set) -> 1115;
error_code(too_many_tables) -> 1116;
error_code(too_many_fields) -> 1117;
error_code(too_big_rowsize) -> 1118;
error_code(stack_overrun) -> 1119;
error_code(wrong_outer_join) -> 1120;
error_code(null_column_in_index) -> 1121;
error_code(cant_find_udf) -> 1122;
error_code(cant_initialize_udf) -> 1123;
error_code(udf_no_paths) -> 1124;
error_code(udf_exists) -> 1125;
error_code(cant_open_library) -> 1126;
error_code(cant_find_dl_entry) -> 1127;
error_code(function_not_defined) -> 1128;
error_code(host_is_blocked) -> 1129;
error_code(host_not_privileged) -> 1130;
error_code(password_anonymous_user) -> 1131;
error_code(password_not_allowed) -> 1132;
error_code(password_no_match) -> 1133;
error_code(update_info) -> 1134;
error_code(cant_create_thread) -> 1135;
error_code(wrong_value_count_on_row) -> 1136;
error_code(cant_reopen_table) -> 1137;
error_code(invalid_use_of_null) -> 1138;
error_code(regexp_error) -> 1139;
error_code(mix_of_group_func_and_fields) -> 1140;
error_code(nonexisting_grant) -> 1141;
error_code(tableaccess_denied_error) -> 1142;
error_code(columnaccess_denied_error) -> 1143;
error_code(illegal_grant_for_table) -> 1144;
error_code(grant_wrong_host_or_user) -> 1145;
error_code(no_such_table) -> 1146;
error_code(nonexisting_table_grant) -> 1147;
error_code(not_allowed_command) -> 1148;
error_code(syntax_error) -> 1149;
error_code(delayed_cant_change_lock) -> 1150;
error_code(too_many_delayed_threads) -> 1151;
error_code(aborting_connection) -> 1152;
error_code(net_packet_too_large) -> 1153;
error_code(net_read_error_from_pipe) -> 1154;
error_code(net_fcntl_error) -> 1155;
error_code(net_packets_out_of_order) -> 1156;
error_code(net_uncompress_error) -> 1157;
error_code(net_read_error) -> 1158;
error_code(net_read_interrupted) -> 1159;
error_code(net_error_on_write) -> 1160;
error_code(net_write_interrupted) -> 1161;
error_code(too_long_string) -> 1162;
error_code(table_cant_handle_blob) -> 1163;
error_code(table_cant_handle_auto_increment) -> 1164;
error_code(delayed_insert_table_locked) -> 1165;
error_code(wrong_column_name) -> 1166;
error_code(wrong_key_column) -> 1167;
error_code(wrong_mrg_table) -> 1168;
error_code(dup_unique) -> 1169;
error_code(blob_key_without_length) -> 1170;
error_code(primary_cant_have_null) -> 1171;
error_code(too_many_rows) -> 1172;
error_code(requires_primary_key) -> 1173;
error_code(no_raid_compiled) -> 1174;
error_code(update_without_key_in_safe_mode) -> 1175;
error_code(key_does_not_exits) -> 1176;
error_code(check_no_such_table) -> 1177;
error_code(check_not_implemented) -> 1178;
error_code(cant_do_this_during_an_transaction) -> 1179;
error_code(error_during_commit) -> 1180;
error_code(error_during_rollback) -> 1181;
error_code(error_during_flush_logs) -> 1182;
error_code(error_during_checkpoint) -> 1183;
error_code(new_aborting_connection) -> 1184;
error_code(dump_not_implemented) -> 1185;
error_code(flush_master_binlog_closed) -> 1186;
error_code(index_rebuild) -> 1187;
error_code(master) -> 1188;
error_code(master_net_read) -> 1189;
error_code(master_net_write) -> 1190;
error_code(ft_matching_key_not_found) -> 1191;
error_code(lock_or_active_transaction) -> 1192;
error_code(unknown_system_variable) -> 1193;
error_code(crashed_on_usage) -> 1194;
error_code(crashed_on_repair) -> 1195;
error_code(warning_not_complete_rollback) -> 1196;
error_code(trans_cache_full) -> 1197;
error_code(slave_must_stop) -> 1198;
error_code(slave_not_running) -> 1199;
error_code(bad_slave) -> 1200;
error_code(master_info) -> 1201;
error_code(slave_thread) -> 1202;
error_code(too_many_user_connections) -> 1203;
error_code(set_constants_only) -> 1204;
error_code(lock_wait_timeout) -> 1205;
error_code(lock_table_full) -> 1206;
error_code(read_only_transaction) -> 1207;
error_code(drop_db_with_read_lock) -> 1208;
error_code(create_db_with_read_lock) -> 1209;
error_code(wrong_arguments) -> 1210;
error_code(no_permission_to_create_user) -> 1211;
error_code(union_tables_in_different_dir) -> 1212;
error_code(lock_deadlock) -> 1213;
error_code(table_cant_handle_ft) -> 1214;
error_code(cannot_add_foreign) -> 1215;
error_code(no_referenced_row) -> 1216;
error_code(row_is_referenced) -> 1217;
error_code(connect_to_master) -> 1218;
error_code(query_on_master) -> 1219;
error_code(error_when_executing_command) -> 1220;
error_code(wrong_usage) -> 1221;
error_code(wrong_number_of_columns_in_select) -> 1222;
error_code(cant_update_with_readlock) -> 1223;
error_code(mixing_not_allowed) -> 1224;
error_code(dup_argument) -> 1225;
error_code(user_limit_reached) -> 1226;
error_code(specific_access_denied_error) -> 1227;
error_code(local_variable) -> 1228;
error_code(global_variable) -> 1229;
error_code(no_default) -> 1230;
error_code(wrong_value_for_var) -> 1231;
error_code(wrong_type_for_var) -> 1232;
error_code(var_cant_be_read) -> 1233;
error_code(cant_use_option_here) -> 1234;
error_code(not_supported_yet) -> 1235;
error_code(master_fatal_error_reading_binlog) -> 1236;
error_code(slave_ignored_table) -> 1237;
error_code(incorrect_global_local_var) -> 1238;
error_code(wrong_fk_def) -> 1239;
error_code(key_ref_do_not_match_table_ref) -> 1240;
error_code(operand_columns) -> 1241;
error_code(subquery_no_1_row) -> 1242;
error_code(unknown_stmt_handler) -> 1243;
error_code(corrupt_help_db) -> 1244;
error_code(cyclic_reference) -> 1245;
error_code(auto_convert) -> 1246;
error_code(illegal_reference) -> 1247;
error_code(derived_must_have_alias) -> 1248;
error_code(select_reduced) -> 1249;
error_code(tablename_not_allowed_here) -> 1250;
error_code(not_supported_auth_mode) -> 1251;
error_code(spatial_cant_have_null) -> 1252;
error_code(collation_charset_mismatch) -> 1253;
error_code(slave_was_running) -> 1254;
error_code(slave_was_not_running) -> 1255;
error_code(too_big_for_uncompress) -> 1256;
error_code(zlib_z_mem_error) -> 1257;
error_code(zlib_z_buf_error) -> 1258;
error_code(zlib_z_data_error) -> 1259;
error_code(cut_value_group_concat) -> 1260;
error_code(warn_too_few_records) -> 1261;
error_code(warn_too_many_records) -> 1262;
error_code(warn_null_to_notnull) -> 1263;
error_code(warn_data_out_of_range) -> 1264;
error_code(n_data_truncated) -> 1265;
error_code(warn_using_other_handler) -> 1266;
error_code(cant_aggregate_2collations) -> 1267;
error_code(drop_user) -> 1268;
error_code(revoke_grants) -> 1269;
error_code(cant_aggregate_3collations) -> 1270;
error_code(cant_aggregate_ncollations) -> 1271;
error_code(variable_is_not_struct) -> 1272;
error_code(unknown_collation) -> 1273;
error_code(slave_ignored_ssl_params) -> 1274;
error_code(server_is_in_secure_auth_mode) -> 1275;
error_code(warn_field_resolved) -> 1276;
error_code(bad_slave_until_cond) -> 1277;
error_code(missing_skip_slave) -> 1278;
error_code(until_cond_ignored) -> 1279;
error_code(wrong_name_for_index) -> 1280;
error_code(wrong_name_for_catalog) -> 1281;
error_code(warn_qc_resize) -> 1282;
error_code(bad_ft_column) -> 1283;
error_code(unknown_key_cache) -> 1284;
error_code(warn_hostname_wont_work) -> 1285;
error_code(unknown_storage_engine) -> 1286;
error_code(unused_1) -> 1287;
error_code(non_updatable_table) -> 1288;
error_code(feature_disabled) -> 1289;
error_code(option_prevents_statement) -> 1290;
error_code(duplicated_value_in_type) -> 1291;
error_code(truncated_wrong_value) -> 1292;
error_code(too_much_auto_timestamp_cols) -> 1293;
error_code(invalid_on_update) -> 1294;
error_code(unsupported_ps) -> 1295;
error_code(get_errmsg) -> 1296;
error_code(get_temporary_errmsg) -> 1297;
error_code(unknown_time_zone) -> 1298;
error_code(warn_invalid_timestamp) -> 1299;
error_code(invalid_character_string) -> 1300;
error_code(warn_allowed_packet_overflowed) -> 1301;
error_code(conflicting_declarations) -> 1302;
error_code(sp_no_recursive_create) -> 1303;
error_code(sp_already_exists) -> 1304;
error_code(sp_does_not_exist) -> 1305;
error_code(sp_drop_failed) -> 1306;
error_code(sp_store_failed) -> 1307;
error_code(sp_lilabel_mismatch) -> 1308;
error_code(sp_label_redefine) -> 1309;
error_code(sp_label_mismatch) -> 1310;
error_code(sp_uninit_var) -> 1311;
error_code(sp_badselect) -> 1312;
error_code(sp_badreturn) -> 1313;
error_code(sp_badstatement) -> 1314;
error_code(update_log_deprecated_ignored) -> 1315;
error_code(update_log_deprecated_translated) -> 1316;
error_code(query_interrupted) -> 1317;
error_code(sp_wrong_no_of_args) -> 1318;
error_code(sp_cond_mismatch) -> 1319;
error_code(sp_noreturn) -> 1320;
error_code(sp_noreturnend) -> 1321;
error_code(sp_bad_cursor_query) -> 1322;
error_code(sp_bad_cursor_select) -> 1323;
error_code(sp_cursor_mismatch) -> 1324;
error_code(sp_cursor_already_open) -> 1325;
error_code(sp_cursor_not_open) -> 1326;
error_code(sp_undeclared_var) -> 1327;
error_code(sp_wrong_no_of_fetch_args) -> 1328;
error_code(sp_fetch_no_data) -> 1329;
error_code(sp_dup_param) -> 1330;
error_code(sp_dup_var) -> 1331;
error_code(sp_dup_cond) -> 1332;
error_code(sp_dup_curs) -> 1333;
error_code(sp_cant_alter) -> 1334;
error_code(sp_subselect_nyi) -> 1335;
error_code(stmt_not_allowed_in_sf_or_trg) -> 1336;
error_code(sp_varcond_after_curshndlr) -> 1337;
error_code(sp_cursor_after_handler) -> 1338;
error_code(sp_case_not_found) -> 1339;
error_code(fparser_too_big_file) -> 1340;
error_code(fparser_bad_header) -> 1341;
error_code(fparser_eof_in_comment) -> 1342;
error_code(fparser_error_in_parameter) -> 1343;
error_code(fparser_eof_in_unknown_parameter) -> 1344;
error_code(view_no_explain) -> 1345;
error_code(frm_unknown_type) -> 1346;
error_code(wrong_object) -> 1347;
error_code(nonupdateable_column) -> 1348;
error_code(view_select_derived) -> 1349;
error_code(view_select_clause) -> 1350;
error_code(view_select_variable) -> 1351;
error_code(view_select_tmptable) -> 1352;
error_code(view_wrong_list) -> 1353;
error_code(warn_view_merge) -> 1354;
error_code(warn_view_without_key) -> 1355;
error_code(view_invalid) -> 1356;
error_code(sp_no_drop_sp) -> 1357;
error_code(sp_goto_in_hndlr) -> 1358;
error_code(trg_already_exists) -> 1359;
error_code(trg_does_not_exist) -> 1360;
error_code(trg_on_view_or_temp_table) -> 1361;
error_code(trg_cant_change_row) -> 1362;
error_code(trg_no_such_row_in_trg) -> 1363;
error_code(no_default_for_field) -> 1364;
error_code(division_by_zero) -> 1365;
error_code(truncated_wrong_value_for_field) -> 1366;
error_code(illegal_value_for_type) -> 1367;
error_code(view_nonupd_check) -> 1368;
error_code(view_check_failed) -> 1369;
error_code(procaccess_denied_error) -> 1370;
error_code(relay_log_fail) -> 1371;
error_code(passwd_length) -> 1372;
error_code(unknown_target_binlog) -> 1373;
error_code(io_err_log_index_read) -> 1374;
error_code(binlog_purge_prohibited) -> 1375;
error_code(fseek_fail) -> 1376;
error_code(binlog_purge_fatal_err) -> 1377;
error_code(log_in_use) -> 1378;
error_code(log_purge_unknown_err) -> 1379;
error_code(relay_log_init) -> 1380;
error_code(no_binary_logging) -> 1381;
error_code(reserved_syntax) -> 1382;
error_code(wsas_failed) -> 1383;
error_code(diff_groups_proc) -> 1384;
error_code(no_group_for_proc) -> 1385;
error_code(order_with_proc) -> 1386;
error_code(logging_prohibit_changing_of) -> 1387;
error_code(no_file_mapping) -> 1388;
error_code(wrong_magic) -> 1389;
error_code(ps_many_param) -> 1390;
error_code(key_part_0) -> 1391;
error_code(view_checksum) -> 1392;
error_code(view_multiupdate) -> 1393;
error_code(view_no_insert_field_list) -> 1394;
error_code(view_delete_merge_view) -> 1395;
error_code(cannot_user) -> 1396;
error_code(xaer_nota) -> 1397;
error_code(xaer_inval) -> 1398;
error_code(xaer_rmfail) -> 1399;
error_code(xaer_outside) -> 1400;
error_code(xaer_rmerr) -> 1401;
error_code(xa_rbrollback) -> 1402;
error_code(nonexisting_proc_grant) -> 1403;
error_code(proc_auto_grant_fail) -> 1404;
error_code(proc_auto_revoke_fail) -> 1405;
error_code(data_too_long) -> 1406;
error_code(sp_bad_sqlstate) -> 1407;
error_code(startup) -> 1408;
error_code(load_from_fixed_size_rows_to_var) -> 1409;
error_code(cant_create_user_with_grant) -> 1410;
error_code(wrong_value_for_type) -> 1411;
error_code(table_def_changed) -> 1412;
error_code(sp_dup_handler) -> 1413;
error_code(sp_not_var_arg) -> 1414;
error_code(sp_no_retset) -> 1415;
error_code(cant_create_geometry_object) -> 1416;
error_code(failed_routine_break_binlog) -> 1417;
error_code(binlog_unsafe_routine) -> 1418;
error_code(binlog_create_routine_need_super) -> 1419;
error_code(exec_stmt_with_open_cursor) -> 1420;
error_code(stmt_has_no_open_cursor) -> 1421;
error_code(commit_not_allowed_in_sf_or_trg) -> 1422;
error_code(no_default_for_view_field) -> 1423;
error_code(sp_no_recursion) -> 1424;
error_code(too_big_scale) -> 1425;
error_code(too_big_precision) -> 1426;
error_code(m_bigger_than_d) -> 1427;
error_code(wrong_lock_of_system_table) -> 1428;
error_code(connect_to_foreign_data_source) -> 1429;
error_code(query_on_foreign_data_source) -> 1430;
error_code(foreign_data_source_doesnt_exist) -> 1431;
error_code(foreign_data_string_invalid_cant_create) -> 1432;
error_code(foreign_data_string_invalid) -> 1433;
error_code(cant_create_federated_table) -> 1434;
error_code(trg_in_wrong_schema) -> 1435;
error_code(stack_overrun_need_more) -> 1436;
error_code(too_long_body) -> 1437;
error_code(warn_cant_drop_default_keycache) -> 1438;
error_code(too_big_displaywidth) -> 1439;
error_code(xaer_dupid) -> 1440;
error_code(datetime_function_overflow) -> 1441;
error_code(cant_update_used_table_in_sf_or_trg) -> 1442;
error_code(view_prevent_update) -> 1443;
error_code(ps_no_recursion) -> 1444;
error_code(sp_cant_set_autocommit) -> 1445;
error_code(malformed_definer) -> 1446;
error_code(view_frm_no_user) -> 1447;
error_code(view_other_user) -> 1448;
error_code(no_such_user) -> 1449;
error_code(forbid_schema_change) -> 1450;
error_code(row_is_referenced_2) -> 1451;
error_code(no_referenced_row_2) -> 1452;
error_code(sp_bad_var_shadow) -> 1453;
error_code(trg_no_definer) -> 1454;
error_code(old_file_format) -> 1455;
error_code(sp_recursion_limit) -> 1456;
error_code(sp_proc_table_corrupt) -> 1457;
error_code(sp_wrong_name) -> 1458;
error_code(table_needs_upgrade) -> 1459;
error_code(sp_no_aggregate) -> 1460;
error_code(max_prepared_stmt_count_reached) -> 1461;
error_code(view_recursive) -> 1462;
error_code(non_grouping_field_used) -> 1463;
error_code(table_cant_handle_spkeys) -> 1464;
error_code(illegal_ha_create_option) -> 1465;
error_code(partition_requires_values_error) -> 1466;
error_code(partition_wrong_values_error) -> 1467;
error_code(partition_maxvalue_error) -> 1468;
error_code(partition_subpartition_error) -> 1469;
error_code(partition_subpart_mix_error) -> 1470;
error_code(partition_wrong_no_part_error) -> 1471;
error_code(partition_wrong_no_subpart_error) -> 1472;
error_code(const_expr_in_partition_func_error) -> 1473;
error_code(no_const_expr_in_range_or_list_error) -> 1474;
error_code(field_not_found_part_error) -> 1475;
error_code(list_of_fields_only_in_hash_error) -> 1476;
error_code(inconsistent_partition_info_error) -> 1477;
error_code(partition_func_not_allowed_error) -> 1478;
error_code(partitions_must_be_defined_error) -> 1479;
error_code(range_not_increasing_error) -> 1480;
error_code(inconsistent_type_of_functions_error) -> 1481;
error_code(multiple_def_const_in_list_part_error) -> 1482;
error_code(partition_entry_error) -> 1483;
error_code(mix_handler_error) -> 1484;
error_code(partition_not_defined_error) -> 1485;
error_code(too_many_partitions_error) -> 1486;
error_code(subpartition_error) -> 1487;
error_code(cant_create_handler_file) -> 1488;
error_code(blob_field_in_part_func_error) -> 1489;
error_code(unique_key_need_all_fields_in_pf) -> 1490;
error_code(no_parts_error) -> 1491;
error_code(partition_mgmt_on_nonpartitioned) -> 1492;
error_code(foreign_key_on_partitioned) -> 1493;
error_code(drop_partition_non_existent) -> 1494;
error_code(drop_last_partition) -> 1495;
error_code(coalesce_only_on_hash_partition) -> 1496;
error_code(reorg_hash_only_on_same_no) -> 1497;
error_code(reorg_no_param_error) -> 1498;
error_code(only_on_range_list_partition) -> 1499;
error_code(add_partition_subpart_error) -> 1500;
error_code(add_partition_no_new_partition) -> 1501;
error_code(coalesce_partition_no_partition) -> 1502;
error_code(reorg_partition_not_exist) -> 1503;
error_code(same_name_partition) -> 1504;
error_code(no_binlog_error) -> 1505;
error_code(consecutive_reorg_partitions) -> 1506;
error_code(reorg_outside_range) -> 1507;
error_code(partition_function_failure) -> 1508;
error_code(part_state_error) -> 1509;
error_code(limited_part_range) -> 1510;
error_code(plugin_is_not_loaded) -> 1511;
error_code(wrong_value) -> 1512;
error_code(no_partition_for_given_value) -> 1513;
error_code(filegroup_option_only_once) -> 1514;
error_code(create_filegroup_failed) -> 1515;
error_code(drop_filegroup_failed) -> 1516;
error_code(tablespace_auto_extend_error) -> 1517;
error_code(wrong_size_number) -> 1518;
error_code(size_overflow_error) -> 1519;
error_code(alter_filegroup_failed) -> 1520;
error_code(binlog_row_logging_failed) -> 1521;
error_code(binlog_row_wrong_table_def) -> 1522;
error_code(binlog_row_rbr_to_sbr) -> 1523;
error_code(event_already_exists) -> 1524;
error_code(event_store_failed) -> 1525;
error_code(event_does_not_exist) -> 1526;
error_code(event_cant_alter) -> 1527;
error_code(event_drop_failed) -> 1528;
error_code(event_interval_not_positive_or_too_big) -> 1529;
error_code(event_ends_before_starts) -> 1530;
error_code(event_exec_time_in_the_past) -> 1531;
error_code(event_open_table_failed) -> 1532;
error_code(event_neither_m_expr_nor_m_at) -> 1533;
error_code(col_count_doesnt_match_corrupted) -> 1534;
error_code(cannot_load_from_table) -> 1535;
error_code(event_cannot_delete) -> 1536;
error_code(event_compile_error) -> 1537;
error_code(event_same_name) -> 1538;
error_code(event_data_too_long) -> 1539;
error_code(drop_index_fk) -> 1540;
error_code(warn_deprecated_syntax) -> 1541;
error_code(cant_write_lock_log_table) -> 1542;
error_code(cant_read_lock_log_table) -> 1543;
error_code(foreign_duplicate_key) -> 1544;
error_code(col_count_doesnt_match_please_update) -> 1545;
error_code(removed_spaces) -> 1546;
error_code(temp_table_prevents_switch_out_of_rbr) -> 1547;
error_code(stored_function_prevents_switch_binlog_format) -> 1548;
error_code(ndb_cant_switch_binlog_format) -> 1549;
error_code(partition_no_temporary) -> 1550;
error_code(partition_const_domain_error) -> 1551;
error_code(partition_function_is_not_allowed) -> 1552;
error_code(ddl_log_error) -> 1553;
error_code(null_in_values_less_than) -> 1554;
error_code(wrong_partition_name) -> 1555;
error_code(cant_change_tx_isolation) -> 1556;
error_code(dup_entry_autoincrement_case) -> 1557;
error_code(event_modify_queue_error) -> 1558;
error_code(event_set_var_error) -> 1559;
error_code(partition_merge_error) -> 1560;
error_code(cant_activate_log) -> 1561;
error_code(rbr_not_available) -> 1562;
error_code(no_triggers_on_system_schema) -> 1563;
error_code(cant_alter_log_table) -> 1564;
error_code(bad_log_engine) -> 1565;
error_code(cant_drop_log_table) -> 1566.

errors() ->
    [ hashchk
     ,nisamchk
     ,no
     ,yes
     ,cant_create_file
     ,cant_create_table
     ,cant_create_db
     ,db_create_exists
     ,db_drop_exists
     ,db_drop_delete
     ,db_drop_rmdir
     ,cant_delete_file
     ,cant_find_system_rec
     ,cant_get_stat
     ,cant_get_wd
     ,cant_lock
     ,cant_open_file
     ,file_not_found
     ,cant_read_dir
     ,cant_set_wd
     ,checkread
     ,disk_full
     ,dup_key
     ,error_on_close
     ,error_on_read
     ,error_on_rename
     ,error_on_write
     ,file_used
     ,filsort_abort
     ,form_not_found
     ,get_errno
     ,illegal_ha
     ,key_not_found
     ,not_form_file
     ,not_keyfile
     ,old_keyfile
     ,open_as_readonly
     ,outofmemory
     ,out_of_sortmemory
     ,unexpected_eof
     ,con_count_error
     ,out_of_resources
     ,bad_host_error
     ,handshake_error
     ,dbaccess_denied_error
     ,access_denied_error
     ,no_db_error
     ,unknown_com_error
     ,bad_null_error
     ,bad_db_error
     ,table_exists_error
     ,bad_table_error
     ,non_uniq_error
     ,server_shutdown
     ,bad_field_error
     ,wrong_field_with_group
     ,wrong_group_field
     ,wrong_sum_select
     ,wrong_value_count
     ,too_long_ident
     ,dup_fieldname
     ,dup_keyname
     ,dup_entry
     ,wrong_field_spec
     ,parse_error
     ,empty_query
     ,nonuniq_table
     ,invalid_default
     ,multiple_pri_key
     ,too_many_keys
     ,too_many_key_parts
     ,too_long_key
     ,key_column_does_not_exits
     ,blob_used_as_key
     ,too_big_fieldlength
     ,wrong_auto_key
     ,ready
     ,normal_shutdown
     ,got_signal
     ,shutdown_complete
     ,forcing_close
     ,ipsock_error
     ,no_such_index
     ,wrong_field_terminators
     ,blobs_and_no_terminated
     ,textfile_not_readable
     ,file_exists_error
     ,load_info
     ,alter_info
     ,wrong_sub_key
     ,cant_remove_all_fields
     ,cant_drop_field_or_key
     ,insert_info
     ,update_table_used
     ,no_such_thread
     ,kill_denied_error
     ,no_tables_used
     ,too_big_set
     ,no_unique_logfile
     ,table_not_locked_for_write
     ,table_not_locked
     ,blob_cant_have_default
     ,wrong_db_name
     ,wrong_table_name
     ,too_big_select
     ,unknown_error
     ,unknown_procedure
     ,wrong_paramcount_to_procedure
     ,wrong_parameters_to_procedure
     ,unknown_table
     ,field_specified_twice
     ,invalid_group_func_use
     ,unsupported_extension
     ,table_must_have_columns
     ,record_file_full
     ,unknown_character_set
     ,too_many_tables
     ,too_many_fields
     ,too_big_rowsize
     ,stack_overrun
     ,wrong_outer_join
     ,null_column_in_index
     ,cant_find_udf
     ,cant_initialize_udf
     ,udf_no_paths
     ,udf_exists
     ,cant_open_library
     ,cant_find_dl_entry
     ,function_not_defined
     ,host_is_blocked
     ,host_not_privileged
     ,password_anonymous_user
     ,password_not_allowed
     ,password_no_match
     ,update_info
     ,cant_create_thread
     ,wrong_value_count_on_row
     ,cant_reopen_table
     ,invalid_use_of_null
     ,regexp_error
     ,mix_of_group_func_and_fields
     ,nonexisting_grant
     ,tableaccess_denied_error
     ,columnaccess_denied_error
     ,illegal_grant_for_table
     ,grant_wrong_host_or_user
     ,no_such_table
     ,nonexisting_table_grant
     ,not_allowed_command
     ,syntax_error
     ,delayed_cant_change_lock
     ,too_many_delayed_threads
     ,aborting_connection
     ,net_packet_too_large
     ,net_read_error_from_pipe
     ,net_fcntl_error
     ,net_packets_out_of_order
     ,net_uncompress_error
     ,net_read_error
     ,net_read_interrupted
     ,net_error_on_write
     ,net_write_interrupted
     ,too_long_string
     ,table_cant_handle_blob
     ,table_cant_handle_auto_increment
     ,delayed_insert_table_locked
     ,wrong_column_name
     ,wrong_key_column
     ,wrong_mrg_table
     ,dup_unique
     ,blob_key_without_length
     ,primary_cant_have_null
     ,too_many_rows
     ,requires_primary_key
     ,no_raid_compiled
     ,update_without_key_in_safe_mode
     ,key_does_not_exits
     ,check_no_such_table
     ,check_not_implemented
     ,cant_do_this_during_an_transaction
     ,error_during_commit
     ,error_during_rollback
     ,error_during_flush_logs
     ,error_during_checkpoint
     ,new_aborting_connection
     ,dump_not_implemented
     ,flush_master_binlog_closed
     ,index_rebuild
     ,master
     ,master_net_read
     ,master_net_write
     ,ft_matching_key_not_found
     ,lock_or_active_transaction
     ,unknown_system_variable
     ,crashed_on_usage
     ,crashed_on_repair
     ,warning_not_complete_rollback
     ,trans_cache_full
     ,slave_must_stop
     ,slave_not_running
     ,bad_slave
     ,master_info
     ,slave_thread
     ,too_many_user_connections
     ,set_constants_only
     ,lock_wait_timeout
     ,lock_table_full
     ,read_only_transaction
     ,drop_db_with_read_lock
     ,create_db_with_read_lock
     ,wrong_arguments
     ,no_permission_to_create_user
     ,union_tables_in_different_dir
     ,lock_deadlock
     ,table_cant_handle_ft
     ,cannot_add_foreign
     ,no_referenced_row
     ,row_is_referenced
     ,connect_to_master
     ,query_on_master
     ,error_when_executing_command
     ,wrong_usage
     ,wrong_number_of_columns_in_select
     ,cant_update_with_readlock
     ,mixing_not_allowed
     ,dup_argument
     ,user_limit_reached
     ,specific_access_denied_error
     ,local_variable
     ,global_variable
     ,no_default
     ,wrong_value_for_var
     ,wrong_type_for_var
     ,var_cant_be_read
     ,cant_use_option_here
     ,not_supported_yet
     ,master_fatal_error_reading_binlog
     ,slave_ignored_table
     ,incorrect_global_local_var
     ,wrong_fk_def
     ,key_ref_do_not_match_table_ref
     ,operand_columns
     ,subquery_no_1_row
     ,unknown_stmt_handler
     ,corrupt_help_db
     ,cyclic_reference
     ,auto_convert
     ,illegal_reference
     ,derived_must_have_alias
     ,select_reduced
     ,tablename_not_allowed_here
     ,not_supported_auth_mode
     ,spatial_cant_have_null
     ,collation_charset_mismatch
     ,slave_was_running
     ,slave_was_not_running
     ,too_big_for_uncompress
     ,zlib_z_mem_error
     ,zlib_z_buf_error
     ,zlib_z_data_error
     ,cut_value_group_concat
     ,warn_too_few_records
     ,warn_too_many_records
     ,warn_null_to_notnull
     ,warn_data_out_of_range
     ,n_data_truncated
     ,warn_using_other_handler
     ,cant_aggregate_2collations
     ,drop_user
     ,revoke_grants
     ,cant_aggregate_3collations
     ,cant_aggregate_ncollations
     ,variable_is_not_struct
     ,unknown_collation
     ,slave_ignored_ssl_params
     ,server_is_in_secure_auth_mode
     ,warn_field_resolved
     ,bad_slave_until_cond
     ,missing_skip_slave
     ,until_cond_ignored
     ,wrong_name_for_index
     ,wrong_name_for_catalog
     ,warn_qc_resize
     ,bad_ft_column
     ,unknown_key_cache
     ,warn_hostname_wont_work
     ,unknown_storage_engine
     ,unused_1
     ,non_updatable_table
     ,feature_disabled
     ,option_prevents_statement
     ,duplicated_value_in_type
     ,truncated_wrong_value
     ,too_much_auto_timestamp_cols
     ,invalid_on_update
     ,unsupported_ps
     ,get_errmsg
     ,get_temporary_errmsg
     ,unknown_time_zone
     ,warn_invalid_timestamp
     ,invalid_character_string
     ,warn_allowed_packet_overflowed
     ,conflicting_declarations
     ,sp_no_recursive_create
     ,sp_already_exists
     ,sp_does_not_exist
     ,sp_drop_failed
     ,sp_store_failed
     ,sp_lilabel_mismatch
     ,sp_label_redefine
     ,sp_label_mismatch
     ,sp_uninit_var
     ,sp_badselect
     ,sp_badreturn
     ,sp_badstatement
     ,update_log_deprecated_ignored
     ,update_log_deprecated_translated
     ,query_interrupted
     ,sp_wrong_no_of_args
     ,sp_cond_mismatch
     ,sp_noreturn
     ,sp_noreturnend
     ,sp_bad_cursor_query
     ,sp_bad_cursor_select
     ,sp_cursor_mismatch
     ,sp_cursor_already_open
     ,sp_cursor_not_open
     ,sp_undeclared_var
     ,sp_wrong_no_of_fetch_args
     ,sp_fetch_no_data
     ,sp_dup_param
     ,sp_dup_var
     ,sp_dup_cond
     ,sp_dup_curs
     ,sp_cant_alter
     ,sp_subselect_nyi
     ,stmt_not_allowed_in_sf_or_trg
     ,sp_varcond_after_curshndlr
     ,sp_cursor_after_handler
     ,sp_case_not_found
     ,fparser_too_big_file
     ,fparser_bad_header
     ,fparser_eof_in_comment
     ,fparser_error_in_parameter
     ,fparser_eof_in_unknown_parameter
     ,view_no_explain
     ,frm_unknown_type
     ,wrong_object
     ,nonupdateable_column
     ,view_select_derived
     ,view_select_clause
     ,view_select_variable
     ,view_select_tmptable
     ,view_wrong_list
     ,warn_view_merge
     ,warn_view_without_key
     ,view_invalid
     ,sp_no_drop_sp
     ,sp_goto_in_hndlr
     ,trg_already_exists
     ,trg_does_not_exist
     ,trg_on_view_or_temp_table
     ,trg_cant_change_row
     ,trg_no_such_row_in_trg
     ,no_default_for_field
     ,division_by_zero
     ,truncated_wrong_value_for_field
     ,illegal_value_for_type
     ,view_nonupd_check
     ,view_check_failed
     ,procaccess_denied_error
     ,relay_log_fail
     ,passwd_length
     ,unknown_target_binlog
     ,io_err_log_index_read
     ,binlog_purge_prohibited
     ,fseek_fail
     ,binlog_purge_fatal_err
     ,log_in_use
     ,log_purge_unknown_err
     ,relay_log_init
     ,no_binary_logging
     ,reserved_syntax
     ,wsas_failed
     ,diff_groups_proc
     ,no_group_for_proc
     ,order_with_proc
     ,logging_prohibit_changing_of
     ,no_file_mapping
     ,wrong_magic
     ,ps_many_param
     ,key_part_0
     ,view_checksum
     ,view_multiupdate
     ,view_no_insert_field_list
     ,view_delete_merge_view
     ,cannot_user
     ,xaer_nota
     ,xaer_inval
     ,xaer_rmfail
     ,xaer_outside
     ,xaer_rmerr
     ,xa_rbrollback
     ,nonexisting_proc_grant
     ,proc_auto_grant_fail
     ,proc_auto_revoke_fail
     ,data_too_long
     ,sp_bad_sqlstate
     ,startup
     ,load_from_fixed_size_rows_to_var
     ,cant_create_user_with_grant
     ,wrong_value_for_type
     ,table_def_changed
     ,sp_dup_handler
     ,sp_not_var_arg
     ,sp_no_retset
     ,cant_create_geometry_object
     ,failed_routine_break_binlog
     ,binlog_unsafe_routine
     ,binlog_create_routine_need_super
     ,exec_stmt_with_open_cursor
     ,stmt_has_no_open_cursor
     ,commit_not_allowed_in_sf_or_trg
     ,no_default_for_view_field
     ,sp_no_recursion
     ,too_big_scale
     ,too_big_precision
     ,m_bigger_than_d
     ,wrong_lock_of_system_table
     ,connect_to_foreign_data_source
     ,query_on_foreign_data_source
     ,foreign_data_source_doesnt_exist
     ,foreign_data_string_invalid_cant_create
     ,foreign_data_string_invalid
     ,cant_create_federated_table
     ,trg_in_wrong_schema
     ,stack_overrun_need_more
     ,too_long_body
     ,warn_cant_drop_default_keycache
     ,too_big_displaywidth
     ,xaer_dupid
     ,datetime_function_overflow
     ,cant_update_used_table_in_sf_or_trg
     ,view_prevent_update
     ,ps_no_recursion
     ,sp_cant_set_autocommit
     ,malformed_definer
     ,view_frm_no_user
     ,view_other_user
     ,no_such_user
     ,forbid_schema_change
     ,row_is_referenced_2
     ,no_referenced_row_2
     ,sp_bad_var_shadow
     ,trg_no_definer
     ,old_file_format
     ,sp_recursion_limit
     ,sp_proc_table_corrupt
     ,sp_wrong_name
     ,table_needs_upgrade
     ,sp_no_aggregate
     ,max_prepared_stmt_count_reached
     ,view_recursive
     ,non_grouping_field_used
     ,table_cant_handle_spkeys
     ,illegal_ha_create_option
     ,partition_requires_values_error
     ,partition_wrong_values_error
     ,partition_maxvalue_error
     ,partition_subpartition_error
     ,partition_subpart_mix_error
     ,partition_wrong_no_part_error
     ,partition_wrong_no_subpart_error
     ,const_expr_in_partition_func_error
     ,no_const_expr_in_range_or_list_error
     ,field_not_found_part_error
     ,list_of_fields_only_in_hash_error
     ,inconsistent_partition_info_error
     ,partition_func_not_allowed_error
     ,partitions_must_be_defined_error
     ,range_not_increasing_error
     ,inconsistent_type_of_functions_error
     ,multiple_def_const_in_list_part_error
     ,partition_entry_error
     ,mix_handler_error
     ,partition_not_defined_error
     ,too_many_partitions_error
     ,subpartition_error
     ,cant_create_handler_file
     ,blob_field_in_part_func_error
     ,unique_key_need_all_fields_in_pf
     ,no_parts_error
     ,partition_mgmt_on_nonpartitioned
     ,foreign_key_on_partitioned
     ,drop_partition_non_existent
     ,drop_last_partition
     ,coalesce_only_on_hash_partition
     ,reorg_hash_only_on_same_no
     ,reorg_no_param_error
     ,only_on_range_list_partition
     ,add_partition_subpart_error
     ,add_partition_no_new_partition
     ,coalesce_partition_no_partition
     ,reorg_partition_not_exist
     ,same_name_partition
     ,no_binlog_error
     ,consecutive_reorg_partitions
     ,reorg_outside_range
     ,partition_function_failure
     ,part_state_error
     ,limited_part_range
     ,plugin_is_not_loaded
     ,wrong_value
     ,no_partition_for_given_value
     ,filegroup_option_only_once
     ,create_filegroup_failed
     ,drop_filegroup_failed
     ,tablespace_auto_extend_error
     ,wrong_size_number
     ,size_overflow_error
     ,alter_filegroup_failed
     ,binlog_row_logging_failed
     ,binlog_row_wrong_table_def
     ,binlog_row_rbr_to_sbr
     ,event_already_exists
     ,event_store_failed
     ,event_does_not_exist
     ,event_cant_alter
     ,event_drop_failed
     ,event_interval_not_positive_or_too_big
     ,event_ends_before_starts
     ,event_exec_time_in_the_past
     ,event_open_table_failed
     ,event_neither_m_expr_nor_m_at
     ,col_count_doesnt_match_corrupted
     ,cannot_load_from_table
     ,event_cannot_delete
     ,event_compile_error
     ,event_same_name
     ,event_data_too_long
     ,drop_index_fk
     ,warn_deprecated_syntax
     ,cant_write_lock_log_table
     ,cant_read_lock_log_table
     ,foreign_duplicate_key
     ,col_count_doesnt_match_please_update
     ,removed_spaces
     ,temp_table_prevents_switch_out_of_rbr
     ,stored_function_prevents_switch_binlog_format
     ,ndb_cant_switch_binlog_format
     ,partition_no_temporary
     ,partition_const_domain_error
     ,partition_function_is_not_allowed
     ,ddl_log_error
     ,null_in_values_less_than
     ,wrong_partition_name
     ,cant_change_tx_isolation
     ,dup_entry_autoincrement_case
     ,event_modify_queue_error
     ,event_set_var_error
     ,partition_merge_error
     ,cant_activate_log
     ,rbr_not_available
     ,no_triggers_on_system_schema
     ,cant_alter_log_table
     ,bad_log_engine
     ,cant_drop_log_table].    

%%====================================================================
%% Internal functions
%%====================================================================
