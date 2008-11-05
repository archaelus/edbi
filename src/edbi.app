{application, edbi,
 [{description, "Erlang Databace Independant Interface"}
  ,{vsn, "0.1"}
  ,{applications, [kernel, stdlib]}
  ,{mod, {edbi_app, []}}
  ,{env, [{pools, []}]}
  ,{modules, [edbi
             ,edbi_app
             ,edbi_driver
             ,edbi_dummy
             ,edbi_pool
             ,edbi_sup
             ,mysql
             ,mysql_auth
             ,mysql_conn
             ,mysql_recv
             ,pgsql
             ,pgsql_proto
             ,pgsql_tcp
             ,pgsql_util
             ]}
  ,{registered, [edbi_sup]}
 ]}.
