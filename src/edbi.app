{application, edbi,
 [{description, "Erlang Databace Independant Interface"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl]},
  {mod, {edbi_app, []}},
  {env, [{pools, []}]},
  {modules, [edbi_app, edbi_sup]},
  {registered, [edbi_sup]}
 ]}.
