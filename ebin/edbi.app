{application, skel,
 [{description, "Erlang Skeletons"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl]},
  {mod, {skel_app, []}},
  {modules, [skel_app, skel_sup]},
  {registered, [skel_sup, skel_server]}
 ]}.
