{application, ihatevans,
 [{description, "ihatevans"},
  {vsn, "0.01"},
  {modules, [
    ihatevans,
    ihatevans_app,
    ihatevans_sup,
    ihatevans_web,
    ihatevans_deps
  ]},
  {registered, []},
  {mod, {ihatevans_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
