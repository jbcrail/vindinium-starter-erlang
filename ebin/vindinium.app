{application, vindinium,
  [{description, "Erlang library for Vindinium"},
   {vsn, "0.0.1"},
   {modules, [vindinium,
              vindinium_state,
              vindinium_board,
              vindinium_bot,
              random_bot,
              fighter_bot,
              greedy_bot,
              slow_bot]},
   {registered, []},
   {applications, [kernel, stdlib, sasl]},
   {env, []}
  ]}.
