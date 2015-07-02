{ application, cowboy2_sample,
 [ { description, "Example usage of Cowboy 2.0." },
   { vsn, "0.1.0" },
   { modules, [] },
   { registered, [] },
   { applications, [ kernel, stdlib, sasl, cowboy ] },
   { env, [] },
   { mod, { cowboy2_sample_app, [] } }
 ]
}.
