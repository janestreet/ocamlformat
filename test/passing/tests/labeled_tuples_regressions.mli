(* https://github.com/janestreet/ocamlformat/pull/49 *)
val f
  :  foo:int * very_long_type_name_so_we_get_multiple_lines
  -> (* cmt *)
     bar:('long_type_var_1, 'long_type_var_2) Long_module_name.t
  -> additional_somewhat_long_type_name