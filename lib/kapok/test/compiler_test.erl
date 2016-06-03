-module(compiler_test).
-include_lib("eunit/include/eunit.hrl").

erl_to_abstract_format(Contents) ->
  case erl_scan:string(Contents) of
    {ok, Tokens, _EndLocation} ->
      {ok, ExprList} = erl_parse:parse_exprs(Tokens),
      ExprList;
    {error, ErrorInfo, ErrorLocation}->
      ?assert(false),
      throw({"scan error, location: ~w, error: ~s~n", [ErrorLocation, ErrorInfo]})
  end.

eval_erlang_expr(Expr) ->
  [Parsed] = erl_to_abstract_format(Expr),
  io:format("~nerlang exprs: ~w~n", [Parsed]),
  {value, Value, _NewBindings} = erl_eval:expr(Parsed, []),
  Value.

eval_kapok_expr(Expr) ->
  File = <<"compiler_test.erl">>,
  {Value, _Env} = kapok_compiler:string(Expr, File),
  Value.


local_call_test() ->
  Output1 = eval_erlang_expr("self()."),
  Output2 = eval_kapok_expr("(self)"),
  ?assertEqual(Output1, Output2).

remote_call_test() ->
  Output1 = eval_erlang_expr("erlang:self()."),
  Output2 = eval_kapok_expr("(erlang.self)"),
  ?assertEqual(Output1, Output2).

list_test() ->
  Output1 = eval_erlang_expr("[1 | [2]]."),
  Output2 = eval_erlang_expr("[1, 2]."),
  Output3 = eval_kapok_expr("(1 2)"),
  Output4 = eval_kapok_expr("[1 2]"),
  ?assertEqual(Output3, Output1),
  ?assertEqual(Output3, Output2),
  ?assertEqual(Output4, Output3).

binary_test() ->
  Output1 = eval_erlang_expr("<<256:8/big-unsigned-integer-unit:1>>."),
  Output2 = eval_kapok_expr("<<(256 (:size 8) :big :unsigned :integer (:unit 1))>>"),
  ?assertEqual(Output1, Output2).

