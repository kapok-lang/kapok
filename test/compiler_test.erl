-module(compiler_test).
-include_lib("eunit/include/eunit.hrl").

erl_to_abstract_format(Contents, Type) ->
  case erl_scan:string(Contents) of
    {ok, Tokens, _EndLocation} ->
      case Type of
        term ->
          {ok, Term} = erl_parse:parse_term(Tokens),
          Term;
        exprs ->
          {ok, ExprList} = erl_parse:parse_exprs(Tokens),
          ExprList;
        form ->
          {ok, Form} = erl_parse:parse_form(Tokens),
          Form;
        _ ->
          throw({"invalid type of erl tokens: ~s~n", [Type]})
      end;
    {error, ErrorInfo, ErrorLocation}->
      ?assert(false),
      throw({"scan error, location: ~w, error: ~s~n", [ErrorLocation, ErrorInfo]})
  end.

eval_erlang_exprs(Exprs) ->
  Parsed = erl_to_abstract_format(Exprs, exprs),
  io:format("~nerlang exprs: ~w~n", [Parsed]),
  {value, Value, _NewBindings} = erl_eval:exprs(Parsed, []),
  Value.

eval_kapok_exprs(Exprs) ->
  Line = 1,
  File = <<"file">>,
  Ast = kapok_compiler:'string_to_ast!'(Exprs, Line, File, []),
  Env = kapok_env:env_for_eval([{line, Line}, {file, File}]),
  {Erl, NewEnv, _NewScope} = kapok_compiler:ast_to_abstract_format(Ast, Env),
  #{vars := Vars} = NewEnv,
  {value, Value, _NewBindings} = erl_eval:exprs(Erl, Vars),
  Value.

local_call_test() ->
  Output1 = eval_erlang_exprs("self()."),
  Output2 = eval_kapok_exprs("(self)"),
  ?assertEqual(Output1, Output2).

remote_call_test() ->
  Output1 = eval_erlang_exprs("erlang:self()."),
  Output2 = eval_kapok_exprs("(erlang.self)"),
  ?assertEqual(Output1, Output2).

list_test() ->
  Output1 = eval_erlang_exprs("[1 | [2]]."),
  Output2 = eval_erlang_exprs("[1, 2]."),
  Output3 = eval_kapok_exprs("(1 2)"),
  Output4 = eval_kapok_exprs("[1 2]"),
  ?assertEqual(Output3, Output1),
  ?assertEqual(Output3, Output2),
  ?assertEqual(Output4, Output3).

binary_test() ->
  Output1 = eval_erlang_exprs("<<256:8/big-unsigned-integer-unit:1>>."),
  Output2 = eval_kapok_exprs("<<(256 (:size 8) :big :unsigned :integer (:unit 1))>>"),
  ?assertEqual(Output1, Output2).

