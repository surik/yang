%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
-module(yang_json).
-compile(export_all).

-define(fd(Fl, A, B), Fl when A =< X, X =< B).

to_json_type(X, {type,_,<<"decimal64">>,I} = T) when is_float(X) ->
    F = case lists:keyfind('fraction-digits', 1, I) of
	    false -> 18;
	    {_,_,F1,_} -> list_to_integer(binary_to_list(F1))
	end,
    case F of
	?fd(1,-922337203685477580.8, 922337203685477580.7) -> ok;
	?fd(2, -92233720368547758.08, 92233720368547758.07) -> ok;
	?fd(3, -9223372036854775.808, 9223372036854775.807) -> ok;
	?fd(4, -922337203685477.5808, 922337203685477.5807) -> ok;
	?fd(5, -92233720368547.75808, 92233720368547.75807) -> ok;
	?fd(6, -9223372036854.775808, 9223372036854.775807) -> ok;
	?fd(7, -922337203685.4775808, 922337203685.4775807) -> ok;
	?fd(8, -92233720368.54775808, 92233720368.54775807) -> ok;
	?fd(9, -9223372036.854775808, 9223372036.854775807) -> ok;
	?fd(10, -922337203.6854775808, 922337203.6854775807) -> ok;
	?fd(11, -92233720.36854775808, 92233720.36854775807) -> ok;
	?fd(12, -9223372.036854775808, 9223372.036854775807) -> ok;
	?fd(13, -922337.2036854775808, 922337.2036854775807) -> ok;
	?fd(14, -92233.72036854775808, 92233.72036854775807) -> ok;
	?fd(15, -9223.372036854775808, 9223.372036854775807) -> ok;
	?fd(16, -922.3372036854775808, 922.3372036854775807) -> ok;
	?fd(17, -92.23372036854775808, 92.23372036854775807) -> ok;
	?fd(18, -9.223372036854775808, 9.223372036854775807) -> ok;
	_ -> error({type_error, [X, T]})
    end,
    list_to_binary(io_lib:fwrite("~." ++ integer_to_list(F) ++ "f", [X]));
to_json_type(X, {type,_,<<"int", _/binary>>,_}) when is_integer(X) ->
    %% list_to_binary(integer_to_list(X));
    X;
to_json_type(X, {type,_,<<"string">>,_}) when is_atom(X) ->
    atom_to_binary(X, latin1);
to_json_type(X, {type,_,<<"string">>,_}) when is_list(X) ->
    iolist_to_binary(X);
to_json_type(X, {type,_,<<"string">>,_}) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
to_json_type(_, void) -> <<"ok">>;
to_json_type(X, {type,_,<<"uint", _/binary>>,_}) when is_integer(X), X >= 0 ->
    list_to_binary(integer_to_list(X));
to_json_type(false, {type,_,<<"boolean">>,_}) -> <<"0">>;
to_json_type(true, {type,_,<<"boolean">>,_}) -> <<"1">>;
to_json_type(X, {type,_,<<"enumeration">>, En} = T) ->
    case [lists:keyfind(value,1,I1) || {enum,_,E1,I1} <- En,
				       E1 == X] of
	[{value,_,V,_}] ->
	    V;
	[] ->
	    error({type_error, [X, T]})
    end;
to_json_type(X, {type, _, <<"union">>, Ts} = Type) ->
    to_json_type_union(Ts, X, Type);
to_json_type(X, _) ->
    X.

to_json_type_union([T|Ts], X, Type) ->
    try to_json_type(X, T)
    catch
	error:_ ->
	    to_json_type_union(Ts, X, Type)
    end;
to_json_type_union([], X, Type) ->
    error({type_error, [X, Type]}).

json_rpc(YangFile) ->
    json_rpc(YangFile, []).

json_rpc(YangFile, Opts0) ->
    Dir = filename:dirname(YangFile),
    Opts = add_path(Dir, Opts0),
    case read(YangFile, Ext = filename:extension(YangFile), Opts) of
	{{ok, Y}, Ext} ->
	    with_module(fun to_json_rpc/3, Y, {Dir, Ext, Opts});
	{Error, _} ->
	    Error
    end.

add_path(".", Opts) ->
    Opts;
add_path(D, Opts) ->
    Opts ++ [{path, D}].


hrl(YangFile, HrlFile) ->
    hrl(YangFile, HrlFile, []).

hrl(YangFile, HrlFile, Opts) ->
    [{module, M, RPCs}] = json_rpc(YangFile, Opts),
    Forms = to_hrl_forms(RPCs, M),
    write_hrl(HrlFile, Forms, Opts).

write_hrl(_HrlFile, _Forms, _Opts) ->
    error(nyi).

read(F) ->
    read(F, filename:extension(F), []).

read(F, ".yang", Opts) ->
    {yang_parser:deep_parse(F, Opts), ".yang"};
read(F, ".eterm", Opts) ->
    {read_eterm(F, Opts), ".eterm"}.

read_eterm(F, Opts) ->
    case lists:keyfind(open_hook, 1, Opts) of
	{_, Open} ->
	    case Open(F, Opts) of
		{ok, Fd} ->
		    try
			Bin = read_open_file(Fd),
			{ok, binary_to_term(Bin)}
		    after
			file:close(Fd)
		    end;
		{error, _} = E ->
		    E
	    end;
	false ->
	    case file:read_file(F) of
		{ok, Bin} ->
		    {ok, binary_to_term(Bin)};
		Error ->
		    Error
	    end
    end.

read_open_file(Fd) ->
    file:position(Fd, bof),
    Read = fun() -> file:read(Fd, 4096) end,
    read_open_file(Read(), Read, <<>>).

read_open_file(eof, _, Acc) ->
    Acc;
read_open_file({ok, Bytes}, Read, Acc) ->
    read_open_file(Read(), Read, <<Acc/binary, Bytes/binary>>).


with_module(F, [{module,_,M,Data}], Dir) ->
    Ms = binary_to_list(M),
    [{module, Ms, F(Data, Dir, Ms)}].


to_hrl_forms(RPCs, _M) ->
    Macros = macro_forms(RPCs, _M),
    Records = record_defs(RPCs, _M),
    Macros ++ Records.

macro_forms(_RPCs, _M) ->
    [].

record_defs(_RPCs, _M) ->
    [].

to_json_rpc(Data, Arg, M) ->
    Imports = imports(Data, Arg),
    Data1 = augment(Data, Imports),
    lists:foldr(
      fun({rpc,_,N,InOut}, Acc) ->
	      Ns = M++":"++binary_to_list(N),
	      [{Ns, mk_rpc_pair(InOut, Ns, Data1, Imports)} | Acc];
	 ({notification,_,N,Elems}, Acc) ->
	      Ns = M++":"++binary_to_list(N),
	      [{Ns, notification(Ns, Elems, Data1, Imports)} | Acc];
	 (_, Acc) ->
	      Acc
      end, [], Data1).

imports(Data, {_Dir,Ext,Opts}) ->
    lists:foldl(
      fun({import,_,F,[{prefix,_,Pfx,_}|_]}, Acc) ->
	      File = binary_to_list(F) ++ Ext,
	      case read(File, Ext, Opts) of
		  {{ok, Y},_} ->
		      case [D || {module,_,N,D} <- Y,
				 N == F] of
			  [ImpData] ->
			      orddict:store(Pfx, ImpData, Acc);
			  [] ->
			      %% error({cannot_import, F, no_such_module})
			      io:fwrite("ERROR: cannot_import ~s (~p)~n",
					[F, no_such_module]),
			      Acc
		      end;
		  {Error,_} ->
		      io:fwrite("ERROR: cannot_import ~s (~p)~n", [F, Error]),
		      Acc
	      end;
	 (_, Acc) ->
	      Acc
      end, orddict:new(), Data).

augment([{augment, _, What, Items}|T], Imports) ->
    case re:split(What, "/") of
	[<<>>, Req, Sub] ->
	    %% we're doing very specific processing here, matching only on what
	    %% we expect to find.
	    case {re:split(Req, ":"), re:split(Sub, ":")} of
		{[Pfx, Method], [Pfx, Part]} ->
		    case [X || X <- orddict:fetch(Pfx, Imports),
				element(3, X) == Method] of
			[{rpc,L,M,InOut}] ->
			    PartAm = binary_to_atom(Part, latin1),
			    case lists:keyfind(PartAm, 1, InOut) of
				{_,_,_,List} = Obj ->
				    [{rpc,L,M,
				      expand_uses(
					lists:keyreplace(PartAm, 1, InOut,
							 setelement(
							   4, Obj, List ++ Items)),
					Pfx)}
				     | augment(T, Imports)];
				_ ->
				    error({cannot_augment, What})
			    end;
			[] ->
			    error({augment_not_found, What})
		    end;
		_ ->
		    error({unfamiliar_augment, What})
	    end;
	_ ->
	    error({unfamiliar_augment, What})
    end;
augment([H|T], Imports) ->
    [H|augment(T, Imports)];
augment([], _) ->
    [].

expand_uses([{Type,L,C,I}|T], Pfx) when Type == input; Type == output ->
    [{Type,L,C,expand_uses_(I, Pfx)} | expand_uses(T, Pfx)];
expand_uses([H|T], Pfx) ->
    [H|expand_uses(T, Pfx)];
expand_uses([], _) ->
    [].

expand_uses_([{uses,Lu,What,Iu} = Uses|T], Pfx) ->
    case binary:match(What, <<":">>) of
	nomatch ->
	    [{uses,Lu,<<Pfx/binary, ":", What/binary>>,Iu}|expand_uses_(T, Pfx)];
	_ ->
	    [Uses|expand_uses_(T, Pfx)]
    end;
expand_uses_([H|T], Pfx) ->
    [H|expand_uses_(T, Pfx)];
expand_uses_([], _) ->
    [].


notification(N, Elems, Data, Imports) ->
    {notification,
     descr(Elems),
     {struct, [{"jsonrpc", "2.0"},
	       {"method", N},
	       {"params", {struct, rpc_params(Elems, Data, Imports)}}]}}.

mk_rpc_pair(InOut, N, Data, Imports) ->
    {_,_,_,I} = lists:keyfind(input, 1, InOut),
    O = case lists:keyfind(output, 1, InOut) of
	    {_,_,_,Ox}  -> Ox;
	    false -> void
	end,
    {descr(InOut),
     {request, {struct, [{"jsonrpc", "2.0"},
			 {"method", N},
			 {"id", ""},
			 {"params", {struct, rpc_params(I, Data, Imports)}}]}},
     {reply, {struct, [{"jsonrpc", "2.0"},
		       {"id", ""},
		       {"result",
			case O of
			    void -> void;
			    _ ->
				{struct, rpc_params(O, Data, Imports)}
			end}]}}}.


rpc_params([{uses,_,G,_}|T], Data, Imports) ->
    {Where, GrpName} = case re:split(G, <<":">>) of
			   [_] ->
			       {Data, G};
			   [Pfx, G1] ->
			       {orddict:fetch(Pfx, Imports), G1}
		       end,
    TypeDefs = [{typedef,L,Type,Def} || {typedef,L,Type,Def} <- Where],
    Data1 = Data ++ TypeDefs,
    case [L || {grouping,_,Grp,L} <- Where,
	       Grp == GrpName] of
	[] ->
	    [{"uses-not-found", binary_to_list(G)} | rpc_params(T, Data1, Imports)];
	[Params] ->
	    rpc_params(log_meta(Params, {grouping, G}), Data1, Imports)
		++ rpc_params(T, Data1, Imports)
    end;
rpc_params([{leaf,_,N,Is}|T], Data, Imports) ->
    [{binary_to_list(N), "", descr(Is), [type(Is,Data,Imports),
					 mandatory(Is)|default(Is)]}
     | rpc_params(T, Data, Imports)];
rpc_params([{anyxml,_,N,Is}|T], Data, Imports) ->
    [{binary_to_list(N), "", descr(Is), [{type,anyxml},
					 mandatory(Is)|default(Is)]}
     | rpc_params(T, Data, Imports)];
rpc_params([{'leaf-list',_,N,Items}|T], Data, Is) ->
    L = binary_to_list(N),
    [{L, {array, [{L, "", descr(Items), [type(Items, Data, Is)]}]},
      descr(Items), [type(Items, Data, Is), mandatory(Items) | default(Items)]}
     | rpc_params(T, Data, Is)];
rpc_params([{list,_,N,Items}|T], Data, Is) ->
    [{binary_to_list(N), {array, [{struct, rpc_params(Items, Data, Is)}]},
      descr(Items), [type(Items, Data, Is), mandatory(Items) | default(Items)]}
     | rpc_params(T, Data, Is)];
rpc_params([{container,_,N,Items}|T], Data, Is) ->
    [{binary_to_list(N), {struct, rpc_params(Items, Data, Is)},
      descr(Items), [type(Items, Data, Is), mandatory(Items) | default(Items)]}
     | rpc_params(T, Data, Is)];
rpc_params([_|T], Data, Is) ->
    rpc_params(T, Data, Is);
rpc_params([], _, _) ->
    [].

log_meta(Params, Info) ->
    [{'$meta', Info}|Params].

markdown(File, JSON) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    try io:fwrite(Fd, "~s~n", [markdown(JSON)])
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

markdown([{module, M, RPCs}|T]) ->
    ["## Module: ", M, "\n\n", markdown_rpcs(RPCs), "\n\n" | markdown(T)];
markdown([]) ->
    [].

markdown_rpcs([{Not, {notification, Descr, Msg}} | T]) ->
    ["#### Notification: ", Not, "\n",
     "```json\n", pp_json(Msg), "\n```\n\n",
     case Descr of
	 "" -> "";
	 _ ->
	     [Descr, "\n\n"]
     end,
     markdown_descriptions(Msg), "\n\n" | markdown_rpcs(T)];
markdown_rpcs([{RPC, {Descr, {request, Req}, {reply, Rep}}} | T]) ->
    ["### RPC: ", RPC, "\n\n",
     "#### Request\n", "```json\n", pp_json(Req), "\n```\n\n",
     case Descr of
	 "" -> "";
	 _ ->
	     [Descr, "\n\n"]
     end,
     markdown_descriptions(Req), "\n\n",
     "#### Reply\n", "```json\n", pp_json(Rep), "\n```\n\n",
     markdown_descriptions(Rep), "\n\n" | markdown_rpcs(T)];
markdown_rpcs([]) ->
    [].

markdown_descriptions(Msg) ->
    case [X || {_,{_,_}} = X <- collect_descriptions(Msg, orddict:new())] of
	[] -> [];
	[{K,{D,T}}|Tail] ->
	    ["**descriptions**\n",
	     "<dl><dt>", K, "</dt>\n", "<dd>", D,
	     " (<b>type:</b> ", type_to_text(T), ")", "</dd>",
	     [["\n<dt>", K1, "</dt>\n", "<dd>", D1,
	       " (<b>type:</b> ", type_to_text(T1), ")", "</dd>"]
	      || {K1,{D1,T1}} <- Tail],
	     "\n</dl>\n\n"]
    end.

collect_descriptions({struct, L}, Acc) ->
    lists:foldl(fun collect_descriptions/2, Acc, L);
collect_descriptions({array, L}, Acc) ->
    lists:foldl(fun collect_descriptions/2, Acc, L);
collect_descriptions({K,{array,L},D,_}, Acc) ->
    lists:foldl(fun collect_descriptions/2,
		orddict:store(K, {D,<<"array">>}, Acc), L);
collect_descriptions({K,{struct,L},D,_}, Acc) ->
    lists:foldl(fun collect_descriptions/2,
		orddict:store(K, {D,<<"object">>}, Acc), L);
collect_descriptions({K,V,D,T}, Acc) ->
    collect_descriptions(V, orddict:store(K, {D,T}, Acc));
collect_descriptions({_K,V}, Acc) ->
    collect_descriptions(V, Acc);
collect_descriptions(_, Acc) ->
    Acc.


pp_json(Json) ->
    pp_json(Json, 0).

pp_json(void, _) ->
    "\"ok\"";
pp_json({struct, []}, I) ->
    [i(I), "{}"];
pp_json({struct, [H|T]}, I) ->
    I1 = I+1,
    ["{", pp_json(H, I1), [[",\n", i(I1), pp_json(Term,I1)] || Term <- T], "}"];
pp_json({array, []}, _I) ->
    ["[]"];
pp_json({array, [H|T]}, I) ->
    I1 = I+1,
    ["[", pp_json(H, I1), [[",\n", i(I1), pp_json(Term,I1)] || Term <- T], "]"];
pp_json(V, _I) when is_binary(V); is_list(V) ->
    io_lib:fwrite("\"~s\"", [V]);
pp_json(V, _I) when is_integer(V) ->
    ["\"", integer_to_list(V), "\""];
pp_json({K,V,_,_}, I) ->
    pp_json_(K, V, I);
pp_json({K,V}, I) ->
    pp_json_(K, V, I).


pp_json_(K,V,I) ->
    Part1 = ["\"", K, "\": "],
    I1 = I + iolist_size(Part1),
    [Part1, pp_json(V, I1)].

descr(L) ->
    case lists:keyfind(description, 1, L) of
	false ->
	    "";
	{_, _, B,_} ->
	    binary_to_list(B)
    end.

type(Is, Data, Imports) ->
    case lists:keyfind(type, 1, Is) of
	false ->
	    {type, undefined};
	{type, L, <<"union">>, Ts} ->
	    {type, L, <<"union">>, [type([T1], Data, Imports) ||
				       {type,_,_,_} = T1 <- Ts]};
	{type, _, T, _} = Type ->
	    case binary:split(T, <<":">>) of
		[Pfx, Ts] ->
		    ImpData = orddict:fetch(Pfx, Imports),
		    case [D1 || {typedef,_,T1,D1} <- ImpData, Ts == T1] of
			[_] = Def ->
			    type(Def, ImpData, Imports);
			[] ->
			    Type
		    end;
		_ ->
		    case [D1 || {typedef,_,T1,D1} <- Data, T == T1] of
			[Def|_] ->
			    type(Def, Data, Imports);
			[] ->
			    Type
		    end
	    end
    end.

mandatory(Is) ->
    case lists:keyfind(mandatory, 1, Is) of
	{mandatory, _, Bool, _} ->
	    {mandatory, Bool};
	false ->
	    {mandatory, false}
    end.

default(Is) ->
    case lists:keyfind(default, 1, Is) of
	{default, _, Default, _} ->
	    [{default, Default}];
	false ->
	    []
    end.

descr_type(Is, Data, Imports) ->
    case lists:keyfind(type, 1, Is) of
	false ->
	    <<"untyped">>;
	{type, _, T, _} = Type ->
	    case binary:split(T, <<":">>) of
		[Pfx, Ts] ->
		    ImpData = orddict:fetch(Pfx, Imports),
		    case [D1 || {typedef,_,T1,D1} <- ImpData, Ts == T1] of
			[Def] ->
			    type(Def, ImpData, Imports);
			[] ->
			    Type
		    end;
		_ ->
		    case [D1 || {typedef,_,T1,D1} <- Data, T == T1] of
			[Def|_] ->
			    type(Def, Data, Imports);
			[] ->
			    Type
		    end
	    end
    end.

%% descr_type({type, _, <<"enumeration">>, [{enum,_,_,_} |_] = En}) ->
%%     {enum, [ {E, I} || {enum,_,E,I} <- En] };
%% descr_type({type, _, T, _}) ->
%%     T.

%% descr_type({type, _, <<"enumeration">>, [{enum,_,E,I} |En]}) ->
%%     [ enum_value(I), " (", E, ")" | [ [ " | ", enum_value(I1), " (", E1, ")"]
%% 				     || {enum,_,E1,I1} <- En ] ];
%% descr_type({type, _, <<"boolean">>, _}) ->
%%     "\"1\" (true) | \"0\" (false)";
%% descr_type({type, _, T, _}) ->
%%     T.

type_to_text(B) when is_binary(B) ->
    B;
type_to_text(L) ->
    case lists:keyfind(type, 1, L) of
	false ->
	    [type_to_text_(undefined), "; ", mandatory_to_text(L)];
	T ->
	    [type_to_text_(T), "; ", mandatory_to_text(L)]
    end.

type_to_text_({type, undefined}) ->
    "untyped";
type_to_text_({type, anyxml}) ->
    "XML";
type_to_text_({type, _, <<"enumeration">>, [{enum,_,E,I} | _] = En}) ->
    [ val2txt(I), " (", E, ")" | [ [ " | ", val2txt(I1), " (", E1, ")"]
				   || {enum, _, E1, I1} <- En] ];
type_to_text_({type, _, <<"boolean">>, _}) ->
    "\"1\" (true) | \"0\" (false)";
type_to_text_({type, _, <<"union">>, Ts}) ->
    [ "One of:", [["~n* ", type_to_text(T)] || T <- Ts] ];
type_to_text_({type, _, T, _}) ->
    T;
type_to_text_(T) when is_binary(T) ->
    T.

mandatory_to_text(L) ->
    case lists:keyfind(mandatory, 1, L) of
	false ->
	    "[<em>mandatory: false</em>]";
	{mandatory,B} when is_boolean(B) ->
	    ["[<em>mandatory: ", atom_to_list(B),"</em>]"]
    end.

%% enum_value(I) ->
%%     {value,_,V,_} = lists:keyfind(value, 1, I),
%%     V.
val2txt(I) ->
    {value,_,V,_} = lists:keyfind(value, 1, I),
    ["\"", binary_to_list(V), "\""].

i(I) ->
    lists:duplicate(I,$\s).
