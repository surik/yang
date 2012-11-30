-module(yang_json_validate).

-export([validate/3, validate/4, validate_type/3, test/0]).

-include("typespec.hrl").

-compile({parse_transform, cut}).

validate(ModuleSpec, Type, Doc) ->
    validate(ModuleSpec, Type, -1, Doc).

validate(ModuleSpec, Type, Depth, Doc) ->
    validate_(yang_typespec:get_type(ModuleSpec, Type), Depth, Doc).

validate_type(Type, Depth, Doc) ->
    validate_(Type, Depth, Doc).
    
%%------------------------------
%% internal functions
%%------------------------------

validate_(false, _, _) ->
    throw({error, unknown_type});
validate_(#object{fields = Fields}, Depth, {Doc}) when is_list(Doc) ->
    validate_fields(Fields, Depth, Doc);
validate_(#object{fields = Fields}, Depth, Doc) when is_list(Doc) ->
    validate_fields(Fields, Depth, Doc);
validate_(Type, _, Doc) ->
    error_logger:error_report([{type, Type}, {doc, Doc}]),
    throw({error, not_impleted_yet}).

%% stop here
validate_fields(_, 0, Doc) -> Doc;
validate_fields(Fields, Depth, Doc) ->
    validate_fields(Fields, Depth, Doc, []).

validate_fields([], _, [], NDoc) ->
    lists:reverse(NDoc);
validate_fields([], _, Doc, NDoc) ->
    error_logger:error_report([{function, validate_fields},
			       {remaining, Doc}, {done, NDoc}]),
    throw({error, too_much_fields, Doc});
validate_fields([Type|Tail], Depth, Doc, NDoc) ->
    N = element(2, Type),
    {Field, Doc1} =
	case lists:keytake(N, 1, Doc) of
	    {value, F, D} -> {F, D};
	    _             -> {false, Doc}
	end,
    case validate_item(N, Field, Depth, Type) of
	undefined -> validate_fields(Tail, Depth, Doc1, NDoc);
	NewField  -> validate_fields(Tail, Depth, Doc1, [NewField|NDoc])
    end.

%% validate_fields(Fields, Depth, Doc) ->
%%     lists:map(validate_field(_, Depth, Doc), Fields).

%% validate_field(Type, Depth, Doc) ->
%%     N = element(2, Type),
%%     validate_item(N, lists:keyfind(N, 1, Doc), Depth, Type).

%% stop here
validate_item(_, V, 0, _) -> V;

%%------------------------------
%% mandatory handling
%%------------------------------

validate_item(_, false, _, #field{mandatory = false, default = undefined}) ->
    undefined;
validate_item(_, false, _, #field{mandatory = false, name = N, default = Default}) ->
    {N, Default};
validate_item(_, false, _, #array{mandatory = false, name = N}) ->
    {N, []};
validate_item(N, false, _, _) ->
    throw({error, missing_field, N});

%%------------------------------
%% complex types
%%------------------------------

validate_item(_, {N, V}, Depth, #field{type = Type}) ->
    {N, validate_item(N, V, Depth, Type)};
validate_item(_, {N, V}, Depth, #array{type = Type})
  when is_list(V) ->
    {N, lists:map(validate_item(N, _, Depth - 1, Type), V)};
validate_item(_, {N, {V}}, Depth, #object{fields = Fields})
  when is_list(V) ->
    {N, {validate_fields(Fields, Depth - 1, V)}};
validate_item(_, {V}, Depth, #struct{fields = Fields})
  when is_list(V) ->
    {validate_fields(Fields, Depth, V)};

%%------------------------------
%% simple types
%%------------------------------

validate_item(_, V, _, #string{})
  when is_binary(V) -> V;

validate_item(_, V, _, {<<"int8">>, _})
  when is_integer(V), V >= -128, V =< 127 -> V;
validate_item(_, V, _, {<<"int16">>, _})
  when is_integer(V), V >= -32768, V =< 32767 -> V;
validate_item(_, V, _, {<<"int32">>, _})
  when is_integer(V), V >= -2147483648, V =< 2147483647 -> V;
validate_item(_, V, _, {<<"int64">>, _})
  when is_integer(V), V >= -9223372036854775808, V =< 9223372036854775807 -> V;

validate_item(_, V, _, {<<"uint8">>, _})
  when is_integer(V), V >= 0, V =< 255 -> V;
validate_item(_, V, _, {<<"uint16">>, _})
  when is_integer(V), V >= 0, V =< 65535 -> V;
validate_item(_, V, _, {<<"uint32">>, _})
  when is_integer(V), V >= 0, V =< 4294967295 -> V;
validate_item(_, V, _, {<<"uint64">>, _})
  when is_integer(V), V >= 0, V =< 18446744073709551615 -> V;

validate_item(_, V, _, {<<"boolean">>, _})
  when is_boolean(V) -> V;

validate_item(N, V, _, Type) ->
    error_logger:info_report([{function, validate_item},
			      {field, N}, {value, V}, {type, Type}]),
    throw({error, invalid_type, {{N, V}, Type}}).

%%------------------------------

typespec(File) ->
    {ok, Yang} = yang:deep_parse_file(File),
    yang:typespec(Yang).

test() ->
    Skel = typespec("priv/crud-api-skeleton.yang"),
    UDR = typespec("priv/udr-api.yang"),
    {ok, Req1, _} = hello_json:decode(<<"{\"message\":\"Server Error\",\"code\":-32099}">>),
    R1 = validate(Skel, <<"rpc_error">>, Req1),
    io:format("R1: ~p~n", [R1]),
    
    {ok, Req2, _} = hello_json:decode(<<"{\"Private-User-Id\":\"demo@komola\",\"Subscriber-Id\":\"demo@komola\",\"Charging-Account\":\"\",\"Credentials\":[{\"Authentication-Id\":\"demo@komola\",\"Authentication-Scheme\":\"password\",\"Authentication-Data\":\"asdads222\"},{\"Authentication-Id\":\"foo@komola\",\"Authentication-Scheme\":\"password\",\"Authentication-Data\":\"test\"},{\"Authentication-Id\":\"asd@komola\",\"Authentication-Scheme\":\"password\",\"Authentication-Data\":\"foobar\"}],\"Public-User-Ids\":[],\"Overlay-Service-Profiles\":[{\"Service-Profile-Id\":\"runway\",\"Override\":[],\"Append\":[]}]}">>),
    R2 = validate(UDR, <<"UserObject">>, Req2),
    io:format("R2: ~p~n", [R2]),

    ok.
