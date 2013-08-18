-module(tbibloom_resource).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, { riakc, bucket, key, idxbucket, idxpages }).

init([]) -> 
	{ok, #ctx{riakc=poolboy:checkout(riak_pool)}}.

allowed_methods(RD, Ctx) ->
	%io:format("Debug: Request Data: ~p~n", [RD]),
	{['HEAD', 'GET', 'PUT'], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", accept_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
	{[{"application/json", reply_json}, {"plain/text", reply_text}], RD, Ctx}.

process_post(RD, Ctx) -> accept_json(RD, Ctx).

reply_json(RD, Ctx) -> 
	%io:format("Debug: reply_json: ~p~n", [wrq:raw_path(RD)]),
	Keys = get_keys(RD, Ctx),
	Json = mochijson2:encode(Keys),
	{Json, RD, Ctx}.

reply_text(RD, Ctx) ->
	%io:format("Debug: reply_text: ~p~n", [wrq:raw_path(RD)]),
	Keys = get_keys(RD, Ctx),
	{Keys, RD, Ctx}.

get_keys(RD, Ctx) ->
	B = erlang:list_to_binary(wrq:path_info(bucket, RD)),
	T = erlang:list_to_binary(wrq:path_info(key, RD)),
	tbibloom_indices:get_index(T, Ctx#ctx{bucket=B}).

accept_json(RD, #ctx{riakc=C}=Ctx) ->
	%io:format("Debug: accept_json: ~p~n", [wrq:raw_path(RD)]),
	Json = wrq:req_body(RD),
	B = erlang:list_to_binary(wrq:path_info(bucket, RD)),
	K = erlang:list_to_binary(wrq:path_info(key, RD)),
	Ctx1 = Ctx#ctx{bucket=B, key=K},
	Object = riakc_obj:new(B, K, Json),
    Body = mochijson2:decode(Json),
    Terms = parse_terms(Body),
 	case index_terms(Terms, Ctx1) of
 		ok ->
			case riakc_pb_socket:put(C, Object) of
				ok -> {true, RD, Ctx};
				ObjErr ->
					io:format("Error saving object: ~p~n", [ObjErr]),
					{{halt, 502}, RD, Ctx}
			end;
 		{ error, IdxErr } ->
 			io:format("Error indexing: ~p~n", [IdxErr]),
 			{{halt, 502}, RD, Ctx}
 	end.

index_terms([], _Ctx1) ->
	ok;
index_terms([Term|Rest], Ctx1) ->
	case tbibloom_indices:index_term(Term, Ctx1) of
		ok -> index_terms(Rest, Ctx1);
		Err -> Err
	end.

finish_request(RD, #ctx{riakc=C}=Ctx) ->
	poolboy:checkin(riak_pool, C),
	{true, RD, Ctx}.

norm_term(T) when is_atom(T) ->
	atom_to_binary(T, utf8).


parse_terms({struct, C}) when is_list(C) ->
	parse_terms(C);
parse_terms(C) when is_binary(C) ->
	[C];
parse_terms(C) when is_integer(C) ->
	to_binary(C);
parse_terms(C) when is_list(C) ->
	parse_terms(C, []).


parse_terms([], Acc) ->
	Acc;

parse_terms([{struct, C}|Rest], Acc) when is_list(C) ->
	parse_terms(C ++ Rest, Acc);

parse_terms([{A1, B1}|Rest], Acc) when is_binary(B1) or is_integer(B1) ->
	A2 = to_binary(A1),
	B2 = to_binary(B1),
	parse_terms(Rest, Acc ++ [<<A2/binary, "_", B2/binary>>]);

parse_terms([A1|Rest], Acc) when is_binary(A1) or is_integer(A1) ->
	parse_terms(Rest, Acc ++ [to_binary(A1)]);

parse_terms([_|Rest], Acc) ->
	parse_terms(Rest, Acc).


to_binary(T) when is_atom(T) ->
	atom_to_binary(T, utf8);
to_binary(T) when is_binary(T) ->
	T;
to_binary(T) when is_float(T) ->
	list_to_binary(float_to_list(T));
to_binary(T) when is_integer(T) ->
	list_to_binary(integer_to_list(T));
to_binary(T) ->
	term_to_binary(T).
