-module(tbibloom_ebloom).
-compile(export_all).

get_filter(Terms) ->
	{ok, Ref} = ebloom:new(length(Terms), 0.01, 123),
	lists:foreach(fun(T) -> add_to_filter(Ref, T) end, Terms),
	ebloom:serialize(Ref).

add_to_filter(Ref, Term) when is_binary(Term) ->
	ebloom:insert(Ref, Term),
	ok;
add_to_filter(_, BadTerm) ->
	ok.

check_filter(Filter, Members) ->
	{ok, Ref} = ebloom:deserialize(Filter),
	check_filter_contains(Ref, Members).

check_filter_contains(_Filter, []) ->
	true;
check_filter_contains(Filter, [Member|Rest]) when is_binary(Member) ->
	case ebloom:contains(Filter, Member) of
		true -> check_filter_contains(Filter, Rest);
		false -> false
	end.