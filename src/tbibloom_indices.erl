-module(tbibloom_indices).
-compile(export_all).

-define(GSETPAGE, 1000).

-record(ctx, { riakc, bucket, key, idxbucket, idxpages }).

index_term(Term, #ctx{riakc=C, bucket=B, key=K}) ->
	IdxB = idx_bucket(B),
	IdxCnt = idx_cnt_key(Term),
	Count = case riakc_pb_socket:counter_val(C, IdxB, IdxCnt) of
		{ok, Int} when Int =< 0 -> 1;
		{error, notfound} -> 1;
		{ok, Int} when Int > 0 -> Int;
		{error, CntErr} ->
			io:format("Error, Unknown response: ~p~n", [CntErr]),
			1
	end,

	PageKey = idx_page_key(Term, term_page_num(Count, ?GSETPAGE)),

	Set = ordsets:from_list([K]),

	SetMember = riakc_obj:new(IdxB, PageKey, Set),
	case riakc_pb_socket:put(C, SetMember) of
		ok ->
			riakc_pb_socket:counter_incr(C, IdxB, IdxCnt, 1);
		SavErr ->
			{error, SavErr}
	end.

get_index(Term, #ctx{riakc=C, bucket=B}=Ctx) ->
	IdxB = idx_bucket(B),
	IdxCnt = idx_cnt_key(Term),
	Keys = case riakc_pb_socket:counter_val(C, IdxB, IdxCnt) of
		{ok, Int} when Int =< 0 -> 
			io:format("Info: Count returned =< 0~n", []),
			[];
		{error, notfound} -> 
			io:format("Info: No count found for ~p~n", [IdxCnt]),
			[];
		{ok, Int} when Int > 0 ->
			MaxPages = term_page_num(Int, ?GSETPAGE),
			get_index_pages(Term, Ctx#ctx{idxbucket=IdxB, idxpages=MaxPages});
		{error, CntErr} ->
			io:format("Error, unknown response: ~p~n", [CntErr]),
			[]
	end,
	Keys.

get_index_pages(Term, Ctx) ->
	get_index_pages(Term, Ctx, 0, []).

get_index_pages(_Term, #ctx{idxpages=Max}, Max, Acc ) ->
	Acc;

get_index_pages(Term, Ctx, Curr, Acc ) ->
	get_index_pages(Term, Ctx, Curr + 1, Acc ++ get_index_page(Term, Ctx, Curr + 1)).

get_index_page(Term, #ctx{riakc=C, idxbucket=B}, PageNum) ->
	io:format("Debug: Retrieving page ~p of ~p.~n", [PageNum, Term]),
	K = idx_page_key(Term, PageNum),
	{ok, Page} = riakc_pb_socket:get(C, B, K),
	Members = case riakc_obj:value_count(Page) of
		1 -> binary_to_term(riakc_obj:get_value(Page));
		Int when Int > 1 ->
			io:format("Debug: Merging page ~p of ~p.~n", [PageNum, Term]),
			Values = [binary_to_term(X) || X <- riakc_obj:get_values(Page)],
			Merged = ordsets:union(Values),
			Page1 = riakc_obj:update_value(Page, term_to_binary(Merged)),
			{Meta, _} = hd(riakc_obj:get_contents(Page)),
			Page2 = riakc_obj:update_metadata(Page1, Meta),
			case riakc_pb_socket:put(C, Page2) of
				ok -> ok;
				{error, MergeErr} ->
					io:format("Merge Error on page ~p: ~p~n", [PageNum, MergeErr]),
					ok
			end,
			Merged
	end,
	ordsets:to_list(Members).

idx_bucket(Bucket) ->
	Bucket.

idx_cnt_key(Term) ->
	<<"_index_", Term/binary, "_count">>.

term_page_num(Count, Max) ->
	trunc(Count/Max) + 1.

idx_page_key(Term, PageNumInt) -> 
	PageNum = list_to_binary(integer_to_list(PageNumInt)),
	<<"_index_", Term/binary, "_page_", PageNum/binary>>.