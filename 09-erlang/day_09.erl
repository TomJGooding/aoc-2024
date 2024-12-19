-module(day_09).
-compile(export_all).

make_blocks(Index, Digit) ->
    case Index rem 2 of
        0 -> lists:duplicate(Digit, Index div 2);
        _ -> lists:duplicate(Digit, -1)
    end.

parse_disk_map(Bin) when is_binary(Bin) ->
    parse_disk_map(binary_to_list(Bin));
parse_disk_map(Str) when is_list(Str) ->
    Digits = [list_to_integer([X]) || X <- string:chomp(Str)],
    lists:append(
      [make_blocks(Index, Digit) || {Index, Digit} <- lists:enumerate(0, Digits)]).

calc_checksum(BlocksList) when is_list(BlocksList) ->
    calc_checksum(list_to_tuple(BlocksList));
calc_checksum(BlocksTuple) when is_tuple(BlocksTuple) ->
    calc_checksum(BlocksTuple, 0, tuple_size(BlocksTuple) - 1, 0).
calc_checksum(_, LeftIdx, RightIdx, Acc) when LeftIdx > RightIdx ->
    Acc;
calc_checksum(BlocksTuple, LeftIdx, RightIdx, Acc) ->
    LeftBlock = element(LeftIdx + 1, BlocksTuple),
    RightBlock = element(RightIdx + 1, BlocksTuple),
    case {LeftBlock, RightBlock} of
        {-1, -1} ->
            calc_checksum(BlocksTuple, LeftIdx, RightIdx - 1, Acc);
        {-1, _} ->
            NewAcc = Acc + (LeftIdx * RightBlock),
            calc_checksum(BlocksTuple, LeftIdx + 1, RightIdx - 1, NewAcc);
        {_, -1} ->
            NewAcc = Acc + (LeftIdx * LeftBlock),
            calc_checksum(BlocksTuple, LeftIdx + 1, RightIdx - 1, NewAcc);
        {_, _} ->
            NewAcc = Acc + (LeftIdx * LeftBlock),
            calc_checksum(BlocksTuple, LeftIdx + 1, RightIdx, NewAcc)
    end.

solve_part_one(Bin) ->
    Blocks = parse_disk_map(Bin),
    calc_checksum(Blocks).

main([InputFile]) ->
    {ok, Bin} = file:read_file(InputFile),

    io:format("--- Day 9: Disk Fragmenter ---~n"),
    io:format("Answer for part 1: ~p~n", [solve_part_one(Bin)]),

    erlang:halt(0).
