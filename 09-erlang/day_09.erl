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

move_file_blocks(Blocks, []) ->
    Blocks;
move_file_blocks(Blocks, Free) ->
    LastBlock = lists:last(Blocks),
    BlocksInit = lists:droplast(Blocks),
    case LastBlock of
        -1 ->
            move_file_blocks(BlocksInit, lists:droplast(Free));
        _ ->
            {BeforeFree, AfterFree} = lists:split(hd(Free), BlocksInit),
            move_file_blocks(BeforeFree ++ [LastBlock] ++ tl(AfterFree), tl(Free))
    end.

calc_checksum(FileBlocks) ->
    lists:sum(lists:map(fun({Index, FileID}) -> Index * FileID end, lists:enumerate(0, FileBlocks))).

solve_part_one(Bin) ->
    Blocks = parse_disk_map(Bin),
    Free = [Index || {Index, Block} <- lists:enumerate(0, Blocks), Block == -1],

    Compact = move_file_blocks(Blocks, Free),

    calc_checksum(Compact).

main([InputFile]) ->
    {ok, Bin} = file:read_file(InputFile),

    io:format("--- Day 9: Disk Fragmenter ---~n"),
    io:format("Answer for part 1: ~p~n", [solve_part_one(Bin)]),

    erlang:halt(0).
