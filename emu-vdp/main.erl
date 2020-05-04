-module(main).

-export([start/0,
         test/0]).

-record(token,
{
    command_id,
    generation,
    iteration,
    index,
    entry,
    data
}).

-record(command,
{
    id,
    name,
    destinations
}).

emu(imul, [D1, D2]) ->
    D1 * D2;
emu(iadd, [D1, D2]) ->
    D1 + D2;
emu(isub, [D1, D2]) ->
    D1 - D2.

find_tokens_to_go(Tokens) ->
    find_tokens_to_go(Tokens, 1).
find_tokens_to_go([], _) ->
    undef;
find_tokens_to_go([_], _) ->
    undef;
find_tokens_to_go([#token{command_id = _CommandId, generation = _Generation, iteration = _Iteration, index = _Index, entry = 0},
                   #token{command_id = _CommandId, generation = _Generation, iteration = _Iteration, index = _Index, entry = 1}
                   | _], I) ->
    I;
find_tokens_to_go([_, T | Tail], I) ->
    find_tokens_to_go([T | Tail], I + 1).

loop([], _) ->
    io:format("program finish~n");
loop(Tokens, Commands) ->
    io:format("Tokens : ~p~n", [Tokens]),

    % Find data.
    PosInTokens = find_tokens_to_go(Tokens),
    io:format("position in tokens list : ~p~n", [PosInTokens]),
    T1 = lists:nth(PosInTokens, Tokens),
    T2 = lists:nth(PosInTokens + 1, Tokens),
    [Cmd] = lists:filter(fun(X) -> X#command.id == T1#token.command_id end,
                         Commands),

    % Execute.
    io:format("EXE : cmd = ~p, tokens = ~p, ~p~n", [Cmd, T1, T2]),
    R = emu(Cmd#command.name, [T1#token.data, T2#token.data]),
    {Sub1, [_, _ | Sub2]} = lists:split(PosInTokens - 1, Tokens),

    % New data.
    Ress = lists:map
    (
        fun(Dst) ->
            case Dst of
                undef ->
                    undef;
                show ->
                    io:format("SHOW RESULT : ~p~n", [R]),
                    undef;
                {ToCmdId, ToEntry} ->
                    T1#token{command_id = ToCmdId, entry = ToEntry, data = R}
            end
        end,
        Cmd#command.destinations
    ),
    NewTokens = Sub1 ++ Sub2 ++ lists:filter(fun(X) -> X /= undef end, Ress),
    loop(lists:sort(NewTokens), Commands).

start() ->
    io:format("Векторный потоковый процессор main:start()~n"),

    % Commands.
    Commands =
    [
        %#command{id = 1, name = imul, destinations = [{3, 0}, {2, 0}]},
        %#command{id = 2, name = iadd, destinations = [{3, 1}, undef]},
        %#command{id = 3, name = isub, destinations = [undef, show]}

        #command{id = 1, name = isub, destinations = [{3, 1}, undef]},
        #command{id = 2, name = isub, destinations = [undef, {3, 0}]},
        #command{id = 3, name = iadd, destinations = [show, undef]}
    ],

    % Inputs.
    Tokens =
    [
        %#token{command_id = 1, entry = 0, generation = 0, iteration = 0, index = 0, data = 1},
        %#token{command_id = 1, entry = 1, generation = 0, iteration = 0, index = 0, data = 2},
        %#token{command_id = 2, entry = 1, generation = 0, iteration = 0, index = 0, data = 3}

        #token{command_id = 2, entry = 1, generation = 0, iteration = 0, index = 0, data = 10},
        #token{command_id = 2, entry = 0, generation = 0, iteration = 0, index = 0, data = 20},
        #token{command_id = 1, entry = 1, generation = 0, iteration = 0, index = 0, data = 30},
        #token{command_id = 1, entry = 0, generation = 0, iteration = 0, index = 0, data = 40}
    ],

    loop(lists:sort(Tokens), lists:sort(Commands)).

%---------------------------------------------------------------------------------------------------
% Тестирование.
%---------------------------------------------------------------------------------------------------

-spec test() -> ok.
%% @doc
%% Тестирование эмулятора.
%%
%% @return
%% Код успешного завершения ok.
test() ->

    % Тестирование vec_mem.
    vec_mem:start(),
    vec_mem:test(),
    io:format("Testing of vec_mem is done.~n"),

    % Тестирование cam_mem.
    cam_mem:start(),
    cam_mem:test(),
    io:format("Testing of cam_mem is done.~n"),

    ok.

%---------------------------------------------------------------------------------------------------
