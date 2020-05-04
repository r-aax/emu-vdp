%% @doc
%% Головной модуль.

% Имя модуля.
-module(main).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспорт функций.
-export([start/0,
         test/0]).

%---------------------------------------------------------------------------------------------------
% Служебные функции.
%---------------------------------------------------------------------------------------------------

-spec generate_dataflow_graph() -> [vdp:command()].
%% @doc
%% Создание графа программы.
generate_dataflow_graph() ->
    [
        #command{id =  1, name =  mul, dsts = [{2, 1}]},
        #command{id =  2, name =  mul, dsts = [{5, 2}]},
        #command{id =  3, name =  mul, dsts = [{5, 1}]},
        #command{id =  4, name =  neg, dsts = [{7, 1}, {8, 1}]},
        #command{id =  5, name =  sub, dsts = [{6, 1}]},
        #command{id =  6, name = sqrt, dsts = [{8, 2}, {7, 2}]},
        #command{id =  7, name =  add, dsts = [{10, 1}]},
        #command{id =  8, name =  sub, dsts = [{11, 1}]},
        #command{id =  9, name =  mul, dsts = [{10, 2}, {11, 2}]},
        #command{id = 10, name =  dvs, dsts = []},
        #command{id = 11, name =  dvs, dsts = []}
    ].

%---------------------------------------------------------------------------------------------------

-spec generate_initial_tokens() -> [vdp:token()].
%% @doc
%% Создание списка начальных токенов.
generate_initial_tokens() ->

    % Коэффициенты квадратного уравнения.
    A = 1.0,
    B = -3.0,
    C = 2.0,

    [
        #token{command_id = 9, state = default, entry = 1, data = {float, 2.0}},
        #token{command_id = 1, state = default, entry = 1, data = {float, 4.0}},
        #token{command_id = 9, state = default, entry = 2, data = {float, A}},
        #token{command_id = 1, state = default, entry = 2, data = {float, A}},
        #token{command_id = 2, state = default, entry = 2, data = {float, C}},
        #token{command_id = 4, state = default, entry = 1, data = {float, B}},
        #token{command_id = 3, state = default, entry = 1, data = {float, B}},
        #token{command_id = 3, state = default, entry = 2, data = {float, B}}
    ].

%---------------------------------------------------------------------------------------------------

-spec get_instructions_semantic() -> [{vdp:command_name(), {integer(), fun()}}].
%% @doc
%% Получение описание семантики инструкций.
get_instructions_semantic() ->
    [
        {
            add,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X + Y}}
                end
            }
        },
        {
            dvs,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X / Y}}
                end
            }
        },
        {
            mul,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X * Y}}
                end
            }
        },
        {
            neg,
            {
                1,
                fun([#token{state = St, entry = 1, data = {float, X}}]) ->
                    #token{state = St, data = {float, -X}}
                end
            }
        },
        {
            sqrt,
            {
                1,
                fun([#token{state = St, entry = 1, data = {float, X}}]) ->
                    #token{state = St, data = {float, math:sqrt(X)}}
                end
            }
        },
        {
            sub,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X - Y}}
                end
            }
        }
    ].

%---------------------------------------------------------------------------------------------------
% Старт модуля.
%---------------------------------------------------------------------------------------------------

-spec start() -> ok.
%% @doc
%% Старт головного модуля.
start() ->
    io:format("main:start() : begin~n"),

    % Получаем граф программы.
    G = generate_dataflow_graph(),

    % Получаем список инициализирующих токенов.
    T = generate_initial_tokens(),

    % Получаем семантику.
    S = get_instructions_semantic(),

    % Запускаем процессы.
    vec_mem:start(),
    cam_mem:start(),
    exe_buf:start(),

    % Запускаем основной цикл.
    loop(T, G, S),

    io:format("main:start() : end~n").

%---------------------------------------------------------------------------------------------------
% Цикл работы с токенами.
%---------------------------------------------------------------------------------------------------

-spec loop(T, G, S) -> ok
      when T :: [vdp:token()],
           G :: [vdp:command()],
           S :: [{vdp:command_name(), {integer(), fun()}}].
%% @doc
%% Цикл для работы с токенами.
%% В цикле может происходит занесение токенов с ассоциативную память,
%% либо обработка инструкции из буфера команд.
%%
%% @param T Список токенов.
%% @param G Граф программы.
%% @param S Семантика комманд.
%%
%% @private
loop([#token{command_id = CId} = TH | TT], G, S) ->

    % Ищем команду в графе.
    [Cmd] = lists:filter(fun(#command{id = Id}) -> Id =:= CId end, G),

    % Ищем ее семантику.
    Name = Cmd#command.name,
    {value, {Name, {Args, _Fun}}} = lists:keysearch(Name, 1, S),

    % Пытаемся поместить токен в cam_mem.
    case cam_mem:set_token(TH, Args) of
        ok ->
            % Токен лег в ассоциативную память.
            ok;
        ?OK(Tokens) ->
            % Нашли комплектт на исполнение, отправляем в буфер.
            exe_buf:add(Tokens)
    end,

    % Продолжаем работу.
    loop(TT, G, S);

loop([], G, S) ->

    % Больше не осталось токенов для добавления.
    % Берем на исполнение набор токенов из буфера.
    case exe_buf:take() of

        ok ->
            % Буфер пустой, завершаем работу.
            ok;

        ?OK(Tokens) ->
            % Достали набор токенов для выполнения инструкции.

            % Достаем идентификатор команды.
            [#token{command_id = CId} | _] = Tokens,

            % Ищем команду в графе.
            [Cmd] = lists:filter(fun(#command{id = Id}) -> Id =:= CId end, G),

            % Ищем ее семантику.
            Name = Cmd#command.name,
            {value, {Name, {_Args, Fun}}} = lists:keysearch(Name, 1, S),

            % Исполняем команду.
            OutTokenTemplate = Fun(Tokens),

            % Формируем выходные токены.
            OutTokens =
                [
                    OutTokenTemplate#token{command_id = OutCId, entry = Entry} 
                    ||
                    {OutCId, Entry} <- Cmd#command.dsts
                ],

            % Если выходных токенов нет, то печатаем на экран шаблон (это финальные данные).
            if
                OutTokens =:= [] ->
                    io:format("Terminal data : ~p~n", [OutTokenTemplate]);
                true ->
                    ok
            end,

            % Продолжаем работу с новыми токенами.
            loop(OutTokens, G, S)
    end.

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

    % Тестирование exe_buf.
    exe_buf:start(),
    exe_buf:test(),
    io:format("Testing of exe_buf is done.~n"),

    ok.

%---------------------------------------------------------------------------------------------------
