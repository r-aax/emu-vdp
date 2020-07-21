%% @doc
%% Тестовый эмулятор.

% Имя модуля.
-module(emu_test).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([test/0]).

%---------------------------------------------------------------------------------------------------
% Служебные функции.
%---------------------------------------------------------------------------------------------------

-spec get_instructions_semantic() -> [{vdp:command_name(), {integer(), fun()}}].
%% @doc
%% Получение описание семантики инструкций.
get_instructions_semantic() ->
    [
        #command_semantic
        {
            name = add,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X + Y}}
                end
        },

        #command_semantic
        {
            name = dvs,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X / Y}}
                end
        },

        #command_semantic
        {
            name = mul,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X * Y}}
                end
        },

        #command_semantic
        {
            name = neg,
            arity = 1,
            function =
                fun([#token{state = St, entry = 1, data = {float, X}}]) ->
                    #token{state = St, data = {float, -X}}
                end
        },

        #command_semantic
        {
            name = sqrt,
            arity = 1,
            function =
                fun([#token{state = St, entry = 1, data = {float, X}}]) ->
                    #token{state = St, data = {float, math:sqrt(X)}}
                end
        },

        #command_semantic
        {
            name = sub,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X - Y}}
                end
        }
    ].

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
% Тестирование модуля.
%---------------------------------------------------------------------------------------------------

-spec test() -> ok.
%% @doc
%% Тестирование модуля.
test() ->

    % Семантика команд конкретного эмулятора.
    Semantic = get_instructions_semantic(),

    % Входные данные эмулятора - программа и входные токены.
    Graph = generate_dataflow_graph(),
    Tokens = generate_initial_tokens(),

    % Запускаем эмулятор.
    main:start(Semantic, Graph, Tokens).

%---------------------------------------------------------------------------------------------------
