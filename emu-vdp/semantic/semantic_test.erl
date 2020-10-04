%% @doc
%% Семантика test.

% Имя модуля.
-module(semantic_test).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([get_semantic_test/0]).

%---------------------------------------------------------------------------------------------------

-spec get_semantic_test() -> [{vdp:command_name(), {integer(), fun()}}].
%% @doc
%% Получение описание семантики инструкций.
%%
%% @returns
%% Описание семантики команд.
get_semantic_test() ->
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
