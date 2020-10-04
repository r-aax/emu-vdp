%% @doc
%% Набоо токенов test.

% Имя модуля.
-module(tokens_test).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([get_tokens_test/0]).

%---------------------------------------------------------------------------------------------------

-spec get_tokens_test() -> [vdp:token()].
%% @doc
%% Создание списка начальных токенов test.
%%
%% @returns
%% Список входных токенов test.
get_tokens_test() ->

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
