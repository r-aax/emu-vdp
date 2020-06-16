
%% @doc
%% Память векторов.

% Имя модуля.
-module(semantic).

% Подключение файлов.
-include("records.hrl").

% Экспортируемые функции
-export([instructions_semantic/0, start/0]).

start() ->
    Pid = spawn(?MODULE, instructions_semantic, []),
    register(emu_vdp, Pid),
    Pid.

% Старт модуля
instructions_semantic() ->
    [
        % Сложение
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

        % Деление
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

        % Умножение
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

        % МОД - деление по модулю (остаток от деления)
        {
            "МОД",
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {int, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X rem Y}}
                end
            }

        },

        % Вычитание
        {
            sub,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X - Y}}
                end
            }
        },

        % ПУ - передача по условию. Если на правый вход поступает "0", то на правом выходе то, что поступило на левый вход
        % иначе на левом выходе то, что поступило на левый вход.
        {
            "ПУ",
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    case Y of
                        0 ->
                            [#token{state = St, data = {float, []}}, 
                             #token{state = St, data = {float, X}}];
                        _ ->
                            [#token{state = St, data = {float, X}}, 
                             #token{state = St, data = {float, []}}]
                    end                       
                end
            }
        },

        % ПДБ - передача с дублированием. На выходе два токена с теми же данными, которые пришли на один вход.
        % Команда необходима для дублирования токенов.
        {
            "ПДБ",
            {
                1,
                fun(#token{command_id = CId, state = St, entry = 1, data = {float, X}}) ->
                    [#token{state = St, data = {float, X}},
                     #token{state = St, data = {float, X}}]                                  

                end
                
            }
            
        },

        % Операция сравнения "<". Если условие верное, то на выходе "1", если не верное - "0".
        {
            "<",
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    case X < Y of
                        false -> #token{state = St, data = 0};
                        true -> #token{state = St, data = 1}
                    end                    
                end
            }

        }

    ].

