
%% @doc
%% Память векторов.

% Имя модуля.
-module(semantic).

% Подключение файлов.
-include("records.hrl").

% Экспортируемые функции
-export([instructions_semantic/0], [test/0]).


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
            rem,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {int, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X rem Y}}
                end
            }

        }

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

        % ПУ - передача по условию
        {
            pu,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    case Y of
                        0 ->
                            [#token{state = St, data = {float, []}, 
                             #token{state = St, data = {float, X}}];
                        _ ->
                            [#token{state = St, data = {float, X}, 
                             #token{state = St, data = {float, []}}]
                    end                       
                end
            }
        },

        % ПДБ - передача с дублированием
        {
            pdb,
            {
                1,
                fun(#token{command_id = CId, state = St, entry = 1, data = {float, X}}) ->
                    [#token{state = St, data = {float, X}},
                     #token{state = St, data = {float, X}}]                                  

                end
                
            }
            
        },

        % Операция сравнения "<"
        {
            min,
            {
                2,
                fun([#token{command_id = CId, state = St, entry = 1, data = {float, X}},
                     #token{command_id = CId, state = St, entry = 2, data = {float, Y}}]) ->
                    case X < Y of ->
                        false -> #token{state = St, data = 0};
                        true -> #token{state = St, data = 1}
                    end                    
                end
            }

        }

    ].

