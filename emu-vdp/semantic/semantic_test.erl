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

        %Арифметические скалярные команды

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
        },

        #command_semantic
        {
            name = iadd,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X + Y}}
                end
        },

        #command_semantic
        {
            name = idvs,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X div Y}}
                end
        },

        #command_semantic
        {
            name = imul,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X * Y}}
                end
        },

        #command_semantic
        {
            name = isub,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X - Y}}
                end
        },

        #command_semantic
        {
            name = mod,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X rem Y}}
                end
        },


        #command_semantic
        {
            name = rnd,
            arity = 1,
           function =
                fun([#token{state = St, entry = 1, data = {float, X}}]) ->
                    #token{state = St, data = {int, round (X)}}
                end
        },

        #command_semantic
        {
            name = int_to_float,
            arity = 1,
            function =
                fun([#token{state = St, entry = 1, data = {int, X}}]) ->
                    #token{state = St, data = {float, float (X)}}
                end            
        },

        %Арифметический сдвиг влево
        
        #command_semantic
        {   
            name = arshftl,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X bsl Y}}
                end
        },

        %Логический сдвиг вправо
        #command_semantic
        {
            name = arshftr,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X bsr Y}}
                end
        },

        %Логические арифметические команды
        
        %Логическое ИЛИ
        #command_semantic
        {
            name = lor,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X or Y}}
                end
        },

        %Логическое И
        #command_semantic
        {
            name = land,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X and Y}}
                end
        },

        %Логическое отрицание
        #command_semantic
        {
            name = lnot,
            arity = 1,
            function =
                fun([#token{state = St, entry = 1, data = {int, X}}]) ->
                    #token{state = St, data = {int, not X}}
                end
        },

        %Определение отношений

        %Больше целочисленное
        #command_semantic
        {
            name = imor,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X > Y}}
                end
        },

        %Больше или равно целочисленное
        #command_semantic
        {
            name = imoreql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X >= Y}}
                end
        },

        %Равно целочисленное
        #command_semantic
        {
            name = ieql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X =:= Y}}
                end
        },

        %Не равно целочисленное
        #command_semantic
        {
            name = inoeql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X =/= Y}}
                end
        },

        %Меньше или равно целочисленное
        #command_semantic
        {
            name = ileseql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X =< Y}}
                end
        },

        %Меньше целочисленное
        #command_semantic
        {
            name = iles,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {int, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {int, Y}}]) ->
                    #token{state = St, data = {int, X < Y}}
                end
        },

        %Больше
        #command_semantic
        {
            name = mor,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X > Y}}
                end
        },

        %Больше или равно
        #command_semantic
        {
            name = moreql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X >= Y}}
                end
        },

        %Равно
        #command_semantic
        {
            name = moreql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X =:= Y}}
                end
        },

        %Не равно
        #command_semantic
        {
            name = noeql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X =/= Y}}
                end
        },

        %Меньше или равно
        #command_semantic
        {
            name = leseql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X =< Y}}
                end
        },

        %Меньше
        #command_semantic
        {
            name = moreql,
            arity = 2,
            function =
                fun([#token{command_id = Id, state = St, entry = 1, data = {float, X}},
                     #token{command_id = Id, state = St, entry = 2, data = {float, Y}}]) ->
                    #token{state = St, data = {float, X >= Y}}
                end
        },

        %Команды передачи
        
        %Передача с дублированием
        #command_semantic
        {
            name = trandubl,
            arity = 1,
            function =
                fun([#token{state = St, entry = 1, data = {int, X}}]) ->
                    {#token{state = St, data = {int, X}}, 
                    #token{state = St, data = {int, X}}}
                end
        }






        
    ].

%---------------------------------------------------------------------------------------------------
