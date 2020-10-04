%% @doc
%% Граф test.

% Имя модуля.
-module(graph_test).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([get_graph_test/0]).

%---------------------------------------------------------------------------------------------------

-spec get_graph_test() -> [vdp:command()].
%% @doc
%% Создание графа программы test.
%%
%% @returns
%% Граф программы test.
get_graph_test() ->
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
        #command{id = 10, name =  dvs, dsts = [out]},
        #command{id = 11, name =  dvs, dsts = [out]}
    ].

%---------------------------------------------------------------------------------------------------
