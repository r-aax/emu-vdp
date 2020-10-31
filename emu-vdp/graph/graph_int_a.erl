% Имя модуля.
-module(graph_add).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([get_graph_add/0]).

get_graph_add() ->
    [
        #command{id = 1, name = iadd, dsts = [{2, 1}]},
        #command{id = 2, name = isub, dsts = [{3, 1}]},
        #command{id = 3, name = imul, dsts = [{4, 2}]},
        #command{id = 2, name = isub, dsts = [{4, 1}]},
        #command{id = 4, name = mod, dsts = [out]}
        
    ].