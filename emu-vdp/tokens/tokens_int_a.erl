% Имя модуля.
-module(tokens_int_a).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([get_tokens_int_a/0]).


get_tokens_int_a() ->

   A = 1,
   B = -3,
   C = 2,

    [
        
        #token{command_id = 1, state = default, entry = 1, data = {int, 3}},
        #token{command_id = 1, state = default, entry = 2, data = {int, B}},
        #token{command_id = 2, state = default, entry = 2, data = {int, A}},
        #token{command_id = 3, state = default, entry = 2, data = {int, 4}},
        #token{command_id = 4, state = default, entry = 2, data = {int, C}}
        %#token{command_id = 4, state = default, entry = 1, data = {int, 4}}
       
       
       
      ].
