%% @doc
%% Модуль с общими сущностями, относящимися к векторному потоковому процессору.

% Имя модуля.
-module(vdp).

% Подключение файлов.
-include("defines.hrl").
-include("records.hrl").

% Экспорт типов данных.
-export_type([data_type/0,
               int_elem/0, float_elem/0, addr_elem/0, elem/0,
               int_vec/0, float_vec/0, addr_vec/0, vec/0,
               data/0,
               command_id/0, command_name/0, entry/0, command_dst/0, command/0,
               token_state/0, token/0,
               error/0]).

% Экспорт функций.
-export([default_data_elem/1]).

%---------------------------------------------------------------------------------------------------
% Типы данных.
%---------------------------------------------------------------------------------------------------

% Тип элемента данных, обрабатываемых процессором.
%% @type data_type() = int | float | addr.
-type data_type() :: int | float | addr.

% Целочисленный элемент данных.
%% @type int_elem() = {int, integer()}
-type int_elem() :: {int, integer()}.

% Вещественный элемент данных.
%% @type float_elem() = {float, float()}
-type float_elem() :: {float, float()}.

% Элемент типа адрес.
%% @type addr_elem() = {addr, integer()}
-type addr_elem() :: {addr, integer()}.

% Элемент данных, обрабатываемых процессором.
%% @type elem() = int_elem() | float_elem() | addr_elem().
-type elem() :: int_elem() | float_elem() | addr_elem().

% Целочисленный вектор.
%% @type int_vec() = [int_elem()].
-type int_vec() :: [int_elem()].

% Вещественный вектор.
%% @type float_vec() = [float_vec()].
-type float_vec() :: [float_vec()].

% Вектор адресов.
%% @type addr_vec() = [addr_elem()].
-type addr_vec() :: [addr_elem()].

% Вектор.
%% @type vec() = int_vec() | float_vec() | addr_vec().
-type vec() :: int_vec() | float_vec() | addr_vec().

% Данные (скаляр или вектор).
%% @type data() = elem() | vec().
-type data() :: elem() | vec().

% Идентификатор команды.
%% @type command_id() = integer().
-type command_id() :: integer().

% Имя команды (обычно идентифицируется атомом или строкой).
%% @type command_name() = term().
-type command_name() :: term().

% Номер входа команды.
%% @type entry() = integer().
-type entry() :: integer().

% Выходящая дуга из команды (назначение). Дуга может быть пустой.
%% @type command_dst() = none | {command_id(), entry_num()}.
-type command_dst() :: none | {command_id(), entry()}.

% Команда.
%% @type command() =
%%      #command
%%      {
%%          id = command_id(),
%%          name = command_name(),
%%          dsts = [command_dst()]
%%      }.
-type command() :: #command
        {
            id :: command_id(),
            name :: command_name(),
            dsts :: [command_dst()]
        }.

% Состояние токена.
% Не определяем конкретный вид состояния,
% состояние определяется семантикой набора команд.
%% @type token_state() = term().
-type token_state() :: term().

% Токен.
%% @type token() =
%%      #token
%%      {
%%          command_id = command_id(),
%%          state = token_state(),
%%          entry = entry(),
%%          data = data()
%%      }.
-type token() :: #token
        {
            command_id :: command_id(),
            state :: token_state(),
            entry :: entry(),
            data :: data()
        }.

% Ошибка.
%% @type error() = err | {err, string()}.
-type error() :: err | {err, string()}.

%---------------------------------------------------------------------------------------------------
% Функции.
%---------------------------------------------------------------------------------------------------

-spec default_data_elem(Type) -> vdp:data_type() | vdp:error()
      when Type :: vdp:data_type().
%% @doc
%% Создание элемента данных по умолчанию.
%%
%% @param
%% Type Элемент данных по умолчанию.
default_data_elem(int) ->
    {int, 0};
default_data_elem(float) ->
    {float, 0.0};
default_data_elem(addr) ->
    {addr, 0};
default_data_elem(_) ->
    ?ERR("unknown data type").

%---------------------------------------------------------------------------------------------------
