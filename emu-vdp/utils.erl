%% @doc
%% Реализация вспомогательных функций.

% Имя модуля.
-module(utils).

% Подключение дефайнов.
-include("defines.hrl").

% Экспортируемые функции.
-export([first_free_nonnegative/1,
         receive_retranslate_ok_and_err/1]).

%---------------------------------------------------------------------------------------------------
% Функции.
%---------------------------------------------------------------------------------------------------

-spec first_free_nonnegative(L) -> integer()
      when L :: [integer()].
%% @doc
%% Поиск первого свободного неотрицательного значения,
%% не входящего в заданный список.
%%
%% @param L Список занятых значений.
%%
%% @returns
%% Первое свободное неотрицательное значение.
first_free_nonnegative(L) ->
    first_free_nonnegative(L, 0).

-spec first_free_nonnegative(L, N) -> integer()
      when L :: [integer()],
           N :: integer().
%% @doc
%% Поиск первого свободного неотрицательного значения,
%% не входящего в заданный список.
%%
%% @param L Список занятых значений.
%% @param N Текущее проверяемое значение.
%%
%% @returns
%% Первое свободное неотрицательное значение.
%%
%% @private
first_free_nonnegative([], N) ->
    N;
first_free_nonnegative([H | T], N) ->
    if
        N < H ->
            N;
        N =:= H ->
            first_free_nonnegative(T, N + 1)
    end.

%---------------------------------------------------------------------------------------------------

-spec receive_retranslate_ok_and_err(From) -> ok | {ok, term()} | err | {err, term()}
      when From :: pid() | atom().
%% @doc
%% Ретрансляция сообщений, обернутых в OK или ERR.
%%
%% @param From Процесс.
%%
%% @returns Ретранслированное значение.
receive_retranslate_ok_and_err(From) ->
    receive
        {From, ?OK(V)} ->
            ?OK(V);
        {From, ok} ->
            ok;
        {From, ?ERR(V)} ->
            ?ERR(V);
        {From, err} ->
            err
    end.

%---------------------------------------------------------------------------------------------------
