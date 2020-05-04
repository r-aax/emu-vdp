%% @doc
%% Буфер наборов токенов для команд, готовых к исполнению.

% Имя модуля.
-module(exe_buf).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([add/1, take/0,
         start/0,
         loop/1,
         test/0]).

%---------------------------------------------------------------------------------------------------
% Интерфейсные функции.
%---------------------------------------------------------------------------------------------------

-spec add(Tokens) -> ok
      when Tokens :: [vdp:token()].
%% @doc
%% Добавление набора токенов в буфер.
%%
%% @param Tokens Набор токенов.
%%
%% @return
%% Код успешного завершения ok.
add(Tokens) ->
    exe_buf ! {self(), {add, Tokens}},
    utils:receive_retranslate_ok_and_err(exe_buf).

%---------------------------------------------------------------------------------------------------

-spec take() -> {ok, [vdp:token()]} | ok.
%% @doc
%% Получение набора токенов для выполнения команды.
%%
%% @return
%% Набор токенов или просто ok, если буфер пуст.
take() ->
    exe_buf ! {self(), take},
    utils:receive_retranslate_ok_and_err(exe_buf).

%---------------------------------------------------------------------------------------------------
% Старт модуля.
%---------------------------------------------------------------------------------------------------

-spec start() -> pid().
%% @doc
%% Запуск процесса exe_buf.
%%
%% @returns
%% Идентификатор процесса.
start() ->
    Pid = spawn(?MODULE, loop, [[]]),
    register(exe_buf, Pid),
    Pid.

%---------------------------------------------------------------------------------------------------
% Бесконечный цикл.
%---------------------------------------------------------------------------------------------------

-spec loop(L) -> ok
      when L :: [[vdp:token()]].
%% @doc
%% Бесконечный цикл процесса, обрабатывающий сообщения.
%%
%% @param L Текущее состояние буфера.
%%
%% @private
loop(L) ->
    receive

        % Добавление набора токенов.
        {From, {add, Tokens}} ->
            NewL = L ++ [Tokens],
            From ! {exe_buf, ok};

        % Получение набора токенов.
        {From, take} ->
            case L of
                [] ->
                    NewL = L,
                    From ! {exe_buf, ok};
                [H | T] ->
                    NewL = T,
                    From ! {exe_buf, ?OK(H)}
            end;

        _ ->
            % Неизвестный тип сигнала просто игнорируется.
            NewL = L
    end,

    % Рекурсивный вызов бесконечного цикла.
    loop(NewL).

%---------------------------------------------------------------------------------------------------
% Тестирование модуля.
%---------------------------------------------------------------------------------------------------

-spec test() -> ok.
%% @doc
%% Тестирование модуля.
test() ->
    ok = add(a),
    ok = add(b),
    ok = add(c),
    ?OK(a) = take(),
    ?OK(b) = take(),
    ?OK(c) = take(),
    ok = take().

%---------------------------------------------------------------------------------------------------
