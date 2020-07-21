%% @doc
%% Головной модуль.

% Имя модуля.
-module(main).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспорт функций.
-export([start/3,
         test/0]).

%---------------------------------------------------------------------------------------------------
% Старт модуля.
%---------------------------------------------------------------------------------------------------

-spec start(S, G, T) -> ok
      when S :: vdp:commands_semantics(),
           G :: vdp:commands(),
           T :: vdp:tokens().
%% @doc
%% Старт головного модуля.
%%
%% @param S Семантика комманд.
%% @param G Граф программы.
%% @param T Список токенов.
start(S, G, T) ->
    io:format("main:start() : begin~n"),

    % Запускаем процессы.
    vec_mem:start(),
    ast_mem:start(),
    exe_buf:start(),

    % Запускаем основной цикл.
    loop(S, G, T),

    io:format("main:start() : end~n").

%---------------------------------------------------------------------------------------------------
% Цикл работы с токенами.
%---------------------------------------------------------------------------------------------------

-spec loop(S, G, T) -> ok
      when S :: vdp:commands_semantics(),
           G :: vdp:commands(),
           T :: vdp:tokens().
%% @doc
%% Цикл для работы с токенами.
%% В цикле может происходит занесение токенов с ассоциативную память,
%% либо обработка инструкции из буфера команд.
%%
%% @param S Семантика комманд.
%% @param G Граф программы.
%% @param T Список токенов.
%%
%% @private
loop(S, G, [#token{command_id = CId} = TH | TT]) ->

    % Ищем команду в графе.
    [Cmd] = lists:filter(fun(#command{id = Id}) -> Id =:= CId end, G),

    % Ищем ее семантику.
    Name = Cmd#command.name,
    {value, {Name, {Args, _Fun}}} = lists:keysearch(Name, 1, S),

    % Пытаемся поместить токен в ast_mem.
    case ast_mem:set_token(TH, Args) of
        ok ->
            % Токен лег в ассоциативную память.
            ok;
        ?OK(Tokens) ->
            % Нашли комплектт на исполнение, отправляем в буфер.
            exe_buf:add(Tokens)
    end,

    % Продолжаем работу.
    loop(S, G, TT);

loop(S, G, []) ->

    % Больше не осталось токенов для добавления.
    % Берем на исполнение набор токенов из буфера.
    case exe_buf:take() of

        ok ->
            % Буфер пустой, завершаем работу.
            ok;

        ?OK(Tokens) ->
            % Достали набор токенов для выполнения инструкции.

            % Достаем идентификатор команды.
            [#token{command_id = CId} | _] = Tokens,

            % Ищем команду в графе.
            [Cmd] = lists:filter(fun(#command{id = Id}) -> Id =:= CId end, G),

            % Ищем ее семантику.
            Name = Cmd#command.name,
            {value, {Name, {_Args, Fun}}} = lists:keysearch(Name, 1, S),

            % Исполняем команду.
            OutTokenTemplate = Fun(Tokens),

            % Формируем выходные токены.
            OutTokens =
                [
                    OutTokenTemplate#token{command_id = OutCId, entry = Entry} 
                    ||
                    {OutCId, Entry} <- Cmd#command.dsts
                ],

            % Если выходных токенов нет, то печатаем на экран шаблон (это финальные данные).
            if
                OutTokens =:= [] ->
                    io:format("Terminal data : ~p~n", [OutTokenTemplate]);
                true ->
                    ok
            end,

            % Продолжаем работу с новыми токенами.
            loop(S, G, OutTokens)
    end.

%---------------------------------------------------------------------------------------------------
% Тестирование.
%---------------------------------------------------------------------------------------------------

-spec test() -> ok.
%% @doc
%% Тестирование эмулятора.
%%
%% @return
%% Код успешного завершения ok.
test() ->

    % Тестирование vec_mem.
    vec_mem:start(),
    vec_mem:test(),
    io:format("Testing of vec_mem is done.~n"),

    % Тестирование ast_mem.
    ast_mem:start(),
    ast_mem:test(),
    io:format("Testing of ast_mem is done.~n"),

    % Тестирование exe_buf.
    exe_buf:start(),
    exe_buf:test(),
    io:format("Testing of exe_buf is done.~n"),

    ok.

%---------------------------------------------------------------------------------------------------
