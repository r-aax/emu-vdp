%% @doc
%% Ассоциативная память.

% Имя модуля.
-module(cam_mem).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспортируемые функции.
-export([get_tokens/0, set_token/2, del_tokens/0,
         start/0,
         loop/0,
         test/0]).

%---------------------------------------------------------------------------------------------------
% Интерфейсные функции.
%---------------------------------------------------------------------------------------------------

-spec get_tokens() -> {ok, [vdp:token()]}.
%% @doc
%% Выдача списка всех токенов.
%%
%% @returns
%% Список всех токенов.
get_tokens() ->
    cam_mem ! {self(), get_tokens},
    utils:receive_retranslate_ok_and_err(cam_mem).

%---------------------------------------------------------------------------------------------------

-spec set_token(Token, Args) -> ok | {ok, [vdp:token()]} | vdp:error()
      when Token :: vdp:token(),
           Args :: integer().
%% @doc
%% Запись токена
%%
%% @param Token Токен.
%% @param Args Количество аргументов команды.
%%
%% @return
%% Пара токенов, сообщение о записи токена, либо сообщение об ошибке.
set_token(Token, Args) ->
    cam_mem ! {self(), {set_token, Token, Args}},
    utils:receive_retranslate_ok_and_err(cam_mem).

%---------------------------------------------------------------------------------------------------

-spec del_tokens() -> ok.
%% @doc
%% Удаление всех токенов.
%%
%% @return
%% Статус успешного завершения ok.
del_tokens() ->
    cam_mem ! {self(), del_tokens},
    utils:receive_retranslate_ok_and_err(cam_mem).

%---------------------------------------------------------------------------------------------------
% Старт модуля.
%---------------------------------------------------------------------------------------------------

-spec start() -> pid().
%% @doc
%% Запуск процесса cam_mem.
%%
%% @returns
%% Идентификатор процесса.
start() ->
    Pid = spawn(?MODULE, loop, []),
    register(cam_mem, Pid),
    Pid.

%---------------------------------------------------------------------------------------------------
% Бесконечный цикл.
%---------------------------------------------------------------------------------------------------

-spec loop() -> ok.
%% @doc
%% Бесконечный цикл процесса, обрабатывающий сообщения.
%%
%% @private
loop() ->
    receive

        % Получение всех токенов.
        {From, get_tokens} ->
            Tokens = get(),
            From ! {cam_mem, ?OK(Tokens)};

        % Занесение токена.
        {From, {set_token, Token, Args}} ->

            % Ключом в таблице является номер команды и состояние токена.
            Key = {Token#token.command_id, Token#token.state},

            % Получаем старый и новый список токенов.
            OldTokens =
                case get(Key) of
                    undefined ->
                        [];
                    ReallyOldTokens ->
                        ReallyOldTokens
                end,
            OldEntries = lists:sort([X#token.entry || X <- OldTokens]),
            NewTokens = lists:sort(fun(X, Y) -> X#token.entry < Y#token.entry end, [Token | OldTokens]),
            Entry = Token#token.entry,
            NewEntries = lists:sort([Entry | OldEntries]),

            % Проверяем, что номера входов не дублируются.
            % Также получаем канонический список номеров входов для команды.
            IsDuplicate = lists:any(fun(X) -> X =:= Entry end, OldEntries),

            if
                (Entry =< 0) orelse (Entry > Args) ->
                    From ! {cam_mem, ?ERR("wrong entry number")};
                IsDuplicate ->
                    From ! {cam_mem, ?ERR("duplicate entry number")};
                true ->
                    CanonEntries = lists:seq(1, Args, 1),
                    if
                        NewEntries =:= CanonEntries ->
                            erase(Key),
                            From ! {cam_mem, ?OK(NewTokens)};
                        true ->
                            put(Key, NewTokens),
                            From ! {cam_mem, ok}
                    end
            end;

        % Удаление всех токенов.
        {From, del_tokens} ->
            erase(),
            From ! {cam_mem, ok};

        _ ->
            % Неизвестный тип сигнала просто игнорируется.
            ok
    end,

    % Рекурсивный вызов бесконечного цикла.
    loop().

%---------------------------------------------------------------------------------------------------
% Тестирование модуля.
%---------------------------------------------------------------------------------------------------

-spec test() -> ok.
%% @doc
%% Тестирование модуля.
test() ->

    % Тестовые идентификатор команды и состояние.
    Id = 1,
    St = test_state,
    Dt = test_data,
    T1 = #token{command_id = Id, state = St, entry = 1, data = Dt},
    T2 = #token{command_id = Id, state = St, entry = 2, data = Dt},
    T3 = #token{command_id = Id, state = St, entry = 3, data = Dt},

    % Тест 1.
    % Добавление токенов для команды из трех аргументов.
    ok = del_tokens(),
    ok = set_token(T1, 3),
    ok = set_token(T2, 3),
    ?OK([T1, T2, T3]) = set_token(T3, 3),
    ?OK([]) = get_tokens(),

    % Тест 2.
    % Добавление токенов для команды в другом порядке.
    ok = del_tokens(),
    ok = set_token(T3, 3),
    ok = set_token(T1, 3),
    ?OK([T1, T2, T3]) = set_token(T2, 3),
    ?OK([]) = get_tokens(),

    % Тест 3.
    % Попытка добавить токен со слишком большим номером входа.
    ok = del_tokens(),
    ?ERR("wrong entry number") = set_token(T3, 2),
    ok = del_tokens(),

    % Тест 4.
    % Дублирование номера входа.
    ok = del_tokens(),
    ok = set_token(T1, 3),
    ?ERR("duplicate entry number") = set_token(T1, 3),
    ok = del_tokens(),

    ok = del_tokens().

%---------------------------------------------------------------------------------------------------
