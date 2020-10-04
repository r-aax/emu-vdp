%% @doc
%% Головной модуль.

% Имя модуля.
-module(main).

% Подключаемые файлы.
-include("defines.hrl").
-include("records.hrl").

% Экспорт функций.
-export([start/3,
         test/0,
         run/3]).

%---------------------------------------------------------------------------------------------------
% Вспомогательные функции.
%---------------------------------------------------------------------------------------------------

-spec find_command_by_id(G, Id) -> vdp:command()
      when G :: vdp:commands(),
           Id :: vdp:command_id().
%% @doc
%% Поиск команды в графе программы.
%%
%% Подразумевается, что в графе есть только одна команда с данным идентификатором.
%% Если оказывается иное, то генерируется исключение.
%%
%% @param G Граф программы.
%% @param Id Идентификатор команды.
%%
%% @returns
%% Команда.
find_command_by_id(G, Id) ->
    case lists:filter(fun(#command{id = LocId}) -> LocId =:= Id end, G) of
        [Cmd] ->
            Cmd;
        _ ->
            throw("can not find command in program graph")
    end.

%---------------------------------------------------------------------------------------------------

-spec find_command_semantic(S, C) -> vdp:command_semantic()
      when S :: vdp:commands_semantics(),
           C :: vdp:command().
%% @doc
%% Поиск семантики команды.
%%
%% Семантика команды обязательно должна быть описана, причем ровно один раз.
%% Если это нет так, то генерируется исключение.
%%
%% @param S Список семантик команд.
%% @param С Команда.
%%
%% @returns
%% Семантика команды.
find_command_semantic(S, C) ->
    case lists:filter(fun(#command_semantic{name = Name}) -> Name =:= C#command.name end,
                      S) of
        [Sem] ->
            Sem;
        _ ->
            throw("can not find command semantic in semantics list")
    end.

%---------------------------------------------------------------------------------------------------

-spec form_single_dst_out_tokens(R, Dst) -> vdp:tokens()
      when R :: vdp:token() | vdp:tokens(),
           Dst :: vdp:command_dst().
%% @doc
%% Генерация списка токенов для одного выхода.
%%
%% @param R Результат для одного выхода.
%% @param Dst Описание выхода.
%%
%% @returns
%% Список токенов для одного выхода.
form_single_dst_out_tokens(_, none) ->
    % Висячий выход, ничего не делаем.
    [];
form_single_dst_out_tokens(R, out) ->
    % Токен для вывода на экран.
    io:format("Out : ~p~n", [R]),
    [];
form_single_dst_out_tokens(R, {Id, Entry}) ->
    Rs =
        if
            is_list(R) ->
                R;
            true ->
                [R]
        end,
    [LocR#token{command_id = Id, entry = Entry} || LocR <- Rs].

%---------------------------------------------------------------------------------------------------

-spec form_out_tokens(R, Dsts) -> vdp:tokens()
      when R :: term(),
           Dsts :: [vdp:command_dst()].
%% @doc
%% Генерация выходных токенов по результату команды.
%%
%% Результат команды может принимать разные представления.
%% Если это просто один токен, то он должен быть раздублирован на все выходы команды.
%% Если это список токенов, то его длина должна соответствовать количеству выходов команды.
%%
%% @param R Результат выполнения команды.
%% @param Dsts Описание выходов команды.
%%
%% @returns
%% Список выходных токенов.
form_out_tokens(R, Dsts) ->
    L = length(Dsts),

    % Обрабатываем результаты команды.
    % Поддерживаем скалярные результаты - оборачиваем в список по числу выходов.
    % Проверяем попутно, что размерности количества результатов и количества выходов равны.
    Rs =
        if
            not is_list(R) ->
                lists:duplicate(L, R);
            L /= length(R) ->
                throw("wrong number of command results");
            true ->
                R
        end,

    % Обрабатываем результат для каждого выхода отдельно,
    % а затем сливаем все выходные токены в единый список.
    Out =
        lists:map
        (
            fun({LocR, LocDst}) ->
                form_single_dst_out_tokens(LocR, LocDst)
            end,
            lists:zip(Rs, Dsts)
        ),
    lists:flatten(Out).

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
%%
%% @returns
%% Код успешного завершения.
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
%%
%% В цикле может происходит занесение токенов с ассоциативную память,
%% либо обработка инструкции из буфера команд.
%%
%% @param S Семантика комманд.
%% @param G Граф программы.
%% @param T Список токенов.
%%
%% @private
loop(S, G, [#token{command_id = Id} = TH | TT]) ->

    % Ищем команду в графе и ее семантику.
    Cmd = find_command_by_id(G, Id),
    Sem = find_command_semantic(S, Cmd),

    % Пытаемся поместить токен в ast_mem.
    case ast_mem:set_token(TH, Sem#command_semantic.arity) of
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
            [#token{command_id = Id} | _] = Tokens,

            % Ищем команду в графе и ее семантику.
            Cmd = find_command_by_id(G, Id),
            Sem = find_command_semantic(S, Cmd),

            % Исполняем команду, а затем по ней формируем списов выходных токенов.
            Fun = Sem#command_semantic.function,
            R = Fun(Tokens),
            OutTokens = form_out_tokens(R, Cmd#command.dsts),

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
% Запуск.
%---------------------------------------------------------------------------------------------------

-spec run(SemanticName, GraphName, TokensName) -> ok
      when SemanticName :: string(),
           GraphName :: string(),
           TokensName :: string().
%% @doc
%% Запуск эмулятора.
%%
%% @return
%% Код успешного завершения ok.
run(SemanticName, GraphName, TokensName) ->

    % Атомы семантики, графа ии токенов.
    SemanticAtom = list_to_atom("semantic_" ++ SemanticName),
    GraphAtom = list_to_atom("graph_" ++ GraphName),
    TokensAtom = list_to_atom("tokens_" ++ TokensName),

    % Собираем нужные модули с семантикой, графом и токенами.
    c:cd("semantic"),
    c:c(SemanticAtom, [{i, ".."}]),
    c:cd("../graph"),
    c:c(GraphAtom, [{i, ".."}]),
    c:cd("../tokens"),
    c:c(TokensAtom, [{i, ".."}]),
    c:cd(".."),

    start(SemanticAtom:get_semantic_test(),
          GraphAtom:get_graph_test(),
          TokensAtom:get_tokens_test()).

%---------------------------------------------------------------------------------------------------
