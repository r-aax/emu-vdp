%% @doc
%% Память векторов.

% Имя модуля.
-module(vec_mem).

% Подключение файлов.
-include("defines.hrl").

% Экспортируемые функции.
-export([new_vec/2,
         get_vec/1, get_vecs/0,
         del_vec/1, del_vecs/0,
         get_elem/2, set_elem/3,
         start/0, test/0,
         loop/0]).

%---------------------------------------------------------------------------------------------------
% Интерфейсные функции.
%---------------------------------------------------------------------------------------------------

-spec new_vec(Type, Len) -> {ok, vdp:addr()} | vdp:error()
      when Type :: vdp:data_type(),
           Len :: integer().
%% @doc
%% Создание нового вектора.
%%
%% @param Type Тип вектора.
%% @param Len Необходимая длина вектора.
%%
%% @returns
%% Адрес созданного вектора.
new_vec(Type, Len) ->
    vec_mem ! {self(), {new_vec, Type, Len}},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------

-spec get_vec(Addr) -> {ok, vdp:vec()} | vdp:error()
      when Addr :: vdp:addr().
%% @doc
%% Получение вектора.
%%
%% @param Addr Адрес вектора.
%%
%% @returns
%% Вектор.
get_vec(Addr) ->
    vec_mem ! {self(), {get_vec, Addr}},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------

-spec get_vecs() -> {ok, [vdp:vec()]}.
%% @doc
%% Выдача списка векторов в памяти векторов.
%%
%% @returns
%% Список векторов в памяти векторов.
get_vecs() ->
    vec_mem ! {self(), get_vecs},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------

-spec del_vec(Addr) -> ok | vdp:error()
      when Addr :: vdp:addr().
%% @doc
%% Удаление вектора.
%%
%% @param Addr Адрес вектора.
%%
%% @returns
%% Код успешного завершения ok или сообщение об ошибке.
del_vec(Addr) ->
    vec_mem ! {self(), {del_vec, Addr}},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------

-spec del_vecs() -> ok.
%% @doc
%% Удаление всех векторов.
%%
%% @return
%% Код успешного завершения ok.
del_vecs() ->
    vec_mem ! {self(), del_vecs},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------

-spec get_elem(Addr, I) -> {ok, vdp:elem()} | vdp:error()
      when Addr :: vdp:addr(),
           I :: integer().
%% @doc
%% Выдача элемента вектора.
%%
%% @param Addr Адрес вектора.
%% @param I Номер элемента.
%%
%% @returns
%% Необходимый элемент вектора.
get_elem(Addr, I) ->
    vec_mem ! {self(), {get_elem, Addr, I}},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------

-spec set_elem(Addr, I, V) -> ok | vdp:error()
      when Addr :: vdp:addr(),
           I :: integer(),
           V :: vdp:elem().
%% @doc
%% Установка элемента вектора.
%%
%% @param Addr Адрес вектора.
%% @param I Номер элемента.
%% @param V Новый элемент.
%%
%% @return
%% Код успешного завершения ok или сообщение об ошибке.
set_elem(Addr, I, V) ->
    vec_mem ! {self(), {set_elem, Addr, I, V}},
    utils:receive_retranslate_ok_and_err(vec_mem).

%---------------------------------------------------------------------------------------------------
% Старт модуля.
%---------------------------------------------------------------------------------------------------

-spec start() -> pid().
%% @doc
%% Запуск процесса vec_mem.
%%
%% @returns
%% Идентификатор процесса.
start() ->
    Pid = spawn(?MODULE, loop, []),
    register(vec_mem, Pid),
    Pid.

%---------------------------------------------------------------------------------------------------
% Служебные функции.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Бесконечный цикл обработки команд.
%---------------------------------------------------------------------------------------------------

-spec loop() -> ok.
%% @doc
%% Бесконечный цикл процесса, обрабатывающий сообщения.
%%
%% @private
loop() ->    
    receive

        % Новый вектор.
        {From, {new_vec, Type, Len}} ->
            Addrs = get_keys(),
            NewAddr = utils:first_free_nonnegative(lists:sort(Addrs)),
            if
                Len =< 0 ->
                    From ! {vec_mem, ?ERR("wrong vector length")};
                true ->
                    DDE = vdp:default_data_elem(Type),
                    case DDE of
                        ?ERR(_) ->
                            From ! {vec_mem, DDE};
                        _ ->
                            NewVec = lists:duplicate(Len, DDE),
                            put(NewAddr, NewVec),
                            From ! {vec_mem, ?OK(NewAddr)}
                    end
            end;

        % Получение вектора.
        {From, {get_vec, Addr}} ->
            case get(Addr) of
                undefined ->
                    From ! {vec_mem, ?ERR("no vector")};
                Vec ->
                    From ! {vec_mem, ?OK(Vec)}
            end;

        % Получние всех векторов.
        {From, get_vecs} ->
            Vecs = get(),
            From ! {vec_mem, ?OK(Vecs)};

        % Удаление вектора.
        {From, {del_vec, Addr}} ->
            case get(Addr) of
                undefined ->
                    From ! {vec_mem, ?ERR("no vector")};
                _ ->
                    erase(Addr),
                    From ! {vec_mem, ok}
            end;

        % Удаление всех векторов.
        {From, del_vecs} ->
            erase(),
            From ! {vec_mem, ok};

        % Выдача элемента вектора.
        {From, {get_elem, Addr, I}} ->
            case get(Addr) of
                undefined ->
                    From ! {vec_mem, ?ERR("no vector")};
                Vec ->
                    Len = length(Vec),
                    if
                        (I < 0) orelse (I >= Len) ->
                            From ! {vec_mem, ?ERR("wrong index")};
                        true ->
                            Elem = lists:nth(I + 1, Vec),
                            From ! {vec_mem, ?OK(Elem)}
                    end
            end;

        %Установка эелемента вектора.
        {From, {set_elem, Addr, I, V}} ->
            case get(Addr) of
                undefined ->
                    From ! {vec_mem, ?ERR("no vector")};
                Vec ->
                    Len = length(Vec),
                    {Type0, _} = lists:nth(1, Vec),
                    if
                        (I < 0) orelse (I >= Len) ->
                            From ! {vec_mem, ?ERR("wrong index")};
                        true ->
                            case V of
                                {Type0, _} ->
                                    {FList, [_ | SList]} = lists:split(I, Vec),
                                    NewVec = FList ++ [V] ++ SList,
                                    put(Addr, NewVec),
                                    From ! {vec_mem, ok};
                                _ ->
                                    From ! {vec_mem, ?ERR("wrong new value")}
                            end
                    end
            end;

        _ ->
            % Неизвестный тип сигнала просто игнорируется.
            ok
    end,

    % Бесконечный цикл.
    loop().

%---------------------------------------------------------------------------------------------------
% Тестирование.
%---------------------------------------------------------------------------------------------------

-spec test() -> ok.
%% @doc
%% Тестирование модуля.
%%
%% @return
%% Код успешного завершения ok.
test() ->

    % Создание новых векторов.
    ok = del_vecs(),
    ?ERR("wrong vector length") = new_vec(int, -5),
    ?OK(0) = new_vec(int, 2),
    ?OK(1) = new_vec(float, 3),
    ?OK(2) = new_vec(addr, 4),
    ?ERR("unknown data type") = new_vec(unknown, 5),

    % Получение вектора.
    ok = del_vecs(),
    ?OK(0) = new_vec(int, 5),
    ?OK(_) = get_vec(0),
    ?ERR("no vector") = get_vec(1),

    % Получение и установка элемента вектора.
    ok = del_vecs(),
    ?OK(0) = new_vec(int, 5),
    ?ERR("no vector") = set_elem(1, 3, 5),
    ?ERR("wrong index") = set_elem(0, 10, 5),
    ?ERR("wrong new value") = set_elem(0, 3, {float, 5.0}),
    ok = set_elem(0, 3, {int, 5}),
    ?OK({int, 5}) = get_elem(0, 3),

    % Удаление векторов.
    ok = del_vecs().

%---------------------------------------------------------------------------------------------------
