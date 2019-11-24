%% @doc
%% Commands emulation.

% Module name.
-module(emu).

-include("defines.hrl").

-export([iadd/2, isub/2, imul/2, idiv/2, rem_/2, shl/2]).

%---------------------------------------------------------------------------------------------------
% General functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Arithmetic with two arguments.
arithmetic_2(D1, D2, F) ->
    R = F(D1, D2),
    [
        [R],
        [R]
    ].

%---------------------------------------------------------------------------------------------------
% Integer functions.
%---------------------------------------------------------------------------------------------------

%% @doc
%% Arithmetic integer instructions with two arguments.
integer_arithmetic_2(D1, D2, F) when is_integer(D1) and is_integer(D2) ->
    arithmetic_2(D1, D2, F).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Adding of integers IADD (СЛЦ).
iadd(D1, D2) ->
    integer_arithmetic_2(D1, D2, fun(X, Y) -> X + Y end).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Subtraction of integers ISUB (ВЧЦ).
isub(D1, D2) ->
    integer_arithmetic_2(D1, D2, fun(X, Y) -> X - Y end).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Multiplication of integers IMUL (УМЦ).
imul(D1, D2) ->
    integer_arithmetic_2(D1, D2, fun(X, Y) -> X * Y end).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Division of integers IDIV (ДЦ).
idiv(D1, D2) ->
    integer_arithmetic_2(D1, D2, fun(X, Y) -> X div Y end).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Reminder REM_ (МД).
rem_(D1, D2) ->
    integer_arithmetic_2(D1, D2, fun(X, Y) -> X rem Y end).

%---------------------------------------------------------------------------------------------------

%% @doc
%% Arithmetic shift SHL (СДА).
shl(D1, D2) ->
    integer_arithmetic_2(D1, D2, fun(X, Y) -> X bsl Y end).

%---------------------------------------------------------------------------------------------------