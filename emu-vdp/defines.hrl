%% @doc
%% Defines.

% Maximum length of the vector.
-define(MAX_VECTOR_LENGTH, 256).

%% @doc
%% State.
-record(state,
{
    command_id,
    generation,
    iteration,
    index
}).

%% @doc
%% Token.
-record(token,
{
    state,
    entry,
    data
}).
