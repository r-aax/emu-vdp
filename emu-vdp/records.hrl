%% @doc
%% Объявление записей.

% Команда.
-record(command,
{
    id :: vdp:command_id(),
    name :: vdp:command_name(),
    dsts :: [vdp:command_dst()]
}).

% Семантика команды.
-record(command_semantic,
{
    name :: vdp:command_name(),
    arity :: integer(),
    function :: fun()
}).

% Токен.
-record(token,
{
    command_id :: vdp:command_id(),
    state :: vdp:token_state(),
    entry :: vdp:entry(),
    data :: vdp:data()
}).
