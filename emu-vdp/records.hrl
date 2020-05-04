%% @doc
%% Объявление записей.

% Команда.
-record(command,
{
    id :: vdp:command_id(),
    name :: vdp:command_name(),
    dsts :: [vdp:command_dst()]
}).

% Токен.
-record(token,
{
    command_id :: vdp:command_id(),
    state :: vdp:token_state(),
    entry :: vdp:entry(),
    data :: vdp:data()
}).
