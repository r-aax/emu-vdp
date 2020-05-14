-module (sc_sem).

-include("defines.hrl").

-export([loop/0, start/0, result/2]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    register(sc_sem, Pid),
    Pid.

result(CmdName, {D1, D2}) ->
    sc_sem ! {self(), {result, CmdName, {D1, D2}}},
    utils:receive_retranslate_ok_and_err(result).

loop() ->

receive

{From, {result, CmdName, {D1, D2}}} ->
    
    case CmdName of
        siMul -> Res = D1 * D2;
        siAdd -> Res = D1 + D2;
        siSab -> Res = D1 - D2;
        siDiv -> Res = D1 / D2;
        siMod -> Res = D1 rem D2        
        %sfRnd -> Res = round D1
    end,

    From ! {result, ?OK(Res)}

end,

loop().

