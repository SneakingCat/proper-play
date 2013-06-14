%% Copyright (C) 2013  Patrik Sandahl
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(cpp_comm).

-export([start_link/1, stop/0, init/2]).
-export([call/1]).

-define(ExtProg, "./TestMain").

start_link(ExtProg) ->
    proc_lib:start_link(?MODULE, init, [self(), ExtProg]).

stop() ->
    ?MODULE ! stop.

%% Once started the call function is the genering entry towards the
%% system under test C++ module. All commands, on the top level, to
%% the system under test are expected to be tuples.
call(Msg)
  when is_tuple(Msg) ->
    rpc_call_port(Msg).
	
init(Parent, ExtProg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtProg}, [{packet, 2}, binary]),
    proc_lib:init_ack(Parent, ok),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg}    ->
	    handle_call(Port, Caller, Msg),
	    loop(Port);
	stop                   ->
	    handle_stop(Port);
	{'EXIT', Port, Reason} ->
	    exit(Reason);
	_Anything              ->
	    loop(Port)
    end.

handle_stop(Port) ->
    Port ! {self(), close},
    receive
	{Port, closed} ->
	    exit(normal)
    end.

handle_call(Port, Caller, Msg) ->
    Port ! {self(), {command, term_to_binary(Msg)}},
    receive
	{Port, {data, Data}} ->
	    Caller ! {reply, binary_to_term(Data)}
    end.

rpc_call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
	{reply, Result} ->
	    Result
    end.
    
