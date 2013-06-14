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

-module(dictionary_prop_tests).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
	 precondition/2, postcondition/3]).
-export([sample_command/0]).

-record(state, {object = undefined :: integer(),
		words  = []        :: [{list(), integer()}]
}).

-define(SUT, dictionary).

%%==============================================================================
%% Test property
%%==============================================================================
prop_dictionary_is_working() ->
    ?FORALL(Cmds, commands(?MODULE),
	    ?TRAPEXIT(
	       begin
		   ?SUT:start(),
		   {History, State, Result} = run_commands(?MODULE, Cmds),
		   ?SUT:stop(),
		   ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
				       [History, State, Result]),
			     aggregate(command_names(Cmds), Result =:= ok))
	       end)).

%%==============================================================================
%% Statem callbacks
%%==============================================================================

initial_state() ->
    #state{object=undefined, words=[]}.

%% For the fresh state force a call to create the object instance
command(#state{object=undefined}) ->
    {call, ?SUT, new, []};
command(#state{object=Object}) ->
    oneof([{call, ?SUT, size, [Object]},
	   {call, ?SUT, reference, [Object, word()]},
	   {call, ?SUT, unreference, [Object, word()]},
	   {call, ?SUT, count, [Object, word()]}
]).

%% Generators
word() ->
    oneof(["the", "be", "to", "of", "and", "a", "in", "that",
	   "any", "these", "give", "day", "most", "us"]).

%%==============================================================================
%% precondition with default behavior
%%==============================================================================
precondition(_State, _Cmd) ->
    true.

%%==============================================================================
%% next_state for new
%%==============================================================================
next_state(State, V, {call, ?SUT, new, []}) ->
    %% Update the state with the value for the object pointer
    State#state{object=V};

%%==============================================================================
%% next_state for reference
%%==============================================================================
next_state(#state{words=Words}=State, _V, 
	   {call, ?SUT, reference, [_Object, Word]}) ->
    %% Update the state with the reference of this word
    State#state{words=reference_word(Words, Word)};

%%==============================================================================
%% next_state for unreference
%%==============================================================================
next_state(#state{words=Words}=State, _V,
	   {call, ?SUT, unreference, [_Object, Word]}) ->
    %% Update the state with the reference of this word
    State#state{words=unreference_word(Words, Word)};

%%==============================================================================
%% next_state with default behavior
%%==============================================================================
next_state(State, _V, _Cmd) ->
    State.

%%==============================================================================
%% postcondition for new
%%==============================================================================
postcondition(_State, {call, ?SUT, new, []}, Result) ->
    %% Just check that we got an integer back
    is_integer(Result);

%%==============================================================================
%% postcondition for size
%%==============================================================================
postcondition(#state{words=Words}, {call, ?SUT, size, [_Object]}, Result) ->
    %% Check that the SUT's opinion of its size is matching the
    %% opinion of the model
    is_integer(Result) andalso length(Words) =:= Result;

%%==============================================================================
%% postcondition for reference
%%==============================================================================
postcondition(_State, {call, ?SUT, reference, [_Object, _Word]}, Result) ->
    %% Just check that the reply is the atom 'ok'
    ok =:= Result;

%%==============================================================================
%% postcondition for unreference
%%==============================================================================
postcondition(_State, {call, ?SUT, unreference, [_Object, _Word]}, Result) ->
    %% Just check that the reply is the atom 'ok'
    ok =:= Result;

%%==============================================================================
%% postcondition for count
%%==============================================================================
postcondition(#state{words=Words}, 
	      {call, ?SUT, count, [_Object, Word]}, Result) ->
    %% The SUT shall tell the same number of counts as the model do
    count(Words, Word) =:= Result.

%%==============================================================================
%% Helper functions
%%==============================================================================

sample_command() ->
    proper_gen:sample(command(#state{object=123456})).

reference_word(Words, Word) ->
    case proplists:lookup(Word, Words) of
	none          -> [{Word, 1}|Words];
	{Word, Count} -> [{Word, Count+1}|proplists:delete(Word, Words)]
    end.

unreference_word(Words, Word) ->
    case proplists:lookup(Word, Words) of
	none          -> Words;
	{Word, 1}     -> proplists:delete(Word, Words);
	{Word, Count} -> [{Word, Count-1}|proplists:delete(Word, Words)]
    end.

count(Words, Word) ->
    case proplists:lookup(Word, Words) of
	none          -> 0;
	{Word, Count} -> Count
    end.
	    
