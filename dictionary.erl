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

-module(dictionary).

%% API to start and stop the C++ communicator itself
-export([start/0, stop/0]).

%% The API for the system under test C++ module
-export([create/0, size/1, reference/2, unreference/2, count/2]).

%% Define the path to the external program which will wrap the system
%% under test C++ module
-define(ExtProg, "./TestMain").
-define(CppComm, cpp_comm).

start() ->
    ?CppComm:start_link(?ExtProg).

stop() ->
    ?CppComm:stop().

%% Create a new instance of the dictionary
create() ->
    ?CppComm:call({create}).

%% Ask a Dictionary object about it's size
size(Object) ->
    ?CppComm:call({size, Object}).

%% Register a word for reference in the dictionary. Update the
%% reference count if already registered
reference(Object, Word) ->
    ?CppComm:call({reference, Object, list_to_binary(Word)}).

%% Unreference a word in the dictionary. If the reference count
%% reaches zero the work shall be removed
unreference(Object, Word) ->
    ?CppComm:call({unreference, Object, list_to_binary(Word)}).

%% Get the reference count for a word in the dictionary
count(Object, Word) ->
    ?CppComm:call({count, Object, list_to_binary(Word)}).
