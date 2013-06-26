%% Module and function names may have been converted to lower case
-module(dictionary).

%% API to start and stop the C++ communicator
-export([start/0,stop/0]).

%% API for the system under test
-export([create/0,size/1,reference/2,unreference/2,count/2]).

%% Handy macros for C++ communication
-define(ExtProg, "./TestMain").
-define(CppComm, cpp_comm).

%% Functions for start and stop
start() ->
    ?CppComm:start_link(?ExtProg).
stop() ->
    ?CppComm:stop().

%% StaticDecl "Dictionary" "create" [Ptr (UserDef "Dictionary")]
create() ->
    ?CppComm:call({create}).

%% MethodDecl "size" [Ptr (UserDef "Dictionary"),Value Integer]
size(Arg1) ->
    ?CppComm:call({size,Arg1}).

%% MethodDecl "reference" [Ptr (UserDef "Dictionary"),Value String,Value Void]
reference(Arg1,Arg2) ->
    ?CppComm:call({reference,Arg1,list_to_binary(Arg2)}).

%% MethodDecl "unreference" [Ptr (UserDef "Dictionary"),Value String,Value Void]
unreference(Arg1,Arg2) ->
    ?CppComm:call({unreference,Arg1,list_to_binary(Arg2)}).

%% MethodDecl "count" [Ptr (UserDef "Dictionary"),Value String,Value Integer]
count(Arg1,Arg2) ->
    ?CppComm:call({count,Arg1,list_to_binary(Arg2)}).

