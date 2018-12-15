%/--------------------------------------------------------------------
%| Copyright 2018 Erisata, UAB (Ltd.)
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%% @doc
%%% This module provides the interface to the axb_limit application.
%%%
-module(axb_limit).
-export([start/0]).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  Convenient function to start this application from
%%  the command line (`erl -s axb_limit').
%%
start() ->
    application:ensure_all_started(?MODULE, permanent).


