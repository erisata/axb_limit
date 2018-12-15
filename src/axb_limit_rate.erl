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
%%% Limits the caller to the specified rate.
%%%
%%% Example:
%%% ````
%%% axb_limit_rate:start_link({local, my_limiter}, 3). % We want 3 tps max.
%%% axb_limit_rate:await(my_limiter, my_caller). % Can block for some time, if limit is reached.
%%% ''''
%%%
%%% In a typical case, these two function calls will be wrapper in
%%% an application specific module. The start_link/2 function will
%%% be called from some supervisor and the ask/2 function from a
%%% process, for which the limit should be applied.
%%%
%%% TODO: The current implementation cannot handle rate limits of more than 1000 tps.
%%%
%%% TODO: Cleanup keys, that are not used anymore.
%%%
-module(axb_limit_rate).
-behaviour(gen_server).
-export([start_link/2, start_link/1, set_rate/2, ask/2, await/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%  @doc
%%  Start the server and register it as `Ref'.
%%  The Rate is specified as a number of events per second.
%%  One can use Rate smaller than 1 to have events with delays more thant second.
%%
-spec start_link(
        Ref :: term(),
        Rate :: number()
    ) ->
        {ok, pid()} |
        {error, Reason :: term()}.

start_link(Ref, Rate) ->
    gen_server:start_link(Ref, ?MODULE, {Rate}, []).


%%  @doc
%%  Start the process without registering it.
%%
-spec start_link(
        Rate :: number()
    ) ->
        {ok, pid()} |
        {error, Reason :: term()}.

start_link(Rate) ->
    gen_server:start_link(?MODULE, {Rate}, []).


%%  @doc
%%  Set new rate for the limiter.
%%  The rate is set asynchronously.
%%
-spec set_rate(
        Ref  :: term(),
        Rate :: number()
    ) ->
        ok.

set_rate(Ref, Rate) ->
    gen_server:cast(Ref, {set_rate, Rate}).


%%  @doc
%%  Ask for execution for permission to proceed.
%%  This function does not block, if needed
%%  it tells the caller to wait.
%%
%%  Here Ref is a reference of the limiter process
%%  and Who is a name of thing to limit.
%%
-spec ask(
        Ref :: term(),
        Who :: term()
    ) ->
        ok |
        {delay, DelayMS :: integer()}.

ask(Ref, Who) ->
    gen_server:call(Ref, {ask, Who}).


%%  @doc
%%  Await for permission to proceed.
%%  This function will block, if the rate limit is reached.
%%
-spec await(
        Ref :: term(),
        Who :: term()
    ) ->
        ok.

await(Ref, Who) ->
    case ask(Ref, Who) of
        {delay, DelayMS} ->
            timer:sleep(DelayMS),
            ok;
        ok ->
            ok
    end.



%%% =============================================================================
%%% Internal state.
%%% =============================================================================

%%
%%  Internal state.
%%
-record(state, {
    rate    :: integer(),
    sleep   :: integer(),
    last_ms :: #{Who :: term() => integer()}
}).



%%% =============================================================================
%%% Callbacks for `gen_server'.
%%% =============================================================================

%%  @private
%%  Initialization.
%%
init({Rate}) ->
    NewState = #state{
        rate    = Rate,
        sleep   = sleep_ms(Rate),
        last_ms = #{}
    },
    {ok, NewState}.


%%  @private
%%  Synchronous calls.
%%
handle_call({ask, Who}, _From, State = #state{sleep = Sleep, last_ms = LastMS}) ->
    NowMS = erlang:monotonic_time(millisecond),
    WhosLastMS = maps:get(Who, LastMS, NowMS - Sleep),
    DiffMS = NowMS - WhosLastMS,
    if
        DiffMS >= Sleep ->
            % Last event was long time ago.
            NewState = State#state{last_ms = LastMS#{Who => NowMS}},
            {reply, ok, NewState};
        true ->
            % We need to wait a bit.
            DelayMS = Sleep - DiffMS,
            NewState = State#state{last_ms = LastMS#{Who => NowMS + DelayMS}},
            {reply, {delay, DelayMS}, NewState}
    end;

handle_call(_Unknown, _From, State) ->
    {reply, undefined, State}.


%%  @private
%%  Asynchronous events.
%%
handle_cast({set_rate, Rate}, State) ->
    NewState = State#state{
        rate    = Rate,
        sleep   = sleep_ms(Rate)
    },
    {noreply, NewState};

handle_cast(_Unknown, State) ->
    {noreply, State}.


%%  @private
%%  Other messages.
%%
handle_info(_Unknown, State) ->
    {noreply, State}.


%%  @private
%%  Process termination.
%%
terminate(_Reason, _State) ->
    ok.


%%  @private
%%  Code upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%  @private
%%  Convert a rate to a duration between events.
%%
sleep_ms(Rate) when is_integer(Rate), Rate > 0 ->
    1000 div Rate;

sleep_ms(Rate) when is_float(Rate), Rate > 0 ->
    erlang:round(1000 / Rate).



%%% ============================================================================
%%% Unit tests.
%%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


basic_test_() ->
    {setup,
        fun () ->
            {ok, _Pid} = axb_limit_rate:start_link({local, basic_test_limiter}, 2)
        end,
        fun ({ok, Pid}) ->
            true = erlang:unlink(Pid),
            true = erlang:exit(Pid, shutdown)
        end,
        [
            {"Check, if rate limiting works.", fun () ->
                {DurationUS, ok} = timer:tc(fun () ->
                    axb_limit_rate:await(basic_test_limiter, basic_test),
                    axb_limit_rate:await(basic_test_limiter, basic_test),
                    axb_limit_rate:await(basic_test_limiter, basic_test)
                end),
                ?assert(DurationUS >  900000),
                ?assert(DurationUS < 1400000)
            end}
        ]
    }.

-endif.
