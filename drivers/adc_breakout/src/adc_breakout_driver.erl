%%%-------------------------------------------------------------------
%%% @author  drimtajm@github
%%% @copyright (C) 2013, Angela Johansson
%%% @doc Implementation module for Sparky's ADC breakout driver
%%%
%%% This A/D converter driver is free software: you can redistribute
%%% it and/or modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation, either version 3
%%% of the License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with this software.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% @end
%%% Created : 16 Nov 2013 by drimtajm
%%%-------------------------------------------------------------------
-module(adc_breakout_driver).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 read_value/0,
	 stop/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APPLICATION, ad_converter_driver_sparky).

-type max_voltage() :: float().
-record(state, {i2c_interface_handle :: pos_integer(),
		max_voltage    :: max_voltage()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read_value() ->
    gen_server:call(?SERVER, read_value, 1000).

stop() ->
    stop(normal).

stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    I2C_address = get_default_value_for(i2c_address),
    case i2c_interface:open_i2c_bus(I2C_address) of
	{ok, Handle} ->
	    InitState =
		#state{i2c_interface_handle = Handle,
		       max_voltage    = get_default_value_for(max_voltage)},
	    {ok, InitState};
	Error        -> {stop, Error}
    end.

get_default_value_for(Parameter) ->
    {ok, Value} = application:get_env(?APPLICATION, Parameter),
    Value.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% TODO: terminate when device/handle not available

%% read_value
handle_call(read_value, _From, #state{i2c_interface_handle = Handle,
				      max_voltage = MaxVoltage} = State) ->
    {ok, Value} = i2c_interface:read_i2c_raw_word(Handle),
    Voltage = convert_value_to_voltage(Value, MaxVoltage),
    {reply, {ok, Voltage}, State};
%% catch all remaining calls
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({stop, Reason}, State) ->
    io:format("Stop due to: ~p~n", [Reason]),
    {stop, Reason, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{i2c_interface_handle = Handle}) ->
    ok = i2c_interface:close_i2c_bus(Handle).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_value_to_voltage(Value, MaxVoltage) ->
    (Value/4095)*MaxVoltage.
