%%%-------------------------------------------------------------------
%%% @author  drimtajm@github
%%% @copyright (C) 2013, Angela Johansson
%%% @doc Application module for the ADS1015 A/D converter driver
%%%
%%% The ADS1015 A/D converter driver is free software: you can redistribute
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
%%% Created : 30 Jul 2013 by drimtajm
%%%-------------------------------------------------------------------
-module(ads1015_driver_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    application:start(ad_converter_driver_ads1015).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ads1015_driver_sup:start_link().

stop(_State) ->
    ok.
