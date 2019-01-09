%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%% Copyright (c) 2018 Pivotal Software, Inc.  All rights reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(sysmon_handler_SUITE).

-include("sysmon_handler.hrl").

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         default_values/1,
         basic_schema/1,
         override_schema/1]).

-define(PLUS1(X), (X+1)).

all() -> [default_values,
          basic_schema,
          override_schema].

%% default values test ensures that the hard-coded defaults in this library
%% are sane
default_values(_CT_Config) ->
    [{proc_limit, ?DEFAULT_PROCESS_LIMIT},
     {port_limit, ?DEFAULT_PORT_LIMIT},
     {gc_ms_limit, ?DEFAULT_GC_MS_LIMIT},
     {schedule_ms_limit, ?DEFAULT_SCHEDULE_MS_LIMIT},
     {heap_word_limit, ?DEFAULT_HEAP_WORD_LIMIT},
     {busy_port, ?DEFAULT_BUSY_PORT},
     {busy_dist_port, ?DEFAULT_BUSY_DIST_PORT}] = sysmon_handler_filter:get_config().


%% basic schema test will check to make sure that all defaults from the schema
%% make it into the generated app.config
basic_schema(CT_Config) ->
    Config = generate_config(CT_Config),

    WordSize = erlang:system_info(wordsize),
    % Note: cuttlefish schema default is 83886080
    HeapWordLimit = (8 * 10 * 1024 * 1024) div WordSize,

    cuttlefish_unit:assert_config(Config, "sysmon_handler.process_limit", ?DEFAULT_PROCESS_LIMIT),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.port_limit", ?DEFAULT_PORT_LIMIT),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.gc_ms_limit", ?DEFAULT_GC_MS_LIMIT),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.schedule_ms_limit", ?DEFAULT_SCHEDULE_MS_LIMIT),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.heap_word_limit", HeapWordLimit),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.busy_port", ?DEFAULT_BUSY_PORT),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.busy_dist_port", ?DEFAULT_BUSY_DIST_PORT).

override_schema(CT_Config) ->
    %% CF_Conf represents the conf file that would be read in by cuttlefish.
    %% this proplists is what would be output by the conf_parse module
    CF_Conf = [
        {["sysmon_handler", "thresholds", "busy_processes"], ?PLUS1(?DEFAULT_PROCESS_LIMIT)},
        {["sysmon_handler", "thresholds", "busy_ports"], ?PLUS1(?DEFAULT_PORT_LIMIT)},
        {["sysmon_handler", "triggers", "process", "garbage_collection"], "1ms"},
        {["sysmon_handler", "triggers", "process", "long_scheduled_execution"], "1ms"},
        {["sysmon_handler", "triggers", "process", "heap_size"], "400MB"},
        {["sysmon_handler", "triggers", "port"], off},
        {["sysmon_handler", "triggers", "distribution_port"], off}
    ],

    WordSize = erlang:system_info(wordsize),
    HeapWordLimit = (400 * 1024 * 1024) div WordSize,

    Config = generate_config(CT_Config, CF_Conf),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.process_limit", ?PLUS1(?DEFAULT_PROCESS_LIMIT)),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.port_limit", ?PLUS1(?DEFAULT_PORT_LIMIT)),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.gc_ms_limit", 1),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.schedule_ms_limit", 1),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.heap_word_limit", HeapWordLimit),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.busy_port", not ?DEFAULT_BUSY_PORT),
    cuttlefish_unit:assert_config(Config, "sysmon_handler.busy_dist_port", not ?DEFAULT_BUSY_DIST_PORT).

generate_config(CT_Config) ->
    generate_config(CT_Config, []).

generate_config(CT_Config, CF_Conf) ->
    SchemaFile = filename:join(?config(data_dir, CT_Config), "sysmon_handler.schema"),
    cuttlefish_unit:generate_config(SchemaFile, CF_Conf).
