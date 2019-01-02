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

-module(rabbit_sysmon_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, basic_schema/1, override_schema/1]).

-define(DEFAULT_PROCESS_LIMIT, 30).
-define(DEFAULT_PORT_LIMIT, 2).
-define(DEFAULT_GC_MS_LIMIT, 0).
-define(DEFAULT_SCHEDULE_MS_LIMIT, 0).
-define(DEFAULT_HEAP_WORD_LIMIT, 40111000).
-define(DEFAULT_BUSY_PORT, true).
-define(DEFAULT_BUSY_DIST_PORT, true).
-define(PLUS1(X), (X+1)).

all() -> [basic_schema, override_schema].

%% basic schema test will check to make sure that all defaults from the schema
%% make it into the generated app.config
basic_schema(CT_Config) ->
    Config = generate_config(CT_Config),

    HeapSize = case erlang:system_info(wordsize) of
                   4 -> ?DEFAULT_HEAP_WORD_LIMIT;
                   8 -> ?DEFAULT_HEAP_WORD_LIMIT div 2
               end,

    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.process_limit", ?DEFAULT_PROCESS_LIMIT),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.port_limit", ?DEFAULT_PORT_LIMIT),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.gc_ms_limit", ?DEFAULT_GC_MS_LIMIT),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.schedule_ms_limit", ?DEFAULT_SCHEDULE_MS_LIMIT),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.heap_word_limit", HeapSize),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.busy_port", ?DEFAULT_BUSY_PORT),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.busy_dist_port", ?DEFAULT_BUSY_DIST_PORT).

override_schema(CT_Config) ->
    %% CF_Conf represents the rabbit.conf file that would be read in by cuttlefish.
    %% this proplists is what would be output by the conf_parse module
    CF_Conf = [
        {["runtime_health", "thresholds", "busy_processes"], ?PLUS1(?DEFAULT_PROCESS_LIMIT)},
        {["runtime_health", "thresholds", "busy_ports"], ?PLUS1(?DEFAULT_PORT_LIMIT)},
        {["runtime_health", "triggers", "process", "garbage_collection"], "1ms"},
        {["runtime_health", "triggers", "process", "long_schedule"], "1ms"},
        {["runtime_health", "triggers", "process", "heap_size"], "400MB"},
        {["runtime_health", "triggers", "port"], off},
        {["runtime_health", "triggers", "distribution_port"], off}
    ],

    WordSize = erlang:system_info(wordsize),
    HeapSize = (400 * 1024 * 1024) div WordSize,

    Config = generate_config(CT_Config, CF_Conf),

    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.process_limit", ?PLUS1(?DEFAULT_PROCESS_LIMIT)),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.port_limit", ?PLUS1(?DEFAULT_PORT_LIMIT)),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.gc_ms_limit", ?PLUS1(?DEFAULT_GC_MS_LIMIT)),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.schedule_ms_limit", ?PLUS1(?DEFAULT_SCHEDULE_MS_LIMIT)),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.heap_word_limit", HeapSize),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.busy_port", not ?DEFAULT_BUSY_PORT),
    cuttlefish_unit:assert_config(Config, "rabbit_sysmon.busy_dist_port", not ?DEFAULT_BUSY_DIST_PORT).

generate_config(CT_Config) ->
    generate_config(CT_Config, []).

generate_config(CT_Config, CF_Conf) ->
    SchemaFile = filename:join(?config(data_dir, CT_Config), "rabbit_sysmon.schema"),
    cuttlefish_unit:generate_config(SchemaFile, CF_Conf).
