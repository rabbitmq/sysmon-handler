PROJECT = sysmon_handler
PROJECT_DESCRIPTION = system_monitor message handler library
PROJECT_MOD = sysmon_handler_app

dep_cuttlefish = hex 2.1.4

TEST_DEPS = lager cuttlefish

include erlang.mk
