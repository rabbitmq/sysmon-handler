PROJECT = sysmon_handler
PROJECT_DESCRIPTION = system_monitor message handler library
PROJECT_MOD = sysmon_handler_app

dep_cuttlefish = git https://github.com/Kyorai/cuttlefish.git develop

TEST_DEPS = lager cuttlefish

include erlang.mk
