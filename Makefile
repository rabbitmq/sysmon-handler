PROJECT = rabbit_sysmon
PROJECT_DESCRIPTION = RabbitMQ system_monitor library
PROJECT_MOD = rabbit_sysmon_app

dep_cuttlefish = git https://github.com/Kyorai/cuttlefish.git develop

TEST_DEPS = lager cuttlefish

include erlang.mk
