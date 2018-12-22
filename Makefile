PROJECT = rabbitmq_sysmon
# PROJECT_DESCRIPTION = RabbitMQ Sysmon Library
# PROJECT_VERSION = 3.0.0
# PROJECT_MOD = rabbit_sysmon

## NB: rabbit_sysmon uses a src/rabbit_sysmon.app.src file

dep_cuttlefish = git https://github.com/Kyorai/cuttlefish.git develop

DEPS = lager cuttlefish

include erlang.mk
