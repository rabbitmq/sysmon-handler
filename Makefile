PROJECT = rabbitmq_sysmon
PROJECT_DESCRIPTION = RabbitMQ Sysmon Library
PROJECT_MOD = rabbit_sysmon_app

TEST_DEPS = rabbitmq_ct_helpers

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.
ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
