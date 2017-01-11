# See LICENSE for licensing information.

REBAR ?= rebar3

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

test:
	@$(REBAR) eunit

dialyze:
	@$(REBAR) dialyzer

.PHONY: compile clean test dialyze
