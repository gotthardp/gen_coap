# See LICENSE for licensing information.

DIALYZER = dialyzer
REBAR = rebar

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

test:
	@$(REBAR) eunit

.coap_dialyzer.plt:
	@$(DIALYZER) --build_plt --output_plt .coap_dialyzer.plt \
		--apps erts kernel stdlib crypto

dialyze: .coap_dialyzer.plt
	@$(DIALYZER) --src src --plt .coap_dialyzer.plt --no_native \
		-Wunmatched_returns -Werror_handling -Wrace_conditions

.PHONY: compile deps clean test dialyze
