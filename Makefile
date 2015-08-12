compile:
	@rebar compile

clean:
	@rebar clean

test:
	@rebar eunit

.PHONY: compile clean test
