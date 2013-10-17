make:
	rebar compile

run:
	ERL_FLAGS="-pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl" erl

test:
	ERL_FLAGS="-pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl" rebar eunit skip_deps=true

.PHONY: test
