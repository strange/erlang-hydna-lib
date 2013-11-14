make:
	rebar update-deps compile

run:
	erl -pa ebin deps/*/ebin -boot start_sasl

test:
	rebar eunit skip_deps=true

.PHONY: test
