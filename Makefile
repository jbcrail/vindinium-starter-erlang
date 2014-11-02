REBAR = $(shell pwd)/rebar

all:
	test -d deps || $(REBAR) get-deps
	$(REBAR) compile

clean:
	rm -rf deps
	$(REBAR) clean
