TOP := $(shell pwd)
REBAR := rebar
ARCHITECTURE := $(shell uname -m)

OTP_RELEASE := $(shell erl +B -noshell -noinput \
        -boot start_clean \
        -eval 'io:format("~s~n", [erlang:system_info(otp_release)]),halt(0).')

#SUBDIRS_NO_DEPS = $(shell ls | grep -v deps)
#BEAMS_NO_DEPS = $(shell find $(SUBDIRS_NO_DEPS) -name \*.beam -print | \
        grep /ebin/)

.PHONY: all deps test clean

all: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

test: all
	$(REBAR) -D test skip_deps=true eunit

clean:
	$(REBAR) clean
