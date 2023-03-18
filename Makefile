all: skiplist.so

CC = gcc
CFLAGS = -g3 -O0 -Wall -fPIC --shared
ERL_INCLUDE_DIR ?= /usr/lib/erlang/erts-9.2/include

skiplist.so: skiplist.h skiplist.c skiplist-nif.c
	$(CC)  $(CFLAGS)  -I$(ERL_INCLUDE_DIR)  $^ -o $@

all: erl
erl:
	erlc skiplist.erl test.erl

clean:
	-rm skiplist.so *.beam