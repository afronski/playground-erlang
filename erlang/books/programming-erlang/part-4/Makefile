CC=gcc
CFLAGS=-c -Wall

SRC=example.c example_driver.c erlang_communication.c
OBJS=$(SRC:.c=.o)

EXE=example

ERLC=erlc
ERLSRC=example.erl

all: $(SRC) $(EXE) erlang

$(EXE): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -o $@

erlang:
	$(ERLC) $(ERLSRC)

clean:
	rm -f *.o *.beam $(EXE)

.PHONY: all clean
