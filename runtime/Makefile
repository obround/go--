CC = clang
CFLAGS = -Wall -Wextra -Wpedantic -O2 -fPIC -I/opt/homebrew/include
LDFLAGS = -L/opt/homebrew/lib

SRCS = main.c strings.c slices.c maps.c print.c panic.c
OBJS = $(SRCS:.c=.o)
LIB = libgoruntime.a

.PHONY: all clean

all: $(LIB)

install: all

$(LIB): $(OBJS)
	ar rcs $@ $^

%.o: %.c runtime.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(LIB)
