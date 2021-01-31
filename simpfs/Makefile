PROJ = simpfs
ifeq ($(CC),cc)
CC = gcc
endif
CFLAGS = -Wall -Iinclude

srcdir = source
incdir = include
builddir = build

objs = $(addsuffix .o,$(basename $(notdir $(wildcard source/*.c))))

all: $(builddir) $(PROJ)

$(builddir):
	mkdir $(builddir)

$(builddir)/%.o: $(srcdir)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(PROJ): $(addprefix $(builddir)/,$(objs))
	$(CC) $(CFLAGS) $^ -o $@

.PHONY=clean
clean:
	rm -rf $(PROJ) $(builddir)
