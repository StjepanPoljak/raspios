PROJ = kernel8
CROSS_COMPILE = aarch64-none-elf
CFLAGS = -Wall -ffreestanding -nostdinc -nostdlib -nostartfiles -Iinclude
LDFLAGS = -nostdlib -nostartfiles
OPTIONS = $(shell awk 'BEGIN{ opts="" } { opts = opts (opts == "" ? "" : OFS) "-D" $$0 } END { print opts }' config.txt)
C_INCLUDE_PATH ?= cc/aarch64-none-elf/include

srcdir = source
incdir = include
builddir = build

objs = $(addsuffix _c.o,$(basename $(notdir $(wildcard source/*.c))))
objs += $(addsuffix _S.o,$(basename $(notdir $(wildcard source/*.S))))

all: $(builddir) $(PROJ).img

$(builddir):
	mkdir $(builddir)

$(builddir)/%_c.o: $(srcdir)/%.c include/*.h
	$(CROSS_COMPILE)-gcc $(OPTIONS) $(CFLAGS) -c $< -o $@

$(builddir)/%_S.o: $(srcdir)/%.S include/*.h
	$(CROSS_COMPILE)-gcc $(OPTIONS) $(CFLAGS) -c $< -o $@

$(builddir)/$(PROJ).elf: $(addprefix $(builddir)/,$(objs)) link.ld
	$(CROSS_COMPILE)-gcc -P -E -x c $(OPTIONS) link.ld > $(builddir)/link.ld
	$(CROSS_COMPILE)-ld $(LDFLAGS) $(filter-out link.ld,$^) -T $(builddir)/link.ld -o $@

$(PROJ).img: $(builddir)/$(PROJ).elf
	$(CROSS_COMPILE)-objcopy -O binary $< $@

objdump: $(builddir)/$(PROJ).elf
	$(CROSS_COMPILE)-objdump -D $< | vi -

.PHONY=clean
clean:
	rm -rf $(PROJ).img $(builddir)
