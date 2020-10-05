PROJ = kernel8
CROSS_COMPILE = aarch64-none-elf
CFLAGS = -Wall -ffreestanding -nostdinc -nostdlib -nostartfiles -Iinclude
LDFLAGS = -nostdlib -nostartfiles
OPTIONS ?= -DBOOT_ADDR=0x01000000 -DSTACK_SIZE=0x1000 -DTOTAL_RAM_MB=1024 -DTARGET_EL=1 -DSECURE=0

objs = start_S.o uart_S.o print_S.o irq_S.o timer_S.o main_c.o

srcdir = source
incdir = include
builddir = build

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

clean:
	rm -rf $(PROJ).img $(builddir)
