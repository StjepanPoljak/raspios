PROJ=kernel8
CROSS_COMPILE=aarch64-none-elf
CFLAGS = -Wall -ffreestanding -nostdinc -nostdlib -nostartfiles -Iinclude
LDFLAGS= -nostdlib -nostartfiles

objs=start.o uart.o print.o irq.o timer.o

srcdir=source
incdir=include
builddir=build

all: $(builddir) $(PROJ).img

$(builddir):
	mkdir $(builddir)

$(builddir)/%.o: $(srcdir)/%.S
	$(CROSS_COMPILE)-gcc $(CFLAGS) -c $< -o $@

$(builddir)/$(PROJ).elf: $(addprefix $(builddir)/,$(objs)) link.ld
	$(CROSS_COMPILE)-ld $(LDFLAGS) $(filter-out link.ld,$^) -T link.ld -o $@

$(PROJ).img: $(builddir)/$(PROJ).elf
	$(CROSS_COMPILE)-objcopy -O binary $< $@

clean:
	rm -rf $(PROJ).img $(builddir)
