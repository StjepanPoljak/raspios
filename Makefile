PROJ=kernel8
CROSS_COMPILE=aarch64-linux-gnu
CFLAGS = -Wall -ffreestanding -nostdinc -nostdlib -nostartfiles
LDFLAGS= -nostdlib -nostartfiles

objs=start.o uart.o print.o

all: $(PROJ).img

%.o: %.S
	$(CROSS_COMPILE)-gcc $(CFLAGS) -c $< -o $@

$(PROJ).elf: $(objs)
	$(CROSS_COMPILE)-ld $(LDFLAGS) $^ -T link.ld -o $@

$(PROJ).img: $(PROJ).elf
	$(CROSS_COMPILE)-objcopy -O binary $< $@

clean:
	rm -rf $(PROJ).elf $(PROJ).img $(objs)
