target_sources(raspios
	PUBLIC
	start.S
	early_mem.S
	print.S
	uart.S
	mmu.c
	${DTB_FILE})
