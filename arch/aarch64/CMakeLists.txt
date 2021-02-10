execute_process(COMMAND bash -c "${CMAKE_C_COMPILER} -P -E -x c ${DCONFIG_OPTIONS} ${OLD_LINKER_SCRIPT} > ${LINKER_SCRIPT}")

target_include_directories(${PROJECT_NAME} PRIVATE ${CC_PATH}/${CROSS_COMPILE}/include)

target_sources(raspios
	PUBLIC
	early_mem.S
	irq.S
	mmu.c
	print.S
	start.S
	uart.S)
