set(KERNEL_IMAGE "${CMAKE_CURRENT_BINARY_DIR}/kernel.img")

set(BOOT_STAGE1_ASM "${CMAKE_CURRENT_SOURCE_DIR}/boot-stage1.asm")
set(BOOT_STAGE1_BIN "${CMAKE_CURRENT_BINARY_DIR}/boot-stage1.bin")

set(BOOT_STAGE1_5_ASM "${CMAKE_CURRENT_SOURCE_DIR}/boot-stage1-5.asm")
set(BOOT_STAGE1_5_ELF "${CMAKE_CURRENT_BINARY_DIR}/boot-stage1-5.elf")
set(BOOT_STAGE1_5_OBJ "${CMAKE_CURRENT_BINARY_DIR}/boot-stage1-5.o")
set(BOOT_STAGE1_5_BIN "${CMAKE_CURRENT_BINARY_DIR}/boot-stage1-5.bin")

set(QEMU_ARGS
    -cpu 486
    -M isapc
    -hda ${KERNEL_IMAGE}
    -boot c
  )

add_custom_target(qemu
    COMMAND qemu-system-i386 ${QEMU_ARGS}
    DEPENDS ${KERNEL_IMAGE}
    COMMENT "Running raspios in QEMU..."
    USES_TERMINAL
)

add_custom_target(qemu_debug
    COMMAND qemu-system-i386 ${QEMU_ARGS} -gdb tcp::5555 -S
    DEPENDS ${KERNEL_IMAGE}
    COMMENT "Running raspios in QEMU (debug)..."
    USES_TERMINAL
)

add_custom_command(
    OUTPUT ${BOOT_STAGE1_BIN}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build-stage1.sh ${BOOT_STAGE1_ASM} ${BOOT_STAGE1_BIN} ${BOOT_STAGE1_5_BIN}
    DEPENDS ${BOOT_STAGE1_ASM}
    COMMENT "Assembling ${BOOT_STAGE1_ASM}"
)

add_custom_command(
    OUTPUT ${BOOT_STAGE1_5_BIN} ${BOOT_STAGE1_5_ELF}
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/bios-print.asm ${CMAKE_CURRENT_BINARY_DIR}/bios-print.asm
    COMMAND nasm -f elf32 ${BOOT_STAGE1_5_ASM} -o ${BOOT_STAGE1_5_OBJ} && ld -m elf_i386 -Ttext 0x8000 ${BOOT_STAGE1_5_OBJ} -o ${BOOT_STAGE1_5_ELF} && objcopy -O binary ${BOOT_STAGE1_5_ELF} ${BOOT_STAGE1_5_BIN}
    DEPENDS ${BOOT_STAGE1_5_ASM}
    COMMENT "Assembling ${BOOT_STAGE1_5_ASM}"
)

add_custom_command(
    OUTPUT ${KERNEL_IMAGE}
    COMMAND dd if=${BOOT_STAGE1_BIN} of=${KERNEL_IMAGE} bs=512 seek=0 conv=notrunc &&
      dd if=${BOOT_STAGE1_5_BIN} of=${KERNEL_IMAGE} bs=512 seek=1 conv=notrunc
    DEPENDS ${BOOT_STAGE1_5_BIN} ${BOOT_STAGE1_BIN}
    COMMENT "Creating ${KERNEL_IMAGE}"
)

add_custom_target(kernel-image ALL DEPENDS ${KERNEL_IMAGE} ${BOOT_STAGE1_BIN})
add_custom_target(boot-stage1-5 ALL DEPENDS ${BOOT_STAGE1_5_BIN})
add_custom_target(boot-stage1 ALL DEPENDS ${BOOT_STAGE1_5_BIN} ${BOOT_STAGE1_BIN})

add_dependencies(${PROJECT_NAME} kernel-image)
add_dependencies(boot-stage1 boot-stage1-5)
add_dependencies(kernel-image boot-stage1)

target_sources(${PROJECT_NAME} PUBLIC)
