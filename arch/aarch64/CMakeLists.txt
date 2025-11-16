execute_process(COMMAND bash -c "${CMAKE_C_COMPILER} -P -E -x c ${DCONFIG_OPTIONS} ${OLD_LINKER_SCRIPT} > ${LINKER_SCRIPT}")

target_include_directories(${PROJECT_NAME} PRIVATE ${CC_PATH}/${CROSS_COMPILE}/include)

set(QEMU_RPI_ARGS
    -M raspi3b
    -serial null
    -serial stdio
    -kernel ${KERNEL_IMAGE}
    -display none
)

set(QEMU_VIRT_MACHINE "virt")
set(QEMU_VIRT_ARGS
    -M virt
    -nodefaults
    -cpu cortex-a57
    -device pl011,chardev=serial0,addr=0x3f215000
    -chardev stdio,id=serial0
    -device loader,file=${KERNEL_IMAGE},addr=0x80000
    -kernel ${KERNEL_IMAGE}
    -display none)


# Add a custom target so you can run: `make qemu`
add_custom_target(qemu
    COMMAND qemu-system-aarch64 ${QEMU_RPI_ARGS}
    DEPENDS ${KERNEL_IMAGE}
    COMMENT "Running raspios in QEMU..."
    USES_TERMINAL
)

add_custom_target(qemu_virt
    COMMAND qemu-system-aarch64 ${QEMU_VIRT_ARGS}
    DEPENDS ${KERNEL_IMAGE}
    COMMENT "Running raspios in QEMU..."
    USES_TERMINAL
)

add_custom_target(qemu_debug
    COMMAND qemu-system-aarch64 ${QEMU_RPI_ARGS} -s -S
    DEPENDS ${KERNEL_IMAGE}
    COMMENT "Running raspios in QEMU (debug)..."
    USES_TERMINAL
)

set(DTB_FILE "${CMAKE_SOURCE_DIR}/bcm2710-rpi-3-b-plus.dtb")
set(DTB_OBJ "${CMAKE_CURRENT_BINARY_DIR}/dtb.o")

add_custom_command(
    OUTPUT ${DTB_OBJ}
    COMMAND ${CMAKE_OBJCOPY} -I binary -O elf64-littleaarch64 -B aarch64 --rename-section .data=.dtb ${DTB_FILE} ${DTB_OBJ}
    DEPENDS ${DTB_FILE}
    COMMENT "Embedding DTB: ${DTB_FILE}"
)

# Define a custom target so CMake knows it must build dtb.o
add_custom_target(embed_dtb DEPENDS ${DTB_OBJ})

# Make your main target depend on it
add_dependencies(${PROJECT_NAME} embed_dtb)

target_link_libraries(${PROJECT_NAME} PUBLIC ${DTB_OBJ})

add_subdirectory(boot)
add_subdirectory(irq)
