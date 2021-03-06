set(CMAKE_SYSTEM_PROCESSOR arm)

set(CC_PATH "${ROOT_DIR}/cc")
set(CROSS_COMPILE "aarch64-none-elf")

if (NOT (EXISTS "${CC_PATH}/bin/${CROSS_COMPILE}-gcc"))
	message("No cross compiler found, installing...")
	execute_process(COMMAND bash -c "cd ${ROOT_DIR} && ./install-cc.sh")
endif()

set(CMAKE_C_COMPILER "${CC_PATH}/bin/${CROSS_COMPILE}-gcc")
set(CMAKE_ASM_COMPILER "${CC_PATH}/bin/${CROSS_COMPILE}-gcc")
set(CMAKE_OBJCOPY "${CC_PATH}/bin/${CROSS_COMPILE}-objcopy")

set(OLD_LINKER_SCRIPT "${LINKER_SCRIPT}")
set(LINKER_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/link.ld")

