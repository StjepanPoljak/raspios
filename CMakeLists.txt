project(raspios)
cmake_minimum_required(VERSION 3.19)

enable_language(ASM)

set(CMAKE_SYSTEM_NAME Generic)

set(ROOT_DIR "${CMAKE_CURRENT_LIST_DIR}")

set(ARCH "$ENV{ARCH}")
set(ARCH_DIR "${ROOT_DIR}/arch/${ARCH}")
set(LINKER_SCRIPT "${ARCH_DIR}/link.ld")

set(CMAKE_C_COMPILER "gcc")
set(CMAKE_ASM_COMPILER "gcc")
set(CMAKE_OBJCOPY "objcopy")
set(DEFAULT_C_FLAGS "-Wall -ffreestanding -nostdinc -nostdlib -nostartfiles")

include("${ARCH_DIR}/setup_arch.cmake")

set(CMAKE_C_FLAGS  "${DEFAULT_C_FLAGS}")
set(CMAKE_ASM_FLAGS "${DEFAULT_C_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "-nostdlib -nostartfiles -T ${LINKER_SCRIPT}")

execute_process(COMMAND bash -c
	"sed '/^\\s*\\#/d' ${ROOT_DIR}/config.txt | awk \
	'BEGIN { opts = \"\" } { opts = opts (opts == \"\" ? \"\" : OFS) \$0 } END { printf opts }'"
	OUTPUT_VARIABLE CONFIG_OPTIONS)

string(REPLACE " " " -D" DCONFIG_OPTIONS "-D${CONFIG_OPTIONS}")

string(REPLACE " " ";" CONFIG_OPTIONS ${CONFIG_OPTIONS})
add_compile_definitions(${CONFIG_OPTIONS})

add_executable(${PROJECT_NAME})

target_include_directories(${PROJECT_NAME} PRIVATE include)

add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
	COMMAND bash -c "${CMAKE_OBJCOPY} -O binary ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME} kernel8.img")

add_subdirectory(arch)
add_subdirectory(device)
add_subdirectory(kernel)

