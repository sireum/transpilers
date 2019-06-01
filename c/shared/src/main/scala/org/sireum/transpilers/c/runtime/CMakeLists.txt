cmake_minimum_required(VERSION 3.9)

project(runtime-static)

set(CMAKE_C_STANDARD 99)

add_compile_options(
        "$<$<CONFIG:Release>:-Werror -O2>"
        "$<$<CONFIG:Debug>:-Werror>")

add_executable(runtime-static
        shared/src/32/ztype.h
        static/src/type.h
        static/type-composite.h
        static/types.c
        static/src/stackframe.c
        static/src/misc.c
        static/all.h
        static/main.c)

target_include_directories(runtime-static
        PUBLIC shared/src/32
        PUBLIC static
        PUBLIC static/src)
