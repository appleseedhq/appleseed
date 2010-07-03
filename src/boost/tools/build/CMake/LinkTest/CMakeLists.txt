cmake_minimum_required(VERSION 2.6.0 FATAL_ERROR)

project(LinkTest)
include_directories (${INCLUDES})
add_executable(link ${SOURCE} COMPILE_FLAGS "${COMPILE_FLAGS}")
