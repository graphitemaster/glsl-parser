cmake_minimum_required(VERSION 3.9.6)

project(glsl-parser)


option(GLSL_PARSER_TESTS "Whether or not to add the test target (requires Python 3)" OFF)


add_library(glsl-parser
  ast.h
  ast.cpp
  lexer.h
  lexer.cpp
  lexemes.h
  parser.h
  parser.cpp
  util.h
  util.cpp)

if((CMAKE_CXX_COMPILER_ID STREQUAL "GNU") OR (CMAKE_CXX_COMPILER_ID MATCHES "Clang"))
  list(APPEND cxxflags -fno-rtti -fno-exceptions -Wall -Wextra)
endif((CMAKE_CXX_COMPILER_ID STREQUAL "GNU") OR (CMAKE_CXX_COMPILER_ID MATCHES "Clang"))

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  list(APPEND cxxflags -Wformat)
endif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")

target_compile_options(glsl-parser PRIVATE ${cxxflags})


add_executable(glsl-parser-main main.cpp)

target_link_libraries(glsl-parser-main PRIVATE glsl-parser)

set_target_properties(glsl-parser-main PROPERTIES OUTPUT_NAME glsl-parser)


if(GLSL_PARSER_TESTS)

  find_package(Python COMPONENTS Interpreter)

  if(Python_FOUND)
    add_test(NAME "glsl-parser-tests"
      COMMAND $<TARGET_FILE:Python::Interpreter> "${PROJECT_SOURCE_DIR}/test.py"
      WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}")
  endif(Python_FOUND)

endif(GLSL_PARSER_TESTS)
