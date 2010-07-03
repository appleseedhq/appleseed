boost_tool_project(Quickbook
  DESCRIPTION "A WikiWiki style documentation tool geared towards C++ documentation"
  AUTHORS "Joel de Guzman <joel -at- boost-consulting.com>"
          "Eric Niebler <eric -at- boost-consulting.com>")

if (BUILD_QUICKBOOK)
  if (MSVC)
    add_definitions(/wd4511 /wd4512 /wd4701 /wd4702 /wd4244 /wd4267 /wd4800)
  endif (MSVC)
  
  #
  # TDS: this needs fixing...  these are the modules that quickbook is
  # dependent on, these paths should be set via some more generic
  # mechanism
  #
  include_directories(
    ${CMAKE_SOURCE_DIR}/libs/spirit/include
    ${CMAKE_SOURCE_DIR}/libs/detail/include
    ${CMAKE_SOURCE_DIR}/libs/config/include
    ${CMAKE_SOURCE_DIR}/libs/type_traits/include
    ${CMAKE_SOURCE_DIR}/libs/program_options/include
    ${CMAKE_SOURCE_DIR}/libs/filesystem/include
    ${CMAKE_SOURCE_DIR}/libs/system/include
    )
  

  boost_add_executable(quickbook
    detail/quickbook.cpp
    detail/actions.cpp
    detail/actions_class.cpp
    detail/utils.cpp
    detail/post_process.cpp
    detail/collector.cpp
    detail/input_path.cpp
    detail/template_stack.cpp
    detail/markups.cpp
    DEPENDS boost_program_options boost_filesystem
    )  
endif (BUILD_QUICKBOOK)