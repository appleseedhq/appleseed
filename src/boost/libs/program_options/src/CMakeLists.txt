boost_add_library(boost_program_options
  cmdline.cpp config_file.cpp options_description.cpp parsers.cpp 
  variables_map.cpp value_semantic.cpp positional_options.cpp 
  utf8_codecvt_facet.cpp convert.cpp winmain.cpp
  SHARED_COMPILE_FLAGS "-DBOOST_PROGRAM_OPTIONS_DYN_LINK=1")
