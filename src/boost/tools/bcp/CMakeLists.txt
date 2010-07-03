boost_tool_project(bcp
DESCRIPTION "The bcp utility is a tool for extracting subsets of Boost."
AUTHORS "John Maddock")

IF (BUILD_BCP)


set (bcp_sources
  add_path.cpp 
  bcp_imp.cpp 
  copy_path.cpp 
  file_types.cpp
  fileview.cpp 
  main.cpp 
  path_operations.cpp 
  scan_cvs_path.cpp 
  licence_info.cpp 
  scan_licence.cpp 
  output_licence_info.cpp
)

boost_tool_dependencies( BOOST_DEPENDS filesystem test regex)

boost_add_executable(
  bcp
  ${bcp_sources}
  DEPENDS boost_system boost_filesystem boost_prg_exec_monitor boost_regex
)

ENDIF (BUILD_BCP)
