#-- Make sure MPI is installed on the users system
FIND_PACKAGE(MPI)

if (NOT MPI_FOUND)
 message(STATUS "MPI Not Found")
endif (NOT MPI_FOUND)

if (MPI_FOUND)
message(STATUS "MPI Found")


add_definitions(-DBOOST_MPI_SOURCE=1)
include_directories(${MPI_INCLUDE_PATH})

boost_add_library(
  boost_mpi
  broadcast.cpp
  communicator.cpp
  computation_tree.cpp
  content_oarchive.cpp
  environment.cpp
  exception.cpp
  graph_communicator.cpp
  group.cpp
  intercommunicator.cpp
  mpi_datatype_cache.cpp
  mpi_datatype_oarchive.cpp
  packed_iarchive.cpp
  packed_oarchive.cpp
  packed_skeleton_iarchive.cpp
  packed_skeleton_oarchive.cpp
  point_to_point.cpp
  request.cpp
  text_skeleton_oarchive.cpp
  DEPENDS boost_serialization
  COMPILE_FLAGS "${MPI_COMPILE_FLAGS}"
  LINK_FLAGS "${MPI_LINK_FLAGS}"
  LINK_LIBS ${MPI_LIBRARIES}
  SHARED_COMPILE_FLAGS "-DBOOST_MPI_DYN_LINK=1"
  )

message(STATUS "boost::mpi::python needs love")

endif (MPI_FOUND)

