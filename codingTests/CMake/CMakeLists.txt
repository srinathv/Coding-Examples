cmake_minimum_required (VERSION 3.2)
project (Tutorial CXX C Fortran)
add_executable(Tutorial tutorial.cxx)

set (testVar "/usr/lib64/par/foo")
set (badVar "/par/foo/usr/lib")
set (badDir "/usr/lib")

message(STATUS "let's see what is test var = ${testVar}")

if (${testVar} MATCHES "^${badDir}")
  message (STATUS "testVar I think we know regex")
else(${testVar} MATCHES "^${badDir}")
  message (STATUS "testVar I don't know how to regex")
endif (${testVar} MATCHES "^${badDir}")

message(STATUS "The system is ${CMAKE_SYSTEM_NAME}")

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set (MACOSX YES)
    add_definitions(-DMACOSX)
endif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
if (MACOSX)
  message(STATUS " On a mac os X")
endif(MACOSX)

find_package (MPI)


message( STATUS ${MPI_C_FOUND})         #  TRUE if FindMPI found MPI flags for <lang>
message( STATUS ${MPI_C_COMPILER})      #  MPI Compiler wrapper for <lang>
message( STATUS ${MPI_C_COMPILE_FLAGS}) #  Compilation flags for MPI programs
message( STATUS ${MPI_C_INCLUDE_PATH})  #  Include path(s) for MPI header
message( STATUS ${MPI_C_LINK_FLAGS})    #  Linking flags for MPI programs
message( STATUS ${MPI_C_LIBRARIES})     #  All libraries to link MPI programs against


message( STATUS ${MPI_CXX_FOUND})         #  TRUE if FindMPI found MPI flags for <lang>
message( STATUS ${MPI_CXX_COMPILER})      #  MPI Compiler wrapper for <lang>
message( STATUS ${MPI_CXX_COMPILE_FLAGS}) #  Compilation flags for MPI programs
message( STATUS ${MPI_CXX_INCLUDE_PATH})  #  Include path(s) for MPI header
message( STATUS ${MPI_CXX_LINK_FLAGS})    #  Linking flags for MPI programs
message( STATUS ${MPI_CXX_LIBRARIES})     #  All libraries to link MPI programs against

message( STATUS ${MPI_Fortran_FOUND})         #  TRUE if FindMPI found MPI flags for <lang>
message( STATUS ${MPI_Fortran_COMPILER})      #  MPI Compiler wrapper for <lang>
message( STATUS ${MPI_Fortran_COMPILE_FLAGS}) #  Compilation flags for MPI programs
message( STATUS ${MPI_Fortran_INCLUDE_PATH})  #  Include path(s) for MPI header
message( STATUS ${MPI_Fortran_LINK_FLAGS})    #  Linking flags for MPI programs
message( STATUS ${MPI_Fortran_LIBRARIES})     #  All libraries to link MPI programs against


# if (${badVar} MATCHES "^${badDir}")
#    message (STATUS "badVar I think we know regex")
# else(${badVar} MATCHES "^${badDir}")
#    message (STATIS "badVar I don't know how to regex")
# endif (${barVar} MATCHES "^${badDir}")
