#Name project and specify source languages
project(ssdSource Fortran CXX)

# Some compilers will work with earlier versions of CMake. (The Cray Compiler Environment requires CMake 2.8.5)
cmake_minimum_required(VERSION 2.8.5)

#Print an error message on an attempt to build inside the source directory tree:
IF ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  MESSAGE(FATAL_ERROR "ERROR! "
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "\nThe ssdSource archvie does not support in-source builds:\n"
    "You must now delete the CMakeCache.txt file and the CMakeFiles/ directory under"
    "the source directory for the book code archive or you will not be able to configure correctly!"
    "\nYou must now run something like:\n"
    "  $ rm -r CMakeCache.txt CMakeFiles/"
    "\n"
    "Please create a different directory and configure ssdSource under that different directory such as:\n"
    "  $ mkdir MY_BUILD\n"
    "  $ cd MY_BUILD\n"
    "  $ cmake [OPTIONS] .."
    )
ENDIF()

#Specify all the subdirectories containing codes that produces executable files:
set(chapters_to_build chapter01 chapter02 chapter03 chapter04 chapter05 chapter06 chapter07 chapter08 chapter09)
foreach(chapter ${chapters_to_build})
  add_subdirectory(${chapter})
endforeach()

#Build a library from the directories that don't produce an executable because they contain no main program. 
#These include appendixA and chapter10.
add_subdirectory(library)

#Report untested Fortran compiler unless explicitly directed to build all examples.
if (${ALL_EXAMPLES_ENABLED})
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "VisualAge|XL|NAG|GNU|Intel|Cray")
else()
  message("\n*** Untested Fortran compiler detected: ${CMAKE_Fortran_COMPILER_ID}.***\n")
  message("\n*** Attempting to build all examples. Please report any failures to rouson@sandia.gov.***\n")
endif()
