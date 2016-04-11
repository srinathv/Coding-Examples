    cmake_minimum_required(VERSION 2.8)


enable_language(Fortran)


add_executable( cMemoryOrder
    print_array_values.c
    cMemoryOrder.c )

add_executable ( fortranMemoryOrder
  print_array_values.c
  fortranMemoryOrder.f )

set (CMAKE_C_COMPILER gcc)
set (CMAKE_FORTRAN_COMPILER gfortran)
