add_executable(hello_world hello_world.F90)

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
   message("*** A reported Gnu Fortran compiler bug prevents compilation of ***")
   message("*** chapter02/figure2.4/oo_hello_world.F90. Substituting        ***")
   message("*** gfortran_oo_hello_world.F90.                                ***\n")
   add_executable(gfortran_oo_hello_world gfortran_oo_hello_world.F90)
else()
   add_executable(oo_hello_world oo_hello_world.F90)
endif()

