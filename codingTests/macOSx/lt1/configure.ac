#                                               -*- Autoconf -*-
## Process this file with autoconf to produce a configure script.
## $Id: configure.ac 714 2011-04-22 18:02:17Z pletzer $

# This is part of Unidata's NetCDF CF Library. package. Copyright
# 2006, see the COPYRIGHT file for more information.

## This puts the cvs ID tag in the output configure script.
AC_REVISION([$Id: configure.ac 714 2011-04-22 18:02:17Z pletzer $])

## Running autoconf on this file will trigger a warning if 
## autoconf is not at least the specified version.
AC_PREREQ([2.59])

## Initialize with name, version, and support email address. 
AC_INIT([libcf], [1.0-beta1], [support@unidata.ucar.edu])
AC_CONFIG_MACRO_DIR([m4])

AC_MSG_NOTICE([libcf AC_PACKAGE_VERSION])

# Find out about the host we're building on.
AC_CANONICAL_HOST

#check for apple
darwin_os=''
case $host in
  x86_64-apple-darwin*)
    darwin_os='yes'
  ;;
esac

AM_CONDITIONAL(HAVE_DARWIN, test x$darwin_os = xyes)
if test "x$HAVE_DARWIN" = "xyes"; then
   AC_SUBST([HAVE_DARWIN], [1])
else
   AC_SUBST([HAVE_DARWIN], [0])
fi


  

# We will output a config.h for the C library, and a fortran inc file,
# nfconfig.inc.
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_HEADERS([nfconfig.inc:nfconfig.in], 
   [sed '1d;s%^/\* \(.*\) \*/%\1%' nfconfig.inc >conftest.tmp && mv conftest.tmp nfconfig.inc])

# This call is required by automake.
AM_INIT_AUTOMAKE([foreign no-installinfo subdir-objects -Wall])

# Check for the existance of this file before proceeding.
AC_CONFIG_SRCDIR([src/nccoord.c])

AC_MSG_NOTICE([checking user options])

# Turn off shared libraries by default
AM_DISABLE_SHARED
# test if --enable-shared was selected
AM_CONDITIONAL(HAVE_SHARED, test x$enable_shared = xyes)
if test "x$enable_shared" = "xyes"; then
	AC_SUBST(HAVE_SHARED, [1])
else
	AC_SUBST(HAVE_SHARED, [0])
fi

# The autoconf manual suggests the following to set a var if it's not
# already set: test "${var+set}" = set || var={value}

# For backward compatibility, check to see if the user has set FC to
# null. If so, treat that as the --disable-fortran option to
# configure.

# Did the user set fc to null?
AC_MSG_CHECKING([whether FC is set to ''])
nc_null_fc=no
test "x${FC+set}" = xset && test "x$FC" = x && nc_null_fc=yes
AC_MSG_RESULT([$nc_null_fc])

# According to the autoconf mailing list gurus, we must test for
# compilers unconditionally. That is, we can't skip looking for the
# fortran compilers, just because the user doesn't want fortran. This
# is due to a limitation in autoconf.

# Find the C compiler.
AC_MSG_NOTICE([finding C compiler])
AC_PROG_CC(mpcc_r xlc cc c89 acc gcc)
AM_PROG_CC_C_O

# Find fortran compiler. If FC isn't set after AC_PROG_FC, we couldn't
# find a fortran compiler. If the user requested the fortran API, then
# bail out right here.
AC_MSG_NOTICE([finding Fortran compiler (required, even if Fortran API is not desired)])

# We must call AC_PROG_FC here, otherwise the LIBTOOL macro below
# will call it, and reset FC to g77.
AC_PROG_FC([mpxlf_r xlf90 fort xlf95 ifort ifc efc pgf95 lf95 g95 gfortran f95 frt pgf77 f77 fort77 fl32 af77 f90 xlf90 pgf90 epcf90 g77])

if test "x$nc_build_f90" = xyes; then
   AC_MSG_NOTICE([configuring fortran])
   # Some f90 compilers change the case of the mod file names. Others
   # require special flags to be set to deal with .f90 files. Find out
   # about this compiler.
   AC_LANG_PUSH(Fortran)
   AC_FC_SRCEXT(f90)
   AC_SEARCH_LIBS([nf90_open], [netcdf])
   AC_LANG_POP(Fortran)
   AX_F90_MODULE_FLAG
   AC_SUBST(MOD_FLAG, ${ax_cv_f90_modflag})
fi

# Does the user want to build documentation?
AC_MSG_CHECKING([whether documentation is to be built])
AC_ARG_ENABLE([docs],
              [AS_HELP_STRING([--enable-docs],
                              [build the libcf documentation (a built version is already \
			      included with the distrubution)])])
test "x$enable_docs" = xyes || enable_docs=no
AC_MSG_RESULT($enable_docs)
AM_CONDITIONAL(BUILD_DOCS, [test x$enable_docs = xyes])

# Does the user want to build gridspec tools?
AC_MSG_CHECKING([whether gridspec tools are to be built])
AC_ARG_ENABLE([gridspec],
              [AS_HELP_STRING([--disable-gridspec],
                              ["don't build the gridspec tools"])])
test "x$enable_gridspec" = xno || enable_gridspec=yes
AC_MSG_RESULT($enable_gridspec)
AM_CONDITIONAL(BUILD_GRIDSPEC, [test x$enable_gridspec = xyes])

# Does the user want to fetch test data and run extra gridspec tests?
AC_MSG_CHECKING([whether gridspec test data should be fetched from Unidata FTP site])
AC_ARG_ENABLE([gridspec-file-tests],
              [AS_HELP_STRING([--enable-gridspec-file-tests],
                              [get GRIDSPEC test data from Unidata FTP server and run extra tests])])
test "x$enable_gridspec_file_tests" = xyes || enable_gridspec_file_tests=no
AC_MSG_RESULT($enable_gridspec_file_tests)
AM_CONDITIONAL(BUILD_GRIDSPEC_FILE_TESTS, [test x$enable_gridspec_file_tests = xyes])

# Does the user want to build cfcheck?
AC_MSG_CHECKING([whether cfcheck is to be built])
AC_ARG_ENABLE([cfcheck],
              [AS_HELP_STRING([--enable-cfcheck],
                              [still in development - not working yet])])
test "x$enable_cfcheck" = xyes || enable_cfcheck=no
AC_MSG_RESULT($enable_cfcheck)
AM_CONDITIONAL(BUILD_CFCHECK, [test x$enable_cfcheck = xyes])

# Does the user want to enable logging?
AC_MSG_CHECKING([whether logging is enabled (useful to CF developers, mainly])
AC_ARG_ENABLE([logging],
              [AS_HELP_STRING([--enable-logging],
                              [enable logging capability. This debugging features \
			      is only of interest to libcf developers.])])
test "x$enable_logging" = "xyes" || enable_logging=no
AC_MSG_RESULT([$enable_logging])
if test "x$enable_logging" = xyes; then
   AC_DEFINE([LOGGING], 1, [if true, turn on logging])
fi

# Does the user want parallel processing?
AC_MSG_CHECKING([whether parallel processing is enabled (CC should be set to mpicc or equivalent])
AC_ARG_ENABLE([parallel],
              [AS_HELP_STRING([--enable-parallel],
                              [enable parallel processing in GRIDSPEC code.])])
test "x$enable_parallel" = "xyes" || enable_parallel=no
AC_MSG_RESULT([$enable_parallel])
if test "x$enable_parallel" = xyes; then
   AC_DEFINE([USE_PARALLEL], 1, [if true, turn on parallel processing])
fi
AM_CONDITIONAL(BUILD_PARALLEL, [test x$enable_parallel = xyes])

# Does the user want to run extra parallel tests?
AC_MSG_CHECKING([whether parallel tests should be run])
AC_ARG_ENABLE([parallel-tests],
              [AS_HELP_STRING([--enable-parallel-tests],
                              [Run extra parallel tests. Ignored if \
                               parallel processing is not enabled.])])
test "x$enable_parallel_tests" = xyes || enable_parallel_tests=no
AC_MSG_RESULT($enable_parallel_tests)
AM_CONDITIONAL(TEST_PARALLEL, [test "x$enable_parallel" = xyes -a "x$enable_parallel_tests" = xyes])

# Does the user want to use netcdf-4?
AC_MSG_CHECKING([whether netCDF-4 is to be used])
AC_ARG_ENABLE([netcdf-4],
              [AS_HELP_STRING([--enable-netcdf-4],
                              [build with netcdf-4 (HDF5 is required)])])

# If hdf5 dir is set, turn on enable-netcdf-4.
if test "x$with_hdf5" != x; then
   enable_netcdf_4=yes
else
   test "x$enable_netcdf_4" = xyes || enable_netcdf_4=no
fi
AC_MSG_RESULT($enable_netcdf_4)
AM_CONDITIONAL(USE_NETCDF4, [test x$enable_netcdf_4 = xyes])
if test "x$enable_netcdf_4" = xyes; then
   AC_DEFINE([USE_NETCDF4], 1, [if true, use netCDF-4])
fi

# Does the user want to turn on HDF4 read ability?
AC_ARG_ENABLE([hdf4], [AS_HELP_STRING([--enable-hdf4],
              [build netcdf-4 with HDF4 read capability (HDF4, HDF5 and zlib required)])])

# Does the user want extra tests on CF sample files?
AC_MSG_CHECKING([whether extra tests on sample data should be performed])
AC_ARG_ENABLE([extra-data-tests],
              [AS_HELP_STRING([--enable-extra-data-tests],
                              [Perform extra tests using CF example files from Unidata web site \
			      (example files must be donloaded separately). This option should \
			      only be used by those with excessive interest in testing libcf.])])
test "x$enable_extra_data_tests" = xyes || enable_extra_data_tests=no
AC_MSG_RESULT($enable_extra_data_tests)
AM_CONDITIONAL(EXTRA_DATA_TESTS, [test x$enable_extra_data_tests = xyes])

# Did the user specify a location for the HDF5 library?
AC_MSG_CHECKING([whether a location for the HDF5 library was specified])
AC_ARG_WITH([hdf5],
              [AS_HELP_STRING([--with-hdf5=<directory>],
                              [Specify location of HDF5 library. Configure will expect \
			      to find subdirs include and lib.])],
            [HDF5DIR=$with_hdf5])
AC_MSG_RESULT([$HDF5DIR])
AM_CONDITIONAL(USE_HDF5_DIR, [test ! "x$HDF5DIR" = x])
AC_SUBST(HDF5DIR, [$HDF5DIR])
if test ! "x$HDF5DIR" = x; then
   LDFLAGS="${LDFLAGS} -L$HDF5DIR/lib"
fi

# Did the user specify a location for the HDF4 library?
AC_MSG_CHECKING([whether a location for the HDF4 library was specified])
AC_ARG_WITH([hdf4],
              [AS_HELP_STRING([--with-hdf4=<directory>],
                              [Specify location of HDF4 library. Configure will \
			      expect to find subdirs include and lib.])],
            [HDF4DIR=$with_hdf4])
AC_MSG_RESULT([$HDF4DIR])
AM_CONDITIONAL(USE_HDF4_DIR, [test ! "x$HDF4DIR" = x])
AC_SUBST(HDF4DIR, [$HDF4DIR])
AM_CONDITIONAL(USE_HDF4, [test x$enable_hdf4 = xyes])

# Did the user specify a location for the ZLIB library?
AC_MSG_CHECKING([whether a location for the ZLIB library was specified])
AC_ARG_WITH([zlib],
              [AS_HELP_STRING([--with-zlib=<directory>],
                              [Specify location of ZLIB library. ZLIB is required \
			      for netCDF-4. Configure will expect to find subdirs \
			      include and lib.])],
            [ZLIBDIR=$with_zlib])
AC_MSG_RESULT([$ZLIBDIR])
AC_SUBST(ZLIBDIR, [$ZLIBDIR])

# Did the user specify a location for the SZLIB library?
AC_MSG_CHECKING([whether a location for the SZLIB library was specified])
AC_ARG_WITH([szlib],
              [AS_HELP_STRING([--with-szlib=<directory>],
                              [Specify location of SZLIB library. SZLIB is not \
			      required for netCDF-4, but you must specify its location \
			      if HDF5 was built with SZLIB. Configure will expect to \
			      find subdirs include and lib.])],
            [SZLIBDIR=$with_szlib])
AC_MSG_RESULT([$SZLIBDIR])
AC_SUBST(SZLIBDIR, [$SZLIBDIR])

AM_CONDITIONAL(USE_ZLIB_DIR, [test ! "x$ZLIBDIR" = x])
AM_CONDITIONAL(USE_SZLIB_DIR, [test ! "x$SZLIBDIR" = x])

libcf_build_regrid=no
AC_MSG_CHECKING([whether regridding API is desired])
AC_ARG_ENABLE([regrid], 
			[AS_HELP_STRING([--disable-regrid],
			["don't try to build regrid API"])])
if test "x$enable_regrid" != "xno"; then 
   libcf_build_regrid=yes
fi

# Do we want to build the fortran API? Check for --disable-f90
nc_build_f90=no
AC_MSG_CHECKING([whether Fortran 90 API is desired])
AC_ARG_ENABLE([f90],
              [AS_HELP_STRING([--disable-f90],
                              ["don't try to build Fortran 90 API"])])
test "x$enable_f90" = "xno" && nc_build_f90=no
test "x$nc_null_fc" = xyes && nc_build_f90=no
AC_MSG_RESULT([$nc_build_f90])
AM_CONDITIONAL(BUILD_F90, [test "x$nc_build_f90" = xyes])

# Did the user specify a location for the netcdf library?
AC_MSG_CHECKING([whether a location for the netCDF library was specified])
AC_ARG_WITH([netcdf],
              [AS_HELP_STRING([--with-netcdf=<directory>],
                              [Specify location of netCDF library. Configure will expect to find \
			      subdirs include and lib.])], [], [with_netcdf=no])
AC_MSG_RESULT([$with_netcdf])
AM_CONDITIONAL([USE_NETCDF_DIR], [test ! x$with_netcdf = xno])
if test ! x$with_netcdf = xno; then
   AC_SUBST(NETCDFDIR, [$with_netcdf])
   AC_SUBST(NCDUMPDIR, [$with_netcdf/bin])
   LDFLAGS="${LDFLAGS} -L$NETCDFDIR/lib"
   AC_PATH_PROG([NCDUMP], [ncdump], [ncdump], [$with_netcdf/bin$PATH_SEPARATOR$PATH])
else
   AC_SUBST(NCDUMPDIR, [../../../../ncdump])
   AC_PATH_PROG([NCDUMP], [ncdump], [ncdump])
fi

lapacklibdir_l=""
AC_ARG_WITH([lapack-libdir], 
   [AS_HELP_STRING([--with-lapack-libdir=<directory>],
        [Specify location of LAPACK library, required for interpolation and regridding.])],
   [lapacklibdir_l="-L$withval"])
LDFLAGS="$lapacklibdir_l $LDFLAGS"

blaslib=""
AC_ARG_WITH([blas-lib],
   [AS_HELP_STRING([--with-blas-lib=<path>],
       [Specify path to blas library, required for interpolation and regridding.])],
   [blaslib="$withval"])
LIBS="$blaslib $LIBS"

lapacklib=""
AC_ARG_WITH([lapack-lib],
   [AS_HELP_STRING([--with-lapack-lib=<path>],
       [Specify path to lapack library, required for interpolation and regridding.])],
   [lapacklib="$withval"])
LIBS="$lapacklib $LIBS"

mpicc=""
mpiexec=""
AC_ARG_WITH([mpicc],
   [AS_HELP_STRING([--with-mpicc=<path>],
       [Specify path MPI C compiler.])],
   [mpicc="$withval"])
if test "x$mpicc" != "x"; then
     AC_ARG_WITH([mpiexec],
          [AS_HELP_STRING([--with-mpiexec=<path>],
	      [Specify path MPI launcher.])],
     [mpiexec="$withval"])
fi
AC_SUBST(MPICC, [$mpicc])
AC_SUBST(MPIEXEC, [$mpiexec])
if test ! "x$mpiexec" = "x"; then
   AC_SUBST(HAVE_MPI, [1])
else
   AC_SUBST(HAVE_MPI, [0])
fi

# Can we find the nccmp utility?
#AC_PATH_PROG([NCCMP], [nccmp])

AC_MSG_NOTICE([finding other utilities])

AC_PROG_INSTALL

AC_MSG_NOTICE([setting up libtool])

# Produce a windows DLL. This macro must be before AM_PROG_LIBTOOL.
AC_LIBTOOL_WIN32_DLL

# Use libtool.
LT_INIT
AC_PROG_LIBTOOL

# To determine fortran subroutine name mangling (C gridspec needs Fortran lapack/blas)
AC_FC_WRAPPERS
AC_FC_LIBRARY_LDFLAGS
LIBS="$LIBS $FCLIBS"

AC_MSG_NOTICE([checking for large file support])

AC_SYS_LARGEFILE

AC_MSG_NOTICE([checking types, headers, and functions])

AC_CHECK_HEADER(stdlib.h, ,AC_DEFINE([NO_STDLIB_H], [], [no stdlib.h]))
AC_CHECK_HEADER(sys/types.h, ,AC_DEFINE([NO_SYS_TYPES_H], [], [no sys_types.h]))
AC_CHECK_HEADER([unistd.h])
AC_CHECK_FUNC(strerror, ,AC_DEFINE([NO_STRERROR], [], [no strerror.h]))
AC_CHECK_FUNC(strlcat,AC_DEFINE([HAVE_STRLCAT],1,[Define this if you have strlcat()]))
AC_FUNC_ALLOCA
AC_STRUCT_ST_BLKSIZE
AC_TYPE_SIZE_T
AC_TYPE_OFF_T
AC_CHECK_TYPE(ssize_t, int)
AC_CHECK_TYPE(ptrdiff_t, int)
AC_CHECK_TYPE(uchar, unsigned char)
AC_C_CHAR_UNSIGNED
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(off_t)
#AC_CHECK_SIZEOF(size_t)

AC_SEARCH_LIBS([deflate], [z])
AC_SEARCH_LIBS([ceil], [m])
AC_SEARCH_LIBS([H5open], [hdf5])
AC_SEARCH_LIBS([H5DSattach_scale], [hdf5_hl])
AC_SEARCH_LIBS([Curl_cfree], [curl])
AC_SEARCH_LIBS([nc_open], [netcdf])

# These tests are written in such a way that a fortran compiler is not 
# necessary. Therefore not using AC_FC_FUNC to determine the number of
# underscores
if test "$libcf_build_regrid" != "no"; then
possible_blas_library_names="sci_quadcore_mp nag essl mkl f77blas blas"
AC_SEARCH_LIBS([dcopy_], [$possible_blas_library_names],
   [AC_DEFINE([HAVE_BLAS_LIB], [1], [blas library containing dcopy_ was found])], 
        [AC_SEARCH_LIBS([dcopy], [$possible_blas_library_names],
	  [AC_DEFINE([HAVE_BLAS_LIB], [1], [blas library containing dcopy was found])]
	 )]
)
possible_lapack_library_names="sci_quadcore_mp nag essl mkl lapack"
AC_SEARCH_LIBS([dgetrf_], [$possible_lapack_library_names], 
   [AC_DEFINE([HAVE_LAPACK_LIB], [1], [lapack library containing dgetrf_ was found])],
      AC_SEARCH_LIBS([dgetrf], [$possible_lapack_library_names],
        [AC_DEFINE([HAVE_LAPACK_LIB], [1], [lapack library containing dgetrf was found])]
      )
)
fi

AC_CHECK_HEADERS([$NETCDFDIR/include/netcdf.h], [], 
	[AC_MSG_ERROR([Header netcdf.h could not be found. Please specify --with-netcdf=<netcdf_dir>.])])

# some parts rely on uuid, if not found we will fall back on own code to 
# generate unique identifiers
HAVE_UUID_H="no"
AC_LANG_PUSH(C)
AC_LINK_IFELSE(
	[AC_LANG_PROGRAM([#include <uuid/uuid.h>], 
                        [uuid_t x; uuid_clear(x);])], 
	[uuid_clear_compiles=yes], 
   	[uuid_clear_compiles=no])
AC_LANG_POP(C)
libuuid=""
if test "$uuid_clear_compiles" = "no"; then
   # on som platforms (e.g. linux), need to link against -luuid
   AC_CHECK_LIB([uuid], [uuid_generate], [libuuid="-luuid"])
   if test "x$libuuid" != "x"; then
      LIBS="$LIBS $libuuid"
      uuid_clear_compiles=yes
   fi
fi
AC_MSG_WARN([Successfully compiling code containing uuid? $uuid_clear_compiles])
if test "$uuid_clear_compiles" = "yes"; then
   HAVE_UUID_H="yes"
   AC_DEFINE([HAVE_UUID_H], [1], [Define if uuid is found])
fi
AM_CONDITIONAL([HAVE_UUID], [test "x$HAVE_UUID_H" = "xyes"])
if test "x$HAVE_UUID_H" = "xyes"; then
   AC_SUBST([HAVE_UUID], [1])
else
   AC_SUBST([HAVE_UUID], [0])
fi

# Python (not required but nice to have)
PYTHON=""
AC_CHECK_PROGS([PYTHON],[python python2])
AM_CONDITIONAL(HAVE_PYTHON, test "x$PYTHON" != "x")

AC_MSG_NOTICE([generating header files and makefiles])

AC_CONFIG_FILES([gridspec/tools/run_hgrid_tests], 
	           [chmod +x gridspec/tools/run_hgrid_tests])
AC_CONFIG_FILES([gridspec/tools/run_vgrid_tests], 
	           [chmod +x gridspec/tools/run_vgrid_tests])
AC_CONFIG_FILES([gridspec/tools/run_topog_tests], 
	           [chmod +x gridspec/tools/run_topog_tests])

AC_CONFIG_FILES([Makefile
		 pycf/Makefile
		 pycf/__init__.py
		 testpy/Makefile
                 cfcheck/Makefile
                 src/Makefile
                 include/Makefile
		 gridspec/Makefile
                 gridspec_api/Makefile
                 gridspec_api/global/Makefile
                 gridspec_api/coord/Makefile
                 gridspec_api/grid/Makefile
                 gridspec_api/data/Makefile
                 gridspec_api/regrid/Makefile
                 gridspec_api/mosaic/Makefile
                 gridspec_api/host/Makefile
		 gridspec/libsrc/Makefile
		 gridspec/libsrc/run_ref_checks.sh
		 gridspec/tools/Makefile
		 examples/Makefile
		 setup.py],
		 [test -f gridspec/libsrc/run_ref_checks.sh && chmod 755 gridspec/libsrc/run_ref_checks.sh ])

AC_OUTPUT()

