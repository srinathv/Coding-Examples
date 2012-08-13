This code archive derives from the examples in the textbook Scientific Software Design: The Object-Oriented Way by Damian Rouson, Jim Xia, and Xiaofeng Xu (Cambridge University Press, 2011).  The preferred method for compiling the code in this directory relies upon the automated, open-source CMake build system (http://www.cmake.org).  After installing CMake, set your current directory to this archive's ssdBuild subdirectory and run the build script corresponding to your compiler (after editing the script to update its details to match your configuration) using a command such as  

../buildScript/ibm-compilers-cmake.sh

The above command would use the IBM XL Fortran and C++ compilers to build the executable files and the library file.

Most subdirectories in this archive also contain Makefiles that can be used to compile the code on platforms with the "make" utility (http://en.wikipedia.org/wiki/Make_(software)).  Each Makefile indicates which compilers correctly compile the code as of August 2011.  We deprecate the use of these Makefiles because of the amount of effort that would be required to keep them up-to-date and the number of files users must customize to build the full archive.  

This archive makes extensive use of the explicit support for object-oriented programming in the Fortran 2003 standard.  As of August 2011, two compilers fully support the Fortran 2003 standard:
1. IBM XL (xlf)
2. The Cray Compiler Environment (cce)
The IBM compiler correctly compiles all examples in this archive.  Bugs in the Cray compiler prevent compilation of code in Chapters 3 and 9 and generate run-time errors in Chapers 5 and 8.  We are in the process of reporting these bugs.  

Pre-release versions of two compilers nominally support all of the Fortran 2003 features in the book and will compile the whole archive once bugs we have reported are fixed:
3. Numerical Algorithms Group (nagfor)
4. Intel (ifort)
One compiler supports all but a handful of Fortran 2003 constructs used in the book and will compile most of the book's examples once bugs we have reported are fixed:
5. Gnu (gfortran)
We have tested this archive on each of these compilers and have adjusted the CMake files to build only those examples that work with the latest version of each compiler (including pre-release or beta compilers where available).  The Portland Group Fortran compiler supports most of the requisite features but has not been tested by the authors.  The Pathscale and g95 compilers do not support a sufficient number of Fortran 2003 features to compile most code in the book.

We encourage readers interested in compiling with an open-source compiler to contribute code to the gfortran project and let the gfortran developers know about your interest in resolving the following bug reports by e-mailing fortran@gcc.gnu.org:
* Bug 39427: This precludes user-defined structure constructors. Below is a workaround for this bug.
* Bug 47545: This prevents compiling with deferred-length components. A workaround is chapter02/figure2.4/gfortran_oo_hello_world.F90.
* Bug 45170 (comment 9): This presumably precludes returning character variables with allocatable lengths.
* Bug 37336: This prevents use of final subroutines. Chapter 5 describes a workaround of sorts.
* Bug 18918: This prevents use of Fortran 2008 coarrays in chapter12/burgers_caf_v4. No workaround is provided.
* Bug 46328: This prevents the occurrence of non-trivial polymorphic operands in type-bound operator calls as in chapter07/strategy_surrogate_f2003. We have no workaround for this.

To work around Bug 39427, we appended an underscore to all user-defined structure constructor names in this archive (no such underscores appear in the book).  Consequent adjustments have been made to the corresponding "public" and "use" statements.  

Any updates to this archive will be accessible via the "Resources Available" link at http://wwww.cambridge.org/Rouson.
