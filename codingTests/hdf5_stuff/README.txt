**************************************************************************

   Module for reading data from HDF-5 and HDF-EOS files with Fortran-90

**************************************************************************

0. Introduction
---------------

This module is written to read user selected datasets and attributes from
HDF-5 and HDF-EOS datafiles for further processing without having to go
into the details of reading such files. The user only needs to know what
he/she wants to read and of what data type (integer, real, string, ...)
and rank (scalar, vector, ...) it is.

The package includes a sample data file and a sample program showing how
the module has to be used. The routines have been tested on only a few
HDF-5 and HDF-EOS files and does certainly not cover all possible types
of data in such files.

The most recent version of the package is available at:
    http://uv-vis.aeronomie.be/software/tools/hdf5read.php

Current version: 1.5 (23 April 2008)


1. Contents of this file
------------------------

0. Introduction
1. Contents of this file
2. Contents of the package
3. Compiling the module
4. Usage of the module
5. Overview of data read by the module
6. Error handling
7. Authors and version history                     


2. Contents of the package
--------------------------

The package contains the following files

  GPL.txt           -- a copy of the GNU Public License, under which
                       this package is released
  Makefile          -- for compiling the sample program
  README.txt        -- this file
  readh5dataset.f90 -- file with the F-90 modules
  sample-read.f90   -- sample program
  sample-file.h5    -- sample HDF-5 file
  sample-file.out   -- output of sample program to screen when run
                       on the sample file
  sample-file.txt   -- dump with h5dump of sample-file.h5

Note: the original name of the sample data file was
  S-O3M_GOME_O3-SO2_L2_20080201230940_002_METOPA_06681_DLR_01.HDF5
which is a GOME-2 level-2 file with SO2 data produced by DLR (Germany).


3. Compiling the module
------------------------

Requirements for the module to work are a Fortran-90 compiler and linker,
a suitable make utility and properly installed HDF-5 libraries.

To compile and link the sample program, a Makefile is supplied in the
package; just typing 'make' should do the trick.

The Makefile is made for and tested with the GNU make utility.

Compiling and linking on your system may require adapting the compiler
and linker settings and the paths to the HDF-5 libraries.


4. Usage of the module
----------------------

To use the module, add a "USE ReadH5dataset" statement to the calling
routine or program. 

The calling routine or program must contain a variable of the same type
and dimension (rank) as the dataset or attribute that is to be read.
The exact sizes of arrays are determined automatically.

The usage of the module is illustrated in the sample-read program, which
can be run on the sample-file.h5 datafile:

    sample-read  sample-file.h5

This writes some elements of the data file to the screen; see the file
sample-file.out

When reading a dataset or attribute from an HDF 5 file, call the
generic routine H5ReadDataset:

   CALL H5ReadDataset ( filename, dsetname, dataset )

or in the case of compound data:

   CALL H5ReadDataset ( filename, dsetname, compoundsetname, dataset )

Alternatively, one can make the following generic calls (which sound
better when reading an attribute) with exactly the same functionality:

   CALL H5ReadAttribute ( filename, attribname, attribute )
   CALL H5ReadAttribute ( filename, attribname, compoundsetname, attribute )

In these calls are:

   'filename'   = a character type variable containing the name of the
                  HDF-5 file

   'dsetname'   = the character type name of the dataset or attribute,
                  including the full HDF-5 group hierarchy, using slashes
                  as the divider symbol (e.g. "/DATA/GEOMETRY/radius");
                  be sure to use the right case on the hierarchy names
   'attribname' = same as 'dsetname' in functionality

   'compoundsetname' = name of the part of a compound set to read

   'dataset'    = a pointer type variable of the right rank and type,
                  that will contain the dataset or attribute
   'attribute'  = same as 'dataset' in functionality


5. Overview of data read by the module
--------------------------------------

The module is designed to read individual, user selected datasets and
attributes from an HDF-5 or HDF-EOS file. The following data types are
supported:

  a) 0-, 1-, 2- or 3-dimensional datasets or attributes, which can be 
     of integer, real or double precision data type (0-dim. means here
     a scalar).

  b) 0- and 1-dimensional character strings in attributes or datasets   
     (0-dim. means here a single string, 1-dim. a 1-D array of strings).

  c) 0- and 1-dimensional integer, real or double parts of compound  
     attributes or datasets (0-dim. means here a scalar); note that  
     the different parts of a compound set have to be read separately.

See the sample-read.f90 program for examples.

The routines handle datasets of groups, attributes of groups, as well
as attributes of datasets of groups:
     /<N_groups>/dataset
     /<N_groups>/attribute
     /<N_groups>/dataset/attribute
where 'N' is 0, 1, 2, ...


6. Error handling
-----------------

If an error occurs in either of the steps of the access to the
data or when reading the data, the routine returns to the calling
routine or program with the variable ErrorFlag set to -1 and the
ErrorMessage string filled with a message. These two can be used
for error tracing in the calling routine.

Note that some routines return a possitive ErrorFlag number, which
does _not_ indicate an error (which is a bit confusing).


7. Also available in the module
-------------------------------

Apart from the data reading routines, the module also makes the following
function available for use:

      INTEGER FUNCTION LengString(str)

This function determines the length of a given string, that is: the last
non-space character of a given string (omitting also the NULL character,
tab, carriage return, line feed, etc).

The function is used in the sample-read.f90 program.


7. Authors and version history
------------------------------

The original module was written by Jeroen van Gent, when at the Royal
Netherlands Meteorological Institute (KNMI), De Bilt, The Netherlands.

The module has been extended by Jos van Geffen (jos.vangeffen@aeronomie.be)
and Jeroen van Gent (jeroen.vangent@aeronomie.be) at the Belgian Institute
for Space Aeronomie (BIRA-IASB), Brussels, Belgium.


  v0.9 - 2003       - original version: reads 1-, 2- and 3-dimensional
                      integer, real or double datasets (no attributes)
  v1.0 - 2004/12/20 - integer type mismatch resolved in calls to h5dread_f
  v1.1 - 2008/02/12 - comments extended
  v1.2 - 2008/04/09 - streamlined opening of groups, datasets, etc.
                    - added attribute reading to existing routines 
                    - added error handling and debug messaging
         2008/04/11 - added 0-dim. integer, real, double reading
                    - added 0-dim. string reading
         2008/04/13 - added 1-dim. string reading
  v1.3   2008/04/21 - added 0- and 1-dim compound reading (int,real,double)
                    - move string and compound reading into interface
                    - error handling of (de)allocate statements
  v1.5   2008/04/23 - public release as package via website


**************************************************************************
