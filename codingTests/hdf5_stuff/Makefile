# *******************************************************************
#
# Makefile for the sample-read.f90 program
#
# *******************************************************************

SHELL  = /bin/sh

# Compiler and linker options
# ===========================

#FC     = ifort
FC     = mpif90
FFLAGS = 

LNK    = $(FFLAGS) -o
LBS    = 

# HDF stuff

#HDF5   = /usr/local
HDF5   = /Users/srinath/software/hdf5-par
HDFLBS = -L$(HDF5)/lib -lhdf5_fortran -lhdf5_f90cstub -lhdf5
INC    = -I$(HDF5)/include -I$(HDF5)/fortran

# Paths where make searches for the source files.

VPATH  = .

# Define what to do with what suffix.

.f90.o:
	$(FC) $(FFLAGS) $(INC) -c $<

.SUFFIXES:
.SUFFIXES: .f90 .o .mod


# Executables
# ===========

EXECS = sample-read


# Make entries
# ============

all:	$(EXECS)


# Object lists
# ============

SAMPLEOBJ = readh5dataset.o sample-read.o


# Dependencies
# ------------

sample-read.o: readh5dataset.o


# Rules for linking
# -----------------

sample-read: $(SAMPLEOBJ)
	$(FC) $(LNK) sample-read $(SAMPLEOBJ) $(LBS) $(HDFLBS)

# -----------------------------------------------------

# Other things
# ============

clean:
	@rm -f *.o *.mod $(EXECS)
rmbak:
	@rm -f *~ *.bak

PACKAGE = GPL.txt  README.txt  Makefile  readh5dataset.f90  \
          sample-read.f90  sample-file.h5  sample-file.out  \
          sample-file.txt.zip  

package:
	@echo " --- Making package ready for release"
	@zip -q sample-file.txt.zip sample-file.txt
	@zip -q hdf5read.zip $(PACKAGE)
	@echo " --- File 'hdf5read.zip' made"

# -----------------------------------------------------

# end of Makefile
# *******************************************************************
