The programs are archived as a self-extracting archive. To extract:

1. Your computer must be in DOS (version 6.22 or earlier) mode. Windows 
cannot be running; the programs will not extract while Windows (any 
version) is running.

2. At the screen prompts, type:

MD C:\NUMRCL <enter>
CD C:\NUMRCL <enter>
XCOPY [A:] PROGRAMS.EXE <enter>     (replace "[A:]" with the 
correct floppy drive name, if different)
PROGRAMS.EXE -d <enter>

This will extract all the programs, in the original subdirectories, into 
the new directory "NUMRCL". C programs are in the C subdirectory, 
FORTRAN in the FOR subdirectory, Mathematica in the MA subdirectory, 
Maple in the MWS subdirectory, and Pascal in the PAS subdirectory. The 
DTA subdirectory contains data files for all the programs.
