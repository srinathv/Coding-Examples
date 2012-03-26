#! /opt/local/bin/python

import tables


timefile=tables.openFile('out.gyro.timedata.h5')
planefile=tables.openFile('gyro00000.h5')

print planefile
print timefile
