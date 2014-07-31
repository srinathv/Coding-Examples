import fileinput
import sys
import glob

def replaceAll(file,searchExp,replaceExp):
    for line in fileinput.input(file, inplace=1):
        if searchExp in line:
            line = line.replace(searchExp,replaceExp)
        sys.stdout.write(line)


F90Files=glob.glob("*.F90")
print ("list of globed files = " + str(F90Files))

oldline='(defined ELEMENT_OPENMP)'
newerline='defined(ELEMENT_OPENMP)'
newline='defined(ELEMENT_OPENMP) || defined(NESTED_OPENMP)'

#for ofile in F90Files:
ofile='edge_mod.F90'
#with open(ofile, "r+") as out:
#    for line in out:
#        #out.write(line.replace(oldline, otheroldline))
#      if oldline in line: 
#        print line
#        print line.replace(oldline, otheroldline)
#        out.write(line.replace(oldline, otheroldline))
#      if otheroldline in line:
#        print line
#        print line.replace(otheroldline, newline)
#        out.write(line.replace(otheroldline,newline))
#      
for lines in fileinput.FileInput(ofile,inplace=1):
    lines = lines.replace(oldline,newerline)
    print lines

for lines in fileinput.FileInput(ofile,inplace=1):
    lines = lines.replace(newerline,newline)
    print lines


