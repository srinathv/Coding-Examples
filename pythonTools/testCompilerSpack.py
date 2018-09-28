#!/usr/bin/env python
import argparse
import sys

parser = argparse.ArgumentParser(description='Spack install test given compiler.')
#parser.add_argument('integers', metavar='N', type=int, nargs='+',
#                   help='an integer for the accumulator')
parser.add_argument('compiler',help='give compiler syntax')

args = parser.parse_args()

print(args.compiler)
fileName='spackInstall_' + args.compiler + '.txt'

import subprocess

result = subprocess.check_output(['spack','list']).split()
for package in result:
    print package

#subprocess.call(['spack','clone','${HOME}/spack-test'])
subprocess.call(['spack','uninstall','--a','--y'])
file = open(fileName,'w') 


testPlist=['zip','zlib','zsh']
for testPackage in testPlist:
    #tpwcompiler=testPackage+'%arm@18.4.1'
    tpwcompiler=testPackage + '%' + args.compiler
    result=subprocess.check_output(['spack','install','--fake',tpwcompiler]).split()
    didItInstall=result[2] + " is " + result[8]
    file.write(didItInstall + '\n')

installedFiles=subprocess.check_output(['spack','find'])
file.write(installedFiles)
file.close()
def main():
    pass

if __name__ == "__main__":
    main()