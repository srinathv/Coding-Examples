#!/usr/bin/env python
import argparse
import sys

parser = argparse.ArgumentParser(description='Spack install test given compiler.')
parser.add_argument('compiler',help='give compiler syntax')
parser.add_argument('--test',help='test with zip.zlib,zsh',action="store_true")

args = parser.parse_args()

fileName='spackInstall_' + args.compiler.replace('@','_') + '.txt'

import subprocess

allPackages = subprocess.check_output(['spack','list']).split()
for package in allPackages:
    print package

#subprocess.call(['spack','clone','${HOME}/spack-test'])
subprocess.call(['spack','uninstall','--a','--y'])
file = open(fileName,'w') 

if args.test:
    packageList=['zip','zlib','zsh']
else:
    packageList = allPackages

for package in packageList:
    tpwcompiler = package + '%' + args.compiler
    result=subprocess.check_output(['spack','install','--fake',tpwcompiler]).split()
#    didItInstall=result[2] + " is " + result[8]
#    file.write(didItInstall + '\n')
#    print result

installedFiles=subprocess.check_output(['spack','find'])
file.write(installedFiles)
file.close()
def main():
    pass

if __name__ == "__main__":
    main()