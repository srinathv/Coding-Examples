#!/usr/bin/env python


import subprocess
result = subprocess.check_output(['spack','list']).split()
for package in result:
    print package

testPlist=['zip','zlib','zsh']
for testPackage in testPlist:
    tpwcompiler=testPackage+'%gcc@8.2.0'
    print tpwcompiler
#    print subprocess.check_output(['spack','install','--fake',tpwcompiler])
    print subprocess.call(['spack','install','--fake',tpwcompiler])
def main():
    pass

if __name__ == "__main__":
    main()