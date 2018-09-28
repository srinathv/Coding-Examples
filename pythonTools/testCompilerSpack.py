#!/usr/bin/env python


import subprocess
result = subprocess.check_output(['spack','list']).split()
for package in result:
    print package

#subprocess.call(['spack','clone','${HOME}/spack-test'])
subprocess.call(['spack','uninstall','--a','--y'])
file = open(“spackInstall_arm_18.4.1.txt”,”w”) 


testPlist=['zip','zlib','zsh']
for testPackage in testPlist:
    tpwcompiler=testPackage+'%arm@18.4.1'
#    print tpwcompiler
    result=subprocess.check_output(['spack','install','--fake',tpwcompiler]).split()
#    print result
#    print result[2] + " is " + result[8]
    didItInstall=result[2] + " is " + result[8]
    file.write(didItInstall)
    #print subprocess.call(['spack','install','--fake',tpwcompiler])

file.close()
def main():
    pass

if __name__ == "__main__":
    main()