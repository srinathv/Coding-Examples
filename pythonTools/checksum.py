import os, sys

dirList=os.listdir(os.getcwd())
for fname in dirList:
    os.system('md5 '+fname)
    os.system('openssl sha1 '+fname)
    os.system('openssl rmd160 '+fname)

