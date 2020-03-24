import os,sys
File = "LICENSE"

packages={}#package name : license type
licensesPhrase=["GENERAL PUBLIC LICENSE","BSD ","CeCILL-C ", "Apache ", "MIT "]
versions={"Version 2.0":"v2.0", "Version 2":"v2", "Version 3":"v3"}
#http://trilinos.sandia.gov/license.html

#if GPL, then determine version 2 or version3 .. GPL_v2, GPL_v3

for root, dirs, files in os.walk('.'):
    for file in files: # loops through directories and files
        if file == File: # compares to your specified conditions
            packageName=root.split('/')[-1]
            fullFileName=root + "/"+ file
            name=fullFileName
            #name=packageName
            with open(fullFileName) as myfile:
              data=myfile.read()
              for licenseType in licensesPhrase:
                if licenseType.lower() in data.lower():
                  if licenseType.lower()=="GENERAL PUBLIC LICENSE".lower():
                    for key, value in versions.items():
                      if key.lower() in data.lower():
                        licenseType="GPL_" + value
                        break
                  packages[name]=licenseType
                  break
            if name not in packages:
              packages[name]="OTHER"
print (len(packages))

for k in sorted(packages, key=packages.get, reverse=False):
 print (k + " has license type " + packages[k])


