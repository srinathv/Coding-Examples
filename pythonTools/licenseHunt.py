import os
File = "LICENSE"

for root, dirs, files in os.walk('.'):
    for file in files: # loops through directories and files
        if file == File: # compares to your specified conditions
            print ("File exists")
            print (root)
            print (dirs)
