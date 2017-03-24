#! /bin/bash

#####
# on Lightning:  need to module swap PrgEnv-cray PrgEnv-intel"



cloneCMD="git clone git@github.com:ParaToolsInc/taucmdr.git"
installDir="$PET_HOME/pkgs/taucmdr-latest"
configLine="./configure --prefix=$PET_HOME/pkgs/taucmdr-latest"
setPerm="chmod -R a+rX,g+w ${installDir}"
setGrp="chgrp -R petttace  ${installDir}"
tsInstallDir="$PET_HOME/pkgs/threadspotter-1.3.10"
mkInstCmd="make install"
branchCmd="git checkout unstable"

if ${cloneCMD} ; then
  echo "git clone succeeded"
else
  echo "git clone failed"
  exit 1
fi



if [ -d "$installDir" ]; then
  echo "Removing old installtion."
  rm -rf "$installDir"
fi

cd taucmdr
${branchCmd}
echo "checkouted unstable branch"


echo "Trying to configure Tau Commander."
if ${configLine} ; then
   echo "Tau Commander configured."
else
  echo "Tau Commander configure failed."
  exit 1
fi

echo "Trying to make and install."
if ${mkInstCmd}; then
  echo "Tau Commander SUCCESFULLY installed."
else
  echo "Tau Commander FAILED to installed."
  exit 1
fi


if ${setPerm} ; then
  echo "${setPerm} succeeded"
else
  echo "${setPerm} failed."
  exit 1
fi

if ${setGrp} ; then
  echo "${setGrp} succeeded"
else
  echo "${setGrp} failed."
  exit 1
fi


#*****
echo "Is  threadspotter-1.3.10 installed???"

if [ -d "$tsInstallDir" ]; then
  echo "YES!!"
else
  echo "NO!!!"




