#! /bin/bash


cloneCMD="git clone git@github.com:ParaToolsInc/taucmdr.git"
installDir="$PET_HOME/pkgs/taucmdr-latest"
configLine="./configure --prefix=$PET_HOME/pkgs/taucmdr-latest"
setPerm="chmod -R go+rX,g+w ${installDir}"
setGrp="chgrp -R petttace  ${installDir}"
tsInstallDir="$PET_HOME/pkgs/threadspotter-1.3.10"

if ${cloneCMD} ; then
  echo "git clone succeeded"
else
  echo "git clone failed"
  exit 1
fi

if [ -d "$installDir" ]; then
  echo "Moving old installtion."
  mv "$installDir" ${installDir}.mv
fi

cd taucmdr
echo "Trying to configure Tau Commander."
if ${configLine} ; then
   echo "Tau Commander configured."
else
  echo "Tau Commander configure failed."
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




