#!/bin/bash

JAR_FILENAME=reqT.jar

function error_check {
   local EXIT_STATUS=$?
   if ! [ $EXIT_STATUS -eq 0 ]; then
      echo "ERROR! Exit Status: $EXIT_STATUS"
      exit
   fi
}

#SET SCALA VERSION
source ~/.sdkman/bin/sdkman-init.sh
sdk use scala 2.12.15
scala -version
error_check

#CLEAN
source makebin.sh
if ! [ -d bin/reqT ]; then
   echo "Cleaning bin/reqT"
   rm -rf bin/reqT
   error_check
fi

#COMPILE
echo "scalac -feature -deprecation -cp \"lib/*\" -d bin src/reqT/*"
echo "Start compilation  ...  $(date)"
scalac -feature -deprecation -cp "lib/*" -d bin src/reqT/*
error_check
echo "Compilation ready!  $(date)"

#COPY RESOURCES
echo "Copying resources  ...  $(date)"
cp -rf resources/* bin/
error_check

#PACKAGE
echo "Packaging reqT into jar file: $JAR_FILENAME"
echo "Start packaging ... $(date)"
jar cfe $JAR_FILENAME reqT.Main -C bin/ . 
error_check
echo "Packaging ready!    $(date)"

#CREATE DIRS
mkdir -p ~/reqT/lib
mkdir -p ~/reqT/bin

#COPY BUILD FILES
echo "Copying $JAR_FILENAME to $HOME/reqT/lib/"
cp $JAR_FILENAME ~/reqT/lib/.

echo "Copying reqt.sh to $HOME/reqT/bin/reqt"
cp reqt.sh ~/reqT/bin/reqt
chmod a+x ~/reqT/bin/reqt

echo "Put $HOME/reqT/bin in your Path to run reqt as a command"

