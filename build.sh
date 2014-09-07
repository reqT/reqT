#!/bin/bash

JAR_FILENAME=reqT.jar

function error_check {
   local EXIT_STATUS=$?
   if ! [ $EXIT_STATUS -eq 0 ]; then
      echo "ERROR! Exit Status: $EXIT_STATUS"
      exit
   fi
}

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
scalac -feature -deprecation -cp "lib/*" -d bin src/reqT/* > /dev/null 2>&1
error_check
echo "Compilation ready!  $(date)"

#COPY RESOURCES
echo "Copying resources  ...  $(date)"
cp -rf resources/* bin/
error_check

#PACKAGE
echo "Packaging reqT into jar file: $JAR_FILENAME"
echo "Start packaging ... $(date)"
jar cfe $JAR_FILENAME reqT.Main -C bin/ . > /dev/null 2>&1
error_check
echo "Packaging ready!    $(date)"

#CREATE DIRS
mkdir -p ~/reqT/lib
mkdir -p ~/reqT/bin

#COPY BUILD FILES
echo "Copying $JAR_FILENAME to $HOME/reqT/lib/"
cp -rf $JAR_FILENAME $HOME/reqT/lib/

echo "Copying reqT.sh to $HOME/reqT/bin/"
cp -rf reqT.sh $HOME/reqT/bin/

echo "If $HOME/reqT/bin is in your Path you can run reqT as a command"
