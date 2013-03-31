#!/bin/bash
JARFILENAME=~/reqT/lib/reqT.jar
export CLASSPATH=$CLASSPATH:$JARFILENAME
echo "Appending $JARFILENAME to CLASSPATH"
echo "$CLASSPATH"
scala $JARFILENAME $@

