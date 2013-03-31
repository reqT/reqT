#!/bin/bash
JARFILENAME=~/reqT/lib/reqT.jar
export CLASSPATH=$CLASSPATH:$JARFILENAME
scala $JARFILENAME $@

