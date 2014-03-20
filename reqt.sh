#!/bin/bash

REQT_JAR=~/reqT/lib/reqT.jar
scala -toolcp $REQT_JAR $REQT_JAR $@

