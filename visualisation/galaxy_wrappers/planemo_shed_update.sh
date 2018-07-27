#!/bin/bash

TARGET=$1
#TARGET="toolshed"
#TARGET="testtoolshed"
#TARGET="local"


realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

ROOTPATH=$(dirname $(realpath "$0"))

cd $ROOTPATH/Barplots
planemo shed_update -t $TARGET

cd $ROOTPATH/Heatmaps
planemo shed_update -t $TARGET
