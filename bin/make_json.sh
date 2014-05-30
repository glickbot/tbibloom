#!/bin/bash

POSSIBLE_TERMS=1000
MAX_TERMS=22
MIN_TERMS=2

if [ -z $1 ]; then
    echo "No file location:"
    echo "$0 file.json <number>"
    exit 1
fi

if [ -z $2 ]; then
    echo "No number given:"
    echo "$0 file.json <number>"
    exit 1
fi

JSON=$1
NUM=$2
MAXLESSMIN=$[ 22 - 2 ]

echo -n "[" > $JSON
for i in `seq 1 $NUM`; do
    if [ $i != 1 ]; then
        echo "," >> $JSON
    fi
    this_json="["
    this_termnum=$[ $RANDOM % $MAXLESSMIN + $MIN_TERMS ]
    for t in `seq 1 $this_termnum`; do
        TERM=$(echo $[ $RANDOM % $POSSIBLE_TERMS ] | md5)
        if [ $t != 1 ]; then
            this_json+=","
        fi
        this_json+="\"$TERM\""
    done
    this_json+="]"
    echo -n $this_json >> $JSON
done
echo "]" >> $JSON
