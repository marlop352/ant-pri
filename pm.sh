#!/bin/bash
file="/home/$USER/ant-pri/dd_map.txt"

while IFS= read line 
do
    RESULT=$(grep -o "$line" $file | wc -l)

    if test $RESULT = "2"
    then
        echo "50% of $line"
    elif test $RESULT = "3"
    then
        echo "75% of $line"
    elif test $RESULT = "4"
    then
        echo "100% of $line"
    fi
done <"$file"


