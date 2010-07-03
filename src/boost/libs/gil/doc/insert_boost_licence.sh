#!/bin/sh
ii=`expr $#`
for file in $@
  do
  if [ $ii -lt $# ]
      then
      echo Processing $file ...
      cat $1 $file > tmp
      mv tmp $file
  fi
  ii=`expr $ii - 1`
done
