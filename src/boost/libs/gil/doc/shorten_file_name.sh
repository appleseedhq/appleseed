#!/bin/sh
if [ $# -lt 1 ]
    then
    $0 html
    $0 png
    exit
fi

echo $# extensions to process
for file_extension in $@
  do
  echo Processing extension $file_extension ...
  kk=0
  for long_file_name in *.$1
    do
    file_name_length=`expr length $long_file_name`
    if [ $file_name_length -gt 20 ]
        then
        kk=`expr $kk + 1`
        short_file_name=`printf "g_i_l_%04d.$1" $kk`
        echo \ \ Shortening $long_file_name to $short_file_name ...
        for ii in `grep -l $long_file_name *`
          do
          sed_string="s/\\\"$long_file_name/\\\"$short_file_name/g"
          sed -i $sed_string $ii
        done
        mv $long_file_name $short_file_name
    fi
  done
done
