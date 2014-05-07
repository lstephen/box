#!/bin/bash

set -e

if [ "$SERF_EVENT" = "member-leave" -o "$SERF_EVENT" = "member-failed" ]
then
  read input
  hostname=`echo $input | awk '{print $1}'`
  directory=/mnt/$hostname

  if [[ $input =~ "role=store" && -d "$directory" ]]
  then
    echo "Unmounting $directory"
    umount $directory || true
    rmdir $directory
  fi
fi

if [ "$SERF_EVENT" = "member-join"  ]
then

  stores=`serf members | grep "role=store"`

  # Read the members list and update entries
  while read -r line;
  do
    hostname=`echo $line | awk '{print $1}'`
    directory=/mnt/$hostname

    if [ ! -d $directory ]
    then
      echo "Mounting $directory"
      mkdir $directory
      mount -t cifs -o guest //$hostname/export $directory
    fi
  done <<< "$stores"

fi
