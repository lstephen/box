#!/bin/bash

set -e

for f in `find /etc/serf/event_handler.d -name '*.sh' | sort -V`
do
    if [[ $f = *.sudo.sh ]]
    then
      echo "Executing with sudo: $f"
      sudo $f
    else 
      echo "Executing $f"
      $f
    fi
done

