#!/bin/bash

set -e

if [ "$SERF_EVENT" = "member-leave"  \
  -o "$SERF_EVENT" = "member-failed" \
  -o "$SERF_EVENT" = "member-join" ]
then
  read input
  hostname=`echo $input | awk '{print $1}'`
  self=`hostname`

  if [ "$hostname" = "$self" ]
  then
    exit 0
  fi
  
  # Remove any existing line for the member we received the event for
  echo "Removing $hostname from /etc/hosts"
  sed -i "/$hostname/d" /etc/hosts

  members=`serf members`

  # Read the members list and update entries
  while read -r line;
  do
    hostname=`echo $line | awk '{print $1}'`
    ipaddress=`echo $line | awk '{print $2}' | cut -d ':' -f1`
    
    echo "Updating $hostname to $ipaddress in /etc/hosts"
    sed -i "/$hostname/d" /etc/hosts
    sed -i '$a'"$ipaddress $hostname" /etc/hosts
  done <<< "$members"

fi

