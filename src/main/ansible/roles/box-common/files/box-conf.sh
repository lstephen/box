#!/bin/bash

set -e

for f in `find /etc/box/ -name '*.conf' | sort -V`
do
    source $f
done

