#!/bin/bash

set -e

mkdir -p /data/home
mount -t aufs -o br:/data/home=rw:/etc/skel=ro none /home/box
chown box:box /home/box

