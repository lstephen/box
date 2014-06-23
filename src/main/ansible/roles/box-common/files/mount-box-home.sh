#!/bin/bash

set -e

mkdir -p /data/home
mount -t aufs -o br:/data/home=rw:/etc/skel=ro none /home/box
chown box:box /home/box

# This last chown being performed after the mount is important.
# Otherwise box doesn't have permissions to their own home directory.
# This is the only thing preventing this being in fstab

