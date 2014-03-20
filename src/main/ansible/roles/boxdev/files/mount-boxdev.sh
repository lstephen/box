#!/bin/bash

set -e

function formatIfRequired {
    if [[ -z `blkid $1` ]]
    then
        mkfs.ext4 -F $1
    fi
}

function formatAndMount {
    formatIfRequired $1
    mkdir -p $2
    mount $1 $2
}

formatAndMount /dev/sdb /mnt/home
formatAndMount /dev/sdc /mnt/sync

mkdir -p /home/boxdev
mount -t aufs -o br:/mnt/home=rw:/etc/skel=ro none /home/boxdev

chown boxdev:boxdev /home/boxdev

