#!/bin/bash

set -e

function as_root {
    echo 'box' | sudo -S $@
}

as_root rm /etc/udev/rules.d/70-persistent-net.rules
as_root ln -s /dev/null /etc/udev/rules.d/75-persistent-net-generator.rules

