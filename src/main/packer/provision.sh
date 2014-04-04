#!/bin/bash

set -e

function as_root {
    echo 'box' | sudo -S $@
}

as_root apt-get update
as_root apt-get install --no-install-recommends -y python-dev gcc
as_root /bin/bash /home/box/ansible/ansible-install.sh

as_root ansible-playbook -v -i /home/box/ansible/hosts /home/box/ansible/playbook.yml

