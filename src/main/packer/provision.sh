#!/bin/bash

set -e

function as_root {
    echo 'boxbase' | sudo -S $@
}

as_root apt-get update
as_root /bin/bash /home/boxbase/ansible/ansible-install.sh

as_root ansible-playbook -v -i /home/boxbase/ansible/hosts /home/boxbase/ansible/playbook.yml

