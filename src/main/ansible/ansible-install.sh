#!/bin/bash

set -e

# python-setuptools for easy_install
# python-software-properties to support adding ppa repositories
apt-get install --no-install-recommends -y python-setuptools python-software-properties

easy_install pip
pip install ansible

