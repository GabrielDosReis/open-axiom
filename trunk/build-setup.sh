#! /bin/sh

error() {
    echo "$1"
    exit 1
}

# set -x

rm -rf autom4te.cache
autoheader || error "could not re-generate config/openaxiom-c-macros.h"
aclocal --output=config/aclocal.m4  -I config --install --force
automake -a -c
autoconf || error "could not re-generate configure"
rm -rf autom4te.cache

# set +x
