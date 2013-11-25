#! /bin/sh

error() {
    echo "$1"
    exit 1
}

# set -x

rm -rf autom4te.cache
autoheader || error "could not re-generate config/openaxiom-c-macros.h"
aclocal -I config
autoconf || error "could not re-generate configure"
automake -a -c --force-missing
rm -rf autom4te.cache

# set +x
