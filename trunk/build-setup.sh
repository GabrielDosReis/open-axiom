#! /bin/sh

error() {
    echo "$1"
    exit 1
}

# set -x

rm -rf autom4te.cache
autoheader || error "could not re-generate config/openaxiom-c-macros.h"
autoconf || error "could not re-generate configure"

## subdirectories that contain Makefile pamphlets of interest
SUBDIRS="	  src/interp             \
		  src/algebra            \
		  src/input              \
"


for d in $SUBDIRS; do
    notangle -t8 $d/Makefile.pamphlet > $d/tmp-Makefile.in \
       || error "could not extract $d/Makefile.in from pamphlet file"
    config/move-if-change $d/tmp-Makefile.in $d/Makefile.in
done

# set +x
