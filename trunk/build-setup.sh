#! /bin/sh

error() {
    echo "$1"
    exit 1
}

# set -x

notangle ./configure.ac.pamphlet > ./tmp-configure.ac \
   || error "could not extract configure.ac from pamphlet file"
config/move-if-change tmp-configure.ac configure.ac

autoheader || error "could not re-generate config/axiom-c-macros.h"
autoconf || error "could not re-generate configure"

## subdirectories that contain Makefile pamphlets of interest
SUBDIRS="     .   \
                  src                    \
                  src/lib                \
		  src/boot               \
		  src/interp             \
		  src/share              \
		  src/algebra            \
		  src/input              \
"


for d in $SUBDIRS; do
    notangle -t8 $d/Makefile.pamphlet > $d/tmp-Makefile.in \
       || error "could not extract $d/Makefile.in from pamphlet file"
    config/move-if-change $d/tmp-Makefile.in $d/Makefile.in
done

# set +x
