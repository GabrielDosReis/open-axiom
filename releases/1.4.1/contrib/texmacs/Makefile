
###############################################################################
# MODULE     : Make file for opeenaxiom plugin
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

CC = gcc
RM = rm -f

all: bin/tm_openaxiom

bin/tm_openaxiom: src/tm_openaxiom.c
	$(CC) src/tm_openaxiom.c src/texbreaker.c -o bin/tm_openaxiom

clean:
	$(RM) bin/tm_openaxiom
