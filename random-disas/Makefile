CC=gcc
INCLUDES=-I../distorm/include/
LIBS=../distorm/distorm3.a -lm -lc
CFLAGS=-std=c99 ${INCLUDES}

all: rdis
	

rdis: rdis.c eval.h eval.c
	${CC} ${CFLAGS} -o rdis rdis.c eval.c ${LIBS}

clean:
	rm rdis
