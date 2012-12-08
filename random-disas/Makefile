CC=gcc
INCLUDES=-I../distorm/include/
LIBS=../distorm/distorm3.a -lm -lc
CFLAGS=-g -std=c99 ${INCLUDES}

all: rdis
	

rdis: rdis.c errors.h eval.h eval.c expr.h expr.c
	${CC} ${CFLAGS} -o rdis rdis.c eval.c expr.c ${LIBS}

clean:
	rm rdis
