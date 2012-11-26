CC=gcc
INCLUDES=-I../distorm/include/
LIBS=../distorm/distorm3.a -lm -lc
CFLAGS=${INCLUDES}

all: rdis
	

rdis: rdis.c
	${CC} ${CFLAGS} -o rdis rdis.c ${LIBS}

clean:
	rm rdis
