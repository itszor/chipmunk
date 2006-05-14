# This is a makefile.

	CHASM =		../asm/chipmunk-as
	CHLINK =	../ld/chipmunk-ld

	CC =		chipmunk-gcc
	CFLAGS =	
	LEX =		flex
	YACC =		bison -d -t

	INCLUDE =	-I../libjtype
	LIBS =		-Ldoptools/chameleon/lib

	OBJ = 		serial.o crt0.o string.o doptest.o
	OBJ2 =		looptest.o crt0.o serial.o string.o string2.o
	OBJ3 =		fntest.o crt0.o serial.o string.o string2.o

	TARGET =	doptest
	TARGET2 =	ctest
	TARGET3 =	fntest

.PHONY:	clean

all:	doptest

clean:
	rm -f *.o looptest.chs string2.chs fntest.chs doptest

$(TARGET):	$(OBJ)
	$(CHLINK) $(OBJ) $(LIBS) -o $@

$(TARGET2):	$(OBJ2)
	$(CHLINK) $(OBJ2) $(LIBS) -o $@

$(TARGET3):	$(OBJ3)
	$(CHLINK) $(OBJ3) $(LIBS) -o $@

#%.chs:	%.c
#	$(CC) $(CFLAGS) $(INCLUDE) $< > $@

%.d:	%.chs
	echo `echo $< | sed s/.chs/.o/g`: $< > $@

%.d:	%.c
	gcc -MM $< > $@

%.o:	%.chs
	$(CHASM) $< -o $@

include	$(OBJ:.o=.d)
include $(OBJ2:.o=.d)
