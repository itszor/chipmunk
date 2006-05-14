# This is a makefile.

	CC =	gcc
	COPT =	-O3 -g -Wall
	LEX =	flex
	YACC =	bison -d -t
	AR   =	ar crs

	LIBS =	-lm

	OBJ = bset.o list.o hash.o cnew.o clist.o pqueue.o map.o buffer.o \
	      strng.o

all:	libjtype.a

clean:
	rm -f libjtype.a *.o

cleaner:
	rm -f libjtype.a *.o *.d

install:
	install libjtype.a /usr/local/lib
	mkdir /usr/local/include/jtype
	install *.h /usr/local/include/jtype

libjtype.a:	$(OBJ)
	$(AR) $@ $(OBJ)

%.o:	%.c
	$(CC) $(COPT) -c $< -o $@

%.d:	%.c
	$(CC) $(CFLAGS) $(INCLUDE) -MM $< > $@

include	$(OBJ:.o=.d)
