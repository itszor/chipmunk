# This is a makefile.

	CC =		gcc
	CFLAGS =	-O3 -g -Wall
	COPT =		-g
	LEX =		flex
	YACC =		bison -d -t
	RANLIB =	ranlib

	INCLUDE =	-I../libjtype -I../sys
	LIBS =		-lm -L../libjtype -ljtype

	OBJ = 		asmparse.tab.o lex.yy.o elfutil.o chasm.o

	TARGET =	chipmunk-as

all:	$(TARGET) libelfutil.a

test:	test.cho bleh.cho crt0.cho $(OBJ)

test-clean:
	rm *.cho

clean:
	rm -f $(TARGET) *.o asmparse.tab.[ch] lex.yy.c

cleaner:
	rm -f $(TARGET) *.o asmparse.tab.[ch] lex.yy.c *.d

$(TARGET):	$(OBJ)
	$(CC) $(COPT) $(OBJ) $(LIBS) -o $@

# This is nasty
libelfutil.a:	elfutil.o
	$(AR) cru $@ elfutil.o
	$(RANLIB) $@

asmparse.tab.c:	asmparse.y
	$(YACC) asmparse.y

asmparse.tab.h:	asmparse.tab.c

lex.yy.c:	asmparse.tab.h asmtok.lex
	$(LEX) asmtok.lex

%.o:	%.c
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

%.d:	%.c
	$(CC) $(INCLUDE) -MM $< > $@

%.cho:	%.chs
	./$(TARGET) $< -o $@

include	$(OBJ:.o=.d)
