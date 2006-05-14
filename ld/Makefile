# This is a makefile.

	CC =		gcc
	CFLAGS =	-O3 -g -Wall
	LEX =		flex
	YACC =		bison -d -t

	INCLUDE =	-I../libjtype -I../asm -I../sys
	LIBS =		-lm -L../asm -lelfutil -L../libjtype -ljtype
	LDFLAGS =

	OBJ = 		main.o program.o archive.o object.o

	TARGET =	chipmunk-ld

all:	$(TARGET)

test:	$(TARGET)
	./$(TARGET) -o doptest ../asm/test.cho ../asm/bleh.cho ../asm/crt0.cho

clean:
	rm -f $(TARGET) doptest *.o

cleaner:
	rm -f $(TARGET) doptest *.o *.d

$(TARGET):	$(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) $(LIBS) -o $@

%.o:	%.c
	$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@

%.d:	%.c
	$(CC) $(INCLUDE) -MM $< > $@

include	$(OBJ:.o=.d)
