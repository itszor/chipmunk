## Process this file with automake to generate Makefile.in

AUTOMAKE_OPTIONS = cygnus

INCLUDES = $(NEWLIB_CFLAGS) $(CROSS_CFLAGS) $(TARGET_CFLAGS)

noinst_LIBRARIES = lib.a

lib_a_SOURCES = crt0.S syscalls.c

# lib_a_OBJ = crt0.o syscalls.o

all: crt0.o

ACLOCAL_AMFLAGS = -I ../../..
CONFIG_STATUS_DEPENDENCIES = $(newlib_basedir)/configure.host

#crt0.lo:	crt0.o
#	cp -f $< $@

#syscalls.lo:	syscalls.o
#	cp -f $< $@

#lib.a:	$(lib_a_OBJ)
#	$(AR) cru $@ $<

crt0.o:	$(srcdir)/crt0.S
	chipmunk-as $< -o $@

#%.o:	$(srcdir)/%.c
#	$(COMPILE) $< -o $@
