#	b_cgen makefile
# makefile for 'c' generation, COMPILE and Linkage of Source Programs!
#  Exclusive for programs in 'c' Utility Source


# Set up the variables
include ../makeutil/optcode
CFLAGS=-Ae $(OPTCODE) -D_HPUX_SOURCE
#CFLAGS=-Aa -D_HPUX_SOURCE

# Location for the stored objects to be shipped.
OBJ=../cutilsrce_o

INCPATH=-I../
CMS_IFLAGS=$(INCPATH)include1
BC_IFLAGS=$(INCPATH)include2

# Combined Include Directories
INC_FLAGS=$(CMS_IFLAGS) $(BC_IFLAGS)

.SUFFIXES:
.SUFFIXES:	.a .o .c 

# Define the steps from .c to executable
.c:
	@echo  == Using .c to executable rule  ==
	$(CC) -c $(CFLAGS) $(INC_FLAGS) ./$*.c
	mv ./$*.o $(OBJ)

all:	target	\
  copy           dbbuild        descerr        display        getpath         \
  hexed          hexed          keytest        mklower        remdels         \
  reset_alt      reset_term     u2isam         wdelwrk        

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)

