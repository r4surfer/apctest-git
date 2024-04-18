#	b_cgen makefile
# makefile for 'c' generation, COMPILE Sub Source Programs!
#  Exclusive for programs in SUB Source output to .o objects


# Set up the variables
include ../optcode
CFLAGS=-c -Ae $(OPTCODE) +z -D_HPUX_SOURCE

OS2=../subsrce2_o
OS3=../subsrce3_o
OR2=../subsrcr2_o
OR3=../subsrcr3_o
OCUSTSUBS=../custsubs_o
OPRESUBS=../presubs_o

# Location for the stored objects to be shipped.
OBJ= \
	$(OR3)


INCPATH=-I../
CMS_IFLAGS=$(INCPATH)include1
BC_IFLAGS=$(INCPATH)include2
# Christie - Added for Oracle
# OR_IFLAGS=-I$\/oracle/product/9.2.0/rdbms/demo
# ORA_IFLAGS=-I$\/oracle/product/9.2.0/rdbms/public

# Combined Include Directories
# INC_FLAGS=$(CMS_IFLAGS) $(BC_IFLAGS) $(OR_IFLAGS) $(ORA_IFLAGS)
INC_FLAGS=$(CMS_IFLAGS) $(BC_IFLAGS)

COMPILE=../b_cgen

.SUFFIXES:
.SUFFIXES:	.a .o .c .bas .csv .c.sav

#	Define the steps from .bas to .o
.bas:
	@echo  == Using .bas to .o rule  ==
	$(COMPILE) ./$*
	$(CC) $(CFLAGS) $(INC_FLAGS) ./$*.c
	mv ./$*.o $(OBJ)
	rm -f ./$*.?

# Define the steps from .c to .o
.c:
	@echo  == Using .c to .o rule  ==
	$(CC) $(CFLAGS) $(INC_FLAGS) ./$*.c
	mv ./$*.o $(OBJ)

#	Define the steps from .csv to .o
.csv:	
	@echo  == Using .csv to .o rule  ==
	rm -f ./$*.c
	touch ./$*.c
	cat ./$*.csv >> ./$*.c
	$(CC) $(CFLAGS) $(INC_FLAGS) ./$*.c
	mv ./$*.o $(OBJ)
	rm -f ./$*.?

#	Define the steps from .c.sav to .o
.c.sav:	
	@echo  == Using .c.sav to .o rule  ==
	rm -f ./$*.c
	touch ./$*.c
	cat ./$*.c.sav >> ./$*.c
	$(CC) $(CFLAGS) $(INC_FLAGS) ./$*.c
	mv ./$*.o $(OBJ)
	rm -f ./$*.?

all:	target	\
  accessck       cmsdbadm       cmslogsb       pipflags       plowal1         \
  plowalts       plownext       plownxt1       read100        read101         \
  read102        read103        read104        read105        readnext        \
  readnxt1       redalt0        redalt1        redalt2        redalt3         \
  redalt4        redalt5        

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)


clean:
	rm -f ./*.? ./core
