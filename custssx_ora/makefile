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
	../custssx_o


INCPATH=-I../
CMS_IFLAGS=$(INCPATH)include1
BC_IFLAGS=$(INCPATH)include2
# Christie - Added for Oracle
OR_IFLAGS=-I$\/oracle/product/11.2.0/rdbms/demo
ORA_IFLAGS=-I$\/oracle/product/11.2.0/rdbms/public

# Combined Include Directories
INC_FLAGS=$(CMS_IFLAGS) $(BC_IFLAGS) $(OR_IFLAGS) $(ORA_IFLAGS)

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
  apcblsub       apcfaxsd       ariprsu2       ariprsu3       ariprsub        \
  connect        discnnct       error          exec           exec1           \
  fetch          fieldinf       flush          getfield       oradescr        \
  query          query1         

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)


clean:
	rm -f ./*.? ./core
