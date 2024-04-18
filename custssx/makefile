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
  ADPLOG         ARIMAIL        ARIMLTX        CARDMAIL       LOGFILE         \
  LOGFILE2       MAILSUB        SEQREAD        SKUMAIL        aesrcvsn        \
  aesvoid        apcamull       apcbarem       apcbfgnd       apcbomcp        \
  apcbomed       apccalsb       apccs1sb       apccs2sb       apccs3sb        \
  apccs4sb       apccs5sb       apccst0b       apccst1b       apccst2b        \
  apccst3b       apccst4b       apccst5b       apccst6b       apccst9b        \
  apccstcc       apccstwg       apccutcc       apccutld       apccutmm        \
  apcdate        apcdescr       apcedi03       apcedi10       apcedipo        \
  apcediqt       apceff1b       apceffwg       apcemp1b       apcemp2b        \
  apcgslit       apchnysb       apcinvsb       apcivsub       apclabtt        \
  apcldsub       apcorlns       apcpassw       apcpause       apcpipsb        \
  apcpl10b       apcpl11b       apcpl1db       apcpl2db       apcpl3ab        \
  apcpl3sb       apcpl41c       apcpl43a       apcpla47       apcplb40        \
  apcplb42       apcplb44       apcplb45       apcplc40       apcplc42        \
  apcplc44       apcplc45       apcpld40       apcpld42       apcpld45        \
  apcple40       apcple42       apcple45       apcplf42       apcplf45        \
  apcplg42       apcplg45       apcplh42       apcplh45       apcpli42        \
  apcpln1b       apcpln2b       apcpln3b       apcpln5b       apcpln6b        \
  apcpln7b       apcpln8b       apcpln9b       apcplndd       apcplndv        \
  apcplnef       apcplo42       apcplp42       apcplr42       apcpls42        \
  apcplt42       apcpltxt       apcplv42       apcplw42       apcplx42        \
  apcplz45       apcpost        apcpr0sb       apcpr1sb       apcpr2sb        \
  apcpr3sb       apcpr4sb       apcpr5sb       apcprsub       apcprwfb        \
  apcprxsb       apcprysb       apcprzsb       apcpulsb       apcrg21b        \
  apcrg22b       apcrg23b       apcrg24b       apcrg25b       apcrg26b        \
  apcrg27b       apcrg28b       apcrg29b       apcrga1b       apcrga23        \
  apcrga2b       apcrga3b       apcrga4b       apcrga5b       apcrga6b        \
  apcrga7b       apcrga8b       apcrga9b       apcrolsb       apcrpsub        \
  apcscnsb       apcsls1        apcsls2        apcsls3        apcsls4         \
  apcsls5        apcsosub       apctime        apcvenlb       awdaddjn        \
  awdaddns       awdbksub       awdbnkln       awdbnkmr       awdcalpr        \
  awdcalxx       awdcalyy       awdcomsb       awdcrdit       awdcutcc        \
  awdcutld       awddescr       awdedi01       awdediad       awdemp0b        \
  awdemp1b       awdgs128       awdibs04       awdmove        awdorasb        \
  awdpartn       awdpla05       awdpla07       awdpla16       awdpla27        \
  awdpla42       awdpla57       awdpla59       awdpla60       awdpla61        \
  awdpla62       awdpla64       awdplb05       awdplb16       awdplb27        \
  awdplb42       awdplb60       awdplc05       awdplc16       awdplc42        \
  awdpld16       awdpld42       awdpld60       awdple42       awdple60        \
  awdplf42       awdplg42       awdplh42       awdpli42       awdplj42        \
  awdplk42       awdpll42       awdplm42       awdpln0b       awdpln42        \
  awdpln9b       awdplo42       awdplp42       awdplq42       awdplr42        \
  awdpls42       awdplt42       awdplu42       awdrgb01       awdrptyy        \
  awdserst       awdstat        awdsurge       compress       cuinpsub        \
  custccr        despla07       dsdpla07       ewdacell       ewdaging        \
  ewdbarem       ewdcalhr       ewdcalsl       ewdcalss       ewdeff1b        \
  ewdeffga       ewdftp01       ewdglssb       ewdlogup       ewdmdy          \
  ewdnomsz       ewdopen        ewdpla44       ewdpla58       ewdpla59        \
  ewdpla60       ewdpla63       ewdpla65       ewdpla66       ewdpla67        \
  ewdpla69       ewdpla71       ewdpla72       ewdpla73       ewdpla77        \
  ewdplb58       ewdplb59       ewdplb60       ewdplb66       ewdplb72        \
  ewdplb73       ewdplb77       ewdplc58       ewdplc72       ewdplc73        \
  ewdplc77       ewdpld58       ewdpld72       ewdple72       ewdplf72        \
  ewdplg42       ewdplg72       ewdplh72       ewdpln6b       ewdplo42        \
  ewdplo43       ewdpls42       ewdpls43       ewdpls45       ewdrfprt        \
  ewdshape       ewdsleep       ewdtbasb       ewduglgb       ewdugqtb        \
  ewdugslb       ewdusrvl       ewdwgupd       expand         genread         \
  hnypst2        invadjob       invavail       invddisp       invglget        \
  invmstsb       invmvmf        invmvmt        invpdisp       invpst1         \
  invpst2        invqdisp       invudisp       invudspf       loginv          \
  ntxlin01       ntxplb42       ntxplc42       ntxpld42       ntxple42        \
  ntxplf42       ntxplg42       ntxplh42       oracust        oradfly         \
  oradfmt        oraempdt       oraemply       oraempmt       oraslmm         \
  prcserst       purprint       rcvupsub       schema         testread        \
  uppercase      

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)


clean:
	rm -f ./*.? ./core
