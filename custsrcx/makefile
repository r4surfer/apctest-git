#	b_cgen makefile 
#
# makefile for 'c' generation, COMPILE and Linkage of Source Programs!

# Set up the variables
include ../optcode
CFLAGS=-Ae $(OPTCODE) -D_HPUX_SOURCE
LD=ld
#LDFLAGS= +s -a archive_shared +b :
LDFLAGS= +s +b :


# Location for the stored objects to be shipped.
OBJ=../object2

# Extra object files for routines that MUST be explicitly loaded
EXT_OBJ=../load2/cuserid.o /opt/langtools/lib/crt0.o

# b_cgen, wisp & disam load modules

CMS_LIB=-lasm -lscrn -lfile -lmat -lwb -lutil -lcmath -lutil -lasm -ldisam \
-ldewisp

# Christie - Added for Oracle -lclntsh
# System libraries
# M_LIB=-lcurses -lm -lc -ldld -lclntsh
M_LIB=-lcurses -lm -lc -ldld

# CMS Subroutine load libraries

CMSSUB_LIBX= \
  -ll1asr2       -ll1ass2       -ll1ass3       -ll1bss2       -ll1css2        \
  -ll1dss2       -ll1ess2       -ll2asr2       -ll2asr3       -ll2ass2        \
  -ll2ass3       -ll2bss2       -ll2css2       -ll3asr2       -ll3ass2        \
  -ll3ass3       -ll4asr2       -ll4asr3       -ll4ass2       -ll4ass3        \
  -ll5asr2       -ll5asr3       -ll5ass2       -ll5ass3       -ll6asr3        \
  -ll6ass2       -ll6ass3       -ll7asr3       -ll7ass3       -ll8asr3        \
  -ll8ass3       -ll9asr2       -ll9ass3       -llaasr3       -llaass2        \
  -llaass3       -llbass3       -llcass3       -lldasr2       -lldasr3        \
  -lldass2       -lldass3       -llpacss       -llxacss       

CMSSUB_LIB=$(CMSSUB_LIBX) $(CMSSUB_LIBX) $(CMSSUB_LIBX) $(CMSSUB_LIBX)

# Define the Combined Parameters

CMSPATH= \
/cms2/apctest1

CMS_IFLAGS=-I$(CMSPATH)/include1
BC_IFLAGS=-I$(CMSPATH)/include2
CMSSUB_DIR=-L$(CMSPATH)/load1
CMS_DIR=-L$(CMSPATH)/load2
# Christie - Added for Oracle
# CMSOR_DIR=-L$\/oracle/product/9.2.0/lib32
# OR_IFLAGS=-I$\/oracle/product/9.2.0/rdbms/demo
# ORA_IFLAGS=-I$\/oracle/product/9.2.0/rdbms/public

INC_FLAGS=$(CMS_IFLAGS) $(BC_IFLAGS) $(OR_IFLAGS) $(ORA_IFLAGS)
# Christie - Added for Oracle - Addes CMSOR_DIR
# LD_DIRS=$(CMSSUB_DIR) $(CMS_DIR) $(CMSOR_DIR)
LD_DIRS=$(CMSSUB_DIR) $(CMS_DIR)
LD_LIBS=$(CMSSUB_LIB) $(CMS_LIB) $(M_LIB)

COMPILE=../b_cgen

.SUFFIXES:
.SUFFIXES:	.a .o .c .bas .csv .c.sav

#	Define the steps from .bas to executable
.bas:
	@echo  == Using .bas to executable rule  ==
	$(COMPILE) ./$*
	$(CC) -c $(CFLAGS) $(INC_FLAGS) ./$*.c 
	$(LD) ./$*.o $(EXT_OBJ) $(LDFLAGS) $(LD_DIRS) $(LD_LIBS) -o $(OBJ)/$@
	strip $(OBJ)/$@
	rm -f ./$@.?

# Define the steps from .c to executable
.c:
	@echo  == Using .c to executable rule  ==
	$(CC) -c $(CFLAGS) $(INC_FLAGS) ./$*.c 
	$(LD) ./$*.o $(EXT_OBJ) $(LDFLAGS) $(LD_DIRS) $(LD_LIBS) -o $(OBJ)/$@
	strip $(OBJ)/$@

#       Define the steps from .csv to executable
.csv:
	@echo  == Using .csv to executable rule  ==
	rm -f ./$*.c
	touch ./$*.c
	cat ./$*.csv >> ./$*.c
	$(CC) -c $(CFLAGS) $(INC_FLAGS) ./$*.c
	$(LD) ./$*.o $(EXT_OBJ) $(LDFLAGS) $(LD_DIRS) $(LD_LIBS) -o $(OBJ)/$@
	strip $(OBJ)/$@
	rm -f ./$@.?

#       Define the steps from .c.sav to executable
.c.sav:
	@echo  == Using .c.sav to executable rule  ==
	rm -f ./$*.c
	touch ./$*.c
	cat ./$*.c.sav >> ./$*.c
	$(CC) -c $(CFLAGS) $(INC_FLAGS) ./$*.c
	$(LD) ./$*.o $(EXT_OBJ) $(LDFLAGS) $(LD_DIRS) $(LD_LIBS) -o $(OBJ)/$@
	strip $(OBJ)/$@
	rm -f ./$@.?

all:	target	\
  aesrcvip       aesrpt01       aesrpt02       aesscann       aesupdte        \
  amtbomcd       amtbomif       amtbompm       apcarcrd       apcclock        \
  apccompt       apccst00       apccst01       apccst02       apccst03        \
  apccst05       apccst06       apccst07       apccst08       apccst09        \
  apccsthp       apccstlr       apccstsc       apcctfix       apccuteq        \
  apccutfp       apccutsc       apcdev         apcedi01       apcedi02        \
  apcedi04       apcedi05       apcedi05       apcedi06       apcedi07        \
  apcedi07       apcedi08       apcedi09       apcedi11       apcediso        \
  apcemped       apcempmn       apcemppg       apcemppt       apcemprp        \
  apcempvr       apcinsr1       apcinsrt       apcinvbo       apcinven        \
  apcinvun       apcivavg       apclogo        apcpipck       apcpl41a        \
  apcpla40       apcpla42       apcpla45       apcpln01       apcpln02        \
  apcpln04       apcpln05       apcpln10       apcpln11       apcpln40        \
  apcpln43       apcpln48       apcpln49       apcpln99       apcplr47        \
  apcprc01       apcprc02       apcprc03       apcprc04       apcprcqt        \
  apcpurgs       apcquick       apcrga01       apcrga02       apcrga03        \
  apcrga04       apcrga10       apcrga11       apcrga12       apcrga13        \
  apcrga21       apcrga22       apcrga24       apcrga30       apcrga32        \
  apcrmk01       apcrolex       apcrolez       apcropcc       apcrpt01        \
  apcrpt02       apcrpt04       apcrpt10       apcrpt11       apcrpt15        \
  apcsale1       apcsale2       apcsales       apcscan2       apcscann        \
  apcshape       apcsku         apcsls03       apcsoaud       apcsoavg        \
  apcstat2       apcstock       apcupcbd       apcvacrp       apcvendr        \
  apcvndpr       ariinput       arijurnl       arirgstr       ariupdte        \
  armflags       armpurg1       armstmnt       armtbsum       atstadpu        \
  atstatop       atstatup       awdbayex       awdcrddt       awdcrdld        \
  awddefin       awdedi02       awdedi03       awdedi04       awdedi06        \
  awdedi08       awdedi09       awdedi10       awdedi11       awdedi12        \
  awdedi13       awdedi14       awdedi15       awdedi16       awdedi17        \
  awdedi18       awdedi19       awdedi20       awdget01       awdhubed        \
  awdlane        awdpl542       awdpl544       awdpl604       awdpl611        \
  awdpl620       awdpl621       awdpl622       awdplb61       awdplc60        \
  awdpln01       awdpln02       awdpln05       awdpln06       awdpln07        \
  awdpln10       awdpln11       awdpln12       awdpln13       awdpln14        \
  awdpln15       awdpln16       awdpln17       awdpln18       awdpln19        \
  awdpln20       awdpln21       awdpln22       awdpln23       awdpln24        \
  awdpln25       awdpln26       awdpln27       awdpln28       awdpln29        \
  awdpln30       awdpln31       awdpln32       awdpln33       awdpln34        \
  awdpln35       awdpln36       awdpln37       awdpln38       awdpln40        \
  awdpln41       awdpln43       awdpln57       awdpln59       awdpln60        \
  awdpln61       awdpln62       awdpln63       awdpln64       awdpln65        \
  awdpln66       awdrga01       awdrga02       awdrpt01       awdrpt02        \
  awdrpt03       awdrpt04       awdrptxx       awdrptyy       awdscann        \
  awdscrap       awdscrlb       awdsku         awdstrb1       awdvlrpt        \
  bcksub         bcksubdev      catinput       cprcdein       cprcderp        \
  crcinput       crcjurnl       crcupdte       cshjurn1       csvmdlld        \
  cusinput       cuslabel       cuslist1       cuslist2       custerm         \
  datfix         des366ar       desaamdl       desaddpc       desadmdl        \
  desamtif       desc2mdl       desccmdl       descolor       descopy         \
  descopy0       descopy1       descopyx       descpcat       descpmdl        \
  descutln       desdate        desdel         desdelbw       desdisc6        \
  desdsmdl       desedilw       desedirt       desemprp       desgencp        \
  desmode1       desmode3       desmode4       desmode6       desmode7        \
  desmodel       desmodpc       despass        despass2       despla40        \
  despln07       despln40       desplx40       desprc         desprice        \
  desstat        desstmdl       desterms       destime        desupdte        \
  desxxmdl       dsdpln07       easydate       efficita       effpurge        \
  ewdbckup       ewdclea2       ewdclean       ewdclndt       ewdclnld        \
  ewdclnud       ewdcompt       ewdconvt       ewdcst01       ewdcussb        \
  ewdefftb       ewdftp02       ewdftp03       ewdftp04       ewdhnyad        \
  ewdintcp       ewdlogck       ewdlogin       ewdlogou       ewdpla67        \
  ewdpln42       ewdpln43       ewdpln45       ewdpln52       ewdpln53        \
  ewdpln54       ewdpln55       ewdpln56       ewdpln57       ewdpln59        \
  ewdpln60       ewdpln61       ewdpln62       ewdpln63       ewdpln64        \
  ewdpln65       ewdpln66       ewdpln67       ewdpln68       ewdpln70        \
  ewdpln71       ewdpln71       ewdpln72       ewdpln73       ewdpln75        \
  ewdpln76       ewdpln77       ewdpln79       ewdpln80       ewdpln82        \
  ewdplp42       ewdplp43       ewdplp72       ewdsls00       ewdtbage        \
  ewdtbsum       ewdugsls       ewdvlrpt       fired          frdefine        \
  frprint1       frprint2       frprint3       gencdsin       gencopy         \
  glrollyr       gltrial        gltrnmit       gnjjurn1       histdscv        \
  histdsdl       histlbl        hnyaddjn       hnyaddns       hnydlete        \
  hnydsply       hnyfshjn       hnyinput       hnymove        hnypipst        \
  hnyretjn       hnyslow        hnyturns       hnyvlrpt       hnywdwjn        \
  hnyzapp        invaddjn       invaddns       invdlete       invdsply        \
  invinput       invqiput       invwdwal       invwdwjn       jbpost1         \
  jbpost2        jbquote1       jbquote2       jbrwkhny       kanbanpt        \
  ldenergy       ldkanban       ldkanold       loadacct       loadaddr        \
  loadbomif      orclpoext      payinput       payjurnl       pgorattr        \
  pgorlntr       pgschdtr       piplate        poprint        procariu        \
  procarix       procinva       procinvw       procrcvi       purgeld         \
  racklbl        rcvhnyrt       rcvinput       rcvjurnl       rcvqcdst        \
  rcvupdte       resendlw       resetld        rgadatfx       rgascann        \
  ropcalc        ropclsin       rophnyin       ropsrch        ropsysin        \
  satgtinp       scremake       scrinput       scrlabel       scrlabex        \
  scrrecvp       scrscann       shpactin       shpjurnl       shpschin        \
  shpxappr       slminput       stcdtlrp       stcinput       stcjurnl        \
  stjjurn1       strinput       test           txtbuild       txtclear        \
  txtpln42       txtshow        updariin       updarinx       updcheck        \
  updcrcin       updselcx       vbkdsply       vbkinput       vbkupdte        \
  venbfinp       vendlete       veninput       venpurrp       vfdefine       

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)


clean:
	rm -f ./*.? ./core
