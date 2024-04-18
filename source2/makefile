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
  ap1099rp       arifcgen       ariinput       arijurnl       arircrmv        \
  arirgstr       arishpin       ariupdte       arixnput       arixprnt        \
  armadjin       armadjrp       armbyact       armfcinp       armflags        \
  armlatin       armlatrp       armpurge       armsettl       armstlrp        \
  armstmnt       armtbage       armtbdtl       armtbsum       armtcinp        \
  arqinqry       bckacks        bckaging       bckbkngs       bckbopdr        \
  bckbyprt       bckcanrp       bckcrhld       bckcrinp       bckdsply        \
  bckflags       bckhprge       bckinput       bcklosrp       bckonlin        \
  bckpmcal       bckpurge       bckquery       bckstat        bckupdte        \
  bomanal        bomapprv       bomautrp       bomcprpt       bomdrill        \
  bomdrilo       bomeffct       bomexplr       bomexpso       bomflags        \
  bomfrmin       bomhnyop       bomimpls       bominput       bommrkup        \
  bomoa          bomopcdr       bomreplr       bomslprt       bomsumry        \
  bomxpldr       calchng1       calchng2       calchng3       calinput        \
  calmenum       catinput       ccrinput       checkreg       chkcreat        \
  chklist        chkprint       chktoss        chkudoit       cmsdates        \
  corarupd       cprcdein       cprcderp       cprcmprp       cprsplin        \
  cprsplrp       crcbfupd       crcdepst       crcinput       crcjurnl        \
  crcrgstr       crcupdte       cshfile1       cshinput       cshjurn1        \
  cshreqir       cshvoid        cusblnce       cusdlete       cusinput        \
  cuslabel       cuslist1       cuslist2       cuslist3       cusptxin        \
  demdtlrp       deminput       dempurge       demrapid       demrevue        \
  fiscalyr       frdefine       frgrpinp       frprint1       frprint2        \
  frprint3       gencdsin       glarchiv       glbalsht       glbatmgt        \
  glbgiput       glbgrpt        glcodes        glcogsin       gldefine        \
  gldetrpt       gldetsum       gldistin       gldsply        glflags         \
  glgencls       glinput        gljnlinp       gljurn98       gljurn99        \
  glpandl        glprint        glprtjnl       glroll         glrollyr        \
  glstart        glstomp        gltrial        glwksht1       glxpurge        \
  glxtfrin       glxtfrrp       gnjinput       gnjjurn1       gnledger        \
  hnyabc         hnyactin       hnyaddjn       hnyaddns       hnyarchv        \
  hnyccacr       hnyccamr       hnyccedt       hnyccgpd       hnycchds        \
  hnycchrp       hnyccmin       hnyccprd       hnyccprg       hnyccrpt        \
  hnyccsed       hnyccsim       hnyccspg       hnyccsrp       hnyccssn        \
  hnycctff       hnycutrp       hnydlete       hnydsply       hnyexrsr        \
  hnyflags       hnyflush       hnyfshjn       hnygedit       hnyhstin        \
  hnyinput       hnylcarp       hnylcbrp       hnylcdrv       hnylcinp        \
  hnylcqry       hnylocpt       hnylotdr       hnymove        hnymslu         \
  hnypicap       hnypicin       hnypigen       hnypiinp       hnypiprm        \
  hnypipst       hnypirec       hnypireg       hnypireq       hnypitic        \
  hnypivar       hnypivdt       hnyprcds       hnypurge       hnyqedit        \
  hnyqiput       hnyqpenz       hnyretin       hnyretjn       hnyrpts         \
  hnyslow        hnyturns       hnyvlgui       hnyvlrpt       hnywdwal        \
  hnywdwjn       hnyzapp        holinput       jbbpcomp       jbclose2        \
  jbcomprp       jbctlrpt       jbdirect       jbdsprpt       jbempinp        \
  jbemplst       jberndft       jbflags        jbhny2jb       jbinpkit        \
  jbissued       jbmanage       jbmdekit       jbmstrpt       jbmttojb        \
  jbpickpr       jbpost1        jbpost2        jbpotprt       jbpurge         \
  jbqdsply       jbreleas       jbrelxrf       jbrwkhny       jbshopip        \
  jbshrtrp       jbstatrp       jbsumrpt       jbtaskpr       jbtccdin        \
  jbtcdpts       jbtcinp        jbtcpost       jbtcprge       jbtcrpt         \
  jbtifprg       jbtohny2       jbvarbkt       jbvarrpt       jbwc2wc         \
  lotarchv       lotmvrpt       lotrecal       oobypart       payaccts        \
  payadjin       paydeflt       paydsply       payhnyrt       payindat        \
  payinpez       payinput       payjurnl       payrcrmv       payupdte        \
  pipbywek       pipcheck       pipdsply       pipgraph       pipinop         \
  pipinsr        piplate        pipmap         pipoanal       pipoutop        \
  pipoutsr       piprestr       pipscan        pipsldrv       pipstkrp        \
  piptrace       pipvrfy        planglbl       planrnge       plnflags        \
  plnmgrpt       plnmsrpt       plnrevue       plnrstr        pmprcinp        \
  pmtblinp       poprint        poreleas       porelxrf       povsarel        \
  proc22         proc22a        proc22aa       proc22ab       proc22ea        \
  proc22eb       proc22ez       proc22ia       proc22ib       proc23          \
  proc24         proc25         proc255        proc2reg       proc31a         \
  proc31b        proc31c        proc32         proc33         proc33mn        \
  proc33st       proc33ye       proc3d3l       proc3d4        proc6d7         \
  proc75         proc75a        proc75b        procaria       procaric        \
  procarid       procarif       procarig       procarim       procario        \
  procarir       procariu       proccal        proccora       proccrcu        \
  proccrrv       proce7         procflsh       procgc01       procgc02        \
  procgc03       procgc04       procgc05       procgc06       procgc07        \
  procgc08       procgc09       procgc10       procgc11       procgc12        \
  procgc13       procgc14       procgc15       procgc17       procgc18        \
  procgc19       procgc20       procgc21       procgc22       procgc23        \
  procgc24       procgc25       procgc26       procgc27       procgc28        \
  procgc29       procgc30       procgc31       procgc32       procgc33        \
  procgc34       procgc43       procgc44       procgc45       procgc46        \
  procgc47       procgc48       procgc54       procglgn       procglyr        \
  prochnya       prochnyr       prochnyw       procqdsa       procqdsb        \
  procqdst       procrcda       procrcdb       procrcva       procrcvb        \
  procrcvd       procrcvi       procrend       procstci       qcrcodes        \
  qcrejrpt       rcvaprpt       rcvhnyrt       rcvinput       rcvjurnl        \
  rcvqcdst       rcvreg         rcvstat        rcvupdte       rcvvarrp        \
  ropcalc        ropclsin       rophnyin       ropsrch        ropsysin        \
  ropxcess       rptparam       rteinput       rteprint       sadedits        \
  saflags        sagprpt        sainqry        saprint        sapurge         \
  sarptinp       sarptsel       sasumedt       sasumriz       satgtinp        \
  serarchv       serexpls       serinput       serlist        serreset        \
  setbom         setlt          sfcumrst       shpactin       shpactrp        \
  shparrpt       shpavail       shpdocpr       shpjurnl       shplabel        \
  shpprof        shpschal       shpschin       shpschrp       shpscxrp        \
  shpxappr       shpxcfrm       slmcomm        slminput       stcbommt        \
  stcbomrp       stccompr       stcdtlrp       stcimple       stcinput        \
  stcjurnl       stcmgtin       stjfile1       stjfile2       stjinput        \
  stjjurn1       stjpass1       stjpass2       strinput       stxinput        \
  stxreprt       sysdates       syspurge       szrmatin       taskbkgd        \
  taskctl        txtcpyin       txtflmnt       txtrptin       txttypes        \
  updariin       updcheck       updcrcin       upddone        updnext         \
  updselct       vbkaging       vbkbyjob       vbkbyprt       vbkdsply        \
  vbkexped       vbkflags       vbkhprge       vbkhstry       vbkinput        \
  vbkpurge       vbkstat        vbkupdte       venbfinp       venblnce        \
  vencodes       vendlete       vendsply       veninput       venlabel        \
  venratng       vfdefine       vpcinput       vpcinqry       vpcsumry        \
  vprinput       vsasheet       vsaship        wcdsply        wcgraph2        \
  wcinput        wcoutop        wcoutsr        wcrealin       wcrestr         \
  wcutil         

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)


clean:
	rm -f ./*.? ./core
