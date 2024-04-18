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
	$(OS2)


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
  aridated       ariexasb       ariextra       arinext        arixprsb        \
  armbalnc       armcblnc       armdates       armglget       armstlno        \
  armtbdsb       armtcdte       arqchksb       arqcuscr       arqtbprm        \
  arqtbsub       askdate        asknumbr       askstrng       askuser         \
  barcodes       bckarisb       bckbopsb       bckcrdsb       bckdaysb        \
  bckduppo       bckminsb       bckpipsb       bckprcsb       bckprefx        \
  bckswtch       bombpdsp       bombrwop       bombrwsb       bomcadsb        \
  bomefdsp       bomfcalc       bomfind        bomopcst       bomopdsp        \
  bomopsub       bomslsub       bomsrtes       bomxplod       botmlin2        \
  cdanpost       cdaqji         cdaqjo         cdaqjt         cdarulsb        \
  chkstat        clrpipin       clrrschd       cmsmainp       compchck        \
  comsub         corarcb        corcrap        corcrbld       corcstsb        \
  cordbap        cordbbld       cordocsb       corjnlsb       corparsb        \
  corprcsb       corvalsb       corxrfsb       cprupdsb       cusappnd        \
  cuscheck       dateoff        datrunsb       demexpls       demstat         \
  ecrapsub       ecrinqsb       enablsub       estcstsb       fadepr          \
  fasub          faupdsub       fileopen       fmtkey         fmtkeyck        \
  fraccum        frngesub       genfmt         genscrn        genslct         \
  getcode        getdem         getempl        getscrn        glaccum         \
  glbalanc       glbatprt       glcmbsub       gldstsub       glperiod        \
  glpstsub       hnyadjob       hnyappnd       hnyapst        hnybinsb        \
  hnyccsip       hnycdist       hnyddisp       hnyelcsb       hnygref         \
  hnyhstf        hnyhstg        hnylcinq       hnylcsub       hnylotpt        \
  hnymstsb       hnypdisp       hnypidef       hnyprcsb       hnyprdsp        \
  hnyprocu       hnyqdisp       hnytotsb       hnytype        hnyudisp        \
  hnyudspf       hnyusdsp       hnyusesb       hstrysub       insursub        \
  jb2tif         jbactsub       jbbomcst       jbcdist        jbcmpsub        \
  jbcrpost       jbdirsub       jbhnyjbs       jbhnysub       jbinuse         \
  jbjnlcls       jbkitsub       jbmtpost       jbpickbp       jbpickcr        \
  jbpicksl       jbqcdtls       jbqcredt       jbqjob         jbqmatl         \
  jbqschdl       jbqvalue       jbrelsub       jbretag        jbseeact        \
  jbtcrate       jbtravel       jbvlpost       jbvsasub       jlbrpost        \
  jmtapost       jmtdpost       jnlclose       jpurpost       jsalpost        \
  lotdisn        lotunq2e       lotvalid       manual         mgtvalus        \
  mlqnext        msgbfgnd       msgcreat       paybkctl       payinpts        \
  paytoss        pfkeysub       pfmcodes       pfmdcvsb       pickyear        \
  pipatcdz       pipatcsb       pipdsub        pipoasub       pipoptmz        \
  piprvwsb       pipslide       piptsub        pjinpsub       planallc        \
  planciw        planrte        plnflsub       plnmaxim       plnrsub         \
  plntrvlr       pmcalsub       pmptblsb       popprint       postatus        \
  povrelsb       prlchprt       prldspno       prlempsb       prlextsb        \
  prlwhtx        prntscrn       ptusedsb       ptxrefsb       rcvdist         \
  rcvstats       rcvupsub       rndchem        rperiods       rstrschd        \
  rtedsply       saperiod       sapostfx       sapostsb       sasuminp        \
  scrnaddr       serinsub       serkit         serselct       serstovr        \
  serxplod       setprnt        setsep         sfadjsub       shostat         \
  showmsg        shpbols        shphnysb       shplabsb       shpovrsb        \
  shppicks       skillsub       slctpass       slctscrn       slctsort        \
  slcttest       sostatus       startovr       stcfoldn       stchnysb        \
  stcimpfl       stclbrsb       stcmapsb       stcmatrx       stcmdmc         \
  stcremap       stcrolup       stcwcasb       szrinsub       testrnge        \
  txtdsply       txtinsub       txtprint       updsub         updusrlg        \
  vbkdatrn       vbklidsp       vbkswtch       venappnd       vendspsb        \
  vfinpsub       vpcdeflt       vpcinqsb       vpcpikme       vpcsumsb        \
  vsashpsb       vsashtsb       wcactsub       wcbuild        wcdpurge        \
  withhold       wrawsupd       

target:
	(if ( test ! -d $(OBJ) ) then       \
		mkdir $(OBJ);               \
	fi)


clean:
	rm -f ./*.? ./core
