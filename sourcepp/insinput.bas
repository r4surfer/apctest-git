        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  IIIII  N   N   SSS   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *    I    NN  N  S        I    NN  N  P   P  U   U    T     *~
            *    I    N N N   SSS     I    N N N  PPPP   U   U    T     *~
            *    I    N  NN      S    I    N  NN  P      U   U    T     *~
            *  IIIII  N   N   SSS   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INSINPUT - NORMAL MANAGEMENT (XXXINPUT) PROGRAM FOR       *~
            *            EMPLOYEE INSURANCE COVERAGE.  NOTE ACCESS TO   *~
            *            INPUT AND EDIT MODES THRU 'LINE SUMMARY' SCREEN*~
            *            CAN ALSO BE USED IN SUBROUTINE FORM AS INSURSUB*~
            *            PART OF THE PERSONNEL SYSTEM.                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/83 ! ORIGINAL                                 ! GLW *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 08/09/96 ! Changes for the year 2000.               ! DXL *~
            * 10/09/97 ! Changed SHOWMSG to SHOSTAT (1 call)      ! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            charges$(4,20)10,            /* CHARGES ACCUMULATED Y TO D */~
            chargefor$(4,20)30,          /* WHAT ARE CHARGES FOR       */~
            coverage$(20)10,             /* AMOUNT OF COVERAGE         */~
            covered$(20)16,              /* WHO IS COVERED?            */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deductable$(3,20)10,         /* DEDUCTABLE - AMT, FOR, MET?*/~
            dedfor$(3,20)30,             /* WHAT IS DED FOR?           */~
            dedmet$(3,20)1,              /* IS DED YET MET THIS YEAR?  */~
            employee$16,                 /* EMPLOYEE CODE              */~
            readkey$50,                  /* GENERAL PURPOSE FILE KEY   */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eligdate$(20)8,              /* ELIGIBILITY DATE           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            freetext$(3,20)50,           /* FREE TEXT                  */~
            filler$(20)178,              /* FREE TEXT                  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            insurance$(20)16,            /* TYPE OF INSURANCE          */~
            insurancedescr$32,           /* TYPE OF INSURANCE          */~
            name$30,                     /* FOR DISPLAY                */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            pass$162,                    /* FJOR COMSUB                */~
            premium$(3,20)10,            /* PREMIUM DATA  -  AMT & FOR?*/~
            premfor$(3,20)30,            /* WHAT IS PREMIUM FOR?       */~
            startdate$(20)8              /* START DATE                 */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APLSKILL ! Applicant skills inventory - personnel s *~
            * #02 ! REQSKILL ! Skills required for a requisition - pers *~
            * #03 ! EMPSKILL ! Employee skills inventory - personnel sy *~
            * #04 ! COMTERM  ! File of common terms for personel.       *~
            * #05 ! APLMASTR ! Applicant master file - part of personne *~
            * #06 ! REQMASTR ! Requisition master file - personnel syst *~
            * #07 ! EMPMASTR ! Employee master file                     *~
            * #08 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #09 ! PERFRNGE ! Fringe benefit file - personnel system   *~
            * #10 ! INSMASTR ! INSURANCE MASTER FILE - PERSONNEL SYSTEM *~
            * #11 ! HISMASTR ! EMPLOYMENT HISTORY MASTER FILE           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "APLSKILL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  13,                     ~
                        alt key  1, keypos =    1, keylen =  29

            select #02, "REQSKILL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =   7,                     ~
                        alt key  1, keypos =    1, keylen =  23

            select #03, "EMPSKILL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  14,                     ~
                        alt key  1, keypos =    1, keylen =  30

            select #04, "COMTERM",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  162,                                  ~
                        keypos =   47, keylen =  16,                     ~
                        alt key  1, keypos =   17, keylen =  46,         ~
                            key  2, keypos =    1, keylen =  62

            select #05, "APLMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =   82, keylen =  11,                     ~
                        alt key  1, keypos =   56, keylen =  37,         ~
                            key  2, keypos =   50, keylen =  43,         ~
                            key  3, keypos =   34, keylen =  59,         ~
                            key  4, keypos =   18, keylen =  75,         ~
                            key  5, keypos =   13, keylen =  80,         ~
                            key  6, keypos =    1, keylen =  92

            select #06, "REQMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  370,                                  ~
                        keypos =   30, keylen =   5,                     ~
                        alt key  1, keypos =   24, keylen =  11,         ~
                            key  2, keypos =    8, keylen =  27,         ~
                            key  3, keypos =    7, keylen =  28,         ~
                            key  4, keypos =    1, keylen =  34

            select #07, "EMPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #08, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  950,                                  ~
                        keypos =  39,  keylen = 12,                      ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50

            select #09, "PERFRNGE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 250,                                   ~
                        keypos = 17,   keylen = 15,                      ~
                     alt key 1, keypos = 1, keylen = 31

            select #10, "INSMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos = 17,   keylen = 15,                      ~
                        alt key  1, keypos =    1, keylen =  31

            select #11, "EMPHSTRY",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =   39, keylen =  15,                     ~
                        alt key  1, keypos =   23, keylen =  31,         ~
                            key  2, keypos =    7, keylen =  47,         ~
                            key  3, keypos =    1, keylen =  53


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

        call "SHOSTAT" ("LINKING TO DATA BASE FOR INSURANCE MANAGEMENT")

           for i% = 1% to 11%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
           if f2%(i%) = 0% then goto L07950
            call "OPENFILE" (#i%, "OUTPT", f2%(i%), rslt$(i%), axd$(i%))
                     close #i%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
L07950:    next i%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

L09120: REM *************************************************************~
            *       S T A R T I N G   P O I N T                         *~
            *                                                           *~
            *************************************************************

        init(" ")                                                        ~
            charges$(),                  /* CHARGES ACCUMULATED Y TO D */~
            chargefor$(),                /* WHAT ARE CHARGES FOR       */~
            coverage$(),                 /* AMOUNT OF COVERAGE         */~
            covered$(),                  /* WHO IS COVERED?            */~
            deductable$(),               /* DEDUCTABLE - AMT, FOR, MET?*/~
            dedfor$(),                   /* WHAT IS DED FOR?           */~
            dedmet$(),                   /* IS DED YET MET THIS YEAR?  */~
            readkey$  ,                  /* GENERAL PURPOSE FILE KEY   */~
            edtmessage$  ,               /* EDIT SCREEN MESSAGE        */~
            eligdate$(),                 /* ELIGIBILITY DATE           */~
            errormsg$  ,                 /* ERROR MESSAGE              */~
            freetext$(),                 /* FREE TEXT                  */~
            inpmessage$  ,               /* INPUT MESSAGE              */~
            insurance$(),                /* TYPE OF INSURANCE          */~
            insurancedescr$  ,           /* TYPE OF INSURANCE          */~
            pass$   ,                    /* FJOR COMSUB                */~
            premium$(),                  /* PREMIUM DATA  -  AMT & FOR?*/~
            premfor$(),                  /* WHAT IS PREMIUM FOR?       */~
            startdate$()                 /* START DATE                 */~

           REM REMOVE THIS SECTION WHEN USED AS A SUBROUTINE
                     init(" ") employee$, name$
L09435: accept                                                           ~
               at (01,02),                                               ~
        "                          INSURANCE BENEFITS MANAGEMENT         ~
        ~             ",                                                  ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!                                                               ~
        ~            !",                                                  ~
               at (05,03),                                               ~
        "!                                                               ~
        ~            !",                                                  ~
               at (06,03),                                               ~
        "!  THIS PROGRAM ALLOWS YOU TO MANAGE JUST THE INSURANCE IN FORCE~
        ~ FOR YOUR   !",                                                  ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!  EMPLOYEES.  PLEASE ENTER THE CODE FOR THE EMPLOYEE DESIRED   ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,30), fac(hex(81)), employee$, ch(12),              ~
               at (08,79),                                               ~
        "!",                                                             ~
               at (09,03),                                               ~
        "!",                                                             ~
               at (09,05), fac(hex(94)), errormsg$, ch(70),              ~
               at (09,79),                                               ~
        "!",                                                             ~
               at (10,03),                                               ~
        "!",                                                             ~
               at (10,79),                                               ~
        "!",                                                             ~
               at (11,03),                                               ~
        "!  IF THE EMPLOYEE IS NOT ON FILE PLEASE EXIT AND USE THE NORMAL~
        ~ EMPLOYEE   !",                                                  ~
               at (12,03),                                               ~
        "!  INFORMATION MANAGEMENT PROGRAM TO INPUT ALL OF THE DATA REQUI~
        ~RED         !",                                                  ~
               at (13,03),                                               ~
        "!",                                                             ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "!",                                                             ~
               at (14,79),                                               ~
        "!",                                                             ~
               at (15,03),                                               ~
        "!",                                                             ~
               at (15,79),                                               ~
        "!",                                                             ~
               at (16,03),                                               ~
        "!",                                                             ~
               at (16,79),                                               ~
        "!",                                                             ~
               at (17,03),                                               ~
        "!",                                                             ~
               at (17,79),                                               ~
        "!",                                                             ~
               at (18,03),                                               ~
        "!",                                                             ~
               at (18,79),                                               ~
        "!",                                                             ~
               at (19,03),                                               ~
        "!",                                                             ~
               at (19,79),                                               ~
        "!",                                                             ~
               at (20,03),                                               ~
        "!",                                                             ~
               at (20,79),                                               ~
        "!",                                                             ~
               at (21,03),                                               ~
        "!",                                                             ~
               at (21,79),                                               ~
        "!",                                                             ~
               at (22,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        "(ENTER) TO GATHER THE BENEFIT DATA FOR THIS EMPLOYEE            ~
        ~             ",                                                  ~
               at (24,03),                                               ~
        "                               (15)PRINT SCREEN       (16)EXIT F~
        ~ROM PROGRAM  ",                                                  ~
           keys(hex(000f10)), key(hh%)
           if hh% = 16% then goto L65000
           if hh% <> 15% then goto L09875
                     call "PRNTSCRN"
                     goto L09435
L09875:    if hh% <> 0% then goto L09435
                     call "READ100" (#8, employee$, f1%(8))
                     if f1%(8) = 1% then goto L09900
                     errormsg$ = "NO SUCH EMPLOYEE ON FILE"
                     goto L09435
L09900:              get #8, using L09905 , lname$, fname$, mname$
L09905:              FMT XX(1), CH(15), CH(10), CH(1)
                     name$ = str(fname$,1,len(fname$))
                     str(name$,len(fname$) + 2, 1) = mname$
                     str(name$,len(fname$) + 4) = lname$
                     init(" ") errormsg$
           REM REMOVE THE SECTION ABOVE WHEN USED AS A SUBROUTINE


           gosub L31000

           goto summaryscreen

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") pass$

            for fieldnr% = 1 to 15
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0 then goto L10172
L10120:         gosub'101(fieldnr%)
                      if keyhit% = 4 then goto L10174
                     if keyhit%  = 16 and fieldnr% = 1 then summaryscreen
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10172:         if keyhit% <> 4% then goto L10180
L10174:               fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10100
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode

L11060:     gosub'111(0%)
                  if keyhit%  = 16 then       summaryscreen
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 15 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L30000
            goto  L09120

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* TYPE OF INSURANCE*/~
                                    L20150,         /* ELIGIBILITY DATE */~
                                    L20200,         /* START DATE       */~
                                    L20250,         /* AMT OF COVERAGE  */~
                                    L20300,         /* WHO IS COVERED?  */~
                                    L20350,         /* PREMIUM 1        */~
                                    L20400,         /* PREMIUM 2        */~
                                    L20450,         /* DEDUCTABLE 1     */~
                                    L20500,         /* DEDUCTABLE 2     */~
                                    L20550,         /* CHARGES 1        */~
                                    L20600,         /* CHARGES 2        */~
                                    L20650,         /* CHARGES 3        */~
                                    L20700,         /* FREE TEXT        */~
                                    L20750,         /* FREE TEXT        */~
                                    L20800          /* FREE TEXT        */
                     return
L20100:     REM DEFAULT/ENABLE FOR TYPE OF INSURANCE
           inpmessage$ = "ENTER THE TYPE OF INSURANCE, IF YOU DON'T KNOW ~
        ~LEAVE IT BLANK"
           call "RJUSTIFY" (inpmessage$)
                return
L20150:     REM DEFAULT/ENABLE FOR ELIGIBILITY DATE
           inpmessage$ = "ENTER THE DATE ON WHICH THE EMPLOYEE BECOMES EL~
        ~IGIBLE FOR THIS INSURANCE"
           call "RJUSTIFY" (inpmessage$)
                return
L20200:     REM DEFAULT/ENABLE FOR START DATE
           inpmessage$ = "ENTER THE DATE ON WHICH THE COVERAGE STARTED"
           call "RJUSTIFY" (inpmessage$)
                return
L20250:     REM DEFAULT/ENABLE FOR AMOUNT OF COVERAGE
           inpmessage$ = "ENTER THE AMOUNT OF COVERAGE"
           call "RJUSTIFY" (inpmessage$)
                return
L20300:     REM DEFAULT/ENABLE FOR WHO IS COVERED?
           inpmessage$ = "ENTER WHO ALL IS COVERED BY THIS PROGRAM"
           call "RJUSTIFY" (inpmessage$)
                return
L20350:     REM DEFAULT/ENABLE FOR PREMIUM DATA  -  AMT & FOR?
           inpmessage$ = "ENTER BOTH THE AMOUNT OF PREMIUM AND WHAT IT IS~
        ~ FOR"
           call "RJUSTIFY" (inpmessage$)
                return
L20400:     REM DEFAULT/ENABLE FOR        ADDITIONAL PREM DATA
           inpmessage$ = "IF THERE IS MORE THAN ONE PREMIUM, ENTER HERE E~
        ~LSE LEAVE BLANK"
           call "RJUSTIFY" (inpmessage$)
                return
L20450:     REM DEFAULT/ENABLE FOR DEDUCTABLE - AMT, FOR, MET?
           inpmessage$ = "ENTER DEDUCTABLE AMOUNT, WHAT IT IS FOR, AND WH~
        ~ETHER IT HAS BEEN MET YET"
           call "RJUSTIFY" (inpmessage$)
                return
L20500:     REM DEFAULT/ENABLE FOR        ADD'L DEDUCTION DATA
           inpmessage$ = "IF THERE IS MORE THAN ONE DEDUCTABLE, ENTER HER~
        ~E ELSE LEAVE BLANK"
           call "RJUSTIFY" (inpmessage$)
                return
L20550:     REM DEFAULT/ENABLE FOR CHARGES ACCUMULATED Y TO D
           inpmessage$ = "ENTER BOTH CHARGES ACCUMULATED TO DATE AND WHAT~
        ~ CHARGES ARE"
           call "RJUSTIFY" (inpmessage$)
                return
L20600:     REM DEFAULT/ENABLE FOR        ADD'L CHARGES ACCUM
           inpmessage$ = "ENTER ADDITIONAL ACCUMULATED CHARGES HERE, ELSE~
        ~ LEAVE BLANK"
           call "RJUSTIFY" (inpmessage$)
                return
L20650:     REM DEFAULT/ENABLE FOR        ADD'L CHARGES ACCUM
           if charges$(2,a%) = " " then enabled% = 0%
           inpmessage$ = "ENTER ADDITIONAL ACCUMULATED CHARGES HERE, ELSE~
        ~ LEAVE BLANK"
           call "RJUSTIFY" (inpmessage$)
                return
L20700:     REM DEFAULT/ENABLE FOR FREE TEXT
                return
L20750:     REM DEFAULT/ENABLE FOR FREE TEXT
                     if freetext$(1,a%) = " " then enabled% = 0%
                return
L20800:     REM DEFAULT/ENABLE FOR FREE TEXT
                     if freetext$(2,a%) = " " then enabled% = 0%
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************



L30000: REM WRITE DATA TO INSMASTR FILE

           readkey$   = str(employee$,1,12) & " "
           call "DELETE" (#10, readkey$  , 12%)


           for i% = 1% to maxi%
           if insurance$(i%) = " " then goto  L30260
           convert i% to seqnr$, pic(###)
                     call "DATUNFMT" (startdate$(i%))
                     call "DATUNFMT" (eligdate$(i%))

        put #10, using L35030,    /* FILE: INSMASTR                     */~
            insurance$(i%),      /* Type of insurance program          */~
            employee$,           /* employee code                      */~
            seqnr$,              /* General purpose sequence number    */~
            eligdate$(i%),       /* Benefit eligibility date           */~
            startdate$(i%),      /* Benefit start date                 */~
            coverage$(i%),       /* Amount of insurance coverage       */~
            covered$(i%),        /* Who is covered by insurance progra */~
            premium$(1,i%),      /* Premium amount(s) - often subscrip */~
            premium$(2,i%),      /* Premium amount(s) - often subscrip */~
            premium$(3,i%),      /* Premium amount(s) - often subscrip */~
            premfor$(1,i%),      /* What is the premium for- often sub */~
            premfor$(2,i%),      /* What is the premium for- often sub */~
            premfor$(3,i%),      /* What is the premium for- often sub */~
            deductable$(1,i%),   /* Deductable amount - often subscrip */~
            deductable$(2,i%),   /* Deductable amount - often subscrip */~
            deductable$(3,i%),   /* Deductable amount - often subscrip */~
            dedfor$(1,i%),       /* What is the deductable for? often  */~
            dedfor$(2,i%),       /* What is the deductable for? often  */~
            dedfor$(3,i%),       /* What is the deductable for? often  */~
            dedmet$(1,i%),       /* Is the deductable yet met? often s */~
            dedmet$(2,i%),       /* Is the deductable yet met? often s */~
            dedmet$(3,i%),       /* Is the deductable yet met? often s */~
            charges$(1,i%),      /* Charges accumulated year to date-o */~
            charges$(2,i%),      /* Charges accumulated year to date-o */~
            charges$(3,i%),      /* Charges accumulated year to date-o */~
            charges$(4,i%),      /* Charges accumulated year to date-o */~
            chargefor$(1,i%),    /* What are the charges for? often su */~
            chargefor$(2,i%),    /* What are the charges for? often su */~
            chargefor$(3,i%),    /* What are the charges for? often su */~
            chargefor$(4,i%),    /* What are the charges for? often su */~
            freetext$(1,i%),     /* Any free text information          */~
            freetext$(2,i%),     /* Any free text information          */~
            freetext$(3,i%),     /* Any free text information          */~
            filler$(i%)          /* filler for rest of record or inter */~

           write #10
L30260:    next i%

           return

L31000: REM GET DATA FROM INSMASTR FILE

           i%, maxi% = 0%
           readkey$   = str(employee$,1,12) & " "
L31050:    call "PLOWNEXT" (#10, readkey$  , 12%, f1%(10) )
           if f1%(10) <> 1% then return
           i%, maxi% = i% + 1%

        get #10, using L35030,    /* FILE: INSMASTR                     */~
            insurance$(i%),      /* Type of insurance program          */~
            employee$,           /* employee code                      */~
            seqnr$,              /* General purpose sequence number    */~
            eligdate$(i%),       /* Benefit eligibility date           */~
            startdate$(i%),      /* Benefit start date                 */~
            coverage$(i%),       /* Amount of insurance coverage       */~
            covered$(i%),        /* Who is covered by insurance progra */~
            premium$(1,i%),      /* Premium amount(s) - often subscrip */~
            premium$(2,i%),      /* Premium amount(s) - often subscrip */~
            premium$(3,i%),      /* Premium amount(s) - often subscrip */~
            premfor$(1,i%),      /* What is the premium for- often sub */~
            premfor$(2,i%),      /* What is the premium for- often sub */~
            premfor$(3,i%),      /* What is the premium for- often sub */~
            deductable$(1,i%),   /* Deductable amount - often subscrip */~
            deductable$(2,i%),   /* Deductable amount - often subscrip */~
            deductable$(3,i%),   /* Deductable amount - often subscrip */~
            dedfor$(1,i%),       /* What is the deductable for? often  */~
            dedfor$(2,i%),       /* What is the deductable for? often  */~
            dedfor$(3,i%),       /* What is the deductable for? often  */~
            dedmet$(1,i%),       /* Is the deductable yet met? often s */~
            dedmet$(2,i%),       /* Is the deductable yet met? often s */~
            dedmet$(3,i%),       /* Is the deductable yet met? often s */~
            charges$(1,i%),      /* Charges accumulated year to date-o */~
            charges$(2,i%),      /* Charges accumulated year to date-o */~
            charges$(3,i%),      /* Charges accumulated year to date-o */~
            charges$(4,i%),      /* Charges accumulated year to date-o */~
            chargefor$(1,i%),    /* What are the charges for? often su */~
            chargefor$(2,i%),    /* What are the charges for? often su */~
            chargefor$(3,i%),    /* What are the charges for? often su */~
            chargefor$(4,i%),    /* What are the charges for? often su */~
            freetext$(1,i%),     /* Any free text information          */~
            freetext$(2,i%),     /* Any free text information          */~
            freetext$(3,i%),     /* Any free text information          */~
            filler$(i%)          /* filler for rest of record or inter */~

                     call "DATEFMT" (startdate$(i%))
                     call "DATEFMT" (eligdate$(i%))

           if i% = 20% then return  /* ARRAYS ARE FULL */
           goto L31050

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: INSMASTR                     */~
            CH(16),              /* Type of insurance program          */~
            CH(12),              /* employee code                      */~
            CH(3),               /* General purpose sequence number    */~
            CH(6),               /* Benefit eligibility date           */~
            CH(6),               /* Benefit start date                 */~
            CH(10),              /* Amount of insurance coverage       */~
            CH(16),              /* Who is covered by insurance progra */~
            3*CH(10),            /* Premium amount(s) - often subscrip */~
            3*CH(30),            /* What is the premium for- often sub */~
            3*CH(10),            /* Deductable amount - often subscrip */~
            3*CH(30),            /* What is the deductable for? often  */~
            3*CH(1),             /* Is the deductable yet met? often s */~
            4*CH(10),            /* Charges accumulated year to date-o */~
            4*CH(30),            /* What are the charges for? often su */~
            3*CH(50),            /* Any free text information          */~
            CH(178)              /* filler for rest of record or inter */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140,         /* TYPE OF INSURANCE*/~
                                    L40140,         /* ELIGIBILITY DATE */~
                                    L40140,         /* START DATE       */~
                                    L40155,         /* AMT OF COVERAGE  */~
                                    L40140,         /* WHO IS COVERED?  */~
                                    L40140,         /* PREMIUM 1        */~
                                    L40140,         /* PREMIUM 2        */~
                                    L40140,         /* DEDUCTABLE 1     */~
                                    L40140,         /* DEDUCTABLE 2     */~
                                    L40140,         /* CHARGES 1        */~
                                    L40140,         /* CHARGES 2        */~
                                    L40140,         /* CHARGES 3        */~
                                    L40125,         /* FREE TEXT        */~
                                    L40125,         /* FREE TEXT        */~
                                    L40125          /* FREE TEXT        */
                     goto L40175

L40125:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40155:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40175:     accept                                                       ~
               at (01,02),                                               ~
                  "INSURANCE FOR ",                                      ~
               at (01,20),                                               ~
                  fac(hex(84)), name$, ch(30),                           ~
               at (02,30),                                               ~
                  fac(hex(8c)), str(pass$,63,50), ch(50),                ~
               at (03,30),                                               ~
                  fac(hex(8c)), str(pass$,113,50), ch(50),               ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "TYPE OF INSURANCE",                                   ~
               at (06,30), fac(lfac$( 1)), insurance$(a%)       , ch(16),~
               at (06,49), fac(hex(8c)), str(pass$,17,30)       , ch(32),~
               at (07,02),                                               ~
                  "ELIGIBILITY DATE",                                    ~
               at (07,30), fac(lfac$( 2)), eligdate$(a%)        , ch(08),~
               at (08,02),                                               ~
                  "START DATE",                                          ~
               at (08,30), fac(lfac$( 3)), startdate$(a%)       , ch(08),~
               at (09,02),                                               ~
                  "AMOUNT OF COVERAGE",                                  ~
               at (09,30), fac(lfac$( 4)), coverage$(a%)        , ch(10),~
               at (10,02),                                               ~
                  "WHO IS COVERED?",                                     ~
               at (10,30), fac(lfac$( 5)), covered$(a%)         , ch(16),~
               at (11,02),                                               ~
                  "PREMIUM DATA  -  AMT & FOR?",                         ~
               at (11,30), fac(lfac$( 6)), premium$(1, a%)      , ch(10),~
               at (11,45), fac(lfac$( 6)), premfor$(1, a%)      , ch(30),~
               at (12,02),                                               ~
                  "       ADDITIONAL PREM DATA",                         ~
               at (12,30), fac(lfac$( 7)), premium$(2, a%)      , ch(10),~
               at (12,45), fac(lfac$( 7)), premfor$(2, a%)      , ch(30),~
               at (13,02),                                               ~
                  "DEDUCTABLE - AMT, FOR, MET?",                         ~
               at (13,30), fac(lfac$( 8)), deductable$(1,a%)    , ch(10),~
               at (13,45), fac(lfac$( 8)), dedfor$    (1,a%)    , ch(30),~
               at (13,78), fac(lfac$( 8)), dedmet$    (1,a%)    , ch(01),~
               at (14,02),                                               ~
                  "       ADD'L DEDUCTION DATA",                         ~
               at (14,30), fac(lfac$( 9)), deductable$(2,a%)    , ch(10),~
               at (14,45), fac(lfac$( 9)), dedfor$    (2,a%)    , ch(30),~
               at (14,78), fac(lfac$( 9)), dedmet$    (2,a%)    , ch(01),~
               at (15,02),                                               ~
                  "CHARGES ACCUMULATED Y TO D",                          ~
               at (15,30), fac(lfac$(10)), charges$(1,a%)       , ch(10),~
               at (15,45), fac(lfac$(10)), chargefor$(1,a%)     , ch(30),~
               at (16,02),                                               ~
                  "       ADD'L CHARGES ACCUM",                          ~
               at (16,30), fac(lfac$(11)), charges$(2, a%)      , ch(10),~
               at (16,45), fac(lfac$(11)), chargefor$(2,a%)     , ch(30),~
               at (17,02),                                               ~
                  "       ADD'L CHARGES ACCUM",                          ~
               at (17,30), fac(lfac$(12)), charges$(3, a%)      , ch(10),~
               at (17,45), fac(lfac$(12)), chargefor$(3,a%)     , ch(30),~
               at (18,02),                                               ~
                  "FREE TEXT",                                           ~
               at (18,30), fac(lfac$(13)), freetext$(1, a%)     , ch(50),~
               at (19,02),                                               ~
                  "FREE TEXT",                                           ~
               at (19,30), fac(lfac$(14)), freetext$(2, a%)     , ch(50),~
               at (20,02),                                               ~
                  "FREE TEXT",                                           ~
               at (20,30), fac(lfac$(15)), freetext$(3, a%)     , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)REENTER      (4)PREVIOUS FIELD",                   ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)TO SUMMARY  ",                                    ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

           init(" ") inpmessage$

               if keyhit% <> 1% then goto L40530
                  goto L41595

L40530:        if keyhit% <> 13 then L40550
                  call "MANUAL" ("INSINPUT")
                  goto L40175

L40550:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40175

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41140,         /* TYPE OF INSURANCE*/~
                                    L41140,         /* ELIGIBILITY DATE */~
                                    L41140,         /* START DATE       */~
                                    L41155,         /* AMT OF COVERAGE  */~
                                    L41140,         /* WHO IS COVERED?  */~
                                    L41140,         /* PREMIUM 1        */~
                                    L41140,         /* PREMIUM 2        */~
                                    L41140,         /* DEDUCTABLE 1     */~
                                    L41140,         /* DEDUCTABLE 2     */~
                                    L41140,         /* CHARGES 1        */~
                                    L41140,         /* CHARGES 2        */~
                                    L41140,         /* CHARGES 3        */~
                                    L41125,         /* FREE TEXT        */~
                                    L41125,         /* FREE TEXT        */~
                                    L41125          /* FREE TEXT        */
                     goto L41175

L41125:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41155:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41175:     accept                                                       ~
               at (01,02),                                               ~
                  "INSURANCE ON FILE ",                                  ~
               at (01,20),                                               ~
                  fac(hex(84)), name$, ch(30),                           ~
               at (02,30),                                               ~
                  fac(hex(8c)), str(pass$,63,50), ch(50),                ~
               at (03,30),                                               ~
                  fac(hex(8c)), str(pass$,113,50), ch(50),               ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "TYPE OF INSURANCE",                                   ~
               at (06,30), fac(lfac$( 1)), insurance$(a%)       , ch(16),~
               at (06,49), fac(hex(8c)), str(pass$,17,30)       , ch(32),~
               at (07,02),                                               ~
                  "ELIGIBILITY DATE",                                    ~
               at (07,30), fac(lfac$( 2)), eligdate$(a%)        , ch(08),~
               at (08,02),                                               ~
                  "START DATE",                                          ~
               at (08,30), fac(lfac$( 3)), startdate$(a%)       , ch(08),~
               at (09,02),                                               ~
                  "AMOUNT OF COVERAGE",                                  ~
               at (09,30), fac(lfac$( 4)), coverage$(a%)        , ch(10),~
               at (10,02),                                               ~
                  "WHO IS COVERED?",                                     ~
               at (10,30), fac(lfac$( 5)), covered$(a%)         , ch(16),~
               at (11,02),                                               ~
                  "PREMIUM DATA  -  AMT & FOR?",                         ~
               at (11,30), fac(lfac$( 6)), premium$(1, a%)      , ch(10),~
               at (11,45), fac(lfac$( 6)), premfor$(1, a%)      , ch(30),~
               at (12,02),                                               ~
                  "       ADDITIONAL PREM DATA",                         ~
               at (12,30), fac(lfac$( 7)), premium$(2, a%)      , ch(10),~
               at (12,45), fac(lfac$( 7)), premfor$(2, a%)      , ch(30),~
               at (13,02),                                               ~
                  "DEDUCTABLE - AMT, FOR, MET?",                         ~
               at (13,30), fac(lfac$( 8)), deductable$(1,a%)    , ch(10),~
               at (13,45), fac(lfac$( 8)), dedfor$    (1,a%)    , ch(30),~
               at (13,78), fac(lfac$( 8)), dedmet$    (1,a%)    , ch(01),~
               at (14,02),                                               ~
                  "       ADD'L DEDUCTION DATA",                         ~
               at (14,30), fac(lfac$( 9)), deductable$(2,a%)    , ch(10),~
               at (14,45), fac(lfac$( 9)), dedfor$    (2,a%)    , ch(30),~
               at (14,78), fac(lfac$( 9)), dedmet$    (2,a%)    , ch(01),~
               at (15,02),                                               ~
                  "CHARGES ACCUMULATED Y TO D",                          ~
               at (15,30), fac(lfac$(10)), charges$(1,a%)       , ch(10),~
               at (15,45), fac(lfac$(10)), chargefor$(1,a%)     , ch(30),~
               at (16,02),                                               ~
                  "       ADD'L CHARGES ACCUM",                          ~
               at (16,30), fac(lfac$(11)), charges$(2, a%)      , ch(10),~
               at (16,45), fac(lfac$(11)), chargefor$(2,a%)     , ch(30),~
               at (17,02),                                               ~
                  "       ADD'L CHARGES ACCUM",                          ~
               at (17,30), fac(lfac$(12)), charges$(3, a%)      , ch(10),~
               at (17,45), fac(lfac$(12)), chargefor$(3,a%)     , ch(30),~
               at (18,02),                                               ~
                  "FREE TEXT",                                           ~
               at (18,30), fac(lfac$(13)), freetext$(1, a%)     , ch(50),~
               at (19,02),                                               ~
                  "FREE TEXT",                                           ~
               at (19,30), fac(lfac$(14)), freetext$(2, a%)     , ch(50),~
               at (20,02),                                               ~
                  "FREE TEXT",                                           ~
               at (20,30), fac(lfac$(15)), freetext$(3, a%)     , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(13)INSTRUCTIONS       (12)DELETE THIS INSURANCE COVER~
        ~AGE",                                                            ~
               at (24,02),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(000c0d0f10)),                                    ~
               key (keyhit%)

               if keyhit% = 12% then goto L41595

               if keyhit% <> 13 then L41550
                  call "MANUAL" ("INSINPUT")
                  goto L41175

L41550:        if keyhit% <> 15 then L41570
                  call "PRNTSCRN"
                  goto L41175

L41570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L41595:        gosub really_delete : if delkey% <> 12% then L41175
        init(" ")                                                        ~
            charges$(1,a%),              /* CHARGES ACCUMULATED Y TO D */~
            charges$(2,a%),              /* CHARGES ACCUMULATED Y TO D */~
            charges$(3,a%),              /* CHARGES ACCUMULATED Y TO D */~
            chargefor$(1,a%),            /* WHAT ARE CHARGES FOR       */~
            chargefor$(2,a%),            /* WHAT ARE CHARGES FOR       */~
            chargefor$(3,a%),            /* WHAT ARE CHARGES FOR       */~
            coverage$(a%),               /* AMOUNT OF COVERAGE         */~
            covered$(a%),                /* WHO IS COVERED?            */~
            deductable$(1,a%),           /* DEDUCTABLE - AMT, FOR, MET?*/~
            deductable$(2,a%),           /* DEDUCTABLE - AMT, FOR, MET?*/~
            deductable$(3,a%),           /* DEDUCTABLE - AMT, FOR, MET?*/~
            dedfor$(1,a%),               /* WHAT IS DED FOR?           */~
            dedfor$(2,a%),               /* WHAT IS DED FOR?           */~
            dedfor$(3,a%),               /* WHAT IS DED FOR?           */~
            dedmet$(1,a%),               /* IS DED YET MET THIS YEAR?  */~
            dedfor$(2,a%),               /* WHAT IS DED FOR?           */~
            dedfor$(3,a%),               /* WHAT IS DED FOR?           */~
            readkey$  ,                  /* GENERAL PURPOSE FILE KEY   */~
            edtmessage$  ,               /* EDIT SCREEN MESSAGE        */~
            eligdate$(a%),               /* ELIGIBILITY DATE           */~
            errormsg$  ,                 /* ERROR MESSAGE              */~
            freetext$(1,a%),             /* FREE TEXT                  */~
            freetext$(2,a%),             /* FREE TEXT                  */~
            freetext$(3,a%),             /* FREE TEXT                  */~
            inpmessage$  ,               /* INPUT MESSAGE              */~
            insurance$(a%),              /* TYPE OF INSURANCE          */~
            pass$   ,                    /* FJOR COMSUB                */~
            premium$(1,a%) ,             /* PREMIUM DATA  -  AMT & FOR?*/~
            premium$(2,a%)  ,            /* PREMIUM DATA  -  AMT & FOR?*/~
            premium$(3,a%)   ,           /* PREMIUM DATA  -  AMT & FOR?*/~
            premfor$(1,a%),              /* WHAT IS PREMIUM FOR?       */~
            premfor$(2,a%),              /* WHAT IS PREMIUM FOR?       */~
            premfor$(3,a%),              /* WHAT IS PREMIUM FOR?       */~
            startdate$(a%)               /* START DATE                 */

           goto summaryscreen

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* TYPE OF INSURANCE*/~
                                    L50150,         /* ELIGIBILITY DATE */~
                                    L50200,         /* START DATE       */~
                                    L50250,         /* AMT OF COVERAGE  */~
                                    L50300,         /* WHO IS COVERED?  */~
                                    L50350,         /* PREMIUM 1        */~
                                    L50400,         /* PREMIUM 2        */~
                                    L50450,         /* DEDUCTABLE 1     */~
                                    L50500,         /* DEDUCTABLE 2     */~
                                    L50550,         /* CHARGES 1        */~
                                    L50600,         /* CHARGES 2        */~
                                    L50650,         /* CHARGES 3        */~
                                    L50700,         /* FREE TEXT        */~
                                    L50750,         /* FREE TEXT        */~
                                    L50800          /* FREE TEXT        */
                     return
L50100:     REM TEST DATA FOR TYPE OF INSURANCE
           init(" ") pass$
           str(pass$,47,16) = str(insurance$(a%) ,1,16)
L50106:    call "COMSUB" (#4, 1%, pass$, 1%, rc%)
           if rc% =  0% then goto L50106
           if str(pass$,1,16) =                                          ~
                        "INSURANCE       " then goto L50124
        errormsg$="THE TERM "& str(insurance$(a%),1,len(insurance$(a%))) ~
             & " IS NOT INSURANCE, PLEASE REENTER"
           init(" ") insurance$(a%)
           call "RJUSTIFY" (errormsg$)
                     return
L50124:    str(insurance$(a%) ,1,16) = str(pass$,47,16)
                return
L50150:     REM TEST DATA FOR ELIGIBILITY DATE
            if eligdate$(a%) = " " or eligdate$(a%) = blankdate$ then return
            call "DATEOK" (eligdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50200:     REM TEST DATA FOR START DATE
            if startdate$(a%) = " " or startdate$(a%) = blankdate$ then return
            call "DATEOK" (startdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50250:     REM TEST DATA FOR AMOUNT OF COVERAGE
             if coverage$(  a%) = " " then coverage$(  a%) = "      NONE"
             if coverage$(  a%) = "0" then coverage$(  a%) = "      NONE"
             if coverage$(  a%) = "      NONE" then return
                     convert coverage$(  a%) to d, data goto L50285
                     if d <= 999999.99 then goto L50275
                     convert d to coverage$(  a%), pic(#######.##)
                     return
L50275:              convert d to coverage$(  a%), pic($######.##)
                     return
L50285: errormsg$ = "PLEASE ENTER COVERAGE AMOUNT IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50300:     REM TEST DATA FOR WHO IS COVERED?
                return
L50350:     REM TEST DATA FOR PREMIUM DATA  -  AMT & FOR?
             if premium$ (1,a%) = " " then premium$ (1,a%) = "      NONE"
             if premium$ (1,a%) = "0" then premium$ (1,a%) = "      NONE"
             if premium$ (1,a%) = "      NONE" then return
                     convert premium$ (1,a%) to d, data goto L50385
                     convert d to premium$ (1,a%), pic($######.##)
                     return
L50385: errormsg$ = "PLEASE ENTER PREMIUM AMOUNT IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50400:     REM TEST DATA FOR        ADDITIONAL PREM DATA
             if premium$ (2,a%) = " " then premium$ (2,a%) = "      NONE"
             if premium$ (2,a%) = "0" then premium$ (2,a%) = "      NONE"
             if premium$ (2,a%) = "      NONE" then return
                     convert premium$ (2,a%) to d, data goto L50435
                     convert d to premium$ (2,a%), pic($######.##)
                     return
L50435: errormsg$ = "PLEASE ENTER PREMIUM AMOUNT IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50450:     REM TEST DATA FOR DEDUCTABLE - AMT, FOR, MET?
        if deductable$(1,a%) = " " then deductable$ (1,a%) = "      NONE"
        if deductable$(1,a%) = "0" then deductable$ (1,a%) = "      NONE"
        if deductable$(1,a%) = "      NONE" then return
                  convert deductable$  (1,a%) to d, data goto L50485
                     convert d to deductable$  (1,a%), pic($######.##)
                     goto L50485
        errormsg$ = "PLEASE ENTER DEDUCTABLE AMOUNT IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50485:    if dedmet$(1,a%) <> "Y" then dedmet$(1,a%) = "N"
           return
L50500:     REM TEST DATA FOR        ADD'L DEDUCTION DATA
        if deductable$(2,a%) = " " then deductable$ (2,a%) = "      NONE"
        if deductable$(2,a%) = "0" then deductable$ (2,a%) = "      NONE"
        if deductable$(2,a%) = "      NONE" then return
                  convert deductable$  (2,a%) to d, data goto L50520
                     convert d to deductable$  (2,a%), pic($######.##)
                     goto L50520
        errormsg$ = "PLEASE ENTER DEDUCTABLE AMOUNT IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50520:    if dedmet$(2,a%) <> "Y" then dedmet$(2,a%) = "N"
                return
L50550:     REM TEST DATA FOR CHARGES ACCUMULATED Y TO D
        if charges$   (1,a%) = " " then charges$    (1,a%) = "      NONE"
        if charges$   (1,a%) = "0" then charges$    (1,a%) = "      NONE"
        if charges$   (1,a%) = "      NONE" then return
                  convert charges$     (1,a%) to d, data goto L50520
                     convert d to charges$     (1,a%), pic($######.##)
                     return
        errormsg$ ="PLEASE ENTER ACCUMULATED CHARGES IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50600:     REM TEST DATA FOR        ADD'L CHARGES ACCUM
        if charges$   (2,a%) = " " then charges$    (2,a%) = "      NONE"
        if charges$   (2,a%) = "0" then charges$    (2,a%) = "      NONE"
        if charges$   (2,a%) = "      NONE" then return
                  convert charges$     (2,a%) to d, data goto L50520
                     convert d to charges$     (2,a%), pic($######.##)
                     return
        errormsg$ ="PLEASE ENTER ACCUMULATED CHARGES IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50650:     REM TEST DATA FOR        ADD'L CHARGES ACCUM
        if charges$   (3,a%) = " " then charges$    (3,a%) = "      NONE"
        if charges$   (3,a%) = "0" then charges$    (3,a%) = "      NONE"
        if charges$   (3,a%) = "      NONE" then return
                  convert charges$     (3,a%) to d, data goto L50520
                     convert d to charges$     (3,a%), pic($######.##)
                     return
        errormsg$ ="PLEASE ENTER ACCUMULATED CHARGES IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50700:     REM TEST DATA FOR FREE TEXT
                return
L50750:     REM TEST DATA FOR FREE TEXT
                return
L50800:     REM TEST DATA FOR FREE TEXT
                return

        summaryscreen

           init(" ") errormsg$


L51055: accept                                                           ~
               at (01,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (02,03),                                               ~
        "! INSURANCE PROGRAMS  ON FILE FOR",                             ~
               at (02,38),                                               ~
         fac(hex(84)), name$, ch(30) ,                                   ~
               at (02,79),                                               ~
        "!",                                                             ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!  INSURANCE          ELIG DATE   START DATE    COVERAGE        ~
        ~            !",                                                  ~
               at (05,03),                                               ~
        "!                                                               ~
        ~            !",                                                  ~
               at (06,03),                                               ~
        "!",                                                             ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!",                                                             ~
               at (07,79),                                               ~
        "!",                                                             ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,79),                                               ~
        "!",                                                             ~
               at (09,03),                                               ~
        "!",                                                             ~
               at (09,79),                                               ~
        "!",                                                             ~
               at (10,03),                                               ~
        "!",                                                             ~
               at (10,79),                                               ~
        "!",                                                             ~
               at (11,03),                                               ~
        "!",                                                             ~
               at (11,79),                                               ~
        "!",                                                             ~
               at (12,03),                                               ~
        "!",                                                             ~
               at (12,79),                                               ~
        "!",                                                             ~
               at (13,03),                                               ~
        "!",                                                             ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "!",                                                             ~
               at (14,79),                                               ~
        "!",                                                             ~
               at (15,03),                                               ~
        "!",                                                             ~
               at (15,79),                                               ~
        "!",                                                             ~
               at (16,03),                                               ~
        "!",                                                             ~
               at (16,79),                                               ~
        "!",                                                             ~
               at (17,03),                                               ~
        "!",                                                             ~
               at (17,79),                                               ~
        "!",                                                             ~
               at (18,03),                                               ~
        "!",                                                             ~
               at (18,79),                                               ~
        "!",                                                             ~
               at (19,03),                                               ~
        "!",                                                             ~
               at (19,79),                                               ~
        "!",                                                             ~
               at (20,03),                                               ~
        "!",                                                             ~
               at (20,79),                                               ~
        "!",                                                             ~
               at (21,03),                                               ~
        "!",                                                             ~
               at (21,79),                                               ~
        "!",                                                             ~
               at (22,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        "CURSOR TO LINE & (ENTER)TO EDIT/DELETE INSURANCE      (1)FIRST  ~
        ~ (5)NEXT PAGE",                                                  ~
               at (24,03),                                               ~
        "(8)TO ADD ANOTHER PROGRAM      (15)PRINT SCREEN      (16)SAVE IN~
        ~SURANCE SHOWN",                                                  ~
           at(05,05), fac(hex(94)), errormsg$                   , ch(73),~
           at(06,06), fac(hex(8c)), insurance$   (screen% +  1%), ch(16),~
           at(06,26), fac(hex(8c)), eligdate$    (screen% +  1%), ch(08),~
           at(06,39), fac(hex(8c)), startdate$   (screen% +  1%), ch(08),~
           at(06,50), fac(hex(8c)), coverage$    (screen% +  1%), ch(10),~
           at(07,06), fac(hex(8c)), insurance$   (screen% +  2%), ch(16),~
           at(07,26), fac(hex(8c)), eligdate$    (screen% +  2%), ch(08),~
           at(07,39), fac(hex(8c)), startdate$   (screen% +  2%), ch(08),~
           at(07,50), fac(hex(8c)), coverage$    (screen% +  2%), ch(10),~
           at(08,06), fac(hex(8c)), insurance$   (screen% +  3%), ch(16),~
           at(08,26), fac(hex(8c)), eligdate$    (screen% +  3%), ch(08),~
           at(08,39), fac(hex(8c)), startdate$   (screen% +  3%), ch(08),~
           at(08,50), fac(hex(8c)), coverage$    (screen% +  3%), ch(10),~
           at(09,06), fac(hex(8c)), insurance$   (screen% +  4%), ch(16),~
           at(09,26), fac(hex(8c)), eligdate$    (screen% +  4%), ch(08),~
           at(09,39), fac(hex(8c)), startdate$   (screen% +  4%), ch(08),~
           at(09,50), fac(hex(8c)), coverage$    (screen% +  4%), ch(10),~
           at(10,06), fac(hex(8c)), insurance$   (screen% +  5%), ch(16),~
           at(10,26), fac(hex(8c)), eligdate$    (screen% +  5%), ch(08),~
           at(10,39), fac(hex(8c)), startdate$   (screen% +  5%), ch(08),~
           at(10,50), fac(hex(8c)), coverage$    (screen% +  5%), ch(10),~
           at(11,06), fac(hex(8c)), insurance$   (screen% +  6%), ch(16),~
           at(11,26), fac(hex(8c)), eligdate$    (screen% +  6%), ch(08),~
           at(11,39), fac(hex(8c)), startdate$   (screen% +  6%), ch(08),~
           at(11,50), fac(hex(8c)), coverage$    (screen% +  6%), ch(10),~
           at(12,06), fac(hex(8c)), insurance$   (screen% +  7%), ch(16),~
           at(12,26), fac(hex(8c)), eligdate$    (screen% +  7%), ch(08),~
           at(12,39), fac(hex(8c)), startdate$   (screen% +  7%), ch(08),~
           at(12,50), fac(hex(8c)), coverage$    (screen% +  7%), ch(10),~
           at(13,06), fac(hex(8c)), insurance$   (screen% +  8%), ch(16),~
           at(13,26), fac(hex(8c)), eligdate$    (screen% +  8%), ch(08),~
           at(13,39), fac(hex(8c)), startdate$   (screen% +  8%), ch(08),~
           at(13,50), fac(hex(8c)), coverage$    (screen% +  8%), ch(10),~
           at(14,06), fac(hex(8c)), insurance$   (screen% +  9%), ch(16),~
           at(14,26), fac(hex(8c)), eligdate$    (screen% +  9%), ch(08),~
           at(14,39), fac(hex(8c)), startdate$   (screen% +  9%), ch(08),~
           at(14,50), fac(hex(8c)), coverage$    (screen% +  9%), ch(10),~
           at(15,06), fac(hex(8c)), insurance$   (screen% + 10%), ch(16),~
           at(15,26), fac(hex(8c)), eligdate$    (screen% + 10%), ch(08),~
           at(15,39), fac(hex(8c)), startdate$   (screen% + 10%), ch(08),~
           at(15,50), fac(hex(8c)), coverage$    (screen% + 10%), ch(10),~
           at(16,06), fac(hex(8c)), insurance$   (screen% + 11%), ch(16),~
           at(16,26), fac(hex(8c)), eligdate$    (screen% + 11%), ch(08),~
           at(16,39), fac(hex(8c)), startdate$   (screen% + 11%), ch(08),~
           at(16,50), fac(hex(8c)), coverage$    (screen% + 11%), ch(10),~
           at(17,06), fac(hex(8c)), insurance$   (screen% + 12%), ch(16),~
           at(17,26), fac(hex(8c)), eligdate$    (screen% + 12%), ch(08),~
           at(17,39), fac(hex(8c)), startdate$   (screen% + 12%), ch(08),~
           at(17,50), fac(hex(8c)), coverage$    (screen% + 12%), ch(10),~
           at(18,06), fac(hex(8c)), insurance$   (screen% + 13%), ch(16),~
           at(18,26), fac(hex(8c)), eligdate$    (screen% + 13%), ch(08),~
           at(18,39), fac(hex(8c)), startdate$   (screen% + 13%), ch(08),~
           at(18,50), fac(hex(8c)), coverage$    (screen% + 13%), ch(10),~
           at(19,06), fac(hex(8c)), insurance$   (screen% + 14%), ch(16),~
           at(19,26), fac(hex(8c)), eligdate$    (screen% + 14%), ch(08),~
           at(19,39), fac(hex(8c)), startdate$   (screen% + 14%), ch(08),~
           at(19,50), fac(hex(8c)), coverage$    (screen% + 14%), ch(10),~
           at(20,06), fac(hex(8c)), insurance$   (screen% + 15%), ch(16),~
           at(20,26), fac(hex(8c)), eligdate$    (screen% + 15%), ch(08),~
           at(20,39), fac(hex(8c)), startdate$   (screen% + 15%), ch(08),~
           at(20,50), fac(hex(8c)), coverage$    (screen% + 15%), ch(10),~
           at(21,06), fac(hex(8c)), insurance$   (screen% + 16%), ch(16),~
           at(21,26), fac(hex(8c)), eligdate$    (screen% + 16%), ch(08),~
           at(21,39), fac(hex(8c)), startdate$   (screen% + 16%), ch(08),~
           at(21,50), fac(hex(8c)), coverage$    (screen% + 16%), ch(10),~
           keys(hex(000105080f10)), key(hitkey%)

           init(" ") errormsg$

           if hitkey% <> 15% then goto L52865
                     call "PRNTSCRN"
                     goto L51055

L52865:    if hitkey% <> 16% then goto L52895
                     goto datasave

L52895:    if hitkey% <> 1% then goto L52935
                     screen% = 0%
                     goto L51055

L52935:    if hitkey% <> 5% then goto L52985
                     screen% = screen% + 1%
                     screen% = min(screen%, max(1%, maxi% - 16%))
                     goto L51055

L52985:    if hitkey% <> 8% then goto L53065
                     if maxi% < 20% then goto L53035
                     errormsg$ = "YOU HAVE ALREADY ENTERED THE MAXIMUM OF~
        ~ 20 POLICIES FOR THIS EMPLOYEE"
                     goto L51055
L53035:              maxi%, a% = maxi% + 1%
                     goto inputmode

L53065:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

           a% = cursor%(1%) - 5%
           if a% < 1% or a% > 16% then goto L51055
           if a% > maxi% then goto L51055
           init(" ") pass$
           str(pass$,47,16) = str(insurance$(a%) ,1,16)
           call "COMSUB" (#4, 9%, pass$, 0%, rc%)
           goto editmode

        really_delete

        accept                                                           ~
           at(05,10), ">>>>>>>>>>>>> DO YOU REALLY WANT TO DELETE? <<<<<<~
        ~<<<<<<<<<<",                                                     ~
           at(07,10), "(12) DELETE IMMEDIATELY                   (1) DO N~
        ~OT DELETE",                                                      ~
           keys(hex(010c)), key(delkey%)
           return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
