        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  FFFFF  RRRR    GGG   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  F      R   R  G        I    NN  N  P   P  U   U    T     *~
            *  FFFF   RRRR   G GGG    I    N N N  PPPP   U   U    T     *~
            *  F      R   R  G   G    I    N  NN  P      U   U    T     *~
            *  F      R   R   GGG   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FRGINPUT - NORMAL MANAGEMENT PROGRAM TO MANAGE FRINGE     *~
            *            BENEFITS FOR THE PERSONNEL SYSTEM.  NOTE THAT  *~
            *            INPUT/EDIT MODES ARE ACCESSED FROM A LINE      *~
            *            SUMMARY SCREEN.                                *~
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
            * 09/15/97 ! Changed SHOWMSG to SHOSTAT (1 call)      ! MLJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        dim                                                              ~
            benefit$(99)16,              /* BENEFIT                    */~
            benefitdescr$(99)32,         /* BENEFIT                    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deduction$(99)10,            /* EMPLOYEE DEDUCTION $/PER   */~
            deletekey$50,                /* GENERAL PURPOSE FILE KEY   */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eligdate$(99)8,              /* ELIGABILITY DATE           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$(99)157,              /* REST OF RECORD             */~
            fname$10, lname$15, mname$1, /* NAMES                      */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            maxdeduct$(99)10,            /* MAXIMUM EMPLOYEE DEDUCT/YR */~
            pass$162,                    /* FOR COMSUB                 */~
            per$(99)30,                  /* PERIODICITY OF DEDUCTION   */~
            name$30,                     /* NAMES                      */~
            nul$1,                       /* .                          */~
            startdate$(99)8              /* START DATE                 */

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

        call "SHOSTAT" ("LINKING TO DATA BASE FOR FRINGE BENEFIT MGMT")

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


        REM *************************************************************~
            *       S T A R T I N G   P O I N T                         *~
            *                                                           *~
            *************************************************************

        init(" ")                                                        ~
            benefit$(),                  /* BENEFIT                    */~
            benefitdescr$(),             /* BENEFIT                    */~
            deduction$(),                /* EMPLOYEE DEDUCTION $/PER   */~
            deletekey$  ,                /* GENERAL PURPOSE FILE KEY   */~
            edtmessage$  ,               /* EDIT SCREEN MESSAGE        */~
            eligdate$(),                 /* ELIGABILITY DATE           */~
            filler$(),                   /* REST OF RECORD             */~
            fname$  , lname$  , mname$ , /* NAMES                      */~
            errormsg$  ,                 /* ERROR MESSAGE              */~
            inpmessage$  ,               /* INPUT MESSAGE              */~
            maxdeduct$(),                /* MAXIMUM EMPLOYEE DEDUCT/YR */~
            per$(),                      /* PERIODICITY OF DEDUCTION   */~
            pass$,                       /* FOR COMSUB                 */~
            nul$ ,                       /* .                          */~
            startdate$()                 /* START DATE                 */

           REM REMOVE THIS SECTION WHEN USED AS A SUBROUTINE
                     init(" ") employee$, name$
L09465: accept                                                           ~
               at (01,02),                                               ~
        "                            FRINGE BENEFIT MANAGEMENT           ~
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
        "!  THIS PROGRAM ALLOWS YOU TO MANAGE JUST THE FRINGE BENEFITS FO~
        ~R YOUR      !",                                                  ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!  EMPLOYEES.  PLEASE ENTER THE CODE FOR THE EMPLOYEE DESIRED   ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,25), fac(hex(81)), employee$, ch(12),              ~
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
           if hh% <> 15% then goto L09950
                     call "PRNTSCRN"
                     goto L09465
L09950:    if hh% <> 0% then goto L09465
                     call "READ100" (#8, employee$, f1%(8))
                     if f1%(8) = 1% then goto L10000
                     errormsg$ = "NO SUCH EMPLOYEE ON FILE"
                     goto L09465
L10000:              get #8, using L10001, lname$, fname$, mname$
L10001:              FMT XX(1), CH(15), CH(10), CH(1)
                     name$ = str(fname$,1,len(fname$))
                     str(name$,len(fname$) + 2, 1) = mname$
                     str(name$,len(fname$) + 4) = lname$
                     init(" ") errormsg$
           REM REMOVE THE SECTION ABOVE WHEN USED AS A SUBROUTINE


           gosub L31000

           goto summaryscreen

        inputmode
            init(" ") pass$
            for fieldnr% = 1 to  8
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10171
L10120:         gosub'101(fieldnr%)
                      if keyhit% = 4 then goto L10172
                      if keyhit%  = 16 and fieldnr% = 1 then summaryscreen
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10171:         if keyhit% <> 4% then goto L10180
L10172:               fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10100
L10180:         next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode

L11060:     gosub'111(0%)
                  if keyhit%  = 16 then  summaryscreen
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  8 then L11060

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
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* BENEFIT          */~
                                    L20150,         /* ELIGABILITY DATE */~
                                    L20200,         /* START DATE       */~
                                    L20250,         /* DEDUCTION        */~
                                    L20300,         /* MAX DEDUCTIO     */~
                                    L20350,         /* FREE TEXT        */~
                                    L20400,         /* FREE TEXT        */~
                                    L20450,         /* FREE TEXT        */~
                                    L20500,         /* .                */~
                                    L20550,         /* .                */~
                                    L20600,         /* .                */~
                                    L20650,         /* .                */~
                                    L20700,         /* .                */~
                                    L20750,         /* .                */~
                                    L20800          /* .                */
                     return
L20100:     REM DEFAULT/ENABLE FOR BENEFIT
           inpmessage$ = "ENTER THE BENEFIT, IF YOU DON'T KNOW IT'S NAME ~
        ~LEAVE IT BLANK"
           call "RJUSTIFY" (inpmessage$)
                return
L20150:     REM DEFAULT/ENABLE FOR ELIGABILITY DATE
           inpmessage$ = "ENTER THE DATE ON WHICH THE EMPLOYEE BECOMES EL~
        ~IGIBLE FOR THIS BENEFIT"
           call "RJUSTIFY" (inpmessage$)
                return
L20200:     REM DEFAULT/ENABLE FOR START DATE
           inpmessage$ = "ENTER THE DATE WHEN THIS BENEFIT STARTED"
           call "RJUSTIFY" (inpmessage$)
                return
L20250:     REM DEFAULT/ENABLE FOR EMPLOYEE DEDUCTION $/PER
           inpmessage$ = "ENTER BOTH THE DOLLAR AMOUNT OF THE DEDUCTION A~
        ~ND IT'S PERIODICITY"
           call "RJUSTIFY" (inpmessage$)
                return
L20300:     REM DEFAULT/ENABLE FOR MAXIMUM EMPLOYEE DEDUCT/YR
           inpmessage$ = "ENTER THE MAXIMUM TO BE DEDUCTED EACH YEAR"
           call "RJUSTIFY" (inpmessage$)
                return
L20350:     REM DEFAULT/ENABLE FOR FREE TEXT
                return
L20400:     REM DEFAULT/ENABLE FOR FREE TEXT
                return
L20450:     REM DEFAULT/ENABLE FOR FREE TEXT
                return
L20500:     REM DEFAULT/ENABLE FOR .
                return
L20550:     REM DEFAULT/ENABLE FOR .
                return
L20600:     REM DEFAULT/ENABLE FOR .
                return
L20650:     REM DEFAULT/ENABLE FOR .
                return
L20700:     REM DEFAULT/ENABLE FOR .
                return
L20750:     REM DEFAULT/ENABLE FOR .
                return
L20800:     REM DEFAULT/ENABLE FOR .
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


L30000: REM WRITE DATA TO PERFRNGE FILE

           deletekey$ = str(employee$,1,12) & " "
           call "DELETE" (#9, deletekey$, 12%)

           for i% = 1% to maxi%
           if benefit$(i%) = " " then goto L30190
                     convert i% to seqnr$, pic(###)
                     call "DATUNFMT" (eligdate$(i%))
                     call "DATUNFMT" (startdate$(i%))

        put #9, using L35030,     /* FILE: PERFRNGE                     */~
            benefit$(i%),        /* Type of fringe benefit             */~
            employee$,           /* employee code                      */~
            seqnr$,              /* General purpose sequence number    */~
            eligdate$(i%),       /* Benefit eligibility date           */~
            startdate$(i%),      /* Benefit start date                 */~
            deduction$(i%),      /* Benefit, amount paid by employee   */~
            maxdeduct$(i%),      /* Benefit, max paid by employee      */~
            per$(i%),            /* Benefit, frequency of employee pay */~
            filler$(i%)          /* filler for rest of record or inter */~

           write #9
L30190:    next i%

           return

L31000: REM GET DATA FROM PERFRNGE FILE

           i%, maxi% = 0%
           deletekey$ = str(employee$,1,12) & " "
L31030:    call "PLOWNEXT" (#9, deletekey$, 12%, f1%(9))
           if f1%(9) <> 1% then return
           i%, maxi% = i% + 1%

        get #9, using L35030,     /* FILE: PERFRNGE                     */~
            benefit$(i%),        /* Type of fringe benefit             */~
            employee$,           /* employee code                      */~
            seqnr$,              /* General purpose sequence number    */~
            eligdate$(i%),       /* Benefit eligibility date           */~
            startdate$(i%),      /* Benefit start date                 */~
            deduction$(i%),      /* Benefit, amount paid by employee   */~
            maxdeduct$(i%),      /* Benefit, max paid by employee      */~
            per$(i%),            /* Benefit, frequency of employee pay */~
            filler$(i%)          /* filler for rest of record or inter */~

                     call "DATEFMT" (eligdate$(i%))
                     call "DATEFMT" (startdate$(i%))

           if i% = 99% then return  /* ARRAYS ARE FULL */
           goto L31030

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: PERFRNGE                     */~
            CH(16),              /* Type of fringe benefit             */~
            CH(12),              /* employee code                      */~
            CH(3),               /* General purpose sequence number    */~
            CH(6),               /* Benefit eligibility date           */~
            CH(6),               /* Benefit start date                 */~
            CH(10),              /* Benefit, amount paid by employee   */~
            CH(10),              /* Benefit, max paid by employee      */~
            CH(30),              /* Benefit, frequency of employee pay */~
            CH(157)              /* filler for rest of record or inter */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140,         /* BENEFIT          */~
                                    L40140,         /* ELIGABILITY DATE */~
                                    L40140,         /* START DATE       */~
                                    L40140,         /* DEDUCTION        */~
                                    L40155,         /* MAX DEDUCTIO     */~
                                    L40140,         /* FREE TEXT        */~
                                    L40140,         /* FREE TEXT        */~
                                    L40140,         /* FREE TEXT        */~
                                    L40140,         /* .                */~
                                    L40140,         /* .                */~
                                    L40140,         /* .                */~
                                    L40140,         /* .                */~
                                    L40140,         /* .                */~
                                    L40140,         /* .                */~
                                    L40140          /* .                */
                     goto L40175

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
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
                  "ENTER FRINGE BENEFITS DATA FOR ",                     ~
               at (01,40), fac(hex(84)), name$                  , ch(30),~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "BENEFIT",                                             ~
               at (06,30), fac(lfac$( 1)), benefit$(a%   )      , ch(16),~
               at (06,49), fac(hex(8c)),str(pass$,17,30)        , ch(30),~
               at (07,02),                                               ~
                  "ELIGABILITY DATE",                                    ~
               at (07,30), fac(lfac$( 2)), eligdate$(a%   )     , ch(08),~
               at (08,02),                                               ~
                  "START DATE",                                          ~
               at (08,30), fac(lfac$( 3)), startdate$(a%   )    , ch(08),~
               at (09,02),                                               ~
                  "EMPLOYEE DEDUCTION $/PER",                            ~
               at (09,30), fac(lfac$( 4)), deduction$(a%   )    , ch(10),~
               at (09,43), "/",                                          ~
               at (09,45), fac(lfac$( 4)),       per$(a%   )    , ch(30),~
               at (10,02),                                               ~
                  "MAXIMUM EMPLOYEE DEDUCT/YR",                          ~
               at (10,30), fac(lfac$( 5)), maxdeduct$(a%   )    , ch(10),~
               at (11,02),                                               ~
                  "FREE TEXT",                                           ~
               at (11,30), fac(lfac$( 6)), str(filler$(a%),1,50), ch(50),~
               at (12,02),                                               ~
                  "FREE TEXT",                                           ~
               at (12,30), fac(lfac$( 7)),str(filler$(a%),51,50), ch(50),~
               at (13,02),                                               ~
                  "FREE TEXT",                                           ~
               at (13,30), fac(lfac$( 8)),str(filler$(a%),101,50),ch(50),~
               at (14,02),                                               ~
                  " ",                                                   ~
               at (14,30), fac(lfac$( 9)), nul$                 , ch(01),~
               at (15,02),                                               ~
                  " ",                                                   ~
               at (15,25), fac(hex(8c)), str(pass$,63,50)       , ch(50),~
               at (16,02),                                               ~
                  " ",                                                   ~
               at (16,25), fac(hex(8c)), str(pass$,113,50)      , ch(50),~
               at (17,02),                                               ~
                  " ",                                                   ~
               at (17,30), fac(lfac$(12)), nul$                 , ch(01),~
               at (18,02),                                               ~
                  " ",                                                   ~
               at (18,30), fac(lfac$(13)), nul$                 , ch(01),~
               at (19,02),                                               ~
                  " ",                                                   ~
               at (19,30), fac(lfac$(14)), nul$                 , ch(01),~
               at (20,02),                                               ~
                  " ",                                                   ~
               at (20,30), fac(lfac$(15)), nul$                 , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)TO SUMMARY  ",                                    ~
                                                                         ~
               keys(hex(000d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40550
                  call "MANUAL" ("FRNGESUB")
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
                  on fieldnr% gosub L41140,         /* BENEFIT          */~
                                    L41140,         /* ELIGABILITY DATE */~
                                    L41140,         /* START DATE       */~
                                    L41140,         /* DEDUCTION        */~
                                    L41155,         /* MAX DEDUCTIO     */~
                                    L41140,         /* FREE TEXT        */~
                                    L41140,         /* FREE TEXT        */~
                                    L41140,         /* FREE TEXT        */~
                                    L41140,         /* .                */~
                                    L41140,         /* .                */~
                                    L41140,         /* .                */~
                                    L41140,         /* .                */~
                                    L41140,         /* .                */~
                                    L41140,         /* .                */~
                                    L41140          /* .                */
                     goto L41175

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
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
                  "EDIT/DELETE FRINGE BENEFITS DATA FOR ",               ~
               at (01,40), fac(hex(84)), name$                  , ch(30),~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "BENEFIT",                                             ~
               at (06,30), fac(lfac$( 1)), benefit$(   a%)      , ch(16),~
               at (06,49), fac(hex(8c)), str(pass$,17,30)       , ch(30),~
               at (07,02),                                               ~
                  "ELIGABILITY DATE",                                    ~
               at (07,30), fac(lfac$( 2)), eligdate$(   a%)     , ch(08),~
               at (08,02),                                               ~
                  "START DATE",                                          ~
               at (08,30), fac(lfac$( 3)), startdate$(   a%)    , ch(08),~
               at (09,02),                                               ~
                  "EMPLOYEE DEDUCTION $/PER",                            ~
               at (09,30), fac(lfac$( 4)), deduction$(   a%)    , ch(10),~
               at (09,43), "/",                                          ~
               at (09,45), fac(lfac$( 4)),       per$(   a%)    , ch(30),~
               at (10,02),                                               ~
                  "MAXIMUM EMPLOYEE DEDUCT/YR",                          ~
               at (10,30), fac(lfac$( 5)), maxdeduct$(   a%)    , ch(10),~
               at (11,02),                                               ~
                  "FREE TEXT",                                           ~
               at (11,30), fac(lfac$( 6)), str(filler$(a%),1,50), ch(50),~
               at (12,02),                                               ~
                  "FREE TEXT",                                           ~
               at (12,30), fac(lfac$( 7)),str(filler$(a%),51,50), ch(50),~
               at (13,02),                                               ~
                  "FREE TEXT",                                           ~
               at (13,30), fac(lfac$( 8)),str(filler$(a%),101,50),ch(50),~
               at (14,02),                                               ~
                  " ",                                                   ~
               at (14,30), fac(lfac$( 9)), nul$                 , ch(01),~
               at (15,02),                                               ~
                  " ",                                                   ~
               at (15,25), fac(hex(8c)), str(pass$,63,50)       , ch(50),~
               at (16,02),                                               ~
                  " ",                                                   ~
               at (16,25), fac(hex(8c)), str(pass$,113,50)      , ch(50),~
               at (17,02),                                               ~
                  " ",                                                   ~
               at (17,30), fac(lfac$(12)), nul$                 , ch(01),~
               at (18,02),                                               ~
                  " ",                                                   ~
               at (18,30), fac(lfac$(13)), nul$                 , ch(01),~
               at (19,02),                                               ~
                  " ",                                                   ~
               at (19,30), fac(lfac$(14)), nul$                 , ch(01),~
               at (20,02),                                               ~
                  " ",                                                   ~
               at (20,30), fac(lfac$(15)), nul$                 , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(13)INSTRUCTIONS               (12)DELETE THIS BENEFIT~
        ~",                                                               ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(000c0d0f10)),                                    ~
               key (keyhit%)

               if keyhit% = 12% then goto  L41595

               if keyhit% <> 13 then L41550
                  call "MANUAL" ("FRNGESUB")
                  goto L41175

L41550:        if keyhit% <> 15 then L41570
                  call "PRNTSCRN"
                  goto L41175

L41570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L41595:     gosub really_delete : if delkey% <> 12% then L41175
        init(" ")                                                        ~
            benefit$(a%),                /* BENEFIT                    */~
            benefitdescr$(a%),           /* BENEFIT                    */~
            deduction$(a%),              /* EMPLOYEE DEDUCTION $/PER   */~
            edtmessage$  ,               /* EDIT SCREEN MESSAGE        */~
            eligdate$(a%),               /* ELIGABILITY DATE           */~
            filler$(a%),                 /* REST OF RECORD             */~
            errormsg$  ,                 /* ERROR MESSAGE              */~
            inpmessage$  ,               /* INPUT MESSAGE              */~
            maxdeduct$(a%),              /* MAXIMUM EMPLOYEE DEDUCT/YR */~
            per$(a%),                    /* PERIODICITY OF DEDUCTION   */~
            nul$ ,                       /* .                          */~
            startdate$(a%)               /* START DATE                 */

           goto summaryscreen

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* BENEFIT          */~
                                    L50150,         /* ELIGABILITY DATE */~
                                    L50200,         /* START DATE       */~
                                    L50250,         /* DEDUCTION        */~
                                    L50300,         /* MAX DEDUCTIO     */~
                                    L50350,         /* FREE TEXT        */~
                                    L50400,         /* FREE TEXT        */~
                                    L50450,         /* FREE TEXT        */~
                                    L50500,         /* .                */~
                                    L50550,         /* .                */~
                                    L50600,         /* .                */~
                                    L50650,         /* .                */~
                                    L50700,         /* .                */~
                                    L50750,         /* .                */~
                                    L50800          /* .                */
                     return
L50100:     REM TEST DATA FOR BENEFIT
           init(" ") pass$
           str(pass$,47,16) = str(benefit$(a%) ,1,16)
L50124:    call "COMSUB" (#4, 1%, pass$, 1%, rc%)
           if rc% =  0% then goto L50124
           if str(pass$,1,16) =                                          ~
                        "BENEFIT         " then goto L50140
          errormsg$ = "THE TERM " & str(benefit$(a%),1,len(benefit$(a%)))~
             & " IS NOT A BENEFIT, PLEASE REENTER"
           init(" ") benefit$(a%)
           call "RJUSTIFY" (errormsg$)
                     return
L50140:    str(benefit$(a%) ,1,16) = str(pass$,47,16)
                return
L50150:     REM TEST DATA FOR ELIGABILITY DATE
            if eligdate$(a%) = " " or eligdate$(a%) = blankdate$ then return
            call "DATEOK" (eligdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50200:     REM TEST DATA FOR START DATE
            if startdate$(a%) = " " or startdate$(a%) = blankdate$ then return
            call "DATEOK" (startdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50250:     REM TEST DATA FOR EMPLOYEE DEDUCTION $/PER
             if deduction$(a%) = " " then deduction$(a%) = "      NONE"
             if deduction$(a%) = "0" then deduction$(a%) = "      NONE"
             if deduction$(a%) = "      NONE" then return
                     convert deduction$(a%) to d, data goto L50265
                     convert d to deduction$(a%), pic($######.##)
                     return
L50265: errormsg$ = "PLEASE ENTER DEDUCTION AMOUNT IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50300:     REM TEST DATA FOR MAXIMUM EMPLOYEE DEDUCT/YR
              if maxdeduct$(a%) = " " then maxdeduct$(a%) = "      NONE"
              if maxdeduct$(a%) = "0" then maxdeduct$(a%) = "      NONE"
              if maxdeduct$(a%) = "      NONE" then return
                     convert maxdeduct$(a%) to d, data goto L50320
                     convert d to maxdeduct$(a%), pic($######.##)
                     return
L50320: errormsg$ = "PLEASE ENTER MAXIMUM DEDUCTION IN DOLLARS AND CENTS"
           call "RJUSTIFY" (errormsg$)
                return
L50350:     REM TEST DATA FOR .
                return
L50400:     REM TEST DATA FOR .
                return
L50450:     REM TEST DATA FOR .
                return
L50500:     REM TEST DATA FOR .
                return
L50550:     REM TEST DATA FOR .
                return
L50600:     REM TEST DATA FOR .
                return
L50650:     REM TEST DATA FOR .
                return
L50700:     REM TEST DATA FOR .
                return
L50750:     REM TEST DATA FOR .
                return
L50800:     REM TEST DATA FOR .
                return

        summaryscreen

           init(" ") errormsg$


L51045: accept                                                           ~
               at (01,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (02,03),                                               ~
        "! FRINGE BENEFITS NOW ON FILE FOR",                             ~
               at (02,38),                                               ~
         fac(hex(84)), name$, ch(30) ,                                   ~
               at (02,79),                                               ~
        "!",                                                             ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!  BENEFIT            ELIG DATE   START DATE    DEDUCTION     MA~
        ~X-DEDUCTION !",                                                  ~
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
        "CURSOR TO LINE & (ENTER)TO EDIT/DELETE BENEFITS        (1)FIRST ~
        ~ (5)NEXT PAGE",                                                  ~
               at (24,03),                                               ~
        "(8)TO ADD ANOTHER BENEFIT      (15)PRINT SCREEN       (16)SAVE B~
        ~ENEFITS SHOWN",                                                  ~
           at(05,05), fac(hex(94)), errormsg$                   , ch(73),~
           at(06,06), fac(hex(8c)), benefit$     (screen% +  1%), ch(16),~
           at(06,26), fac(hex(8c)), eligdate$    (screen% +  1%), ch(08),~
           at(06,39), fac(hex(8c)), startdate$   (screen% +  1%), ch(08),~
           at(06,50), fac(hex(8c)), deduction$   (screen% +  1%), ch(10),~
           at(06,66), fac(hex(8c)), maxdeduct$   (screen% +  1%), ch(10),~
           at(07,06), fac(hex(8c)), benefit$     (screen% +  2%), ch(16),~
           at(07,26), fac(hex(8c)), eligdate$    (screen% +  2%), ch(08),~
           at(07,39), fac(hex(8c)), startdate$   (screen% +  2%), ch(08),~
           at(07,50), fac(hex(8c)), deduction$   (screen% +  2%), ch(10),~
           at(07,66), fac(hex(8c)), maxdeduct$   (screen% +  2%), ch(10),~
           at(08,06), fac(hex(8c)), benefit$     (screen% +  3%), ch(16),~
           at(08,26), fac(hex(8c)), eligdate$    (screen% +  3%), ch(08),~
           at(08,39), fac(hex(8c)), startdate$   (screen% +  3%), ch(08),~
           at(08,50), fac(hex(8c)), deduction$   (screen% +  3%), ch(10),~
           at(08,66), fac(hex(8c)), maxdeduct$   (screen% +  3%), ch(10),~
           at(09,06), fac(hex(8c)), benefit$     (screen% +  4%), ch(16),~
           at(09,26), fac(hex(8c)), eligdate$    (screen% +  4%), ch(08),~
           at(09,39), fac(hex(8c)), startdate$   (screen% +  4%), ch(08),~
           at(09,50), fac(hex(8c)), deduction$   (screen% +  4%), ch(10),~
           at(09,66), fac(hex(8c)), maxdeduct$   (screen% +  4%), ch(10),~
           at(10,06), fac(hex(8c)), benefit$     (screen% +  5%), ch(16),~
           at(10,26), fac(hex(8c)), eligdate$    (screen% +  5%), ch(08),~
           at(10,39), fac(hex(8c)), startdate$   (screen% +  5%), ch(08),~
           at(10,50), fac(hex(8c)), deduction$   (screen% +  5%), ch(10),~
           at(10,66), fac(hex(8c)), maxdeduct$   (screen% +  5%), ch(10),~
           at(11,06), fac(hex(8c)), benefit$     (screen% +  6%), ch(16),~
           at(11,26), fac(hex(8c)), eligdate$    (screen% +  6%), ch(08),~
           at(11,39), fac(hex(8c)), startdate$   (screen% +  6%), ch(08),~
           at(11,50), fac(hex(8c)), deduction$   (screen% +  6%), ch(10),~
           at(11,66), fac(hex(8c)), maxdeduct$   (screen% +  6%), ch(10),~
           at(12,06), fac(hex(8c)), benefit$     (screen% +  7%), ch(16),~
           at(12,26), fac(hex(8c)), eligdate$    (screen% +  7%), ch(08),~
           at(12,39), fac(hex(8c)), startdate$   (screen% +  7%), ch(08),~
           at(12,50), fac(hex(8c)), deduction$   (screen% +  7%), ch(10),~
           at(12,66), fac(hex(8c)), maxdeduct$   (screen% +  7%), ch(10),~
           at(13,06), fac(hex(8c)), benefit$     (screen% +  8%), ch(16),~
           at(13,26), fac(hex(8c)), eligdate$    (screen% +  8%), ch(08),~
           at(13,39), fac(hex(8c)), startdate$   (screen% +  8%), ch(08),~
           at(13,50), fac(hex(8c)), deduction$   (screen% +  8%), ch(10),~
           at(13,66), fac(hex(8c)), maxdeduct$   (screen% +  8%), ch(10),~
           at(14,06), fac(hex(8c)), benefit$     (screen% +  9%), ch(16),~
           at(14,26), fac(hex(8c)), eligdate$    (screen% +  9%), ch(08),~
           at(14,39), fac(hex(8c)), startdate$   (screen% +  9%), ch(08),~
           at(14,50), fac(hex(8c)), deduction$   (screen% +  9%), ch(10),~
           at(14,66), fac(hex(8c)), maxdeduct$   (screen% +  9%), ch(10),~
           at(15,06), fac(hex(8c)), benefit$     (screen% + 10%), ch(16),~
           at(15,26), fac(hex(8c)), eligdate$    (screen% + 10%), ch(08),~
           at(15,39), fac(hex(8c)), startdate$   (screen% + 10%), ch(08),~
           at(15,50), fac(hex(8c)), deduction$   (screen% + 10%), ch(10),~
           at(15,66), fac(hex(8c)), maxdeduct$   (screen% + 10%), ch(10),~
           at(16,06), fac(hex(8c)), benefit$     (screen% + 11%), ch(16),~
           at(16,26), fac(hex(8c)), eligdate$    (screen% + 11%), ch(08),~
           at(16,39), fac(hex(8c)), startdate$   (screen% + 11%), ch(08),~
           at(16,50), fac(hex(8c)), deduction$   (screen% + 11%), ch(10),~
           at(16,66), fac(hex(8c)), maxdeduct$   (screen% + 11%), ch(10),~
           at(17,06), fac(hex(8c)), benefit$     (screen% + 12%), ch(16),~
           at(17,26), fac(hex(8c)), eligdate$    (screen% + 12%), ch(08),~
           at(17,39), fac(hex(8c)), startdate$   (screen% + 12%), ch(08),~
           at(17,50), fac(hex(8c)), deduction$   (screen% + 12%), ch(10),~
           at(17,66), fac(hex(8c)), maxdeduct$   (screen% + 12%), ch(10),~
           at(18,06), fac(hex(8c)), benefit$     (screen% + 13%), ch(16),~
           at(18,26), fac(hex(8c)), eligdate$    (screen% + 13%), ch(08),~
           at(18,39), fac(hex(8c)), startdate$   (screen% + 13%), ch(08),~
           at(18,50), fac(hex(8c)), deduction$   (screen% + 13%), ch(10),~
           at(18,66), fac(hex(8c)), maxdeduct$   (screen% + 13%), ch(10),~
           at(19,06), fac(hex(8c)), benefit$     (screen% + 14%), ch(16),~
           at(19,26), fac(hex(8c)), eligdate$    (screen% + 14%), ch(08),~
           at(19,39), fac(hex(8c)), startdate$   (screen% + 14%), ch(08),~
           at(19,50), fac(hex(8c)), deduction$   (screen% + 14%), ch(10),~
           at(19,66), fac(hex(8c)), maxdeduct$   (screen% + 14%), ch(10),~
           at(20,06), fac(hex(8c)), benefit$     (screen% + 15%), ch(16),~
           at(20,26), fac(hex(8c)), eligdate$    (screen% + 15%), ch(08),~
           at(20,39), fac(hex(8c)), startdate$   (screen% + 15%), ch(08),~
           at(20,50), fac(hex(8c)), deduction$   (screen% + 15%), ch(10),~
           at(20,66), fac(hex(8c)), maxdeduct$   (screen% + 15%), ch(10),~
           at(21,06), fac(hex(8c)), benefit$     (screen% + 16%), ch(16),~
           at(21,26), fac(hex(8c)), eligdate$    (screen% + 16%), ch(08),~
           at(21,39), fac(hex(8c)), startdate$   (screen% + 16%), ch(08),~
           at(21,50), fac(hex(8c)), deduction$   (screen% + 16%), ch(10),~
           at(21,66), fac(hex(8c)), maxdeduct$   (screen% + 16%), ch(10),~
           keys(hex(000105080f10)), key(hitkey%)

           init(" ") errormsg$

           if hitkey% <> 15% then goto L52925
                     call "PRNTSCRN"
                     goto L51045

L52925:    if hitkey% <> 16% then goto L52955
                     goto datasave

L52955:    if hitkey% <> 1% then goto L52995
                     screen% = 0%
                     goto L51045

L52995:    if hitkey% <> 5% then goto L53045
                     screen% = screen% + 1%
                     screen% = min(screen%, max(1%, maxi% - 16%))
                     goto L51045

L53045:    if hitkey% <> 8% then goto L53085
                     if maxi% < 99% then goto L53055
                     errormsg$ = "YOU HAVE ALREADY ENTERED THE MAXIMUM OF~
        ~ 99 BENEFITS FOR THIS EMPLOYEE"
                     goto L51045
L53055:              maxi%, a% = maxi% + 1%
                     goto inputmode

L53085:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

           a% = cursor%(1%) - 5%
           if a% < 1% or a% > 16% then goto L51045
           if a% > maxi% then goto L51045
           init(" ") pass$
           str(pass$,47,16) = str(benefit$(a%) ,1,16)
           call "COMSUB" (#4, 9%, pass$, 0%, rc%)
           goto editmode

        really_delete

        accept                                                           ~
           at(05,10), ">>>>>>>>>>>>> DO YOU REALLY WANT TO DELETE? <<<<<<~
        ~<<<<<<<<<<",                                                     ~
           at(07,10), "(12) DELETE IMMEDIATLY                    (1) DO N~
        ~ OT DELETE",                                                     ~
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
