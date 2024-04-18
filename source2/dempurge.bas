        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  DDDD   EEEEE  M   M  PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  D   D  E      MM MM  P   P  U   U  R   R  G      E       *~
            *  D   D  EEEE   M M M  PPPP   U   U  RRRR   G GGG  EEEE    *~
            *  D   D  E      M   M  P      U   U  R   R  G   G  E       *~
            *  DDDD   EEEEE  M   M  P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMPURGE - PROGRAM TO CLEAN UP DEMAND MASTER FILE.  WE CAN*~
            *            GO THROUGH AND CLEAN UP ALL DEMAND TYPES OTHER *~
            *            THAN SALES ORDER ORIGINATED DEMANDS, BUT THEY  *~
            *            SHOULD BE HANDLED VIA THE SALES ORDER SYSTEM.  *~
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
            * 12/19/83 ! ORIGINAL                                 ! KEN *~
            * 10/25/86 ! WCOUT File Format Change                 ! HES *~
            * 03/08/94 ! Changed to support BOMSPHDR              ! WPH *~
            * 10/14/95 ! Minor change for year 2000 support.      ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            demstatus$1,                                                 ~
            demtype$1,                                                   ~
            demcode$16,                                                  ~
            demline$3,                                                   ~
            dempart$25,                                                  ~
            demqty$10,                                                   ~
            cmpdate$8,                                                   ~
            duedate$8,                                                   ~
            delekey$100,                                                 ~
            bomkey$100,                                                  ~
            readkey$100,                                                 ~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            plbasedate$8,                /* FROM SYSFILE2              */~
            pldate$8,                    /* PLANNED FOR COMP BEFORE    */~
            salesok$1,                   /* PURGE SALES ORDERS         */~
            stdemand$16,                 /* FROM DEMAND                */~
            todemand$16,                 /*  TO  DEMAND                */~
            undate$8                     /* UNPLANNED & DUE BEFORE     */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

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
            * # 1 ! DEMMASTR ! Demand Master File                       *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 5 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * # 6 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 7 ! WCMASTR  ! Work center master file                  *~
            * # 8 ! WCOUT    ! Planned work center use detail rec       *~
            * # 9 ! PIPIN    ! Planned inventory additions detail       *~
            * #10 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #11 ! SFMASTR2 ! Sales forecast master file               *~
            * #12 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #13 ! PIPCROSS ! hard peg cross reference                 *~
            * #14 ! BOMSPEC  ! options selected file                    *~
            * #15 ! JBPIPXRF ! OPTIONS PEGS                             *~
            * #16 ! BOMSPHDR ! Header file for options                  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select # 2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select # 5, "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   94,                                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select # 6, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 7, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select # 8, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27

            select # 9, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #10, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #11, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #12, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #13, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #14, "BOMSPEC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 26,   keylen = 54,                      ~
                        alt key  1, keypos =   57, keylen =  23          ~

            select #15, "JBPIPXRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 63,                                    ~
                        keypos = 1,  keylen = 63,                        ~
                        alt key  1, keypos =   45, keylen =  19          ~

            select #16, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            call "EXTRACT" addr("ID", userid$)
            readkey$=userid$

            call "READ100" (#2, "MONTHS OPEN", f1%(2))
            if f1%(2)=0 then L64990
            get #2, using L09180, plbasedate$
L09180:         FMT XX(32), CH(6)
            call "DATE" addr("G-", plbasedate$, date$, today%, err%)
            if err%<>0 then L64990
            today%=today%+1%
            if today%<1 or today%>490 then L64990

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "DEMPURGE: " & str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, pldate$, undate$, stdemand$, todemand$, ~
                      salesok$
            inpmessage$="This Program will clear obsolete demands as indi~
        ~cated above."
            for fieldnr% = 1 to  5
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10160
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
L10160:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  5 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
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
            if stdemand$<>"ALL" then L19100
                init (hex(00)) stdemand$
                init (hex(ff)) todemand$
L19100:         goto L19130
            if todemand$=" " then todemand$=stdemand$
               stdemand$=stdemand$ addc all(hex(ff))
L19130:     if stdemand$>todemand$ then L65000
            call "DATUNFMT" (pldate$)
            call "DATUNFMT" (undate$)
            gosub L30000
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* FROM DEMAND      */~
                                    L20200,         /*  TO  DEMAND      */~
                                    L20300,         /* UNPLANNED DATE   */~
                                    L20400,         /* PLANNED DATE     */~
                                    L20500          /* SALES ORDERS     */
                     return
L20100:     REM DEFAULT/ENABLE FOR FROM DEMAND
                stdemand$="ALL"
                return
L20200:     REM DEFAULT/ENABLE FOR  TO  DEMAND
                if stdemand$="ALL" then enabled%=0%
                return
L20300:     REM DEFAULT/ENABLE FOR UNPLANNED & DUE BEFORE
                inpmessage$="Unplanned demands due on or before this date~
        ~ will be PURGED"
                undate$=date$
                return
L20400:     REM DEFAULT/ENABLE FOR PLANNED FOR COMP BEFORE
                inpmessage$="Planned demands due for completion on or bef~
        ~ore this date will be PURGED"
                pldate$=date$
                return
L20500:     REM DEFAULT/ENABLE FOR SALES ORDERS
                inpmessage$="Sales Order Demands can be purged only if FI~
        ~LLED."
                salesok$="N"
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

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode


L30000: REM *************************************************************~
            *        DO THE WORK HERE, GET THE DEMANDS AND IF THEY      *~
            *        MEET THE CRITERIA, KILL 'EM.  (TAKING CARE NOT     *~
            *        TO LEAVE ANY XREF FILES HANGING ABOUT).            *~
            *************************************************************
            call "SHOSTAT" ("Purging demand master file")

            init (hex(00)) readkey$
            str(readkey$,1,16)=str(stdemand$,1,16)
L30080:     call "PLOWALTS" (#1,readkey$,1%,0%,f1%(1))
                if f1%(1)=0 then return
                if str(readkey$,1,16)>todemand$ then return
            get #1, using L35030, demstatus$, demtype$, duedate$,         ~
                    demcode$, demline$, dempart$, demqty$, cmpdate$

            if cmpdate$ = " " then call "DATUNFMT" (cmpdate$)
            if demtype$<"3" then L30600                /* SALES ORDER */
L30150:     if demstatus$>"1" then L30300              /*PLANNED*/
            if duedate$>undate$ then L30080            /*NOT YET */
L30165: REM TARGET BRANCH FOR CLEANUP AFTER UNPLANNING
            delekey$=str(demcode$,1,16)&str(demline$,1,3)&hex(00)
            call "REDALT1" (#1,delekey$,1%,f1%(1))  /* DEMMASTR */
            if f1%(1)<>0 then delete #1
            call "DELETE" (#13,delekey$,19%)        /* PIPCROSS */
            str(delekey$,20,4)=hex(00000000)

*       Only delete BOMSPEC if no BOMSPHDR record exists
            call "REDALT1"(#16, delekey$, 1%, f1%(16))  /* header */
                if f1%(16) = 0% then L30220
                goto L30080

L30220:     call "PLOWALTS"(#14,delekey$,1%,19%,f1%(14))
            if f1%(14)=0 then L30080
            get #14, using L30250, bomkey$
L30250:         FMT XX(25),CH(54)
            call "DELETE" (#14,bomkey$,54%)         /* BOMSPEC */
            goto L30220

L30300: REM TEST AND UNPLAN JUST TO FREE ANY ADVICE TYPE RESOURCES
            if cmpdate$>pldate$ then L30080               /* NOT YET */

            call "DATE" addr("G-", plbasedate$, duedate$, demdate%, err%)
                if err%<> 0%  then L30165
                demdate%=demdate%+1%
                demdate%=max(1,min(demdate%,490))

            call "DATE" addr("G-", plbasedate$, cmpdate$, cmpdate%, err%)
                if err%<> 0%  then L30165
                cmpdate%=cmpdate%+1%
                cmpdate%=max(1,min(cmpdate%,490))

                demqty=0
                convert demqty$ to demqty, data goto L30410

L30410:       call "UNPLAN"              /* UNPLAN IT                  */~
                    (demcode$,           /* DEMAND CODE                */~
                     demline$,           /* DEMAND LINE                */~
                     "3",                /* HARD 3, DONT MESS FORCASTS */~
                     cmpdate%,           /* DATE PLANNED FOR SO/SF ONLY*/~
                     demdate%,           /* DEMAND ORIGINAL DUE DATE   */~
                     dempart$,           /* PART TO UNPLAN             */~
                     demqty,             /* QUANTITY TO UNPLAN         */~
                     today%,                                             ~
                     "Y",                /* UNPLAN OPTION              */~
                     "N",                /* UNPLANNING REPORT          */~
                     #5,                 /* JBCROSS2                   */~
                     #6,                 /* PIPMASTR                   */~
                     #7,                 /* WCMASTR                    */~
                     #8,                 /* WCOUT                      */~
                     #9,                 /* PIPIN                      */~
                     #10,                /* PIPOUT                     */~
                     #11,                /* SFMASTR2                   */~
                     #12,                /* SFCUM2                     */~
                     #13,                /* PIPCROSS                   */~
                     #15)                /* JBPIPXRF                   */

                goto L30165

L30600: REM TEST S. O. TYPE DEMAND, CLOBBER IF APPROPRIATE, DON'T UNPLAN

            if salesok$ <> "Y" then L30080

            delekey$=str(demcode$,1,16)&str(demline$,1,3)&hex(00)
            call "PLOWNEXT" (#10, delekey$, 19%, f1%(10))
                if f1%(10) <> 0% then L30080

            goto L30150

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE  DEMMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(1),               /* Type - used generically for specia */~
            XX(1),               /* priority code                      */~
            CH(6),               /* Date delivery requested            */~
            CH(16),              /* Demand code (ie  sales order numbe */~
            CH(3),               /* Demand line (ie  SO line if SO dem */~
            CH(25),              /* Part code                          */~
            CH(10),              /* Quantity of something in packed de */~
            XX(4),               /* work-center code                   */~
            XX(3),               /* The specific BOM identifier for al */~
            XX(3),               /* The specific routing for multiple  */~
            XX(3),               /* Warehouse or Stores                */~
            XX(6),               /* Date last planned - for level 2 de */~
            CH(6),               /* Planned completion date for level  */~
            XX(9),               /* customer code                      */~
            XX(1),               /* approval status byte               */~
            XX(15),       /*5*CH(3) id of user granting approval       */~
            XX(10)        /*5*BI(2) date approval granted mmdd         */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40170,         /* FROM DEMAND      */~
                                    L40170,         /*  TO  DEMAND      */~
                                    L40170,         /* UNPLANNED DATE   */~
                                    L40170,         /* PLANNED DATE     */~
                                    L40170          /* SALES ORDERS     */
                     goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "PURGE OLD DEMANDS FROM FILE",                         ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FROM Demand",                                         ~
               at (06,30), fac(lfac$( 1)), stdemand$            , ch(16),~
               at (07,02),                                               ~
                  " TO  Demand",                                         ~
               at (07,30), fac(lfac$( 2)), todemand$            , ch(16),~
               at (08,02),                                               ~
                  "Unplanned & due before",                              ~
               at (08,30), fac(lfac$( 3)), undate$              , ch(08),~
               at (09,02),                                               ~
                  "Planned & complete before",                           ~
               at (09,30), fac(lfac$( 4)), pldate$              , ch(08),~
               at (10,02),                                               ~
                  "Purge sales order demands",                           ~
               at (10,30), fac(lfac$( 5)), salesok$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("DEMPURGE")
                  goto L40240

L40650:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41170,         /* FROM DEMAND      */~
                                    L41170,         /*  TO  DEMAND      */~
                                    L41170,         /* UNPLANNED DATE   */~
                                    L41170,         /* PLANNED DATE     */~
                                    L41170          /* sales orders     */
                     goto L41240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02),                                               ~
                  "PURGE OLD DEMANDS FROM FILE",                         ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FROM Demand",                                         ~
               at (06,30), fac(lfac$( 1)), stdemand$            , ch(16),~
               at (07,02),                                               ~
                  " TO  Demand",                                         ~
               at (07,30), fac(lfac$( 2)), todemand$            , ch(16),~
               at (08,02),                                               ~
                  "Unplanned & due before",                              ~
               at (08,30), fac(lfac$( 3)), undate$              , ch(08),~
               at (09,02),                                               ~
                  "Planned & complete before",                           ~
               at (09,30), fac(lfac$( 4)), pldate$              , ch(08),~
               at (10,02),                                               ~
                  "Purge sales order demands",                           ~
               at (10,30), fac(lfac$( 5)), salesok$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)START PURGE",                                     ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41650
                  call "MANUAL" ("DEMPURGE")
                  goto L41240

L41650:        if keyhit% <> 15 then L41690
                  call "PRNTSCRN"
                  goto L41240

L41690:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* FROM DEMAND      */~
                                    L50200,         /*  TO  DEMAND      */~
                                    L50300,         /* UNPLANNED DATE   */~
                                    L50400,         /* PLANNED DATE     */~
                                    L50500          /* sales orders     */
                     return
L50100:     REM TEST DATA FOR FROM DEMAND
                if stdemand$="ALL" then todemand$=" "
                if todemand$<>" " then L50245
                return

L50200:     REM TEST DATA FOR  TO  DEMAND
                if stdemand$="ALL" then todemand$=" "
                if stdemand$="ALL" then return
L50245:         if todemand$=" " then todemand$=stdemand$
                if todemand$>=stdemand$ then return
                     errormsg$="Invalid range, please respecify"
                     return

L50300:     REM TEST DATA FOR UNPLANNED & DUE BEFORE
                call "DATEOK" (undate$,err%,errormsg$)
                return

L50400:     REM TEST DATA FOR PLANNED FOR COMP BEFORE
                call "DATEOK" (pldate$,err%,errormsg$)
                return
L50500:     REM TEST FOR SALES ORDERS OK
                if salesok$="Y" then return
                if salesok$="N" then return
                   errormsg$="Enter 'Y' or 'N', Please"
                   return

L64990:     ask% = 2%
            call "ASKUSER" (ask%, "*** PLANNING CALENDAR ERROR *** ", "An~
        ~ Error has occured on the Production Planning Calendar", " ", "Pr~
        ~ess RETURN to Exit this program and correct the problem")

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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
