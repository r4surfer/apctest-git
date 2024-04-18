        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      PPPP   RRRR   IIIII  N   N  TTTTT          *~
            *  G      L      P   P  R   R    I    NN  N    T            *~
            *  G  GG  L      PPPP   RRRR     I    N N N    T            *~
            *  G   G  L      P      R  R     I    N  NN    T            *~
            *   GGG   LLLLL  P      R   R  IIIII  N   N    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPRINT  - Prints a Balance Sheet Type Report, using user *~
            *            selected data as defined in the report format  *~
            *            specification files. Optional comparisons can  *~
            *            be made to previous year or budgets.           *~
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
            * 09/23/87 ! ORIGINAL (Copy from FRPRINT1)            ! RAC *~
            * 12/19/89 ! Numerous changes to get it to work!!!    ! MLJ *~
            * 12/05/91 ! Chg'd for Prev/Next year budget exposure.! MLJ *~
            * 01/07/92 ! QC Mod - Fixed Column label problem.     ! MLJ *~
            * 04/07/92 ! PRR 11069 - 'GLP001' added to RPTPARAM to! MLJ *~
            *          !   prompt for printing parameters.        !     *~
            * 02/09/93 ! PRR 12360 - Report subtitles now always  ! MLJ *~
            *          !   print in leftmost text column.         !     *~
            *          ! PRR 12390 - Zone input fields now align  !     *~
            *          !   with zone header.                      !     *~
            *          ! Misc - Fixed numerous implied integers   !     *~
            * 02/23/93 ! Modified to allow reporting against a    ! MLJ *~
            *          !   against a range of periods.            !     *~
            * 02/03/94 ! PRR 13090 - Fixed underscore & double    ! MLJ *~
            *          !   underscore printing inconsistancies.   !     *~
            * 02/07/04 ! PRR 13075 - Added ASKUSER to determin    ! MLJ *~
            *          !   starting period used for YTD calcs when!     *~
            *          !   reporting period >= 14.  Only displayed!     *~
            *          !   if report contains CB, LCB, YA or LYA  !     *~
            *          !   instruction codes.                     !     *~
            * 03/16/95 ! PRR 13366 - Fixed DATA GOTO on convert   ! JBK *~
            *          !   statement to avoid an infinite loop.   !     *~
            * 06/24/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            acct1$(10)9,                 /* FROM ACCOUNT               */~
            acct2$(10)9,                 /* TO ACCOUNT                 */~
            base$(10)3,                  /* FROM ACCOUNT               */~
            colcom$(10)3,                /* Column Commas?             */~
            colfmt$(10)1,                /* Column Formats             */~
            collgth$(10)2,               /* Column Lengths             */~
            collgth%(10),                /* Column Lengths             */~
            colpos$(10)3,                /* Column Positions           */~
            colpos%(10),                 /* Column Positions           */~
            coltext$45,                  /* Column Text                */~
            comp$3,                      /* COMPANY (DBASE POINTER)    */~
            companyheader$50,            /* COMPANY HEADING            */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$45,                    /* REPORT DESCRIPTION         */~
            dolprt$(10)1,                /* Dollar Sign Print Indicator*/~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$12,                   /* FILLER AT END OF RECORD    */~
            fixed$(10)3,                 /* RUN TIME OVERRIDE FLAG     */~
            group$(10)6,                 /* ACCOUNT GROUPING CODE      */~
            header$79,                   /* HEADER FOR SCREEN          */~
            header1$79,                  /* HEADER FOR SCREEN          */~
            hfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            ieflag$(4)1,                 /* INCLUDE/EXCLUDE FLAGS      */~
            iemesg$(4)7,                 /* INCLUDE/EXCLUDE MEMO       */~
            inst$(10)4,                  /* INSTRUCTION CODE           */~
            inst_codes$24,               /* VALID INSTRUCTIONS         */~
            message$79,                  /* INPUT MESSAGE              */~
            month1$2,                    /* STARTING PERIOD RANGE      */~
            month2$2,                    /* ENDING PERIOD RANGE        */~
            months$64,                   /* MONTHS LIST AVAILABLE      */~
            mdate$32,                    /* DATES OF AVAILABLE MONTHS  */~
            mdescr$32,                   /* DATES OF AVAILABLE MONTHS  */~
            mfac$(3)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            paren$3,                     /* DEFAUT PARENTHESIS OPTION  */~
            pdescr$25,                   /* PERIOD RANGE DESCRIPTION   */~
            pfdescr$(2)79,               /* DESCRIPTION OF PFKEYS      */~
            plowkey$60,                  /* WORK VARIABLE              */~
            print_if$(10)1,              /* PRINT/TOTAL CONSTRAINT FLAG*/~
            print$(10)3,                 /* PRINT CODE                 */~
            readkey$60,                  /* WORK VARIABLE              */~
            record$132,                  /* DATA RETRIEVED FROM SUB    */~
            record1$132,                 /* DATA RETRIEVED FROM SUB    */~
            report$3,                    /* REPORT ID.                 */~
            reportheader$50,             /* FLOATING REPORT HEADER     */~
            reverse$(10)3,               /* REVERSE SIGN INDICATOR     */~
            round$3,                     /* ROUND OPTION (DEFUALT)     */~
            rptid$6,                     /* REPORT ID                  */~
            set$1,                       /* SET OF BOOKS TO USE        */~
            tdate$8,tdate1$8,            /* Temporary Date Variables   */~
            text$(10)45,                 /* TEXT FOR LINE ITEMS        */~
            text1$(10)16,                /* TEXT FOR COLUMN HEADERS    */~
            text2$(10)16,                /* TEXT FOR COLUMN HEADERS    */~
            tfac$1,                      /* FIELD ATTRIBUTE CHARACTERS */~
            title$50,                    /* DEFAULT REPORT TITLE       */~
            total$(10)20,                /* TOTALING SPEC FOR LINE     */~
            totals(20),                  /* REPORT TOTALS ACCUMULATOR  */~
            zero$3,                      /* PRINT IF ZERO? (DEFUALT)   */~
            z$(4)35,                     /* ZONE DESCR FOR PRINT       */~
            zone%(4,2),                  /* START, LENGTH IN ACCOUNT   */~
            zone$(4,2)2,                 /* START, LENGTH IN ACCOUNT   */~
            zonename$(4)20,              /* LABLE FOR ZONE IN ACCOUNT  */~
            zonespec1$(16,1)1,           /* USERS SELECTION FOR ZONE 1 */~
            zonespec2$(16,1)1,           /* USERS SELECTION FOR ZONE 2 */~
            zonespec3$(16,1)1,           /* USERS SELECTION FOR ZONE 3 */~
            zonespec4$(16,1)1            /* USERS SELECTION FOR ZONE 4 */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! GLNAMES  ! Financial Statement Header File          *~
            * # 2 ! GLFORMAT ! Financial Statement Format File          *~
            * # 3 ! SYSFILE2 ! SYSTEM INFORMATION (WHICH MONTHS OPEN)   *~
            * # 9 ! WORKFILE ! WORKFILE FOR GLACCUM RETURNED DATA       *~
            *************************************************************

            select #1,  "GLNAMES",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 3

            select #2,  "GLFORMAT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1400,                                  ~
                        keypos = 1, keylen = 9

            select  #3, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #9, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 136,                                   ~
                        keypos = 1, keylen = 4

            call "SHOSTAT" ("Opening data files, one moment please.")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "WORKOPEN" (#9, "SHARE", 100%, 0%)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

        REM Valid INSTRUCTION Codes...
            inst_codes$ = "H   P   D   DC  U   DU  "
            rptid$ = "GLP001"

        REM Format Dates For PERIOD Display Generation...
            months$ = "Jan.Feb.Mar.Apr.May Jun.Jul.Aug.Sep.Oct.Nov.Dec."&~
                      "Dec.Jan.Feb.Mar."

                call "READ100" (#3, "FISCAL DATES", f1%(3%))
                     if f1%(3%) <> 0% then L09200
                        u3% = 0%
                        call "ASKUSER" (u3%, "*** FISCAL DATE ERROR ***",~
                        "Fiscal dates not found in file SYSFILE2", " ",  ~
                        "Press (RETURN) to cancel program")
                       goto L65000
L09200:      get #3, using L09210, periods%
L09210:      FMT XX(20), BI(2)

            call "COMPNAME" (2%, companyheader$, f1%(3%))

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000  /* Clear Variables For Input */

            for fieldnr% = 1% to 9%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10240
L10130:         gosub'101(1%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10210
L10160:                   if fieldnr% < 2%  then L10130
                          fieldnr% = fieldnr% - 1%
                          gosub'061(fieldnr%)
                          if enabled% <> 0% then L10130
                          goto L10160
L10210:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 32% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10130
L10240:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
L11090:     gosub'101(2%, 0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       print_it
                  if keyhit% <>  0% then       L11090
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% <  3% or fieldnr% > 15% then L11090
            if fieldnr% =  8% then L11090
            if fieldnr% =  9% or fieldnr% = 10% then L11090
            if fieldnr% = 11% then L11090
            if fieldnr% > 12% then fieldnr% = fieldnr% - 5%
            if fieldnr% >  2% then fieldnr% = fieldnr% - 1%

            gosub'061(fieldnr%)
L11220:     gosub'101(3%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11220
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11220
            goto editmode

        REM *************************************************************~
            *             G E N E R A T E   R E P O R T                 *~
            *                                                           *~
            * SCANS FORMAT FILE, RESPONDING TO LINES ACCORDINGLY.       *~
            *************************************************************

        print_it
            pageline% = 1000%
            pagenumber% = 0%
            p1$ = "REPORT:" : p2$ = report$
            call "SHOSTAT" ("Printing Report for " & mdate$)
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            call "STRING" addr ("CT", title$, 50%)

        REM Set Up ZONE Print Information...
            if zonename$(1%) = " " then L12210
                z$(1%) = zonename$(1%) & ": " & zonespec1$()
                if zonespec1$() = " " then z$(1%) =zonename$(1%) & ": All"
                if ieflag$(1%) = "X" then z$(1%) = zonename$(1%) &       ~
                                              ": All But " & zonespec1$()
L12210:     if zonename$(2%) = " " then L12260
                z$(2%) = zonename$(2%) & ": " & zonespec2$()
                if zonespec2$() = " "then z$(2%) =zonename$(2%) & ": All"
                if ieflag$(2%) = "X" then z$(2%) = zonename$(2%) &       ~
                                              ": All But " & zonespec2$()
L12260:     if zonename$(3%) = " " then L12320
                z$(3%) = zonename$(3%) & ": " & zonespec3$()
                if zonespec3$() = " " then z$(3%) = zonename$(3%)& ": All"
                if ieflag$(3%) = "X" then z$(3%) = zonename$(3%) &       ~
                                              ": All But " & zonespec3$()

L12320: REM Loop Through FORMAT Lines...
            readkey$ = str(report$) & hex(0000)
L12330:     call "PLOWNEXT" (#2%, readkey$, 3%, f1%(2%))
                if f1%(2%) = 0% then L12580
            gosub L30350
        REM Action depends on INSTRUCTION Code...
            descr$ = " "
            if inst$(1%) = " " then L12430
               search inst_codes$ =str(inst$(1%),,4%) to cursor%() step 4%
            if cursor%(1%) > 0% then L12434
L12430:     gosub get_balances
            goto L12330
L12434:     inst% = (cursor%(1%)+3%)/4%
                on inst% gosub insth,        /* SET HEADER LINE 2      */~
                               instp,        /* FORCE NEW PAGE         */~
                               instd,        /* PRINT DESCRIPTION ONLY */~
                               instdc,       /* CENTERED DESCRIPTION   */~
                               instu,        /* PRINT UNDERLINES       */~
                               instdu        /* PRINT DOUBLE UNDERLINES*/
                goto L12330
L12580:     lastreport$ = report$
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode

        REM *************************************************************~
            * G E T   B A L A N C E   I N S T R U C T I O N   C O D E S *~
            *                                                           *~
            * Retrieves account balance(s) & assoc data for report lines*~
            *************************************************************

        get_balances
            call "GLACCUM"  (period1%,period2%,comp$, inst$(), group$(), ~
                                acct1$(), acct2$(), zone%(),             ~
                                str(zonespec1$()), str(zonespec2$()),    ~
                                str(zonespec3$()), str(zonespec4$()),    ~
                                fixed$(), ieflag$(), print_if$(),        ~
                                reverse$(),print$(),paren$,round$,zero$, ~
                                total$(), totals(), text$(), columns%,   ~
                                colpos%(), collgth%(), colfmt$(),        ~
                                colcom$(), dolprt$(), 0%, periods%,      ~
                                #9, set$, u3%, bal%)
            if u3% = 1% then return  /* Nothing To Be Printed */
            if u3% = 99% then L65000  /* Crash */

            plowkey$ = all(hex(00))
L13210:     call "PLOWNEXT" (#9, plowkey$, 0%, f1%(9%))
                if f1%(9%) = 0% then return

        REM Load Information generated by GLACCUM....
            get #9, using L13310, c%, record$

L13310:     FMT BI(4),      /* Account Number                          */~
                CH(132)     /* Text Or Account Description             */

            gosub print_line
            goto L13210

        REM *************************************************************~
            *            O T H E R   I N S T R U C T I O N S            *~
            *                                                           *~
            * Page Control & Cosmetic Routine Are Here.                 *~
            *************************************************************

        instd : REM Print TEXT Only...
                gosub form_control : print using L15530, text$(1%): return

        instdc : REM Print DESCRIPTION centered...
                 call "STRING" addr ("CT", text$(1%), 40%)
                 gosub form_control : print using L15470,text$(1%): return

        instp : REM Force NEW PAGE...
                date% = 1%
                pageline% = 1000% : return

        insth : REM Report SUBTITLE...
                date% = 0%
                reportheader$ = text$(1%)
                call "STRING" addr ("CT", reportheader$, 50%)
                pageline% = 1000% : gosub form_control : return

        instu : REM Print Underlines...
                init(" ") record$
                for u% = 1% to columns%
                    if str(inst$(u%),1%,1%) <> "U" then L14395
                        str(record$,colpos%(u%),collgth%(u%)) = all("-")
L14395:         next u%
                gosub print_line
                return

        instdu: REM Print Double Underlines...
                init(" ") record$
                for u% = 1% to columns%
                    if str(inst$(u%),1%,2%) <> "DU" then L14440
                        str(record$,colpos%(u%),collgth%(u%)) = all("=")
L14440:         next u%
                gosub print_line
                return

        print_line
                gosub form_control
                print using L15530, record$
                return

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROL PAGING                                            *~
            *************************************************************

        form_control
            pageline% = pageline% + 1%

            if pageline% < 59% then return
               print page
               pagenumber% = pagenumber% + 1%
               pageline% = 8%
               print using L15350, date$, rptid$
               print using L15380, " ", companyheader$, pagenumber%
            if title$ = " " then L15125
               print using L15410, " ", title$
               pageline% = pageline% + 1%
L15125:     if reportheader$ = " " then L15140
               print using L15410, " ", reportheader$
               pageline% = pageline% + 1%
L15140:     if date% = 1% then print using L15440, " ", mdate$ else       ~
                               print using L15462, " ", mdate$
               p1$, p2$, z$() = " "
               print
            if reportheader$ = " " then pageline% = pageline% - 1%
               record1$ = " "

            for h% = columns% to 1% step -1%
               call "STRING" addr ("CT", text1$(h%), collgth%(h%))
               str(record1$,colpos%(h%),collgth%(h%)) = text1$(h%)
            next h%

            if str(text2$()) = " " then str(record1$,,16%) = coltext$
            if record1$ = " " then L15270
               print using L15530, record1$
               pageline% = pageline% + 1%
               record1$ = " "

L15270:     for h% = columns% to 1% step -1%
               call "STRING" addr ("CT", text2$(h%), collgth%(h%))
               str(record1$,colpos%(h%),collgth%(h%)) = text2$(h%)
            next h%

            if record1$ <> " " then str(record1$,,16%) = coltext$
            if record1$ = " " and pageline% > 8% then L15320
               if record1$ = " " then L15330
                   print using L15530, record1$
                   pageline% = pageline% + 1%
L15320:            print
L15330:     return

L15350: %RUN DATE: ########                                              ~
        ~                                               GLPRINT: ########

L15380: %###################################  ###########################~
        ~#######################                           PAGE: ###

L15410: %###################################  ###########################~
        ~#######################

L15440: %###################################          ###################~
        ~##############

L15462: %###################################          ###################~
        ~##############

L15470: %                                             ###################~
        ~#####################

L15530: %################################################################~
        ~#################################################################~
        ~#

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1      ~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1% : message$=" "
                  on fieldnr% gosub L20350,         /* REPORT NUMBER    */~
                                    L20390,         /* DESCRIPTION      */~
                                    L20440,         /* DEFAULT TITLE    */~
                                    L20490,         /* DEFAULT PAREN    */~
                                    L20550,         /* ROUND?           */~
                                    L20610,         /* PRINT IF ZERO?   */~
                                    L20720,         /* ZONE ONE         */~
                                    L20720,         /* ZONE TWO         */~
                                    L20720          /* ZONE THREE       */
                  return

            deffn'061(fieldnr%)
                  message$=" "
                  enabled% = 1%
                  on fieldnr% gosub L20360,         /* REPORT NUMBER    */~
                                    L20410,         /* PERIOD           */~
                                    L20470,         /* TITLE            */~
                                    L20520,         /* PAREN            */~
                                    L20580,         /* ROUND?           */~
                                    L20640,         /* PRINT IF ZERO?   */~
                                    L20720,         /* ZONE ONE         */~
                                    L20720,         /* ZONE TWO         */~
                                    L20720          /* ZONE THREE       */
                     return

L20350: REM Default/Enable - REPORT NUMBER...
L20360:         message$ = "Enter A Blank Report Number To Search For Des~
        ~ired Report."
                return

L20390: REM Default/Enable - Reporting PERIOD...
                month1$, month2$ = defmonth$
L20410:         message$ = "Enter Range Of Periods To Be Reported."
                return

L20440: REM Default/Enable - TITLE, SET OF BOOKS...
                get #1, using L20460, title$, set$
L20460:         FMT XX(33), CH(50), POS(646), CH(1)
L20470:         message$= "Enter Title To Print On Report Header."
                return

L20490: REM Default/Enable - PARENTHESIS...
                get #1, using L20510, paren$
L20510:         FMT XX(83), CH(3)
L20520:         message$ = "Enter 'YES' If You Want Parenthesis Rather Th~
        ~an Minus Signs."
                return

L20550: REM Default/Enable - ROUNDING...
                get #1, using L20570, round$
L20570:         FMT XX(89), CH(3)
L20580:         message$ = "Enter 'YES' If You Don't Want 'Cents' Printed~
        ~ On Dollar Amounts."
                return

L20610: REM Default/Enable - PRINT WHEN ZERO...
                get #1, using L20630, zero$
L20630:         FMT XX(86), CH(3)
L20640:         message$ = "Enter 'NO' If You Don't Want To Print Line Wh~
        ~en amount(s) On Line Are All 0."
                return

L20720: REM Default/Enable - ZONE specifications...
                if zonename$(fieldnr% - 6%) = " " then enabled% = 0%     ~
                     else header1$ = "     'O' For Only, 'X' For All But"
                message$ = "Leave Blank For All, Or Enter " &            ~
                    zonename$(fieldnr% -6%) & " To Be Printed/Excluded."
                return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************

            acct1$(),                    /* FROM ACCOUNT               */~
            acct2$(),                    /* TO ACCOUNT                 */~
            base$(),                     /* FROM ACCOUNT               */~
            defmonth$,                   /* DEFAULT MONTH              */~
            descr$,                      /* REPORT DESCRIPTION         */~
            errormsg$,                   /* ERROR MESSAGE              */~
            filler$,                     /* FILLER AT END OF RECORD    */~
            fixed$(),                    /* RUN TIME OVERRIDE FLAG     */~
            group$(),                    /* ACCOUNT GROUPING CODE      */~
            header1$,                    /* SCREEN HEADER              */~
            reportheader$,               /* REPORT HEADER              */~
            ieflag$(),                   /* INCLUDE/EXCLUDE FLAGS      */~
            iemesg$(),                   /* INCLUDE/EXCLUDE FLAGS      */~
            inst$(),                     /* INSTRUCTION CODE           */~
            mdescr$,                     /* INPUT TEXT                 */~
            message$,                    /* INPUT MESSAGE              */~
            month1$,                     /* STARTING PERIOD RANGE      */~
            month2$,                     /* ENDING PERIOD RANGE        */~
            paren$,                      /* DEFAUT PARENTHESIS OPTION  */~
            pdescr$,                     /* PERIOD RANGE DESCRIPTION   */~
            print$(),                    /* PRINT CODE                 */~
            print_if$(),                 /* PRINT/TOTAL CONSTRAINT FLAG*/~
            report$,                     /* REPORT ID.                 */~
            reverse$(),                  /* REVERSE SIGN INDICATOR     */~
            round$,                      /* ROUND OPTION (DEFUALT)     */~
            set$,                        /* SET OF BOOKS TO USE        */~
            text$(),                     /* TEXT FOR LINE ITEMS        */~
            title$,                      /* DEFAULT REPORT TITLE       */~
            total$(),                    /* TOTALING SPEC FOR LINE     */~
            z$(),                        /* ZONE DESCR FOR PRINT       */~
            zero$,                       /* PRINT IF ZERO? (DEFUALT)   */~
            zone$(),                     /* START, LENGTH IN ACCOUNT   */~
            zonename$(),                 /* LABLE FOR ZONE IN ACCOUNT  */~
            zonespec1$(),                /* USERS SELECTION FOR ZONE 1 */~
            zonespec2$(),                /* USERS SELECTION FOR ZONE 2 */~
            zonespec3$(),                /* USERS SELECTION FOR ZONE 3 */~
            zonespec4$() = " "           /* USERS SELECTION FOR ZONE 4 */

            mat totals = zer             /* REPORT TOTALS ACCUMULATOR  */
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
            k% = 0%
            call "STARTOVR" (k%)
            on k% + 1% goto L29928, L29934
            return

L29928: REM Start Over - (ENTER)...
                   return clear
                   goto inputmode
L29934: REM Return to Dispaly - (P.F. KEY 1)...
                   return

L30000: REM *************************************************************~
            *              L O A D   O L D   R E P O R T                *~
            *                                                           *~
            * LOADS OLD REPORT FROM DISK FILES.                         *~
            *************************************************************

            onfile% = 0%

        REM Load Report...
            call "READ100" (#1, report$, f1%(1%))
                if f1%(1%) = 0% then return

        REM Load ZONE Information from Header...
            onfile% = 1%
            get #1, using L30150, zone$(), zonename$(),columns$, coltext$,~
                    colpos$(), collgth$(), colfmt$(), colcom$(),         ~
                    text1$(), text2$()
L30150:     FMT XX(92), 8*CH(2), 4*CH(20), CH(2), CH(45), 10*CH(3),      ~
                10*CH(2), 10*CH(1), 10*CH(3), 10*CH(16), 10*CH(16)

            convert columns$ to columns%

            for c% = 1% to columns%
                convert colpos$(c%) to colpos%(c%)
                convert collgth$(c%) to collgth%(c%)
            next c%

            mat zone% = zer
            for i% = 1% to 4%
                for j% = 1% to 2%
                     if zone$(i%,j%) <> " " then convert zone$(i%,j%) to ~
                                                             zone%(i%,j%)
                next j%
            next i%

            for i% = 1% to 4%
                if zonename$(i%) = " " then L30348
                if zone%(i%,1%) = 0% then L30348
                on i% goto L30280, L30290, L30300, L30310
L30280:         mat redim zonespec1$(zone%(i%,2%),1%)1% : goto L30320
L30290:         mat redim zonespec2$(zone%(i%,2%),1%)1% : goto L30320
L30300:         mat redim zonespec3$(zone%(i%,2%),1%)1% : goto L30320
L30310:         mat redim zonespec4$(zone%(i%,2%),1%)1% : goto L30320
L30320:     next i%
L30348:   return

L30350: REM Load LINE Instructions...
            get #2, using L30580, report$, c%, inst$(),                   ~
                          group$(), acct1$(), acct2$(),                  ~
                          fixed$(), print$(), reverse$(),                ~
                          text$(), total$(), print_if$()
            return

L30580:         FMT CH(03),              /* REPORT NUMBER              */~
                    BI(2),               /* SEQUENCE NUMBER            */~
                    10*CH(4),            /* INSTRUCTION CODE           */~
                    10*CH(06),           /* ACCOUNT GROUP CODE         */~
                    10*CH(16),           /* START ACCOUNT NUMBER       */~
                    10*CH(16),           /* END ACCOUNT NUMBER         */~
                    10*CH(03),           /* IGNORE ZONING FLAG         */~
                    10*CH(03),           /* PRINT FLAG                 */~
                    10*CH(03),           /* REVERSE?                   */~
                    10*CH(45),           /* DESCRIPTION                */~
                    10*CH(20),           /* TOTALING SPECS             */~
                    10*CH(01),           /* TOTAL TYPE TO PRINT        */~
                    CH(225)              /* FILLER                     */

        REM *************************************************************~
            *      I N P U T   /   E D I T   M O D E   P A G E   1      *~
            *                                                           *~
            * SERVES INPUT LOOP AND EDIT MODE FOR PAGE ONE OF DOCUMENT. *~
            *************************************************************

            deffn'101(screen%, fieldnr%)
                  gosub set_keys
                  init(hex(84)) hfac$()
                  init(hex(8c)) mfac$(), tfac$
                  if fieldnr% = 0% then init(hex(86)) str(hfac$(),2%)
                  if header1$ <> " " then tfac$ = hex(ac)
                  header$  = "Last Report Printed: " & lastreport$
                  if lastreport$ = " " then header$ = " "
                  str(header$,63%) = "GLPRINT: " & cms2v$

                  on fieldnr% gosub L40330,         /* REPORT NUMBER    */~
                                    L40300,         /* PERIOD           */~
                                    L40300,         /* TITLE            */~
                                    L40330,         /* PAREN            */~
                                    L40330,         /* ROUND?           */~
                                    L40330,         /* PRINT IF ZERO?   */~
                                    L40330,         /* ZONE ONE         */~
                                    L40330,         /* ZONE TWO         */~
                                    L40330          /* ZONE THREE       */
                     goto L40400

L40300: REM Set FAC'S - UPPER/LOWER Case Input...
                      hfac$(fieldnr%) = hex(80)
                      return
L40330: REM Set FAC'S - UPPER CASE Only Input...
                      hfac$(fieldnr%) = hex(81)
                      return
        REM Set FAC'S - NUMERIC Only Input...
                      hfac$(fieldnr%) = hex(82)
                      return

L40400: accept                                                           ~
               at (01,02), "Financial Statements Report Generation",     ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "REPORT NUMBER",                              ~
               at (06,17), fac(hfac$(1%)), report$               ,ch(03),~
               at (06,36), fac(hex(8c)),  descr$                 ,ch(32),~
                                                                         ~
               at (08,02), "Select Period Range To Report",              ~
               at (08,33), fac(hfac$(2%)), month1$              , ch(02),~
               at (08,37), "Thru ",                                      ~
               at (08,43), fac(hfac$(2%)), month2$              , ch(02),~
               at (08,47), fac(hex(8c)),  pdescr$               , ch(30),~
                                                                         ~
               at (09,02), "Department Title:",                          ~
               at (09,23), fac(hfac$(3%)), title$               , ch(50),~
                                                                         ~
               at (10,02), "Parenthesis To Show Negatives?",             ~
               at (10,36), fac(hfac$(4%)), paren$                ,ch(03),~
                                                                         ~
               at (11,02), "Round Amounts To Nearest Dollar?",           ~
               at (11,36), fac(hfac$(5%)), round$                ,ch(03),~
                                                                         ~
               at (12,02), "Print Line When All Zeroes?",                ~
               at (12,36), fac(hfac$(6%)), zero$                 ,ch(03),~
                                                                         ~
                                                                         ~
               at (17,28), fac(tfac$),    header1$               ,ch(39),~
               at (18,03), fac(hex(8c)),  zonename$(1%)          ,ch(20),~
               at (18,28), fac(hfac$(7%)), str(zonespec1$())            ,~
               at (18,46), fac(hfac$(7%)), ieflag$(1%)           ,ch(01),~
               at (18,49), fac(hex(84)),  iemesg$(1%)            ,ch(07),~
                                                                         ~
               at (19,03), fac(hex(8c)),  zonename$(2%)          ,ch(20),~
               at (19,28), fac(hfac$(8%)), str(zonespec2$())            ,~
               at (19,46), fac(hfac$(8%)), ieflag$(2%)           ,ch(01),~
               at (19,49), fac(hex(84)),  iemesg$(2%)            ,ch(07),~
                                                                         ~
               at (20,03), fac(hex(8c)),  zonename$(3%)          ,ch(20),~
               at (20,28), fac(hfac$(9%)), str(zonespec3$())            ,~
               at (20,46), fac(hfac$(9%)), ieflag$(3%)           ,ch(01),~
               at (20,49), fac(hex(84)),  iemesg$(3%)            ,ch(07),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L40970
                  call "MANUAL" ("GLPRINT")
                  goto L40400

L40970:        if keyhit% <> 15% then L41010
                  call "PRNTSCRN"
                  goto L40400

L41010:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                    S E T   K E Y S                        *~
            *                                                           *~
            * Sets PF Keys & Descriptions Based On SCREEN%...           *~
            *************************************************************

        set_keys                 /* Cleansliness Is Next To Godliness? */
            on screen% goto L49110,         /* Header, Input Mode       */~
                            L49280,         /* Header, Edit Mode        */~
                            L49370          /* All Scrns, Edit Field    */

L49110: REM  ................... Header, Input mode .....................
            pfdescr$(1%) ="(1)Start Over    (4)Previous Field     (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%) ="                                              ~
        ~                 (16)Exit Program"
            pfkeys$ = hex(0001040d0f10ffffffffffffffff)
            str(pfdescr$(2%),63%,1%) = hex(84)

*        Turn Off Appropriate Fields...
            if fieldnr% = 1% then L49240
                str(pfdescr$(2%),63%)  = " "    /* Shut Off Exit Optn  */
                str(pfkeys$,6%,1%) = hex(ff)
                goto L49260
L49240:     str(pfdescr$(1%),,35%) =" "         /* Shut Off Prev Field */
            str(pfkeys$,2%,2%) = hex(ffff)
L49260: return

L49280: REM ................... Header, Edit mode .......................
            pfdescr$(1%) ="(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%) ="                                              ~
        ~                 (16)Print Report"
            pfkeys$ = hex(00010d0f10ffffffffffffffffffff)
            str(pfdescr$(2%),63%,1%) = hex(84)
        return

L49370: REM ............... All Screens, Field Edit .....................
            pfdescr$(1%) ="(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%) ="(ENTER) Validate Modification(s)              ~
        ~                                 "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* REPORT NUMBER    */~
                                    L50290,         /* PERIOD           */~
                                    L50460,         /* TITLE            */~
                                    L50500,         /* PAREN            */~
                                    L50590,         /* ROUND?           */~
                                    L50680,         /* PRINT IF ZERO?   */~
                                    L50830,         /* ZONE ONE         */~
                                    L50830,         /* ZONE TWO         */~
                                    L50830          /* ZONE THREE       */
                     return

L50200: REM Test data - REPORT NUMBER...
                call "GETCODE" (#1, report$, descr$, 1%, 0, f1%(1%))
                if f1%(1%) <> 0% then L50250
                    errormsg$ = "Report Not On File"
                    return
L50250:         gosub L30000
                if onfile% = 0% then errormsg$ = "Report Not On File"
                gosub L51010
                return

L50290: REM Test data - PERIOD...
            if month1$ = "?" then month1$ = " "
                period1%, period2% = 0%
            convert month1$ to period1%, data goto L50294
L50294:     mdescr$ = hex(06) & "Select Starting G/L Period"
            call "GLPERIOD" (#3, period1%, mdescr$, 1%, 0%, u3%)
                 if u3% = 1% then L50299
L50297:              errormsg$ = "Invalid Choice For Period"
                     return
L50299:     convert period1% to month1$, pic(##)
                pdescr$ = str(mdescr$)
            if month2$ <> " " then L50305
                month2$ = month1$ : period2% = period1%
                goto L50319
L50305:     if month2$ = "?" then month2$ = " "
            convert month2$ to period2%, data goto L50307
L50307:     mdescr$ = hex(06) & "Select Ending G/L Period"
            call "GLPERIOD" (#3, period2%, mdescr$, 1%, 0%, u3%)
                 if u3% = 1% then L50310 else L50297
L50310:     convert period2% to month2$, pic(##)
                str(pdescr$,16%,9%) = str(mdescr$,16,9%)
            if period2% >= period1% then L50319
                errormsg$ = "Starting Period MUST be LESS than or EQUAL"&~
                            " To Ending Period"
                return
L50319:     mdate$ = " "
            tdate$ = str(pdescr$,2%,8%)
            call "DATUNFMT" (tdate$, 0%, tdate1$)
            convert str(pdescr$,2%,2%) to month%
                m% = month% * 4% - 3%
                mdate$ = str(months$,m%,4%) & " " & str(pdescr$,5%,2%) & ~
                         ", " & str(tdate1$,1%,4%) & " Thru "
            tdate$ = str(pdescr$,16%,8%)
            call "DATUNFMT" (tdate$, 0%, tdate1$)
            convert str(pdescr$,16%,2%) to month%
                m% = month% * 4% - 3%
                str(mdate$,20%) = str(months$,m%,4%) & " "              &~
                        str(pdescr$,19%,2%) & ", "& str(tdate1$,1%,4%)

            bal% = 1%                                   /* Set defaults */
            if period1% < 14% or ask% = 0%  then return   /* Not Needed */
L50360:         bal% = 0%
                call "ASKUSER" (bal%, "*** YTD CALCULATION ***",         ~
                     "Do you want YTD calculations to begin with Period"&~
                     " 1 or Periods 14?", "Press PF(1) to begin with Pe"&~
                     "riod 1    -OR-", "Press PF(14) to begin with Peri"&~
                     "od 14.")
                if bal% = 1% or bal% = 14% then return
                    goto L50360

L50460: REM Test data - REPORT TITLE...
                return

L50500: REM Test data - PARENTHESIS...
                if paren$ = " " then paren$ = "NO "
                if str(paren$,,1%) = "Y" then paren$ = "YES"
                if str(paren$,,1%) = "N" then paren$ = "NO "
                if paren$ = "YES" then return
                if paren$ = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO': " & paren$
                return

L50590: REM Test data - ROUND...
                if round$ = " " then round$ = "NO "
                if str(round$,,1%) = "Y" then round$ = "YES"
                if str(round$,,1%) = "N" then round$ = "NO "
                if round$ = "YES" then return
                if round$ = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO': " & round$
                return

L50680: REM Test data - PRINT WHEN ZERO...
                if zero$ = " " then zero$ = "YES"
                if str(zero$,,1%) = "Y" then zero$ = "YES"
                if str(zero$,,1%) = "N" then zero$ = "NO "
                if zero$ = "YES" then return
                if zero$ = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO': " & zero$
                return

L50830: REM Test data - ZONE specifications...
                k% = fieldnr% - 6%
                on k% goto L50860, L50870, L50880
L50860:         if zonespec1$() <> " " then L50930 : goto L50890
L50870:         if zonespec2$() <> " " then L50930 : goto L50890
L50880:         if zonespec3$() <> " " then L50930 : goto L50890
L50890:         ieflag$(k%) = " "
                if zonename$(k%) = " " then return
                iemesg$(k%) = "ALL"
                return
L50930:         if pos("OX" = ieflag$(k%)) = 0 then L50970
                     iemesg$(k%) = "Only"
                     if ieflag$(k%) = "X" then iemesg$(k%) = "All But"
                     return
L50970:         errormsg$= "Please Indicate Desired Only/Exclude Option."
                if ieflag$(k%) <> " " then errormsg$ = "Enter 'O' Or 'X'"
            return

L51010: REM Determine If ASKUSER Message May Be Needed...
            ask% = 0%
            readkey$ = str(report$) & hex(0000)
L51040:     call "PLOWNEXT" (#2, readkey$, 3%, f1%(2%))
                if f1%(2%) = 0% then return
            get #2 using L51070, inst$()
L51070:         FMT POS(6), 10*CH(4)
            for i% = 1% to 10%
                if ask% = 1% then return
                    if inst$(i%) = "CB  " or inst$(i%) = "LCB " or       ~
                       inst$(i%) = "YA  " or inst$(i%) = "LYA " then     ~
                       ask% = 1%
            next i%
            goto L51040

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

            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#9)
            end
