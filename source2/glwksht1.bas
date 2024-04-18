        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      W   W  K   K   SSS   H   H  TTTTT    1     *~
            *  G      L      W   W  K  K   S      H   H    T     11     *~
            *  G GGG  L      W   W  KKK     SSS   HHHHH    T      1     *~
            *  G   G  L      W W W  K  K       S  H   H    T      1     *~
            *   GGG   LLLLL   W W   K   K   SSS   H   H    T    11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLWKSHT1 - THIS PROGRAM PRINTS OUT A SIMPLE TRIAL BALANCE *~
            *            WITH ADDED COLUMNS FOR ADJUSTMENTS AND         *~
            *            RECONCILIATIONS TO BE USED AS AN ACCOUNTANT'S  *~
            *            WORKSHEET                                      *~
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
            * 07/16/84 ! ORIGINAL                                 ! BLT *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 09/22/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 04/10/89 ! Display current period end date as defalt! GGO *~
            * 01/11/93 ! Page 0 Facs fix,  & End Report Time.     ! RJH *~
            * 03/22/95 ! Corrected out of balance on prior year   ! MLJ *~
            *          ! P&L reporting.                           !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            coname$60,                   /* Company name               */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dates$(32)8,                                                 ~
            dual_books$1,                /* Dual books in effect?      */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                                                    ~
            lines$1,                     /* ENTER THE NUMBER OF LINES  */~
            mdate$(4)8,                                                  ~
            period$8,                    /* ENTER THE CURRENT PERIOD   */~
            pf16msg$16, pf16fac$1, pf16key$1,                            ~
            rptid$6,                     /* Report ID                  */~
            set$1, setdescr$32, sethdr$60,/* Set of books to use       */~
            setmsg$18,                   /* Screen message for SET     */~
            time$8,                      /* Time of day                */~
            n%(2),                                                       ~
            debit$14,                                                    ~
            credit$14,                                                   ~
            glacct$16,                                                   ~
            gldesc$30,                                                   ~
            gltype$1,                                                    ~
            amount(32),                                                  ~
            acctplow$9

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
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
            * #01 ! SYSFILE2 ! SYSTEM DEFAULT FILE                      *~
            * #02 ! GLMAIN   ! General Ledger Main File                 *~
            * #12 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01,  "SYSFILE2", varc, indexed, recsize = 500,       ~
                        keypos  =  1,   keylen  =  20

            select #02,  "GLMAIN", varc, indexed, recsize =  300,        ~
                        keypos =    1, keylen =   9                      ~

            select #12,  "GLMAIN2", varc, indexed, recsize =  300,       ~
                        keypos =    1, keylen =   9                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#02, "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#01, "SWITCHS.GL", f1%(1))
                if f1%(1) = 0% then goto L09000
            get #01 using L02510, dual_books$
L02510:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if dual_books$ = "Y" then lo% = 1% else lo% = 2%
            if dual_books$ = "Y" then setmsg$ = "G/L System to use:"
            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (12%, coname$, 0%)
            str(line2$,62) = "GLWKSHT1: " & str(cms2v$,1,8)
            rptid$ = "G/L014"
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            call "READ100" (#01, "FISCAL DATES", f1%(1))
                if f1%(1) <> 0% then L09259
            err% = 0%
            call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE",         ~
                            "Review your fiscal date structure",         ~
                           "Press PF-16 to Acknowledge and Exit Program",~
                           " ")
            goto L65000

L09259:     get #1, using L09260, periods%, mdate$(4), monthopen%, dates$()
L09260:         FMT XX(20), BI(2), CH(8), XX(128), BI(2), 32*CH(8)
            period$ = dates$(monthopen%+15%)
            call "DATEFMT" (period$)
            periods% = periods%

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, lines$, set$, sethdr$,     ~
                setdescr$
            set = 1 : main% = 2% /* Default values */
            pf16msg$="(16)EXIT PROGRAM":pf16fac$=hex(84):pf16key$=hex(10)
            for fieldnr% = lo% to  3%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11051:     inpmessage$ = edtmessage$
            pf16msg$="(16)PRINT REPORT":pf16fac$=hex(84):pf16key$=hex(10)

L11060:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then datasave
                  if keyhit% <>  0 then L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < lo% or fieldnr% > 3% then L11060
            pf16fac$=hex(9c):pf16key$=hex(ff)
            gosub'051(fieldnr%)
L11130:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11051

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if dual_books$ = "Y"                                         ~
                then call "SHOSTAT" ("Printing " & setdescr$ & " Trial "&~
                    "Balance Worksheet")                                 ~
                else call "SHOSTAT" ("Printing Trial Balance Worksheet")

            totaldebit = 0
            totalcredit = 0
            pagenumber% = 0%
            linecount% = 1000%
            acctplow$ = all(hex(00))
L18120:     call "PLOWNEXT" (#main%, acctplow$, 0%, f1%(main%))
                if f1%(main%) = 0% then end_report
            get #main%, using L18150, glacct$, gldesc$, gltype$, amount()
L18150:         FMT CH(9), CH(30), CH(1), XX(4), 32*PD(14,4)
            if set = 1                                                   ~
                then call "GLFMT" (glacct$)                              ~
                else call "GLFMT2" (glacct$)
            glbal = 0
            init (" ") debit$, credit$
            if linecount% + 1% + lines% > 56% then gosub pagetotal
            if gltype$ = "R" or gltype$ = "E" then L18280
            for i% = 1% to currentmonth%
                glbal = round(glbal + amount(i%), 2%)
            next i%
            go to L18410

L18280:     if currentmonth% < 29% then L18370
            for i% = 16% to currentmonth%
                glpal = round(glpal + amount(i%), 2%)
            next i%
            for i% = 16% to currentmonth%
                glbal = round(glbal + amount(i%), 2%)
            next i%
            go to L18410

L18370:     for i% = 1% to currentmonth%
                glbal = round(glbal + amount(i%), 2%)
            next i%

L18410:     if glbal < 0 then L18470
               convert glbal to debit$, pic (###,###,###.##)
               pagedebit = round(pagedebit + glbal, 2%)
               totaldebit = round(totaldebit + glbal, 2%)
               go to L18510

L18470:     convert abs(glbal) to credit$, pic (###,###,###.##)
            pagecredit = round(pagecredit + glbal, 2%)
            totalcredit = round(totalcredit + glbal, 2%)

L18510:     print using L19400, glacct$, gldesc$, debit$, credit$

            if lines% = 0% then goto L18570
            for i% = 1% to lines%
                print using L19430
            next i%

L18570:     linecount% = linecount% + lines% + 1%
            init (" ") debit$, credit$
            go to L18120

        pagetotal
            if pagenumber% = 0% then L18680
               convert pagedebit to debit$, pic (###,###,###.##)
               convert abs(pagecredit) to credit$, pic (###,###,###.##)
               print
               print
               print using L19520, debit$, credit$
L18680:     pagecredit, pagedebit = 0
            init (" ") debit$, credit$
            gosub new_page
            return

        new_page
            select printer(134)
            if pagenumber% <> 0% then goto L18870
                time$ = " " : call "TIME" (time$)
*        ** Fix Facs **
L18762:         i% = pos(str(i$()) > hex(7f))
                if i% = 0% then L18770
                    str(i$(), i%, 1%) = hex(20)
                    goto L18762

L18770:         call "SETPRNT" (rptid$, " ", 0%, 0%)
                pagesw% = 1%
                gosub L18880
                pagesw% = 0%
                print skip (2)
                print using L19710, "R E P O R T   S E L E C T I O N S:"
                print
                for x% = 4% to 20%
                    print using L19710, i$(x%)
                next x%
L18870:     pagenumber% = pagenumber% + 1%
L18880:     print page
            print using L19580, date$, time$, coname$, "-" & rptid$
            print using L19610, pagenumber%
            print using L19660, sethdr$
            print using L19640, period$
            print
            if pagesw% <> 0% then return
            print using L19680
            print using L19460
            print using L19490
            print using L19680
            linecount% = 9%
            return

        end_report
            if currentmonth% < 29% then L19220
            if linecount% + 1% + lines% > 56% then gosub pagetotal
            if glpal < 0 then L19110
            convert glpal to debit$, pic (###,###,###.##)
            pagedebit = round(pagedebit + glpal, 2%)
            totaldebit = round(totaldebit + glpal, 2%)
            go to L19140

L19110:     convert abs(glpal) to credit$, pic (###,###,###.##)
*          PAGECREDIT = ROUND(PAGECREDIT + GLPAL, 2%)
*          TOTALCREDIT = ROUND(TOTALCREDIT + GLPAL, 2%)
L19140:     glacct$ = "*  NOTE  *"
            gldesc$ = "PRIOR YEAR P&L NOT YET CLOSED"
            print using L19400, glacct$, gldesc$, debit$, credit$

            if lines% = 0% then goto L19220
            for i% = 1% to lines%
                print using L19430
            next i%

L19220:     convert pagedebit to debit$, pic (###,###,###.##)
            convert pagecredit to credit$, pic (###,###,###.##)
            print
            print
            print using L19520, debit$, credit$

            convert totaldebit to debit$, pic (###,###,###.##)
            convert totalcredit to credit$, pic (###,###,###.##)
            print
            print using L19550, debit$, credit$

        REM CLOSE PRINTER AND SEND THIS BATCH OFF
            print
            time$ = " "   :   call "TIME" (time$)
            print using L19730, time$            /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode

L19400: %!############!###########################!##############!#######~
        ~#######!         !         !         !         !         !       ~
        ~  !
L19430: %!            !                           !              !       ~
        ~       !         !         !         !         !         !       ~
        ~  !
L19460: %! ACCOUNT    ! DESCRIPTION               !  T R I A L   B A L A ~
        ~N C E  !     ADJUSTMENTS   !  INCOME STATMENT  !   BALANCE SHEET ~
        ~  !
L19490: %! NUMBER     !                           !     DEBIT    !    CRE~
        ~DIT    !  DEBIT  !  CREDIT !  DEBIT  !  CREDIT !  DEBIT  !  CREDI~
        ~T !
L19520: %             ! PAGE TOTALS               !##############!#######~
        ~#######!         !         !         !         !         !       ~
        ~  !
L19550: %             ! GRAND TOTALS              !##############!#######~
        ~#######!         !         !         !         !         !       ~
        ~  !
L19580: %RUN: ######## @ ########            ############################~
        ~################################                     GLWKSHT1####~
        ~###
L19610: %                                             A C C O U N T A N T~
        ~ 'S   W O R K S H E E T                                   PAGE: #~
        ~###
L19640: %                                                   FOR THE PERIO~
        ~D ENDED ########
L19660: %                                    ############################~
        ~################################
L19680: %+------------+---------------------------+----------------------~
        ~-------+-------------------+-------------------+-----------------~
        ~--+
L19710: %                          ######################################~
        ~##########################################
L19730:       %                                                 *** END O~
        ~F REPORT  @ ########  ***

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20060,         /* G/L set of books */~
                                    L20100,         /* CURRENT PERIOD   */~
                                    L20200          /* ENTER LINES      */
                     return

L20060: REM DEFAULT/ENABLE FOR G/L set of books
            enabled% = 1%
            inpmessage$ = "Enter '1' for Statutory G/L set of books; '2"&~
                "' for Local Authority."
            return

L20100: REM DEFAULT/ENABLE FOR ENTER THE CURRENT PERIOD
            enabled% = 1%
            inpmessage$ = "Enter the Period-End date of the desired month"
            return

L20200: REM DEFAULT/ENABLE FOR ENTER THE NUMBER OF LINES
            inpmessage$ = "Enter the # of extra (blank) lines to print "&~
                "for each account (0 - 9)"
            enabled% = 1%
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
            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

        deffn'101(fieldnr%)
            if fieldnr% = 0%                                             ~
                then init(hex(84)) lfac$()                               ~
                else init(hex(8c)) lfac$()
            if dual_books$ <> "Y" then lfac$(1) = hex(9c)
            on fieldnr% gosub L40210,         /* G/L set of books       */~
                              L40150,         /* CURRENT PERIOD         */~
                              L40210          /* ENTER LINES            */
            goto L40250

L40150:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40210:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40250:     accept                                                       ~
               at (01,02), "Accountant's Trial Balance Worksheet",       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),   setmsg$              , ch(18),~
               at (06,30), fac(lfac$( 1)), set$                 , ch(01),~
               at (06,40), fac(hex(8c)),   setdescr$            , ch(30),~
                                                                         ~
               at (07,02), "Trial Balance report date:",                 ~
               at (07,30), fac(lfac$( 2)), period$              , ch(08),~
                                                                         ~
               at (08,02), "# of blank lines:",                          ~
               at (08,30), fac(lfac$( 3)), lines$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(pf16fac$), pf16msg$              , ch(16),~
                                                                         ~
               keys(hex(00010d0f) & pf16key$), key (keyhit%)

               if keyhit% <> 13 then L40550
                  call "MANUAL" ("GLWKSHT1")
                  goto L40250

L40550:        if keyhit% <> 15 then goto L40590
                  call "PRNTSCRN"
                  goto L40250

L40590:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50121,         /* G/L set of books       */~
                              L50280,         /* CURRENT PERIOD         */~
                              L50410          /* ENTER LINES            */
            return

L50121: REM TEST DATA FOR G/L set of books
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ = " " then goto L50180
                errormsg$ = "G/L System code must be '1' (Statutory) or"&~
                    " '2' (Local Authority)"
                return
L50180:     setdescr$ = "Statutory"
            main% = 2%
            if set = 1 then goto L50230
                setdescr$ = "Local Authority"
                main% = 12%
L50230:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            return

L50280: REM TEST DATA FOR ENTER THE CURRENT PERIOD
            call "DATEOK" (period$, u3%, errormsg$)
                if errormsg$ <> " " then return
            call "DATUNFMT" (period$)
            search dates$() = str(period$,1,8) to n%() step 8%
            call "DATEFMT" (period$)
                if n%(1) <> 0% then L50380
            errormsg$ = "There is no such period ending date in the fisca~
        ~l dates record"
                return
L50380:     currentmonth% = int(n%(1)/8%) + 1%
            return

L50410: REM TEST DATA FOR ENTER THE NUMBER OF LINES
            if lines$ = " " then lines$ = "0"
            convert lines$ to lines%, data go to L50440
            if lines% > -1% and lines% < 10% then return
L50440:     errormsg$ = "# of blank lines must be between 0 and 9"
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

            call "SHOSTAT" ("One Moment Please")
            end
