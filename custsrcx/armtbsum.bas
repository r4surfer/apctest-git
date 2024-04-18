        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT  BBBB    SSS   U   U  M   M   *~
            *  A   A  R   R  MM MM    T    B   B  S      U   U  MM MM   *~
            *  AAAAA  RRRR   M M M    T    BBBB    SSS   U   U  M M M   *~
            *  A   A  R   R  M   M    T    B   B  D   D  U   U  M   M   *~
            *  A   A  R   R  M   M    T    BBBB   DDDD    UUU   M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTBSUM - Prints Summary Trial Balance.                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/02/86 ! Original                                 ! ERN *~
            * 04/08/88 ! As of date can be same as settlmnt date  ! DAW *~
            * 04/13/89 ! Mod to ARMAGING args, added selects      ! RJM *~
            *          !  No functional mods, stubs to handle     !     *~
            *          !  Multi-Currency.                         !     *~
            * 06/12/89 ! Added Display option                     ! SID *~
            * 11-16-89 ! Added Aging argument array to ARMAGING   ! MJB *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            * 07/25/01 ! (EWD001)  Mod to add text from Atrium to ! CMG *~
            *          !           end of rpt.  - Turned OFF      !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$(2)12, acctu$(2)12,     /* GL Account Range           */~
            acctie$1,                    /* GL Account Range           */~
            ageper$1,                    /* Aging Per Disc or Net Date */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            asof$8, asofu$8,             /* As Of Date                 */~
            billto_display$(48)14,       /* Bill-to Aging and Display  */~
            billto$9, billtoname$30,     /* Bill-to Code and Name      */~
            billto(9), billto$(6)14,     /* Bill-to Aging and Print    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            columnhead1$79,              /* Col. head for display scrn */~
            compname$60,                 /* Company Name               */~
            curr$1,                      /* Multi Currency Flag        */~
            cage$(50)76,                 /* Currency Aging array       */~
            currency$4,                  /* NOT USED RIGHT NOW         */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$(8)9,                /* Customer code for display  */~
            cusname$(8)30,               /* Customer name for display  */~
            cus_range$(4)9,              /* Bill-to Customer Range     */~
            date$8,                      /* Date for screen display    */~
            dtldoc$8,                    /* Srce Doc for Dtl display   */~
            dtlsrce$1,                   /* Srce of Doc for display    */~
            edtmessage$79,               /* Edit screen message        */~
            edtmessage2$79,              /* Message for TB_Detail disp */~
            errormsg$79,                 /* Error message              */~
            hdrs$(5)13,                  /* Report Column Headings     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            pfkeys$32,                   /* PF Key Options             */~
            last_stl$8,                  /* Last Settling Date         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            message$(3)80,               /* Message for Totals Display */~
            min_bal$10,                  /* Minimum Balance            */~
            pastdue$1, pastdue_prnt$3,   /* Past Due Items Only?       */~
            pf16$16, pf4$18, pf8$26,     /* PF Key Literals            */~
            pf14$26,                     /* PF Key Literals            */~
            pfdescr$(2)79,               /* PFKEY description          */~
            plowkey$50,                  /* A Plow Key                 */~
            readkey$50,                  /* Read Key                   */~
            report(6),                   /* Report Totals              */~
            shiptos$9,                   /* Customer for Dtl display   */~
            stat_curr_code$4,            /* Statutory Currency Flag    */~
            userid$3,                    /* Current User Id            */~
            text$(6)135                  /* Text for End of Rpt  EWD001*/

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* File Read Status Flag      */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #2  ! SYSFILE2 ! System File                              *~
            * #3  ! CUSTOMER ! Customer Master                          *~
            * #4  ! GLMAIN   ! General Ledger Master File               *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen = 21

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 9

            select #44, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))

*          ARMTBCEX not opened or used, open if multi-currency enabled

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            edtmessage2$ = "Position cusor and press (RETURN) to see" &  ~
                           " Trial Balance Detail"


            readkey$ = "ARM.LAST.SETTLING"

*        Check for Multi-Currency usage and get STATUTORY currency code
            curr$ = "N"
            stat_curr_code$ = " "
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
            if f1%(2) = 0% then L09150        /* Multi-Currency not used */
                get #2 using L09141, curr$, stat_curr_code$
L09141:             FMT POS(21), CH(1), CH(4)

L09150:     last_stl$ = blankdate$
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09180, last_stl$
L09180:         FMT XX(20), CH(6)
            call "DATEFMT" (last_stl$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf8$=" ": pf16$="(16)Exit Program"
            pf14$ = "  "
            init(" ") errormsg$, inpmessage$, asof$, cus_range$(),       ~
                      pastdue$, min_bal$, aging$(), ageper$,             ~
                      acct$(), acctie$, acctu$()

            mat aging% = zer
                aging%(1) =  30% : aging%( 2) =  60% : aging%(3) =  90%
                aging%(4) = 120% : aging%(10) =   5%
                aging%(8) = -99999%  :  aging%(5), aging%(9) = 99999%


            for fieldnr% = 1% to 6%
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10290
L10190:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10190
L10290:         gosub'151(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%

            gosub aging_parameters


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " " : pf8$ = "(8)Modify Aging Parameters"
            pf16$ = "(16)Print Report"
            pf14$ = "(14)Display Summary Aging"
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then gosub aging_parameters
                  if keyhit%  = 14 then display_controller
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       edtpg1
L11160:     fieldnr% = min(max(1%, cursor%(1) - 5%), 7%)
            if fieldnr% = lastfieldnr% then edtpg1
            if fieldnr% <> 7% then L11220
                  gosub aging_parameters  :  goto L11300
L11220:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf8$, pf16$ = " "
L11250:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11250
            gosub'151(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11250
L11300:              lastfieldnr% = fieldnr%
                     goto L11160


        aging_parameters
            call "ARMDATES" (asofu$, "A/R SUMMARY AGING REPORT",         ~
                             aging%(), 1%, 1%, aging$(), u3%)
            if u3% <> 1% then return
                return clear all
                goto   inputmode


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Print Report.                                             *~
            *************************************************************

        print_report
            call "SHOSTAT" ("Printing Summary Trial Balance")

            call "TIME" (time$)
            billto$ = hex(ff)

            pastdue_prnt$ = "YES"
            if pastdue$ = "N" then pastdue_prnt$ = "NO"

            if min_bal$ = " " then L12180
                convert min_bal$ to min_bal, data goto exit_program

L12180:     call "COMPNAME" (12%, compname$, u3%)
            select printer  (134)
            call "SETPRNT"  ("ARM004", " ", 0%, 0%)
            plowkey$ = cus_range$(3)
            mat report = zer
            billtos%   = 0%
            for b% = 1% to 5%
                if b% = 1% then prev% = aging%(8)           else         ~
                                prev% = aging%(b%-1%) + 1%
                put hdrs$(b%) using L12280, prev%, aging%(b%)
L12280:              %-#### - -####
            next b%
            if aging%(8) = -99999% then                                  ~
                     put hdrs$(1) using L12320, "BEFORE", aging%(1) + 1%
L12320:                   % ###### -####
            if aging%(9) =  99999% then                                  ~
                     put hdrs$(5) using L12320, "  OVER", aging%(4)

*        First Print Selection Criteria Page
            short% =  1%
            page%  = -1%
            gosub page_heading
            print skip(2)
            print using L13360
            print
            print using L13380, asof$
            if cus_range$(1) = "ALL" then                                ~
                print using L13400, "ALL", " ", " "   else                ~
                print using L13400, cus_range$(1), "TO", cus_range$(2)
            print using L13420, pastdue_prnt$
            if min_bal$ = " " then print using L13440, "NONE"             ~
                              else print using L13440, min_bal$
            if ageper$ = "D" then print using L13460
            if ageper$ = "N" then print using L13480
            if ageper$ = "I" then print using L13500
            if acct$(1) = "ALL" then L12525
               if acctie$ = "I" then                                     ~
                  print using L13511, acct$(1), acct$(2)                  ~
                                else                                     ~
                  print using L13513, acct$(1), acct$(2)
L12525:     short% = 0%
            gosub page_heading



        report_loop
*        Get next record and test against selected customer range.
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then end_report
            if str(plowkey$,,9) > cus_range$(4) then end_report

*        Now get agings and test against report criteria.
            billto$ = str(plowkey$,,9) : str(plowkey$,10) = hex(ff)

*                   ARMAGING now handles multicurrency. But the 2nd to
*                   last arg is 0% and this causes the sub to work in
*                   a NON-Multi-Currency mode. See ARMAGING for futher
*                   details & see ARMSTMNT for example of use where
*                   Customers being invoiced w/ more than 1 currency
*                   can have each currency type accounted/totaled
*                   separately.

            call "ARMAGING" (billto$, " ", asofu$, ageper$, aging%(),    ~
                  #1, billto(), #44, #2, 0%, currency$, cage$(),         ~
                  acctie$, acctu$(1), acctu$(2))

            if pastdue$ = "Y" and billto(9) = 0 then report_loop
            if min_bal$ <> " " and billto(8) <= min_bal then report_loop

*        Print Bill-to
            if line% > 55% then gosub page_heading
            call "DESCRIBE" (#3, billto$, billtoname$, 0%, f1%(3))
            if f1%(3) = 0% then billtoname$ = "** NOT ON FILE **"
            init(" ") billto$()
                convert billto(8) to billto$(1), pic(-##,###,###.00)
                report(1) = report(1) + billto(8)
                for c% = 1% to 5%
                     if billto(c%) <> 0 then convert billto(c%) to       ~
                                      billto$(c%+1%), pic(-##,###,###.00)
                     report(c%+1%) = report(c%+1%) + billto(c%)
                next c%
            print using L13580, billto$, billtoname$,                     ~
                               billto$(1), billto$(2), billto$(3),       ~
                               billto$(4), billto$(5), billto$(6)
            billtos% = billtos% + 1%
            line%    = line%    + 1%
            goto report_loop


        end_report
            gosub report_totals
            print
            print "** END OF REPORT **"
            close printer
            goto exit_program

                                                        /*  (EWD001) - Begin  */
            print
            print

            str(text$(1),1%,43%) = "'THE RECEIVABLES DESCRIBED HEREIN HAVE BEEN"
            str(text$(1),44%,43%) = "SOLD PURSUANT TO A PURCHASE AND SALE AGREEM"
            str(text$(1),87%,43%) = "ENT, DATED AS OF JULY    , 2001, AS THE    "

            str(text$(2),1%,43%) = "SAME MAY FROM TIME TO TIME BE AMENDED, SUPP"
            str(text$(2),44%,43%) = "LEMENTED OR OTHERWISE MODIFIED, BETWEEN CER"
            str(text$(2),87%,43%) = "TAIN ENTITIES LISTED ON SCHEDULE I THERETO,"

            str(text$(3),1%,43%) = " AS ORIGINATORS, AND ATRIUM FUNDING CORPORA"
            str(text$(3),44%,43%) = "TION, AS PURCHASER, AND AN UNDIVIDED, FRACT"
            str(text$(3),87%,43%) = "IONAL OWNERSHIP INTEREST IN THE RECEIVABLES" 

            str(text$(4),1%,43%) = "DESCRIBED HEREIN HAS BEEN SOLD TO FAIRWAY F"
            str(text$(4),44%,43%) = "INANCE CORPORATION PURSUANT TO A RECEIVABLE"
            str(text$(4),87%,43%) = "S PURCHASE AGREEMENT, DATED AS OF JULY, " 

            str(text$(5),1%,43%) = "2001 AS THE SAME MAY FROM TIME TO TIME BE A"
            str(text$(5),44%,43%) = "MENDED, SUPPLEMENTED OR OTHERWISE MODIFIED,"
            str(text$(5),87%,43%) = " AMONG ATRIUM FUNDING CORP., AS SELLER, ATR"

            str(text$(6),1%,43%) = "IUM COMPANIES, INC., AS SERVICER, FAIRWAY F"
            str(text$(6),44%,43%) = "INANCE CORPORATION, AND BMO NESBITT BURNS C"
            str(text$(6),87%,43%) = "ORP., AS AGENT.'"

            print using L13660, text$(1)
            print using L13660, text$(2)
            print using L13660, text$(3)
            print using L13660, text$(4)
            print using L13660, text$(5)
            print using L13660, text$(6)
                                                        /*  (EWD001) -   End  */

            close printer
            goto exit_program


        page_heading
            page% = page% + 1%
            line% = 6%
            print page
            print using L13300, date$, time$, compname$
            print using L13330, asof$, page%
            print
            if short% = 1% then return
                print using L13520, hdrs$(1), hdrs$(2), hdrs$(3),         ~
                                   hdrs$(4), hdrs$(5)
                print using L13550
                return


        report_totals
            init (" ")billto$()
            for c% = 1% to 6%
                convert report(c%) to billto$(c%), pic(-##,###,###.00)
            next c%
            print using L13620
            print using L13650, billtos%,                                 ~
                               billto$(1), billto$(2), billto$(3),       ~
                               billto$(4), billto$(5), billto$(6)
            return



        REM *************************************************************~
            *          I M A G E   S T A T E M E N T S                  *~
            *************************************************************


L13300: %RUN DATE: ######## ########              #######################~
        ~#####################################                ARMTBSUM:ARM~
        ~004
L13330: %   AS OF: ########                                        A/R SU~
        ~MMARY AGING REPORT                                        PAGE: #~
        ~###
L13360: %                                         ---------------  REPORT~
        ~ SELECTION CRITERIA  ---------------
L13380: %                                                      AS OF DATE~
        ~: ########
L13400: %                                                   FOR CUSTOMERS~
        ~: ######### ## #########
L13420: %                                                  PAST DUES ONLY~
        ~? ###
L13440: %                                                 MINIMUM BALANCE~
        ~: ##########
L13460: %                                                         AGE PER~
        ~: CASH DISCOUNT DUE DATE
L13480: %                                                         AGE PER~
        ~: NET DUE DATE
L13500: %                                                         AGE PER~
        ~: SOURCE DOCUMENT (INVOICE) DATE
L13511: %                                                         INCLUDE~
        ~ G/L ACCOUNTS ############ THRU ############
L13513: %                                                         EXCLUDE~
        ~ G/L ACCOUNTS ############ THRU ############
L13520: % BILL-TO  BILL-TO NAME                        TOTAL DUE  #######~
        ~######  #############  #############  #############  ############~
        ~#
L13550: %--------- ------------------------------  -------------  -------~
        ~------  -------------  -------------  -------------  ------------~
        ~-
L13580: %######### ############################## ############## ########~
        ~###### ############## ############## ############## #############~
        ~#

L13620: %                                          -------------  -------~
        ~------  -------------  -------------  -------------  ------------~
        ~-
L13650: %    ** REPORT TOTALS  (##### BILL-TOS)   ############## ########~
        ~###### ############## ############## ############## #############~
        ~#
                                                        /*  (EWD001)    */
L13660: %################################################################~
        ~###################################################################


        REM *************************************************************~
            *     D I S P L A Y   C O N T R O L   S E C T I O N         *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        display_controller

            pfkeys$ = (hex(0001050d0f101e))
            billtos%   = 0%  :  page% = 1%  :  errormsg$ = " "
            init(" ") cuscode$(), cusname$(), billto_display$()
         pfdescr$(1) = "(1)Start Over                    (13)Instructions~
        ~              (15)Print Screen"
         pfdescr$(2) = "                  (5)Next        (30)Print Report~
        ~              (16)Exit Program"
            billto$ = hex(ff)
            counter% = 0% : top% = 0%

            pastdue_prnt$ = "YES"
            if pastdue$ = "N" then pastdue_prnt$ = "NO"

            if min_bal$ = " " then L14210
                convert min_bal$ to min_bal, data goto exit_program

L14210:     plowkey$ = cus_range$(3)
            mat report = zer
            for b% = 1% to 5%
                if b% = 1% then prev% = aging%(8)           else         ~
                                prev% = aging%(b%-1%) + 1%
                put hdrs$(b%) using L14280, prev%, aging%(b%)
L14280:              %-#### - -####
            next b%
            if aging%(8) = -99999% then                                  ~
                     put hdrs$(1) using L14320, "BEFORE", aging%(1) + 1%
L14320:                   % ###### -####
            if aging%(9) =  99999% then                                  ~
                     put hdrs$(5) using L14320, "  OVER", aging%(4)

        display_loop
*        Get next record and test against selected customer range.

            if counter% = 8% then gosub do_display                       ~
                             else gosub L14380
                  if keyhit% =   0  then tb_inquiry
                  if keyhit% =   1  then gosub startover
                  if keyhit% =  16  then exit_program
                  if keyhit% =  30  then print_report
L14380:     call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then do_totals_display
            if str(plowkey$,,9) > cus_range$(4) then                     ~
                                do_totals_display

*        Now get agings and test against report criteria.
            billto$ = str(plowkey$,,9) : str(plowkey$,10) = hex(ff)

*                   ARMAGING now handles multicurrency. But the 2nd to
*                   last arg is 0% and this causes the sub to work in
*                   a NON-Multi-Currency mode. See ARMAGING for futher
*                   details & see ARMSTMNT for example of use where
*                   Customers being invoiced w/ more than 1 currency
*                   can have each currency type accounted/totaled
*                   separately.

            call "ARMAGING" (billto$, " ", asofu$, ageper$, aging%(),    ~
                  #1, billto(), #44, #2, 0%, currency$, cage$(),         ~
                  acctie$, acctu$(1), acctu$(2))

            if pastdue$ = "Y" and billto(9) = 0 then display_loop
            if min_bal$ <> " " and billto(8) <= min_bal then display_loop

*        Display Bill-to
            call "DESCRIBE" (#3, billto$, billtoname$, 0%, f1%(3))
            if f1%(3) = 0% then billtoname$ = "** NOT ON FILE **"
            init(" ") billto$()
                convert billto(8) to billto$(1), pic(-##,###,###.00)
                report(1) = report(1) + billto(8)
                for c% = 1% to 5%
                     if billto(c%) <> 0 then convert billto(c%) to       ~
                                      billto$(c%+1%), pic(-##,###,###.00)
                     report(c%+1%) = report(c%+1%) + billto(c%)
                next c%

            billtos% = billtos% + 1%
            counter% = counter% + 1%
            if counter% <= 8% then gosub assign_display_arrary           ~
                              else gosub init_variables_arrary
            goto display_loop

        assign_display_arrary /* Assign output to a displaying arrary */
                if counter% = 1% then top% = 1%                          ~
                                 else top% = top% + 6%
                cuscode$(counter%) = billto$
                cusname$(counter%) = billtoname$
                billto_display$(top% + 0%) = billto$(1)
                billto_display$(top% + 1%) = billto$(2)
                billto_display$(top% + 2%) = billto$(3)
                billto_display$(top% + 3%) = billto$(4)
                billto_display$(top% + 4%) = billto$(5)
                billto_display$(top% + 5%) = billto$(6)
        return

        init_variables_arrary
            init(" ") cuscode$(), cusname$(), billto_display$()
            counter% = 1% : top% = 1%
            gosub assign_display_arrary
        return

        do_totals_display
            pfkeys$ = (hex(0001020a0d0f101e))
         pfdescr$(1) = "(1)Start Over    (10)See Totals  (13)Instructions~
        ~              (15)Print Screen"
         pfdescr$(2) = "(2)First                         (30)Print Report~
        ~              (16)Exit Program"
            convert billtos% to billtos$, pic(#####)
            for s% = 1% to 6%
                convert report(s%) to billto$(s%), pic(-##,###,###.00)
            next s%
            message$(1) = "** Totals: " & billtos$ & " bill-tos " &      ~
                          "   ** Total Due: " & billto$(1)

            message$(2) = billto$(2)&billto$(3)&billto$(4)&billto$(5)&   ~
                          billto$(6)



            message$(3) = " ** Press (RETURN) to continue ** "
            counter% = 8%
            goto display_loop
        return

        tb_inquiry
            ret% = 0%
            if cursor%(1%) < 6% or cursor%(1%) > 22% then L15140
            line_position% = cursor%(1%)
            line_position% = line_position% - 5%
            line_position% = int((line_position% + 1%) /  2%)
            billto$ = cuscode$(line_position%)
            call "ARQTBSUB" (billto$, aging%(), asofu$, ageper$, #1, #3, ~
                             "N", ret%, dtldoc$, shiptos$, dtlsrce$,     ~
                             curr$, stat_curr_code$, acctie$, acct$(1),  ~
                             acct$(2))
L15140: goto display_loop

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            enabled% = 1%
                  on fieldnr% gosub L20160,         /* As Of Date       */~
                                    L20220,         /* Bill-to Range    */~
                                    L20350,         /* Past Due Only?   */~
                                    L20410,         /* Minimum Balance  */~
                                    L20460,         /* Age Per Date     */~
                                    L20600          /* GL Range         */
                     return

L20160
*        As Of Date                            ASOF$
            inpmessage$ = "Transactions posted after this date are"  &   ~
                          " ignored by the report."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
            return

L20220
*        Bill-to Customer Range                CUS_RANGE$()
            inpmessage$ = "Enter range to print (ALL, FIRST, LAST)."
            if str(cus_range$()) = " " then cus_range$(1) = "ALL"
            return

L20350
*        Past Due Items Only?                  PASTDUE$
            inpmessage$ = "Enter 'Y' to list Past Due Customers Only."
            if pastdue$ = " " then pastdue$ = "N"
            return

L20410
*        Minimum Balance                       MIN_BAL$
            inpmessage$ = "Enter Minimum Balance to List.  Leave blank" &~
                          " to list all balances."
            return

L20460
*        Age Per Date                          AGEPER$
            inpmessage$ = "'D' to Age per Cash Disc Date, 'N'"        &  ~
                          " by Net Due Date, or 'I' for Invoice Date."
            if ageper$  = " " then ageper$ = "N"
            return

L20600
*        Selected Account
            inpmessage$ = "Enter G/L Account Range or 'ALL' & Include" & ~
                          "/Exclude selection (via I or E)."
            if acctie$  = " " then acctie$  = "I"
            if acct$(1) <> " " then return
               acct$(1) = "ALL"
               acct$(2) = " "
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%, edit%)
                  str(line2$,62%) = "ARMTBSUM: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  if edit% = 1% then lfac$(7) = hex(9c)
                  on fieldnr% gosub L40220,         /* As Of Date       */~
                                    L40220,         /* Bill-to Range    */~
                                    L40220,         /* Past Due Only?   */~
                                    L40250,         /* Minimum Balance  */~
                                    L40220,         /* Aging per date   */~
                                    L40220          /* GL Range         */
                  goto L40290

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40250:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R SUMMARY AGING REPORT",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "As Of Date",                                 ~
               at (06,30), fac(lfac$( 1)), asof$                , ch(08),~
               at (06,49), "Last Settled on ",                           ~
               at (06,65), fac(hex(8c)), last_stl$              , ch(08),~
                                                                         ~
               at (07,02), "Bill-to Customer Range", at(07,41), "to",    ~
               at (07,30), fac(lfac$( 2)), cus_range$(1)        , ch(09),~
               at (07,45), fac(lfac$( 2)), cus_range$(2)        , ch(09),~
                                                                         ~
               at (08,02), "Past Due Customers Only?",                   ~
               at (08,30), fac(lfac$( 3)), pastdue$             , ch(01),~
                                                                         ~
               at (09,02), "Minimum Balance",                            ~
               at (09,30), fac(lfac$( 4)), min_bal$             , ch(10),~
                                                                         ~
               at (10,02), "Age Per Date (Disc/Net/Inv)",                ~
               at (10,30), fac(lfac$( 5)), ageper$              , ch(01),~
                                                                         ~
               at (11,02), "Include/Exclude G/L Accts:",                 ~
               at (11,30), fac(lfac$( 6)), acct$(1)             , ch(12),~
               at (11,43), "to",                                         ~
               at (11,46), fac(lfac$( 6)), acct$(2)             , ch(12),~
               at (11,60), "Include/Exclude",                            ~
               at (11,76), fac(lfac$( 6)), acctie$              , ch(01),~
                                                                         ~
                                                                         ~
               at (13,02), "Aging Parameters:",                          ~
               at (13,20), "1)", at(14,20), "2)", at(15,20), "3)",       ~
               at (16,20), "4)", at(17,20), "5)", at(13,30), "Days",     ~
               at (13,23), fac(lfac$( 7)), aging%( 1)       ,pic(-#####),~
               at (14,23), fac(lfac$( 7)), aging%( 2)       ,pic(-#####),~
               at (15,23), fac(lfac$( 7)), aging%( 3)       ,pic(-#####),~
               at (16,23), fac(lfac$( 7)), aging%( 4)       ,pic(-#####),~
               at (17,23), fac(lfac$( 7)), aging%( 5)       ,pic(-#####),~
               at (13,37), fac(lfac$( 7)), aging$( 1)           , ch(20),~
               at (14,37), fac(lfac$( 7)), aging$( 2)           , ch(20),~
               at (15,37), fac(lfac$( 7)), aging$( 3)           , ch(20),~
               at (16,37), fac(lfac$( 7)), aging$( 4)           , ch(20),~
               at (17,37), fac(lfac$( 7)), aging$( 5)           , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (22,20), fac(hex(8c)), pf8$,                           ~
               at (23,20), fac(hex(8c)), pf14$,                          ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104080e0d0f10)), key(keyhit%)


               if keyhit% <> 13 then L40940
                  call "MANUAL" ("ARMTBSUM")
                  goto L40290

L40940:        if keyhit% <> 15 then L40980
                  call "PRNTSCRN"
                  goto L40290

L40980:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return


        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Display Summary Aging Screen.                             *~
            *************************************************************

        do_display
             str(line2$,62%) = "ARMTBSUM: " & str(cms2v$,,8%)
          columnhead1$ = "BILL_TO   BILL_TO NAME                         ~
        ~             TOTAL DUE           "

L42090:  accept                                                          ~
           at (01,02),                                                   ~
           "A/R SUMMARY AGING DISPLAY",                                  ~
           at (01,66), "Today:",                                         ~
           at (01,73), fac(hex(8c)), date$                      , ch(08),~
           at (02,02), fac(hex(ac)), line2$                     , ch(79),~
           at (03,02), fac(hex(94)), errormsg$                  , ch(79),~
           at (04,02), fac(hex(84)), columnhead1$               , ch(79),~
           at (05,03), fac(hex(ac)), hdrs$(1)                   , ch(13),~
           at (05,19), fac(hex(ac)), hdrs$(2)                   , ch(13),~
           at (05,35), fac(hex(ac)), hdrs$(3)                   , ch(13),~
           at (05,52), fac(hex(ac)), hdrs$(4)                   , ch(13),~
           at (05,68), fac(hex(ac)), hdrs$(5)                   , ch(13),~
           at (06,02), fac(hex(84)), cuscode$(1)                , ch(9) ,~
           at (06,12), fac(hex(84)), cusname$(1)                , ch(30),~
           at (06,57), fac(hex(84)), billto_display$(1)         , ch(14),~
           at (07,02), fac(hex(8c)), billto_display$(2)         , ch(14),~
           at (07,18), fac(hex(8c)), billto_display$(3)         , ch(14),~
           at (07,34), fac(hex(8c)), billto_display$(4)         , ch(14),~
           at (07,50), fac(hex(8c)), billto_display$(5)         , ch(14),~
           at (07,67), fac(hex(8c)), billto_display$(6)         , ch(14),~
           at (08,02), fac(hex(84)), cuscode$(2)                , ch(9) ,~
           at (08,12), fac(hex(84)), cusname$(2)                , ch(30),~
           at (08,57), fac(hex(84)), billto_display$(7)         , ch(14),~
           at (09,02), fac(hex(8c)), billto_display$(8)         , ch(14),~
           at (09,18), fac(hex(8c)), billto_display$(9)         , ch(14),~
           at (09,34), fac(hex(8c)), billto_display$(10)        , ch(14),~
           at (09,50), fac(hex(8c)), billto_display$(11)        , ch(14),~
           at (09,67), fac(hex(8c)), billto_display$(12)        , ch(14),~
           at (10,02), fac(hex(84)), cuscode$(3)                , ch(9) ,~
           at (10,12), fac(hex(84)), cusname$(3)                , ch(30),~
           at (10,57), fac(hex(84)), billto_display$(13)        , ch(14),~
           at (11,02), fac(hex(8c)), billto_display$(14)        , ch(14),~
           at (11,18), fac(hex(8c)), billto_display$(15)        , ch(14),~
           at (11,34), fac(hex(8c)), billto_display$(16)        , ch(14),~
           at (11,50), fac(hex(8c)), billto_display$(17)        , ch(14),~
           at (11,67), fac(hex(8c)), billto_display$(18)        , ch(14),~
           at (12,02), fac(hex(84)), cuscode$(4)                , ch(9) ,~
           at (12,12), fac(hex(84)), cusname$(4)                , ch(30),~
           at (12,57), fac(hex(84)), billto_display$(19)        , ch(14),~
           at (13,02), fac(hex(8c)), billto_display$(20)        , ch(14),~
           at (13,18), fac(hex(8c)), billto_display$(21)        , ch(14),~
           at (13,34), fac(hex(8c)), billto_display$(22)        , ch(14),~
           at (13,50), fac(hex(8c)), billto_display$(23)        , ch(14),~
           at (13,67), fac(hex(8c)), billto_display$(24)        , ch(14),~
           at (14,02), fac(hex(84)), cuscode$(5)                , ch(9) ,~
           at (14,12), fac(hex(84)), cusname$(5)                , ch(30),~
           at (14,57), fac(hex(84)), billto_display$(25)        , ch(14),~
           at (15,02), fac(hex(8c)), billto_display$(26)        , ch(14),~
           at (15,18), fac(hex(8c)), billto_display$(27)        , ch(14),~
           at (15,34), fac(hex(8c)), billto_display$(28)        , ch(14),~
           at (15,50), fac(hex(8c)), billto_display$(29)        , ch(14),~
           at (15,67), fac(hex(8c)), billto_display$(30)        , ch(14),~
           at (16,02), fac(hex(84)), cuscode$(6)                , ch(9) ,~
           at (16,12), fac(hex(84)), cusname$(6)                , ch(30),~
           at (16,57), fac(hex(84)), billto_display$(31)        , ch(14),~
           at (17,02), fac(hex(8c)), billto_display$(32)        , ch(14),~
           at (17,18), fac(hex(8c)), billto_display$(33)        , ch(14),~
           at (17,34), fac(hex(8c)), billto_display$(34)        , ch(14),~
           at (17,50), fac(hex(8c)), billto_display$(35)        , ch(14),~
           at (17,67), fac(hex(8c)), billto_display$(36)        , ch(14),~
           at (18,02), fac(hex(84)), cuscode$(7)                , ch(9) ,~
           at (18,12), fac(hex(84)), cusname$(7)                , ch(30),~
           at (18,57), fac(hex(84)), billto_display$(37)        , ch(14),~
           at (19,02), fac(hex(8c)), billto_display$(38)        , ch(14),~
           at (19,18), fac(hex(8c)), billto_display$(39)        , ch(14),~
           at (19,34), fac(hex(8c)), billto_display$(40)        , ch(14),~
           at (19,50), fac(hex(8c)), billto_display$(41)        , ch(14),~
           at (19,67), fac(hex(8c)), billto_display$(42)        , ch(14),~
           at (20,02), fac(hex(84)), cuscode$(8)                , ch(9) ,~
           at (20,12), fac(hex(84)), cusname$(8)                , ch(30),~
           at (20,57), fac(hex(84)), billto_display$(43)        , ch(14),~
           at (21,02), fac(hex(8c)), billto_display$(44)        , ch(14),~
           at (21,18), fac(hex(8c)), billto_display$(45)        , ch(14),~
           at (21,34), fac(hex(8c)), billto_display$(46)        , ch(14),~
           at (21,50), fac(hex(8c)), billto_display$(47)        , ch(14),~
           at (21,67), fac(hex(8c)), billto_display$(48)        , ch(14),~
           at (22,02), fac(hex(a4)), edtmessage2$               , ch(79),~
           at (23,02), fac(hex(8c)), pfdescr$(1)                , ch(79),~
           at (23,65), fac(hex(8c)), pfdescr$(2)                , ch(79),~
                   keys(pfkeys$), key(keyhit%)

             if keyhit% <> 2  then L42420
                if (str(plowkey$,,9) > cus_range$(4)) and (billtos% < 8%)~
                         then goto do_display
                goto display_controller

L42420:      if keyhit% <> 5 then L42790
                init(" ") cuscode$(), cusname$(), billto_display$()
                counter% = 0% : top% = 0%
                goto display_loop

L42790:      if keyhit% <> 10 then L42798
                call "ASKUSER" (2%, "** Trial Balance Summary Aging To"& ~
                           "tals", message$(1), message$(2), message$(3))
                goto L42090

L42798:      if keyhit% <> 13 then L42830
                call "MANUAL" ("ARMTBSUM")
                goto L42090

L42830:      if keyhit% <> 15 then L42870
                call "PRNTSCRN"
                goto L42090

L42870:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%, edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* As Of Date       */~
                                    L50300,         /* Bill-to Range    */~
                                    L50410,         /* Past Due Only?   */~
                                    L50520,         /* Minimum Balance  */~
                                    L50620,         /* Age Per          */~
                                    L50700          /* GL range         */
                  return

L50160
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (asof$)      :  asofu$ = asof$
                call "DATUNFMT" (last_stl$)
                if asof$ <  last_stl$ then errormsg$ =                   ~
        "As Of Date cannot be before last settling date."
                call "DATEFMT" (asof$)
                call "DATEFMT" (last_stl$)
                if edit% = 2% then                                       ~
                     call "ARMDATES" (asofu$, " ", aging%(), 1%, 1%,     ~
                                                           aging$(), 99%)
                return

L50300
*        Bill-to Customer Range                CUS_RANGE$()
            call "TESTRNGE" (cus_range$(1), cus_range$(2),               ~
                             cus_range$(3), cus_range$(4),               ~
                             errormsg$)
            return

L50410
*        Past Due Items Only?                  PASTDUE$
            if pastdue$ = "N" or pastdue$ = "Y" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50520
*        Minimum Balance                       MIN_BAL$
            if min_bal$ = " " then return
                convert min_bal$ to temp%, data goto L50550 : goto L50580
L50550:              errormsg$ = "Leave Blank or enter Minimum Balance" &~
                                 " (-99999999 to +999999999)."
                     return
L50580:         if temp% < -9999999 or temp% > 999999999 then L50550
                convert temp% to min_bal$, pic(-#########)
                return

L50620
*        Age Per                               AGEPER$
            if pos("DIN" = ageper$) > 0% then return
                errormsg$ = "Enter 'D', 'N', or 'I'."
                return

L50700
*        Select G/L Account
            if acct$(1) = "ALL" then L50860
            if (pos("IE" = acctie$) = 0%) then acctie$ = "I"
                call "GLVALID" (acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#4, acct$(1), temp$, 0%, 0, f1%(4))
                if acct$(2) = " " then acct$(2) = acct$(1)
                call "GLVALID" (acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#4, acct$(2), temp$, 0%, 0, f1%(4))
                if acct$(2) < acct$(1) then errormsg$ = "Invalid Range."
                acctu$(1) = acct$(1) : call "GLUNFMT" (acctu$(1))
                acctu$(2) = acct$(2) : call "GLUNFMT" (acctu$(2))
                return
L50860:     acct$(2) = " " : acctu$(1) = "ALL" : acctu$(2) = " "
            acctie$ = "I"
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("ARM004", " ", 0%, 1%)
            end
