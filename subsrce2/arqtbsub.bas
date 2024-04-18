        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR    QQQ   TTTTT  BBBB    SSS   U   U  BBBB    *~
            *  A   A  R   R  Q   Q    T    B   B  S      U   U  B   B   *~
            *  AAAAA  RRRR   Q   Q    T    BBBB    SSS   U   U  BBBB    *~
            *  A   A  R   R  Q Q Q    T    B   B      S  U   U  B   B   *~
            *  A   A  R   R   QQQ     T    BBBB    SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARQTBSUB - Trial Balance Inquiry Subroutine.              *~
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
            * 12/29/86 ! Original                                 ! ERN *~
            * 06/10/88 ! Added aging by source document date.     ! ERN *~
            * 04/13/89 ! Mod to ARMAGING args, added selects      ! RJM *~
            *          !  No functional mods, stubs to handle     !     *~
            *          !  Multi-Currency.                         !     *~
            * 08/15/89 ! Added Multi-Currency visability to screen! MLJ *~
            *          !  displays.                               !     *~
            * 11/16/89 ! Added Currency aging array to argument   ! MJB *~
            *          !  list of ARMAGING.                       !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            * 11/12/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "ARQTBSUB"   (billto$,       /* Bill-to for Inquiry        */~
                          aging%(),      /* Aging Parameters           */~
                          asofu$,        /* As Of Date (unfmtd)        */~
                          ageper$,       /* 'D' Disc, 'N' Net          */~
                          #1,            /* ARMTRIAL Channel           */~
                          #3,            /* CUSTOMER                   */~
                          srcedisp$,     /* OK to Display Srce Dtl?    */~
                          ret%,          /* In/Out Status Flag         */~
                          dtldoc$,       /* Srce Doc for Dtl Display   */~
                          dtlcus$,       /* Customer for Dtl Display   */~
                          dtlsrce$,      /* Source of Doc for Display  */~
                          curr$,         /* Multi-Currency usage flag  */~
                          stat_curr_code$,                               ~
                          acctie$,       /* GL Ranges                  */~
                          acctf1$,       /* GL Ranges                  */~
                          acctf2$)       /* GL Ranges                  */

*        BILLTO$ is the Bill-to Customer for Display.  If not on file
*          the subrtn ends.  If no Trial Balance Records are on file
*          an ASKUSER is issued and the subrtn ends.
*
*        AGING%() contains the aging parameters for the subroutine
*          ARMAGING.
*
*        ASOFU$ is the As Of date (unformatted) for the display and
*          agings.
*
*        AGEPER$ is 'D' to age per Discount Due Date, 'N' to age per
*          Net Due Date, or 'I' to age per source document date.
*
*        SRCEDISP$ is 'Y' if the detail display of invoices and checks
*          is allowed else 'N'.  The caller must handle the pass thru
*          to the appropriate subroutines for the detail display.
*
*        RET% is the incomming and outgoing status flag.
*          IN  - 0%  Initial Call (first time for bill-to).
*                1%  Return from Detail Display.
*
*        DTLDOC$ is the source document number for detail display.
*
*        DTLCUS$ is the Customer of the Source Doc for Detail Display.
*
*        DTLSRCE$ is the source of the Source Doc for Detail Display
*          (I or C for Invoice or Check).
*        MULTI-CURRENCY:  CURR$ is the multi-currency flag.  "Y" if on,
*           "N" if off.   STAT_CURR_CODE$ is the statutory currency code,
*           all blanks if CURR$ = "N".


        dim                                                              ~
            acctf1$12, acctf2$12,        /* GL Ranges                  */~
            acctu1$12, acctu2$12,        /* GL Ranges                  */~
            acctie$1, acct$9,            /* GL Ranges                  */~
            ageper$1,                    /* D=Disc  N=Net Due  I=Invce */~
            aging(9),                    /* Aged Balances              */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            aging_lit$35,                /* STATUTORY Values Literal   */~
            aging_msg$79,                /* Aging Screen Message       */~
            asof$8, asofu$6,             /* As Of Date                 */~
            bals(3),                     /* Balances - STATUTORY       */~
            cbals(3),                    /* Balances - TRANSACTION     */~
            cage$(50)76,                 /* Currency Aging array       */~
            billto$9, billtoname$30,     /* Bill-to Code and Name      */~
            bol$3,                       /* Bill-of-Lading Number      */~
            curr$1,                      /* Multi-Currency Usage Flag  */~
            currency$4,                  /* ARMCBLNC - Returned        */~
            currkey$21,                  /* ARMTBC Prime Key           */~
            currcex$25,                  /* ARMTBCEX Alt Key           */~
            cursor%(2),                  /* Cursor location for edit   */~
            cdate$6,                     /* ARMCBLNC - Returned        */~
            date$8,                      /* Date for screen display    */~
            disc(2),                     /* Cash Disc Percents         */~
            discdue$(2)8,                /* Cash Disc Due Dates        */~
            disp$(14)79,                 /* TB - STATUTORY detail      */~
            dispidx$(14)30,              /* Index for above            */~
            docdate$8,                   /* Source Doc Date            */~
            dtlcus$9, dtldoc$8,          /* Detail Display Parameters  */~
                      dtlsrce$1,         /*                            */~
            errormsg$79,                 /* Error message              */~
            grace%(2),                   /* Grace Days Allowed         */~
            hdr1$(9)14, hdr2$(2)16,      /* Screen Column Headings     */~
                        hdr3$(4)20,      /*                            */~
            highardate$8,                /* High A/R Date              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inv_curr$4,                  /* ARMTBCRC Invoice Currency  */~
            lastchk$8, lastinv$8,        /* Last transaction dates     */~
            lastchg$20, lastuser$3,      /* Last Change Made On & By   */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            mcdisp$(14)79,               /* TB - TRANSACTION Detail    */~
            mc_display$1,                /* TRANSACTION Display Flag   */~
            netdue$(2)8,                 /* Net Due Dates              */~
            pct(5),                      /* Percents                   */~
            pf$(3)79, pfkeys$20,         /* PF Literals and Keys       */~
            po$(2)16,                    /* PO Numbers                 */~
            postdate$8,                  /* Post Date                  */~
            readkey$50,                  /* A Read Key                 */~
            sd_literal$35,               /* Settlement Display Literal */~
            sd_tcode$4,                  /* Settlement TRANSACTION Code*/~
            shipto$9, shiptoname$30,     /* Ship-to Customer Info      */~
            so$20,                       /* Sales Order and BOL        */~
            srcedoc$8,                   /* Summary Aging              */~
            srcedisp$1,                  /* Ok to Display Details?     */~
            stat_curr_code$4,            /* STATUTORY Currency Code    */~
            stlmnt$14,                   /* TB Detail                  */~
            store$30,                    /* Shipping Store             */~
            summary$1,                   /* Report Summary Option      */~
            tbdisp$(14)79,               /* TB display detail lines    */~
            tbkey$30,                    /* Trial Balance Plowkey      */~
            tran_curr_code$4,            /* TRANSACTION Currency Code  */~
            type$30                      /* Srce Doc Srce & Type       */


        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * # 1 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * # 3 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #9  ! SYSFILE2 ! System File                              *~
            * #42 ! ARMTBCRC ! Multi-Currency Invoice Settlement        *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #9,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 20

            select #42, "ARMTBCRC",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #44, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #45, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#45, fs%(45), f2%(45), 0%, rslt$(45))

            if curr$ = "N" then goto L09000  /* Multi-currency not used */ ~

            call "OPENCHCK" (#42, fs%(42), f2%(42), 0%, rslt$(42))
            call "OPENCHCK" (#44, fs%(44), f2%(44), 0%, rslt$(44))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            aging_lit$ = "*** ALL VALUES ARE IN STATUTORY ***"

            acctu1$ = acctf1$
            acctu2$ = acctf2$
            if acctf1$ = "ALL" then L09070
               call "GLUNFMT" (acctu1$)
               call "GLUNFMT" (acctu2$)

L09070: if ret% = 1% then main_screen

            date$ = date    :  call "DATEFMT" (date$)
            asof$ = asofu$  :  call "DATEFMT" (asof$)
            coneqv, conunt = 0
            call "READ100" (#3, billto$, f1%(3))
            if f1%(3) = 0% then exit_program
                    get #3 using L09160, billtoname$, crlimit
L09160:              FMT POS(10), CH(30), POS(526), PD(14,4),            ~
                         PD(14,4), POS(733), PD(14,4), XX(8), 2*PD(14,4),~
                         CH(6)
            lastinv$, lastchk$, highardate$ = " "               /* JIC */
            open_orders, openar, highar = 0                     /* JIC */
            call "READ100" (#45, billto$, f1%(45%))
            if f1%(45%) <> 0% then get #45 using L09186, lastinv$,         ~
                lastchk$, highardate$, open_orders, openar, highar
L09186:              FMT POS(90), 2*CH(6), XX(6), CH(6), PD(14,4), XX(8),~
                          2*PD(14,4)
                exposure = open_orders + openar
                call "DATEFMT" (lastinv$)
                call "DATEFMT" (lastchk$)
                call "DATEFMT" (highardate$)

            tbkey$ = str(billto$,,9) & hex(00)
            call "PLOWNEXT" (#1, tbkey$, 9%, f1%(1))
            if f1%(1) = 1% then L09310
                call "ASKUSER" (2%, "TRIAL BALANCE INQUIRY",             ~
                          "There are no open items for the Bill-to.",    ~
                          " ", "Press any PF Key to continue...")
                goto exit_program
L09310:     line2$ = "Bill-to: " & billto$ & "  " & billtoname$
            str(line2$,62%) = "ARQTBSUB: " & str(cms2v$,,8%)

            hdr1$(1) = "Settlement No."  :  hdr2$(1) = "Current Values"
            hdr1$(2) = "Document"        :  hdr2$(2) = "Original Values"
            hdr1$(3) = "Srce"
            hdr1$(4) = "Type"            :  hdr3$(1) = " Days"
            hdr1$(5) = "Doc Date"        :  hdr3$(2) = "  Range in Dates"
            hdr1$(6) = "Disc%"           :  hdr3$(3) = "B a l a n c e s"
            hdr1$(7) = "Disc Due"        :  hdr3$(4) = "% of Total"
            hdr1$(8) = " Net Due"
            hdr1$(9) = "    Amount"

            goto first_screen

        REM *************************************************************~
            *                  M A I N   S C R E E N                    *~
            *-----------------------------------------------------------*~
            * Handles TB summary listing screen.                        *~
            *************************************************************

        main_screen
            inpmessage$ = "Position Cursor and Press Return to see"  &   ~
                          " Trial Balance Detail."
            ret% = 0%
            gosub'101(1%)
            errormsg$ = " "
                if keyhit%  =  0% then show_tb_detail
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then find_stlmnt
                if keyhit%  = 10% then summary_aging
                if keyhit%  = 11% then change_dtl_level
                if keyhit%  = 12% then show_srce_doc
                if keyhit%  = 14% then print_report
                if keyhit%  = 16% then exit_program
                if keyhit%  = 32% then exit_program
                goto main_screen

        first_screen
            tbkey$ = str(billto$,,9) & hex(00)
            first% = 1%
            goto L10300
        next_screen
            first% = 0%
L10300:     gosub load_summary
            goto  main_screen

        change_dtl_level
            summary% = summary% + 1%
            if summary% = 2% then summary% = 0%
            goto first_screen

        find_stlmnt
            stlmnt$ = " "
            gosub'101(2%)
                if keyhit%  =  1% then main_screen
                     tbkey$ = str(billto$,,9) & stlmnt$ & hex(00)
                     goto next_screen


        show_tb_detail
            i% = cursor%(1) - 5%
            if i% < 1% or i% > dispidx% then main_screen
            readkey$ = str(billto$,,9) & str(dispidx$(i%))
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then main_screen

            get #1 using L10590, stlmnt$, disc(1), discdue$(1), grace%(1),~
                                netdue$(1), po$(1), lastuser$, lastchg$, ~
                                amt, type$, srcedoc$, postdate$,         ~
                                docdate$, shipto$, so$, bol$, store$,    ~
                                disc(2), discdue$(2), grace%(2),         ~
                                netdue$(2), po$(2), tran_curr_code$
L10590:         FMT XX(9), CH(12), PD(14,4), CH(6), BI(1), CH(6), CH(16),~
                    CH(3), CH(6), PD(14,4), XX(11), CH(2), CH(8), CH(6), ~
                    CH(6), CH(9), CH(16), 2*CH(3), PD(14,4), CH(6),      ~
                    BI(1), CH(6), CH(16), CH(4)
            amt = round(amt, 2)
            stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) & "-" &   ~
                                              str(stlmnt$,11)
            call "DESCRIBE" (#3, shipto$, shiptoname$, 0%, f1%(3))
            if type$ = "II" then type$ = "Invoice                       "
            if type$ = "IA" then type$ = "Invoice Adjustment            "
            if type$ = "IC" then type$ = "Credit Memo                   "
            if type$ = "IF" then type$ = "Finance Charge                "
            if type$ = "CP" then type$ = "Payment                       "
            if type$ = "CA" then type$ = "Cash Adjustment               "
            if type$ = "CB" then type$ = "Balance Forward Payment       "
            if type$ = "CD" then type$ = "Balance Forward Distribution  "
            if type$ = "CU" then type$ = "Unapplied Payment             "
            if store$ <> " " then store$ = "Issued from Store " & store$
            call "DATEFMT" (docdate$)
            call "DATEFMT" (postdate$)
            call "DATEFMT" (discdue$(1))
            call "DATEFMT" (discdue$(2))
            call "DATEFMT" (netdue$(1))
            call "DATEFMT" (netdue$(2))
            call "DATEFMT" (lastchg$)
            if lastchg$ <> " " then lastchg$ = lastchg$ & " by " &       ~
                                    lastuser$
            if bol$ <> " " then so$ = so$ & "-" & bol$


            if curr$ = "N" then L10870
               str(currcex$) = str(tran_curr_code$) & str(readkey$,1,21)
               call "REDALT0" (#44, currcex$, 1%, f1%(44))
               if f1%(44) <> 1% then L10780
                   get #44 using L10777, sd_tcode$, sd_tamt
L10777:               FMT CH(4), POS(26), PD(14,4)
                   sd_tamt = round(sd_tamt, 2)
                   goto L10870
L10780:        str(currkey$) = str(readkey$,1,21)
               call "READ100" (#42, currkey$, f1%(42))
               if f1%(42) <> 1% then L10800
                   get #42 using L10793, sd_tcode$, sd_tamt
L10793:               FMT CH(4), POS(26), PD(14,4)
                   sd_tamt = round(sd_tamt, 2)
                   goto L10870
L10800:        sd_tcode$ = str(stat_curr_code$)
               sd_tamt   = amt

L10870:     inpmessage$ = "Press PF-16 to Return to Trial Balance"  &    ~
                          " Listing."
L10890:     gosub'102
                if keyhit%  =  0% then main_screen
                if keyhit%  = 16% then main_screen
                if keyhit%  = 32% then exit_program
                goto L10890


        summary_aging
            inpmessage$ = "Press PF-16 to Return to Trial Balance" &     ~
                          " Listing."
            call "SHOSTAT" ("Aging Bill-to " & billto$)
            call "ARMDATES" (asofu$, "A/R TRIAL BALANCE INQUIRY",        ~
                             aging%(), 1%, 0%, aging$(), 99%)

*                   ARMAGING now handles multicurrency. But the 2nd to
*                   last arg is 0% and this causes the sub to work in
*                   a NON-Multi-Currency mode. See ARMAGING for futher
*                   details & see ARMSTMNT for example of use where
*                   Customers being invoiced w/ more than 1 currency
*                   can have each currency type accounted/totaled
*                   separately.

            call "ARMAGING" (billto$, " ", asofu$, ageper$, aging%(),    ~
                  #1, aging(), #44, #9, 0%, currency$, cage$(),          ~
                  acctie$, acctu1$, acctu2$)

            if ageper$ = "D" then aging_msg$ = "CASH DISCOUNT DUE"
            if ageper$ = "N" then aging_msg$ = "NET DUE"
            if ageper$ = "I" then aging_msg$ = "SOURCE DOCUMENT"
            aging_msg$ = "SUMMARY AGING PER " & aging_msg$ &             ~
                         " DATE, AS OF "  & asof$      & ":"
            mat pct = zer
            if aging(8) = 0 then L11160
                for a% = 1% to 5%
                     pct(a%) = round((100 * aging(a%))/aging(8), 2)
                next a%


L11160:     gosub'103                   /* Display Screen - No Entry   */
                if keyhit%  =  0% then main_screen
                if keyhit%  = 16% then main_screen
                if keyhit%  = 32% then exit_program
                goto L11160


        show_srce_doc
            i% = cursor%(1) - 5%
            if i% < 1% or i% > dispidx% then main_screen
            dtldoc$  = str(dispidx$(i%),13,8)
            dtlcus$  = str(dispidx$(i%),21,9)
            dtlsrce$ = str(dispidx$(i%),30,1)
            ret%     = 1%
            end  /* Caller handles linkage */


        print_report
            summary$ = "D" : if summary% = 1% then summary$ = "S"
            call "ARMTBDSB" ("ARQTBSUB", "ARM008", asofu$, billto$,      ~
                             billto$, summary$, " ", "N", " ", #1, #3,   ~
                             acctie$, acctf1$, acctf2$)
            goto main_screen


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
            *                L O A D   S U M M A R Y                    *~
            *-----------------------------------------------------------*~
            * Loads data for summary screen display.                    *~
            *************************************************************
        load_summary
            call "PLOWNEXT" (#1, tbkey$, 9%, eof%)
            if eof% = 1% then L30100
                errormsg$ = "End of Trial Balance reached."
                return
L30100:     init(" ") disp$(), dispidx$(), mcdisp$(), tbdisp$()
            dispidx%, i% = 0%
            goto L30230

        tb_loop
            if dispidx% = 14% then return

            call "PLOWNEXT" (#1, tbkey$, 9%, eof%)
            if eof% = 1% then L30230
                if dispidx% = 14% then return
                     disp$(i%+1%), mcdisp$(i%+1%) = "*END OF LIST"
                     return

L30230:     if summary% = 1% and str(tbkey$,20,2) <> "00" then L30256
            get #1 using L30250, acct$, postdate$
L30250:         FMT POS(76), CH(9), POS(97), CH(6)
            if str(tbkey$,20,2) <> "00" then L30270
            if acctf1$ = "ALL" then L30270
               if acctie$ = "I" then L30258
                  if acct$ < acctu1$ then L30270
                  if acct$ > acctu2$ then L30270
L30256:              str(tbkey$,20,2) = hex(ffff)
                     goto tb_loop
L30258:           if acct$ < acctu1$ then L30256
                  if acct$ > acctu2$ then L30256

L30270:     if postdate$ > asofu$ then tb_loop

            dispidx%, i% = dispidx% + 1%
            get #1 using L30330, dispidx$(i%), disc, str(disp$(i%),52,6), ~
                          str(disp$(i%),61,6), amt, str(disp$(i%),26,1), ~
                          str(disp$(i%),31,1), str(dispidx$(i%),13),     ~
                          postdate$, str(disp$(i%),36,8), tran_curr_code$
L30330:         FMT POS(10), CH(12), PD(14,4), CH(6), XX(1), CH(6),      ~
                    POS(68), PD(14,4), POS(87), 2*CH(1), CH(8), 2*CH(6), ~
                    POS(177), CH(4)
                amt = round(amt, 2)
            str(disp$(i%), 1,14) = str(dispidx$(i%), 1,8) & "-" &        ~
                                   str(dispidx$(i%), 9,2) & "-" &        ~
                                   str(dispidx$(i%),11,2)
            str(dispidx$(i%),21,9) = billto$
            if str(disp$(i%),26,1) = "I" then                            ~
                get #1 using L30420, str(dispidx$(i%),21,9) /* Ship-to  */
L30420:              FMT POS(109), CH(9)
            str(dispidx$(i%),30,1) = str(disp$(i%),26,1)
            if i% = 1% then L30490
                if str(dispidx$(i%),,10) = str(dispidx$(i%-1%),,10)      ~
                                          then str(disp$(i%), 1,12) = " "
                if str(dispidx$(i%),, 8) = str(dispidx$(i%-1%),, 8)      ~
                                          then str(disp$(i%), 1, 9) = " "
L30490:     str(disp$(i%),17, 8) = str(dispidx$(i%),13,8)
            if str(disp$(i%),26,1) = "I" then str(disp$(i%),26,4)= "Inv "
            if str(disp$(i%),26,1) = "C" then str(disp$(i%),26,4)= "Cash"
                on pos("ICFAPUBD" = str(disp$(i%),31,1)) gosub L30550,    ~
                     L30560, L30570, L30580, L30590, L30600, L30610, L30620
                goto L30630
L30550:              str(disp$(i%),31,4) = "Invc"  :  return
L30560:              str(disp$(i%),31,4) = "CM  "  :  return
L30570:              str(disp$(i%),31,4) = "FC  "  :  return
L30580:              str(disp$(i%),31,4) = "Adj "  :  return
L30590:              str(disp$(i%),31,4) = "Pay "  :  return
L30600:              str(disp$(i%),31,4) = "Unap"  :  return
L30610:              str(disp$(i%),31,4) = "BF  "  :  return
L30620:              str(disp$(i%),31,4) = "Dist"  :  return
L30630:     call "DATEFMT" (str(disp$(i%),36,8))
            call "DATEFMT" (str(disp$(i%),52,8))
            call "DATEFMT" (str(disp$(i%),61,8))
            call "CONVERT" (disc, 2.2, str(disp$(i%),45, 6))
            call "CONVERT" (amt , 2.2, str(disp$(i%),70,10))
            if curr$ = "N" then L30800
                mcdisp$(i%) = disp$(i%)
                str(currcex$) = str(tran_curr_code$) & str(tbkey$)
                call "REDALT0" (#44, currcex$, 1%, f1%(44))
                if f1%(44) <> 1% then L30700
                    get #44 using L30693, str(mcdisp$(i%),26,4), mcamt
L30693:                 FMT CH(4), POS(26), PD(14,4)
                    call "CONVERT" (mcamt, 2.2, str(mcdisp$(i%),70,10))
                    goto L30800
L30700:        str(currkey$) = str(tbkey$)
               call "READ100" (#42, currkey$, f1%(42))
               if f1%(42) <> 1% then L30796
                  get #42 using L30741, inv_curr$, mcamt
L30741:              FMT CH(04), POS(26), PD(14,4)
                  str(mcdisp$(i%),26,4) = inv_curr$
                  call "CONVERT" (mcamt, 2.2, str(mcdisp$(i%),70,10))
                  goto L30800

L30796:     str(mcdisp$(i%),26,4) = stat_curr_code$

L30800:     if summary% = 0% then tb_loop
                call "ARMCBLNC" (billto$, str(dispidx$(i%),,10), asofu$, ~
                                 10%, "N", #1, #3, bals(), #44, #42, #9, ~
                                 currency$, cdate$, coneqv, conunt,      ~
                                 cbals())
                call "CONVERT" (bals(1), 2.2, str(disp$(i%),70,10))
                call "CONVERT" (cbals(1), 2.2, str(mcdisp$(i%),70,10))
                str(tbkey$,20,2) = hex(ffff)
                goto tb_loop


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(opt%)   /* OPT% is 1 for Display; 2 for Settlement # */
            if summary% = 0% then hdr1$(9) = "    Amount" else           ~
                                  hdr1$(9) = "   Balance"
            init(hex(8e)) lfac$()  :  lfac$(15) = hex(9c)
            if opt% = 1% then L40140
                gosub setpf1b
                init(hex(8c)) lfac$() : lfac$(15) = hex(81)
                goto L40163
L40140:     for i% = 1% to dispidx%
                if str(disp$(i%),13,2) = "00" then lfac$(i%) = hex(86)
            next i%
            gosub setpf1a

L40163:     mc_display$ = "N"
*        Trial Balance Setup - STATUTORY Currency
            hdr1$(3) = "Srce"
            mat tbdisp$ = disp$
            goto L40190
L40168
*        Trial Balance Setup - TRANSACTION Currency
            hdr1$(3) = "Curr"
            mat tbdisp$ = mcdisp$

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: TRIAL BALANCE LISTING",    ~
               at (01,66), "As Of:",                                     ~
               at (01,73), fac(hex(8c)), asof$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr1$(1)               , ch(14),~
               at (04,18), "---SOURCE DOCUMENT INFO.---",                ~
               at (05,18), fac(hex(ac)), hdr1$(2)               , ch(08),~
               at (05,27), fac(hex(ac)), hdr1$(3)               , ch(04),~
               at (05,32), fac(hex(ac)), hdr1$(4)               , ch(04),~
               at (05,37), fac(hex(ac)), hdr1$(5)               , ch(08),~
               at (04,47), "--ADJUSTED PAY TERMS--",                     ~
               at (05,47), fac(hex(ac)), hdr1$(6)               , ch(05),~
               at (05,53), fac(hex(ac)), hdr1$(7)               , ch(08),~
               at (05,62), fac(hex(ac)), hdr1$(8)               , ch(08),~
               at (05,71), fac(hex(ac)), hdr1$(9)               , ch(10),~
                                                                         ~
               at (06,02), fac(lfac$( 1)), tbdisp$( 1)          , ch(79),~
               at (07,02), fac(lfac$( 2)), tbdisp$( 2)          , ch(79),~
               at (08,02), fac(lfac$( 3)), tbdisp$( 3)          , ch(79),~
               at (09,02), fac(lfac$( 4)), tbdisp$( 4)          , ch(79),~
               at (10,02), fac(lfac$( 5)), tbdisp$( 5)          , ch(79),~
               at (11,02), fac(lfac$( 6)), tbdisp$( 6)          , ch(79),~
               at (12,02), fac(lfac$( 7)), tbdisp$( 7)          , ch(79),~
               at (13,02), fac(lfac$( 8)), tbdisp$( 8)          , ch(79),~
               at (14,02), fac(lfac$( 9)), tbdisp$( 9)          , ch(79),~
               at (15,02), fac(lfac$(10)), tbdisp$(10)          , ch(79),~
               at (16,02), fac(lfac$(11)), tbdisp$(11)          , ch(79),~
               at (17,02), fac(lfac$(12)), tbdisp$(12)          , ch(79),~
               at (18,02), fac(lfac$(13)), tbdisp$(13)          , ch(79),~
               at (19,02), fac(lfac$(14)), tbdisp$(14)          , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (22,41), fac(lfac$(15)), stlmnt$              , ch(10),~
                     keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L40610

               if keyhit% <>  7 then L40610
                  if mc_display$ = "Y" then L40600
                      mc_display$ = "Y"
                      str(pf$(3),1,12) = "(7)Stat Curr"
                      goto L40168
L40600:           str(pf$(3),1,12) = "(7)Tran Curr"
                  goto L40163

L40610:        if keyhit% <> 13 then L40650
                  call "MANUAL" ("ARQTBSUB")
                  goto L40190

L40650:        if keyhit% <> 15 then L40690
                  call "PRNTSCRN"
                  goto L40190

L40690:        close ws  :  u3% = u3%
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf1a
           pf$(1) = "(2)First Screen   ( 8)Find Settlement             "&~
                    "             (13)Instructions"
           pf$(2) = "(5)Next Screen    (10)Summary Aging           (12)"&~
                    "Show Doc Dtl (15)Print Screen"
               if curr$ = "N" then                                       ~
           pf$(3) = "                  (11)Show Settlement Detail  (14)"&~
                    "Print Report (16)Exit Display"                      ~
               else                                                      ~
           pf$(3) = "(7)Tran Curr      (11)Show Settlement Detail  (14)"&~
                    "Print Report (16)Exit Display"
           pfkeys$ = hex(0102ffff05ff0708ff0a0b0c0d0e0f10ffff2000)
           if curr$ = "Y" then L40775  : str(pf$(3), 1,12)  = " "
                                        str(pfkeys$, 7, 1) = hex(ff)
L40775:    if first% = 0% then L40785  : str(pf$(1), 1,16)  = " "
                                        str(pfkeys$, 2, 1) = hex(ff)
L40785:    if eof%   = 1% then L40795  : str(pf$(2), 1,16)  = " "
                                        str(pfkeys$, 5, 1) = hex(ff)
L40795:    if srcedisp$ = "Y" then L40810
                                        str(pf$(2),47,16)  = " "
                                        str(pfkeys$,12, 1) = hex(ff)
L40810:    if summary% = 0% then str(pf$(3),23,24) = "Show Item Balances"
           return

        setpf1b
           pf$(1) = "(1)Return to Display  Find Settlement             "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102
            if curr$ = "N" then L41064
               sd_literal$ = "---Statutory---   --Transaction---"
               lfac$(10) = hex(84)  :  goto L41080
L41064:        sd_literal$, sd_tcode$ = " "  :  lfac$(10) = hex(9c)

L41080:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: SETTLEMENT DETAIL",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Settlement Number",                          ~
               at (04,30), fac(hex(84)),   stlmnt$              , ch(14),~
               at (05,02), "Ship-to Customer",                           ~
               at (05,30), fac(hex(84)),   shipto$              , ch(09),~
               at (05,49), fac(hex(84)),   shiptoname$          , ch(30),~
               at (06,02), "Source Document",                            ~
               at (06,30), fac(hex(84)),   srcedoc$             , ch(08),~
               at (06,49), fac(hex(84)),   type$                , ch(30),~
               at (07,02), "Source Document Date",                       ~
               at (07,30), fac(hex(84)),   docdate$             , ch(08),~
               at (07,49), fac(hex(84)),   store$               , ch(30),~
               at (08,02), "Date Posted",                                ~
               at (08,30), fac(hex(84)),   postdate$            , ch(08),~
               at (09,02), "Sales Order - BOL",                          ~
               at (09,30), fac(hex(84)),   so$                  , ch(20),~
               at (10,31), fac(hex(84)),   sd_literal$          , ch(34),~
               at (11,02), "Transaction Amount",                         ~
               at (11,31), fac(hex(84)),   amt    , pic(-###,###,###.##),~
               at (11,50), fac(lfac$(10)), sd_tamt, pic(-###,###,###.##),~
               at (11,66), fac(hex(84)),   sd_tcode$            , ch(04),~
                                                                         ~
               at (13,30), fac(hex(ac)),   hdr2$(1)             , ch(16),~
               at (13,49), fac(hex(ac)),   hdr2$(2)             , ch(16),~
               at (14,02), "Terms: Cash Discount %",                     ~
               at (14,30), fac(hex(84)),   disc(1)       , pic(-####.##),~
               at (14,49), fac(hex(84)),   disc(2)       , pic(-####.##),~
               at (15,02), "       Discount Due",                        ~
               at (15,30), fac(hex(84)),   discdue$(1)          , ch(08),~
               at (15,49), fac(hex(84)),   discdue$(2)          , ch(08),~
               at (16,02), "       Grace Days",                          ~
               at (16,30), fac(hex(84)),   grace%(1),      pic(-#######),~
               at (16,49), fac(hex(84)),   grace%(2),      pic(-#######),~
               at (17,02), "       Net Due Date",                        ~
               at (17,30), fac(hex(84)),   netdue$(1)           , ch(08),~
               at (17,49), fac(hex(84)),   netdue$(2)           , ch(08),~
               at (18,02), "Customer PO Number",                         ~
               at (18,30), fac(hex(84)),   po$(1)               , ch(16),~
               at (18,49), fac(hex(84)),   po$(2)               , ch(16),~
               at (19,02), "Last Changed",                               ~
               at (19,30), fac(hex(84)),   lastchg$             , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Display",                           ~
                     keys(hex(000d0f1020)),  key(keyhit%)

               if keyhit% <> 13 then L41640
                  call "MANUAL" ("ARQTBSUB")
                  goto L41080

L41640:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41080

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103

            if curr$ = "N" then lfac$(18) = hex(9c) else                 ~
                                lfac$(18) = hex(84)

L42080:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: AGING SUMMARY",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), aging_msg$             , ch(79),~
               at (05,08), fac(hex(ac)), hdr3$(1)               , ch(06),~
               at (06,08), fac(hex(84)), aging%(1)        , pic(-#####), ~
               at (07,08), fac(hex(84)), aging%(2)        , pic(-#####), ~
               at (08,08), fac(hex(84)), aging%(3)        , pic(-#####), ~
               at (09,08), fac(hex(84)), aging%(4)        , pic(-#####), ~
               at (10,08), fac(hex(84)), aging%(5)        , pic(-#####), ~
                                                                         ~
               at (05,16), fac(hex(ac)), hdr3$(2)               , ch(20),~
               at (06,16), fac(hex(84)), aging$(1)              , ch(20),~
               at (07,16), fac(hex(84)), aging$(2)              , ch(20),~
               at (08,16), fac(hex(84)), aging$(3)              , ch(20),~
               at (09,16), fac(hex(84)), aging$(4)              , ch(20),~
               at (10,16), fac(hex(84)), aging$(5)              , ch(20),~
                                                                         ~
               at (05,41), fac(hex(ac)), hdr3$(3)               , ch(15),~
               at (06,41), fac(hex(84)), aging(1) , pic(-###,###,###.##),~
               at (07,41), fac(hex(84)), aging(2) , pic(-###,###,###.##),~
               at (08,41), fac(hex(84)), aging(3) , pic(-###,###,###.##),~
               at (09,41), fac(hex(84)), aging(4) , pic(-###,###,###.##),~
               at (10,41), fac(hex(a4)), aging(5) , pic(-###,###,###.##),~
               at (11,30), "Total Due:",                                 ~
               at (11,41), fac(hex(84)), aging(8) , pic(-###,###,###.##),~
                                                                         ~
               at (05,60), fac(hex(ac)), hdr3$(4)               , ch(10),~
               at (06,62), fac(hex(84)), pct(1)           , pic(-###.##),~
               at (07,62), fac(hex(84)), pct(2)           , pic(-###.##),~
               at (08,62), fac(hex(84)), pct(3)           , pic(-###.##),~
               at (09,62), fac(hex(84)), pct(4)           , pic(-###.##),~
               at (10,62), fac(hex(84)), pct(5)           , pic(-###.##),~
                                                                         ~
               at (12,02), fac(lfac$(18)), aging_lit$           , ch(35),~
               at (13,02), "BILL-TO CUSTOMER RECAP AS OF TODAY:",        ~
               at (14,08), "Total Open Order Dollars",                   ~
               at (14,37), fac(hex(84)),open_orders,pic(-###,###,###.##),~
               at (15,08), "Total Accounts Receivable",                  ~
               at (15,37), fac(hex(a4)), openar    ,pic(-###,###,###.##),~
               at (16,08), "      Credit Exposure",                      ~
               at (16,37), fac(hex(84)), exposure  ,pic(-###,###,###.##),~
               at (16,55), "Credit Limit",                               ~
               at (16,68), fac(hex(84)), crlimit     ,  pic(###,###,###),~
                                                                         ~
               at (18,08), "High A/R Balance on",                        ~
               at (18,28), fac(hex(84)), highardate$            , ch(08),~
               at (18,37), fac(hex(84)), highar    ,pic(-###,###,###.##),~
               at (18,55), "Last Invoice:",                              ~
               at (18,69), fac(hex(84)), lastinv$               , ch(08),~
               at (19,55), "Last Cash   :",                              ~
               at (19,69), fac(hex(84)), lastchk$               , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Display",                           ~
                     keys(hex(000d0f1020)), key(keyhit%)

               if keyhit% <> 13 then L42740
                  call "MANUAL" ("ARQTBSUB")
                  goto L42080

L42740:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42080


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
            end
