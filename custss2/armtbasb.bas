        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT  BBBB    AAA    SSS   BBBB    *~
            *  A   A  R   R  MM MM    T    B   B  A   A  S      B   B   *~
            *  AAAAA  RRRR   M M M    T    BBBB   AAAAA   SSS   BBBB    *~
            *  A   A  R   R  M   M    T    B   B  A   A      S  B   B   *~
            *  A   A  R   R  M   M    T    BBBB   A   A   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTBASB - Prints Detail Trial Balance Report (aged).     *~
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
            * 06/30/87 ! Original                                 ! ERN *~
            * 02/09/88 ! UPDATE DOCUMENT DATE TO USE CORRECT DATE ! BPN *~
            * 06/09/88 ! Added aging by invoice date              ! ERN *~
            * 04/13/89 ! Mod to ARMAGING args, added selects      ! RJM *~
            *          !  No functional mods, stubs to handle     !     *~
            *          !  Multi-Currency.                         !     *~
            * 11/16/89 ! Added Currency aging array to ARMAGING   ! MJB *~
            *          !  argument list.                          !     *~
            * 10/28/91 ! PRR 12140.  Added call to GLUNFMT so that! JDH *~
            *          !  range would work with 12 char. accts.   !     *~
            *          ! PRR 11016.  Added option to include zero !     *~
            *          !  balance transactions.                   !     *~
            *          ! PRR 11148.  Added account range.         !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            * 09/11/91 ! MOD FOR PO TO REPORT                     ! RHH *~
            * 03/27/98 ! Mod to Condition for R6.04.03 Used the   ! RHH *~
            *          !    most current version of Caelus Sub(EWD!     *~
            * 07/25/01 ! (EWD001)  Mod to add text from Atrium to ! CMG *~
            *          !           end of rpt.     - TURNED OFF   !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARMTBASB"   (caller$,       /* Calling Program ID         */~
                          rptid$,        /* Report ID                  */~
                          asof$,         /* Report As Of date          */~
                          ageper$,       /* Age Per Date (Due/Net/Inv) */~
                          aging%(),      /* Aging Parameters           */~
                          cus_from$,     /* Bill-to Range- From        */~
                          cus_to$,       /*                To          */~
                          summary$,      /* Summary Option (D/S)       */~
                          types$,        /* Selected Doc Srces/Types   */~
                          pastdue$,      /* Past Due List Parameters   */~
                          min_bal$,      /* Min Balance Parameters     */~
                          acctu$(),      /* Selected A/R Account       */~
                          acctie$,       /* Selected A/R Acct Inc/Excl */~
                          #1, #2, #3)    /* ARMTRIAL, CUSTOMER, GLMAIN */

*        AS OF date may be supplied in either 6 or 8 characters.
*
*        AGEPER$ is 'D' to Age from Discount Due Date -or- 'N' to age
*          per the New Due Date -or- 'I' to age per Source Doc Date.
*
*        AGING%() contains the aging parameters from ARMAGING.
*
*        CUSTOMER RANGE is valid display parameters per TESTRNGE; i.e.,
*          ALL, FIRST and LAST may be supplied.
*
*        TYPES is a string of two character entries: Source then Type.
*          If passed blank the all sources and types are assumed.
*
*        PAST DUE is passed as 'N' if the option is to be ignored.
*          Otherwise the number of days that an item must be past due
*          to list on the report is supplied.
*
*        MINIMUM BALANCE is passed as blanks if option is to be ignored.
*          Otherwise the field contains a valid numeric.
*
*        If ACCTU$ is non-blank then only items posted to that G/L
*          account will be listed.

        dim                                                              ~
            acct$9,                      /* Account number from TB file*/~
            acctf$(2)12,                 /* Selected Account for Print */~
            acctu$(2)12,                 /* Selected Account Number    */~
            acctie$1,                    /* Selected Account Inc/Excl  */~
            acct_descr$(2)30,            /*                            */~
            ageper$1,                    /* Disc, Net, or Invoice      */~
            aging%(10),                  /* Aging Parameters           */~
            amt$13, amthdr$11,           /* Trans/Totals Print Amount  */~
            asof$8, asoffmtd$8,          /* As Of Date                 */~
            bals(10), bals$(5)13,        /* Transaction Balance        */~
            billto(10), billto_ctl$9,    /* Bill-to Aging Totals, Ctl  */~
            billto$9, billtoname$32,     /* Bill-to Customer and Name  */~
            cage$(50)76,                 /* Currency Aging array       */~
            caller$8,                    /* Calling Program Name       */~
            compname$60,                 /* Compnay Name               */~
            contact$40,                  /* Contact and phone          */~
            currency$4,                  /* NOT USED RIGHT NOW         */~
            cus_from$9, cus_to$9,        /* Customer Range from Caller */~
            cus_range$(2)9,              /* Bill-to Customer Range     */~
            date$8,                      /* Date for screen display    */~
            discdue$8,                   /* Discount Due Date          */~
            docdate$8,                   /* DOCUMENT DATE              */~
            duedate$8,                   /* Due Date for Report        */~
            errormsg$79,                 /* Error message              */~
            hdrs$(5)13,                  /* Report Column Headings     */~
            include$1,                   /* Include zero balances?     */~
            min_bal$10,                  /* Minimum Balance            */~
            netdue$8,                    /* Net Due Date               */~
            p%(1),                       /* Receiver for Searches      */~
            pastdate$6,                  /* Past Due Date              */~
            pastdue$4, pastdue_prnt$32,  /* Past Due Items Only?       */~
            plowkey$50, plowkey1$50,     /* Plow Key Variables         */~
            phone$10,                    /* Phone Number               */~
            po$16,                       /* Customer PO                */~
            postdate$8,                  /* Date Transaction Posted    */~
            prnt_id$15,                  /* Caller and Report ID       */~
            report(10),                  /* Report Totals              */~
            rptid$6,                     /* Report ID                  */~
            srcedoc$8,                   /* Source Document Number     */~
            stlmnt$12, stlmnt_prnt$14,   /* Settlement Number          */~
            summary$1,                   /* Summary (S) or Detail (D)  */~
            time$8,                      /* Report Run Time            */~
            type$2,                      /* Source and Type Codes      */~
            type_index$20,               /* Index to Type Descrs       */~
            type_descrs$(2,10)30,        /* Descriptors for Types      */~
            types$20,                    /* Selected Document Types    */~
            text$(6)135                  /* Text for End of Rpt  EWD001*/

        dim f1%(64)                      /* Read Status Flag           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #2  ! CUSTOMER ! Customer Master                          *~
            * #9  ! SYSFILE2 ! System File                              *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #9,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 20

            select #44, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")

*          ARMTBCEX not opened or used, open if multi-currency enabled

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Printing Aged Detail Trial Balance")

            date$ = date : call "DATEFMT" (date$)
                           call "TIME"    (time$)
            prnt_id$ = str(caller$,,8) & "-" & rptid$

            if len(asof$) = 8% then call "DATUNFMT" (asof$)
                asoffmtd$ = asof$  :  call "DATEFMT" (asoffmtd$)

            call "TESTRNGE" (cus_from$, cus_to$,                         ~
                             cus_range$(1), cus_range$(2), errormsg$)
                if errormsg$ <> " " then exit_program
                billto$ = hex(ff)

            if summary$ = " " then summary$  = "S"

            include$ = "N"
            if min_bal$ <> "I" then L09220
                include$ = "Y" : min_bal$ = " "

L09220:     pastdate$   = all(hex(ff)) : pastdue_prnt$ = "NONE"
            if pastdue$ = "N" then L09290
                convert pastdue$ to pastdue%, data goto exit_program
                call "DATE" addr ("G+", asof$, pastdue%, pastdate$, u3%)
                pastdue_prnt$ = pastdate$ : call "DATEFMT" (pastdue_prnt$)
                pastdue_prnt$ = "ITEMS DUE ON OR BEFORE " & pastdue_prnt$

L09290:     if min_bal$ = " " then L09320
                convert min_bal$ to min_bal, data goto exit_program

L09320:     call "COMPNAME" (12%, compname$, u3%)
            select printer  (134)
            call "SETPRNT"  (rptid$, " ", 0%, 0%)
            plowkey$ = cus_range$(1)
            billto_items%, report_items% = 0%

            type_index$ = "IIICIFIACPCUCACBCD"
                type_descrs$(1,1) = "INVOICE  "
                type_descrs$(1,2) = "CR MEMO  "
                type_descrs$(1,3) = "FIN CHRGE"
                type_descrs$(1,4) = "INV ADJ  "
                type_descrs$(1,5) = "PAYMENT  "
                type_descrs$(1,6) = "UNAPPLIED"
                type_descrs$(1,7) = "CASH ADJ "
                type_descrs$(1,8) = "BF PAYMNT"
                type_descrs$(1,9) = "BF DISTR "

                type_descrs$(2,1) = "Invoices"
                type_descrs$(2,2) = "Credit Memos"
                type_descrs$(2,3) = "Finance Charges"
                type_descrs$(2,4) = "Invoicing Adjustments"
                type_descrs$(2,5) = "Payments"
                type_descrs$(2,6) = "Unapplied Payments"
                type_descrs$(2,7) = "Cash Adjustments"
                type_descrs$(2,8) = "Balance Forward Payments"
                type_descrs$(2,9) = "Balance Forward Distributions"

            for b% = 1% to 5%
                if b% = 1% then prev% = aging%(8)           else         ~
                                prev% = aging%(b%-1%) + 1%
                put hdrs$(b%) using L09630, prev%, aging%(b%)
L09630:              %-#### - -####
            next b%
            if aging%(8) = -99999% then                                  ~
                     put hdrs$(1) using L09670, "BEFORE", aging%(1) + 1%
L09670:                   % ###### -####
            if aging%(9) =  99999% then                                  ~
                     put hdrs$(5) using L09670, "  OVER", aging%(4)

            amthdr$ = "TRANSACTION"
            if summary$ = "S" then amthdr$ = "   OPEN    "
            billto_ctl$ = all(hex(fe))

        REM *************************************************************~
            *              P R I N T   R E P O R T                      *~
            *-----------------------------------------------------------*~
            * Prints Report per parameters requested.                   *~
            *************************************************************

*        First Print Selection Criteria Page
            short% =  1%
            page%  = -1%
            gosub page_heading
            print skip(2)
            print using L20110
            print
            print using L20130, asoffmtd$
            if cus_from$ = "ALL" then                                    ~
                print using L20150, "ALL", " ", " "   else                ~
                print using L20150, cus_from$, "TO", cus_to$
            if summary$ = "S" then print using L20170, "SUMMARY" else     ~
                                   print using L20170, "DETAIL"

            if types$ <> " " then L10230
                print using L20190, "ALL"
                goto L10320
L10230:     for t% = 1% to len(types$)/2
              if str(types$,t%*2%-1%,2%) = " " then L10300
                search type_index$ = str(types$,t%*2%-1%,2%) to p%()     ~
                                                                  step 2%
                p%(1) = (p%(1) + 1%) / 2%
                if t% = 1% then print using L20190, type_descrs$(2,p%(1)) ~
                           else print using L20210, type_descrs$(2,p%(1))
L10300:     next t%

L10320:     print using L20230, pastdue_prnt$

            if min_bal$ = " " then print using L20250, "NONE"             ~
                              else print using L20250, min_bal$

            if include$ = "Y" then                                       ~
                             print using L20265, "INCLUDING ZERO BALANCES"

            if ageper$ = "D" then print using L20270
            if ageper$ = "N" then print using L20290
            if ageper$ = "I" then print using L20310

            if acctu$(1) = "ALL" then L10480
              for i% = 1% to 2%
                acctf$(i%) = acctu$(i%)
                call "GETCODE" (#3, acctf$(i%), acct_descr$(i%), 0%, 99, ~
                                                                  f1%(3))
                if f1%(3) = 1% then L10450
                     acct_descr$(i%) = "Not on File."
L10450:         next i%

                if acctie$ = "I" then                                    ~
                   print using L20330, acctf$(1), acct_descr$(1)          ~
                else                                                     ~
                print using L20341, acctf$(1), acct_descr$(1)

                if acctu$(1) <> acctu$(2) then                           ~
                     print using L20350, acctf$(2), acct_descr$(2)
                call "GLUNFMT" (acctu$(1))
                call "GLUNFMT" (acctu$(2))

L10480:     short% = 0%
            gosub page_heading
            mat billto = zer
            mat report = zer

        report_loop
*        Get next record and test against selected customer range.
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then end_report
            if str(plowkey$,,9) > cus_range$(2) then end_report

*        Now see if transaction encountered meets the criteria.
            if str(plowkey$,20,2) <> "00" then L10654
                get #1 using L10630, stlmnt$, netdue$, acct$, type$,      ~
                                    postdate$, docdate$
L10630:              FMT XX(9), CH(12), POS(37), CH(6), POS(76), CH(9),  ~
                         POS(87), CH(2), POS(97), CH(6), CH(6)

                if acctu$(1) = "ALL" then L10670
                   if acctie$ = "I" then L10665
                     if acct$ < acctu$(1) then L10670
                     if acct$ > acctu$(2) then L10670
L10654:                 str(plowkey$,20,2) = hex(ffff)
                        goto report_loop
L10665:              if acct$ < acctu$(1) then L10654
                     if acct$ > acctu$(2) then L10654

L10670:         if postdate$ > asof$ then report_loop
                if types$ = " " then L10710
                     search types$ = type$ to p%() step 2
                     if p%(1) = 0% then report_loop
L10710:         if netdue$ > pastdate$ then report_loop
                if min_bal$ = " " then L10790

*                   ARMAGING now handles multicurrency. But the 2nd to
*                   last arg is 0% and this causes the sub to work in
*                   a NON-Multi-Currency mode. See ARMAGING for futher
*                   details & see ARMSTMNT for example of use where
*                   Customers being invoiced w/ more than 1 currency
*                   can have each currency type accounted/totaled
*                   separately.

                     call "ARMAGING" (str(plowkey$,,9), str(stlmnt$,,10),~
                                    asof$, ageper$, aging%(), #1, bals(),~
                                    #44, #9, 0%, currency$, cage$(),     ~
                                    acctie$, acctu$(1), acctu$(2))

                     if bals(8) >= min_bal then L10790
                          str(plowkey$,20,2) = hex(ffff)
                          goto report_loop

L10790
*        If a new customer has been encountered print totals.
            if billto$ = hex(ff) then L10830
            if billto$ = str(plowkey$,,9) then L10930
                gosub customer_totals
L10830:         billto$ = str(plowkey$,,9)
                call "DESCRIBE" (#2, billto$, billtoname$, 1%, f1%(2))
                if f1%(2) = 0% then billtoname$ = "(** NOT ON FILE **)"  ~
                               else get #2 using L10870, contact$, phone$
L10870:                                  FMT POS(433), CH(20), CH(10)
                if phone$ <> " " then                                    ~
                     contact$ = contact$ & "  " & str(phone$,,3) & "-" & ~
                                str(phone$,4,3) & "-" & str(phone$,7)
                if line% >  50% then line% = 857%

L10930
*        Now, finally, print the dirt on the settlement.
            if min_bal$ <> " " then      /* Have to reload '00' trans */ ~
                     call "READ100" (#1, str(billto$) & stlmnt$, f1%(1))
            gosub load_trans   /* Get and describe the '00' trans      */

*                   ARMAGING now handles multicurrency. But the 2nd to
*                   last arg is 0% and this causes the sub to work in
*                   a NON-Multi-Currency mode. See ARMAGING for futher
*                   details & see ARMSTMNT for example of use where
*                   Customers being invoiced w/ more than 1 currency
*                   can have each currency type accounted/totaled
*                   separately.

            if min_bal$ = " " then                                       ~
                call "ARMAGING" (billto$, str(stlmnt$,,10),              ~
                                 asof$, ageper$, aging%(), #1, bals(),   ~
                                 #44, #9, 0%, currency$, cage$(),        ~
                                 acctie$, acctu$(1), acctu$(2))

            aged = 0
            for b% = 1% to 5%
                call "CONVERT" (bals(b%), 2.2, bals$(b%))
                if bals(b%) = 0 then bals$(b%) = " " else aged = bals(b%)
            next b%
            if include$ = "Y" then L11060
            if aged = 0 then L11150
L11060:     call "CONVERT" (transamt, 2.2, amt$)
            if summary$ = "S" then call "CONVERT" (aged, 2.2, amt$)
            if ageper$  = "D" then duedate$ = discdue$                   ~
                              else duedate$ =  netdue$
            call "DATEFMT" (duedate$)
            call "DATEFMT" (docdate$)
            gosub print_trans   /* Print '00' guy   */
            mat billto  = billto + bals
            if summary$ = "D" then L11180
L11150:         str(plowkey$,20,2) = hex(ffff)
                goto report_loop

L11180
*       * Handle Detail Option
            duedate$  = " "
            plowkey1$ = plowkey$
            str(plowkey$,20,2) = hex(ffff)

          trans_loop
            call "PLOWNEXT" (#1, plowkey1$, 19%, f1%(1))
            if f1%(1) = 0% then report_loop
                gosub load_trans
                if postdate$ > asof$ then trans_loop
                     bals$() = " "
                     call "CONVERT" (transamt, 2.2, amt$)
                     call "DATEFMT" (docdate$)
                     gosub print_trans
                     goto  trans_loop


        end_report
            gosub customer_totals
            gosub report_totals
            print
            print "*** END OF REPORT ***"
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

            print using L20630, text$(1)
            print using L20630, text$(2)
            print using L20630, text$(3)
            print using L20630, text$(4)
            print using L20630, text$(5)
            print using L20630, text$(6)
                                                        /*  (EWD001) -   End  */

            close printer
            goto exit_program


*       ****  SUBROUTINES  *********************************************

        page_heading
            page% = page% + 1%
            line% = 7%
            print page
            print using L20050, date$, time$, compname$, prnt_id$
            print using L20080, asoffmtd$, page%
            print
            if short% = 1% then return
                print using L20360, amthdr$
                print using L20390, hdrs$(1), hdrs$(2), hdrs$(3),         ~
                                   hdrs$(4), hdrs$(5)
                print using L20420
                if billto$ <> hex(ff) then gosub print_billto
                return

        print_billto
            print using L20450, billto$, billtoname$
            line% = line% + 1%
            if contact$ = " " then L11660
                print using L20460, contact$
                line% = line% + 1%
L11660:     billto_ctl$ = billto$
            return

        customer_totals
            total = 0
            if billto_items% = 0% then return
*          IF BILLTO_ITEMS% = 1% THEN 11550
                for b% = 1% to 5%
                     total = total + billto(b%)
                     call "CONVERT" (billto(b%), 2.2, bals$(b%))
                     if billto(b%) = 0 then bals$(b%) = " "
                next b%
                call "CONVERT" (total, 2.2, amt$)
                print using L20510
                print using L20550, billto$, billto_items%, amt$,         ~
                                   bals$(1), bals$(2), bals$(3),         ~
                                   bals$(4), bals$(5)
                line% = line% + 2%
            print
            line% = line% + 1%
            report_items% = report_items% + billto_items%
            mat report    = report        + billto
            mat billto    = zer
            billto_items% = 0%
            return


        report_totals
            total = 0
            for b% = 1% to 5%
                 total = total + report(b%)
                 call "CONVERT" (report(b%), 2.2, bals$(b%))
            next b%
            call "CONVERT" (total, 2.2, amt$)
            print
            print using L20580, report_items%, amt$, bals$(1), bals$(2),  ~
                               bals$(3), bals$(4), bals$(5)
            if total = 0 then L12100
                for b% = 1% to 5%
                     pct = round(100*(report(b%) / total), 2)
                     call "CONVERT" (pct, 2.2, bals$(b%))
                next b%
                print using L20620, bals$(1), bals$(2), bals$(3),         ~
                                   bals$(4), bals$(5)
L12100:     mat report = zer
            return


        load_trans   /* Load Transaction from Buffer and Describe      */
            get #1 using L12170, stlmnt$, discdue$, netdue$, po$,         ~
                                transamt, type$, srcedoc$, docdate$
L12170:         FMT XX(9), CH(12), XX(8), CH(6), XX(1), CH(6), CH(16),   ~
                  POS(68), PD(14,4), POS(87), CH(2), CH(8), XX(6), CH(6)
            if str(stlmnt$,11,2) = "00" then                             ~
                stlmnt_prnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) &~
                                                 "-" & str(stlmnt$, 11)  ~
                                        else                             ~
                stlmnt_prnt$ = "            " & str(stlmnt$, 11)

            search type_index$ = type$ to p%() step 2%
            type% = (p%(1) + 1%) / 2%
            return


        print_trans       /* Print the transaction detail line         */
            if line% > 55% then gosub page_heading
            if billto$ <> billto_ctl$ then gosub print_billto
                                                  /* (EWD) - Begin     */
            print using L20470, stlmnt_prnt$,                            ~
                               str(type_descrs$(1%,type%),1%,1%), po$,   ~
                               docdate$, duedate$, amt$,                 ~
                               bals$(1), bals$(2), bals$(3),             ~
                               bals$(4), bals$(5)
                                                  /* (EWD) - End        */
            line% = line% + 1%
            billto_items% = billto_items% + 1%
            return


        REM *************************************************************~
            *          I M A G E   S T A T E M E N T S                  *~
            *************************************************************


L20050: %RUN DATE: ######## ########              #######################~
        ~#####################################                ############~
        ~###
L20080: %   AS OF: ########                                      A/R AGED~
        ~ DETAIL TRIAL BALANCE                                     PAGE: #~
        ~###
L20110: %                                         ---------------  REPORT~
        ~ SELECTION CRITERIA  ---------------
L20130: %                                                      AS OF DATE~
        ~: ########
L20150: %                                                   FOR CUSTOMERS~
        ~: ######### ## #########
L20170: %                                                  SUMMARY OPTION~
        ~: #######
L20190: %                                         SELECTED DOCUMENT TYPES~
        ~: ##################################
L20210: %                                                                ~
        ~  ##################################
L20230: %                                               PAST DUE CRITERIA~
        ~: ################################
L20250: %                                                 MINIMUM BALANCE~
        ~: ##########
L20265: %                                                                ~
        ~  #########################
L20270: %                                                         AGE PER~
        ~: CASH DISCOUNT DUE DATE
L20290: %                                                         AGE PER~
        ~: NET DUE DATE
L20310: %                                                         AGE PER~
        ~: SOURCE DOCUMENT (INVOICE) DATE
L20330: %                                           INCLUDING G/L ACCOUNT~
        ~: ############  ##############################
L20341: %                                           EXCLUDING G/L ACCOUNT~
        ~: ############  ##############################
L20350: %                                                        THROUGH ~
        ~  ############  ##############################
L20360: %        SETTLEMENT   DOCUMENT  DOCUMENT            ########### -~
        ~ - - - - - - - - - -  A G E D   O P E N   A / R  - - - - - - - - ~
        ~- -
                                                 /* (EWD) - Begin    */
L20390: %    NUMBER     T   PO NUMBER     DATE   DUE DATE     AMOUNT    #~
        ~############ ############# ############# ############# ##########~
        ~###
L20420: %-------------- - ------------- -------- -------- ------------- -~
        ~------------ ------------- ------------- ------------- ----------~
        ~---
L20450: %######### ################################
L20460: %          Contact: ###########################################
L20470: %############## # ############# ######## ######## ############# #~
        ~############ ############# ############# ############# ##########~
        ~###
                                                  /* (EWD) - End      */  

L20510: %                                                 ------------- -~
        ~------------ ------------- ------------- ------------- ----------~
        ~---

L20550: %  * BILL-TO ######### (##### ITEMS)              ############# #~
        ~############ ############# ############# ############# ##########~
        ~###
L20580: % ** REPORT TOTALS     (##### ITEMS)              ############# #~
        ~############ ############# ############# ############# ##########~
        ~###

L20620: %        PERCENT OF TOTAL AMOUNT DUE                            #~
        ~############ ############# ############# ############# ##########~
        ~###

                                                        /*  (EWD003)    */
L20630: %################################################################~
        ~###################################################################


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
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            end
