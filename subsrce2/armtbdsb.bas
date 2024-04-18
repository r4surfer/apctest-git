        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT  BBBB   DDDD    SSS   BBBB    *~
            *  A   A  R   R  MM MM    T    B   B  D   D  S      B   B   *~
            *  AAAAA  RRRR   M M M    T    BBBB   D   D   SSS   BBBB    *~
            *  A   A  R   R  M   M    T    B   B  D   D      S  B   B   *~
            *  A   A  R   R  M   M    T    BBBB   DDDD    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTBDSB - Prints Detail Trial Balance Report.            *~
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
            * 07/11/89 ! Rounded the transaction amount           ! JDH *~
            * 08/23/89 ! Added "STATUTORY" to line heading, will  ! MLJ *~
            *          !  only appear if Multi-Currency in use.   !     *~
            * 02/14/90 ! Changed rounding to at printing so that  ! JDH *~
            *          !  amounts can total with more precision.  !     *~
            * 09/12/90 ! Paid invoices rpt 'Days Late' as 'Paid'. ! JDH *~
            *          !  Above resolves PRR 11315.               !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARMTBDSB"   (caller$,       /* Calling Program ID         */~
                          rptid$,        /* Report ID                  */~
                          asof$,         /* Report As Of date          */~
                          cus_from$,     /* Bill-to Range- From        */~
                          cus_to$,       /*                To          */~
                          summary$,      /* Summary Option (D/S)       */~
                          types$,        /* Selected Doc Srces/Types   */~
                          pastdue$,      /* Past Due List Parameters   */~
                          min_bal$,      /* Min Balance Parameters     */~
                          #1, #2,        /* ARMTRIAL, CUSTOMER         */~
                          acctie$,       /* GL Ranges                  */~
                          acctf1$,       /* GL Ranges                  */~
                          acctf2$)       /* GL Ranges                  */

*        AS OF date may be supplied in either 6 or 8 characters.
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


        dim                                                              ~
            acctf1$12, acctf2$12,        /* GL Ranges                  */~
            acctu1$12, acctu2$12,        /* GL Ranges                  */~
            acctie$1, acct$9,            /* GL Ranges                  */~
            amt_hdng$13,                 /* Column Heading for Amounts */~
            asof$8, asoffmtd$8,          /* As Of Date                 */~
            bals(3),                     /* Transaction Balance        */~
            billto$9, billtoname$30,     /* Bill-to Customer and Name  */~
            caller$8,                    /* Calling Program Name       */~
            cashdisc$6,                  /* Cash Discount Percent      */~
            compname$60,                 /* Compnay Name               */~
            curr$1,                      /* Multi-Currency Flag        */~
            cus_from$9, cus_to$9,        /* Customer Range from Caller */~
            cus_range$(2)9,              /* Bill-to Customer Range     */~
            date$8,                      /* Date for screen display    */~
            days_late$5,                 /* Aging Days                 */~
            discdue$8,                   /* Discount Due Date          */~
            errormsg$79,                 /* Error message              */~
            min_bal$10,                  /* Minimum Balance            */~
            netdue$8,                    /* Net Due Date               */~
            p%(1),                       /* Receiver for Searches      */~
            pastdate$6,                  /* Past Due Date              */~
            pastdue$4, pastdue_prnt$32,  /* Past Due Items Only?       */~
            plowkey$50, plowkey1$50,     /* Plow Key Variables         */~
            po$16,                       /* Customer PO                */~
            postdate$8,                  /* Date Transaction Posted    */~
            prnt_id$15,                  /* Caller and Report ID       */~
            rptid$6,                     /* Report ID                  */~
            srcedoc$8,                   /* Source Document Number     */~
            stlmnt$12, stlmnt_prnt$14,   /* Settlement Number          */~
            summary$1,                   /* Summary (S) or Detail (D)  */~
            time$8,                      /* Report Run Time            */~
            type$2,                      /* Source and Type Codes      */~
            type_index$20,               /* Index to Type Descrs       */~
            type_descrs$(2,10)30,        /* Descriptors for Types      */~
            types$20                     /* Selected Document Types    */


        dim f1%(32),                     /* Read Status Flag           */~
            f2%(32),                     /* = 0 If File Is Open        */~
            fs%(32),                     /* = 1 If Open, -1 If Does Not*/~
                                         /* Exist, 0 If Not Yet Checked*/~
            rslt$(32)20                  /* Text From File Open        */

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
            * #3  ! SYSFILE2 ! System File                              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "SYSFILE2",                                      ~
                         varc,  indexed,  recsize = 500,                 ~
                         keypos = 1,  keylen = 20

            call "OPENCHCK" (#3, fs%(3), f2%(3), 0%, rslt$(3))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Check for Multi-Currency usage
            curr$ = "N"
            call "READ100" (#3, "SWITCHS.CUR", f1%(3))
            if f1%(3) = 0% then L09060
                get #3 using L09056, curr$
L09056:             FMT POS(21), CH(01)

L09060:     call "SHOSTAT" ("Printing Detail Trial Balance")

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
            if curr$    = "Y" then goto L09222
            if summary$ = "S" then amt_hdng$ = "      BALANCE"           ~
                              else amt_hdng$ = " TRANS AMOUNT"
               goto L09240
L09222:     if summary$ = "S" then amt_hdng$ = "      BALANCE"           ~
                              else amt_hdng$ = "       AMOUNT"

L09240:     pastdate$   = all(hex(ff)) : pastdue_prnt$ = "NONE"
            if pastdue$ = "N" then L09310
                convert pastdue$ to pastdue%, data goto exit_program
                call "DATE" addr ("G+", asof$, pastdue%, pastdate$, u3%)
                pastdue_prnt$ = pastdate$ : call "DATEFMT" (pastdue_prnt$)
                pastdue_prnt$ = "ITEMS DUE ON OR BEFORE " & pastdue_prnt$

L09310:     if min_bal$ = " " then L09340
                convert min_bal$ to min_bal, data goto exit_program

L09340:     call "COMPNAME" (12%, compname$, u3%)
            select printer  (134)
            call "SETPRNT"  (rptid$, " ", 0%, 0%)
            plowkey$ = cus_range$(1)
            billto_total , report_total  = 0
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

            acctu1$ = acctf1$ : acctu2$ = acctf2$
            if acctf1$ = "ALL" then L10370
               if acctie$ = "I" then print using L20261, acctf1$, acctf2$ ~
                                else print using L20263, acctf1$, acctf2$
               call "GLUNFMT" (acctu1$)
               call "GLUNFMT" (acctu2$)

L10370:     short% = 0%
            gosub page_heading



        report_loop
*        Get next record and test against selected customer range.
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then end_report
            if str(plowkey$,,9) > cus_range$(2) then end_report

*        Now see if transaction encountered meets the criteria.
            if str(plowkey$,20,2) <> "00" then L10525
                get #1 using L10510, stlmnt$, netdue$, acct$, type$,      ~
                                    postdate$
L10510:              FMT XX(9), CH(12), POS(37), CH(6), POS(76), CH(9),  ~
                         POS(87), CH(2), POS(97), CH(6)
                if acctf1$ = "ALL" then L10530
                   if acctie$ = "I" then L10527
                      if acct$ < acctu1$ then L10530
                      if acct$ > acctu2$ then L10530
L10525:                  str(plowkey$,20,2) = hex(ffff)
                         goto report_loop
L10527:               if acct$ < acctu1$ then L10525
                      if acct$ > acctu2$ then L10525

L10530:         if postdate$ > asof$ then report_loop
                if types$ = " " then L10570
                     search types$ = type$ to p%() step 2
                     if p%(1) = 0% then report_loop
L10570:         if netdue$ > pastdate$ then report_loop
                if min_bal$ = " " then L10650
                     call "ARMBALNC" (str(plowkey$,,9), stlmnt$, asof$,  ~
                                      10%, "N", #1, #2, bals())
                     if bals(1) >= min_bal then L10650
                          str(plowkey$,20,2) = hex(ffff)
                          goto report_loop

L10650
*        If a new customer has been encountered print totals.
            if billto$ = hex(ff) then L10690
            if billto$ = str(plowkey$,,9) then L10760
                gosub customer_totals
L10690:         billto$ = str(plowkey$,,9)
                call "DESCRIBE" (#2, billto$, billtoname$, 0%, f1%(2))
                if f1%(2) = 0% then billtoname$ = "** NOT ON FILE **"
                if line% > 50 then line% = 857% else                     ~
                                  print using L20370, billto$, billtoname$
                line% = line% + 1%

L10760
*        Now, finally, print the dirt on the settlement.
            if min_bal$ <> " " then      /* Have to reload '00' trans */ ~
                     call "READ100" (#1, str(billto$) & stlmnt$, f1%(1))
            gosub load_trans   /* Get and describe the '00' trans      */
            if summary$ = "D" then L10910

*       * Handle Summary Option
            if min_bal$ = " " then call "ARMBALNC" (billto$, stlmnt$,    ~
                                                    asof$, 10%, "N",     ~
                                                    #1, #2, bals())
            str(plowkey$,20,2) = hex(ffff)
            transamt = bals(1)
            gosub print_trans
            goto report_loop

L10910
*       * Handle Detail Option
            gosub print_trans            /* Print '00' guy   */
            plowkey1$ = plowkey$
            str(plowkey$,20,2) = hex(ffff)

          trans_loop
            call "PLOWNEXT" (#1, plowkey1$, 19%, f1%(1))
            if f1%(1) = 0% then report_loop
                gosub load_trans
                     call "DATUNFMT" (postdate$)
                     if postdate$ > asof$ then trans_loop
                     call "DATEFMT"  (postdate$)
                gosub print_trans
                goto  trans_loop


        end_report
            gosub customer_totals
            gosub report_totals
            print
            print "** END OF REPORT **"
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
            if curr$  = "N" then print using L20280 else                  ~
                                 print using L20302
                print using L20310, amt_hdng$
                print using L20340
                if billto$ <> hex(ff) then                               ~
                                 print using L20370, billto$, billtoname$
                return


        customer_totals
            if billto_items% = 0% then return
            if billto_items% = 1% then L11390
                print using L20430, billto$, billto_items%,               ~
                                   round(billto_total, 2)
                line% = line% + 1%
L11390:         print
                line% = line% + 1%
                report_items% = report_items% + billto_items%
                report_total  = report_total  + billto_total
                billto_items%, billto_total = 0
                return


        report_totals
            print using L20460, report_items%, round(report_total, 2)
            return


        load_trans   /* Load Transaction from Buffer and Describe      */
            get #1 using L11550, stlmnt$, cashdisc, discdue$, netdue$,    ~
                                po$, transamt, type$, srcedoc$, postdate$
L11550:         FMT XX(9), CH(12), PD(14,4), CH(6), XX(1), CH(6), CH(16),~
                    POS(68), PD(14,4), POS(87), CH(2), CH(8), CH(6)
            if str(stlmnt$,11,2) <> "00" then L11660
                call "DATE" addr("G-", netdue$, asof$, days_late%, u3%)
                if days_late% < -9999 or days_late% > 9999 then          ~
                                    days_late% = sgn(days_late%) * 9999%
                convert days_late% to days_late$, pic(####-)
                stlmnt_prnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) &~
                                                 "-" & str(stlmnt$, 11)
                goto L11690

L11660:         discdue$, netdue$, days_late$ = " "
                stlmnt_prnt$ = "            " & str(stlmnt$, 11)

L11690:     search type_index$ = type$ to p%() step 2%
            type% = (p%(1) + 1%) / 2%
            cashdisc$ = " "
            if cashdisc <> 0 then                                        ~
                                call "CONVERT" (cashdisc, 2.2, cashdisc$)
            call "DATEFMT" (postdate$)
            call "DATEFMT" (discdue$ )
            call "DATEFMT" (netdue$  )
            return


        print_trans       /* Print the transaction detail line         */
            if line% > 55% then gosub page_heading
            if str(stlmnt$,11,2) <> "00" then L11820
                if summary$ <> "D" then L11816
                    call "ARMBALNC" (billto$, stlmnt$, asof$, 10%, "N",  ~
                                     #1, #2, bals())
L11816:             if bals(1) < .01 and bals(1) > -.01 then             ~
                                                     days_late$ = "Paid"
L11820:     print using L20390, stlmnt_prnt$, srcedoc$,                   ~
                               type_descrs$(1,type%), postdate$, po$,    ~
                               cashdisc$, discdue$, netdue$,             ~
                               round(transamt, 2), days_late$
            line% = line% + 1%
            billto_items% = billto_items% + 1%
            billto_total  = billto_total  + transamt
            return


        REM *************************************************************~
            *          I M A G E   S T A T E M E N T S                  *~
            *************************************************************


L20050: %RUN DATE: ######## ########              #######################~
        ~#####################################                ############~
        ~###
L20080: %   AS OF: ########                                        A/R DE~
        ~TAIL TRIAL BALANCE                                        PAGE: #~
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
L20261: %                                          INCLUDING G/L ACCOUNTS~
        ~: ############ thru ############
L20263: %                                          EXCLUDING G/L ACCOUNTS~
        ~: ############ thru ############

L20280: %                                           DOCUMENT           DO~
        ~C POST                   CASH  DISCOUNT   NET                   D~
        ~AYS
L20302: %                                           DOCUMENT           DO~
        ~C POST                   CASH  DISCOUNT   NET        STATUTORY  D~
        ~AYS
L20310: %BILL-TO NUMBER AND NAME     SETTLEMENT NO.  NUMBER  DOC. TYPE   ~
        ~DATE   CUSTOMER PO      DISC % DUE DATE DUE DATE #############  L~
        ~ATE
L20340: %--------------------------- -------------- -------- --------- --~
        ~------ ---------------- ------ -------- -------- ------------- --~
        ~---
L20370: %######### ##############################

L20390: %                            ############## ######## ######### ##~
        ~###### ################ ###### ######## ######## -#####,###.## ##~
        ~###

L20430: %  * BILL-TO ######### TOTALS  (##### ITEMS)                     ~
        ~                                                 -#####,###.##*

L20460: % ** REPORT TOTAL  (###,### ITEMS)                               ~
        ~                                                 -#####,###.##**


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
