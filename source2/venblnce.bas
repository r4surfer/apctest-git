        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  EEEEE  N   N  BBBB   L      N   N   CCC   EEEEE   *~
            *  V   V  E      NN  N  B   B  L      NN  N  C   C  E       *~
            *  V   V  EEEE   N N N  BBBB   L      N N N  C      EEEE    *~
            *   V V   E      N  NN  B   B  L      N  NN  C   C  E       *~
            *    V    EEEEE  N   N  BBBB   LLLLL  N   N   CCC   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENBLNCE - Reset the Vendor's current, outstanding        *~
            *            balance based on (+) non-direct invoices       *~
            *            (liabilities) and (-) non-direct, non-frozen   *~
            *            (liabilities) cash disbursements.              *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/30/93 ! Original (PRR 12797)                     ! JIM *~
            * 01/25/94 ! Largely Rewrote, included Update of open ! JBK *~
            *          !   invoice amount in PAYMASTR and Multi-  !     *~
            *          !   Currency Gain_Loss considerations.     !     *~
            * 01/16/95 ! PRR 13343. Non-A/P Lia. accts now to 50. ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            accttyp$1,                   /* Payables Acct Type         */~
            company$60,                  /* Company or Division Name   */~
            cshacct$9,                   /* Check Account              */~
            cshaccttyp$1,                /* Check Account Type         */~
            cshcheck$8,                  /* Check #                    */~
            cshseq$3,                    /* Check Line Seq Number      */~
            cshstatus$1,                 /* Check Line Status          */~
            cshinvoice$16,               /* Check Invoice Number       */~
            cshvendor$9,                 /* Check Vendor Code          */~
            curr$1,                      /* Multi-Currancy Switch      */~
            date$8,                      /* Date for report display    */~
            index%(1),                   /* Search Receiver            */~
            invamt$10,                   /* Invoice Amount for Report  */~
            invdate$8,                   /* Invoice Date               */~
            invoice$16,                  /* PAYMASTR Invoice #         */~
            newinvbal$10,                /* Re-calculated Invoice Bal  */~
            nonapacct$(50)9,             /* Valid Non-AP Liability Acct*/~
            oldaccttyp$1,                /* Old Account Type           */~
            oldinvbal$10,                /* Old Invoice Balance        */~
            payacct$9,                   /* Accounts Payable Account   */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rtitle1$65,                  /* Report Title 1             */~
            rptid$8,                     /* Report ID                  */~
            runtime$8,                   /* Program Run Time           */~
            remark$17,                   /* Report Remarks             */~
            ttacct$9,                    /* Subroutine Account Argument*/~
            ttype$1,                     /* Subroutine Account Type Arg*/~
            vendor$9, vendordesc$30      /* Vendor Code & Description  */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #1  ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #2  ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * #4  ! CSHLINES ! CASH DISBUSREMENTS CHECK DETAIL FILE     *~
            * #5  ! SYSFILE2 ! SYSTEM CATCH ALL FILE                    *~
            * #6  ! CSHLNCUR ! Currency Specific - CSHLINES             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #2,  "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25

            select #4,  "CSHLINES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen = 20,                      ~
                        alt key  1, keypos =   21, keylen =  16, dup

            select #5,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1, keylen = 20

            select #6,  "CSHLNCUR",                                      ~
                        varc,  indexed,  recsize = 100,                  ~
                        keypos =  5,  keylen = 20,                       ~
                        alt key   1, keypos =   1,  keylen = 44

L02380:     u3% = 0%                            /* Widow in the middle */
            call "ASKUSER" (u3%, "*** EXCLUSIVE USE REQUIRED ***",       ~
                "VENBLNCE requires exclusive use of the following files"&~
                ":", "VENDOR, PAYMASTR, CSHLINES", "Press (RETURN) to c"&~
                "ontinue; PF(1) to abort.")
            if u3% =  1% then goto exit_program
            if u3% <> 0% then goto L02380

L02460:     updatepaymst% = 0%
            call "ASKUSER" (updatepaymst%, "*** UPDATE INVOICE BALAN"   &~
                            "CES ***", "Should The Open Amount For Ea"  &~
                            "ch Invoice Be Re-Calculated and Re-set?",   ~
                            "Press PF(16) to Re-set; PF(9) to Continue" &~
                            " Without Re-set;", "-or- PF(1) to Abort.")
            if updatepaymst%  =  1% then exit_program
            if updatepaymst% <>  9% and  updatepaymst% <> 16% then L02460
            if updatepaymst%  =  9% then updatepaymst%  =  0%
            if updatepaymst%  = 16% then updatepaymst%  =  1%

            call "SHOSTAT" ("Opening Files, One Moment Please")

            rslt$(1%), rslt$(2%), rslt$(4%) = "REQUIRED"
            call "OPENFILE" (#01, "IO   ", f2%(1%), rslt$(1%), axd$)
            call "OPENFILE" (#02, "IO   ", f2%(2%), rslt$(2%), axd$)
            call "OPENFILE" (#04, "IO   ", f2%(4%), rslt$(4%), axd$)
            call "OPENFILE" (#05, "SHARE", f2%(5%), rslt$(5%), axd$)

            if f2%(1%) + f2%(2%) + f2%(4%) = 0% then goto L09000
                call "ASKUSER" (0%, "*** ERROR OPENING FILES ***",       ~
                     "An error was encountered opening a required file."&~
                     " ", "Press any PF key to abort.")
                goto exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

             init (" ")  nonapacct$()
             plowkey$ = "APACCOUNTS-NON" & " "
             call "READ100" (#5, plowkey$ , f1%(5%))
                 if f1%(5%) = 0% then L10000
             get #5 using L09110, nonacctnum%, nonapacct$()
L09110:          FMT XX(20), BI(2), 50*CH(09)

*        Check for Multi-Currency
            curr$ = "N" :  plowkey$ = "SWITCHS.CUR"
            call "READ100" (#5, plowkey$, f1%(5%))
                if f1%(5%) <> 0% then get #5 using L09170, curr$
L09170:              FMT POS(21), CH(1)
                if curr$ <> "Y" then L09345
                call "OPENCHCK" (#6, 0%, 0%,   0%, " ")

L09345:     rtitle1$ = "Invoice Balance Recalculation and Reset Report"
            call "STRING" addr("CT", rtitle1$, 65%)

            date$ = date
            call "DATEFMT" (date$)
            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)
            rptid$ = "A/P013"
            page% = 0%
            line% = 99%

L10000: REM *************************************************************~
            * ... and the work gets done here.                          *~
            *************************************************************

        read_vendor
            call "READNEXT" (#1, f1%(1%))                    /* VENDOR */
                if f1%(1%) = 0% then goto end_program
            get #1 using L10080, vendor$, vendordesc$
L10080:         FMT CH(9), CH(30)
            if vendor$ = hex(000000000000000000) then read_vendor

            venbal = 0                   /* In case of no transactions */

*        Status report to user.
            call "SHOSTAT" ("Now updating vendor " & vendor$ &           ~
                " (" & vendordesc$ & ")")

*        Accumulate non-direct Invoice (liability) total from PAYMASTR.
            init (hex(00)) plowkey$
            str(plowkey$,,9%) = vendor$

        read_paymastr
            newinvbal = 0  :  check% = 0%
            call "PLOWNEXT" (#2, plowkey$, 9%, f1%(2%))    /* PAYMASTR */
                if f1%(2%) = 0% then unapplied_search
            get #2 using L10270, invoice$, invdate$, payacct$, accttyp$,  ~
                                invamt, oldinvbal
L10270:         FMT POS(10), CH(16), POS(42), CH(6), POS(57), CH(9),     ~
                    CH(1), POS(111), 2*PD(14,4)

*        Toss the Recurring Invoices out of the calculation.
            if str(invoice$,1%,1%) = "#" and str(invoice$,6%,11%) = " "  ~
                then goto read_paymastr

*        Accumulate Invoice 'L'iabilities.
            oldaccttyp$ = accttyp$
            gosub'100 (payacct$, accttyp$)
                accttyp$ = ttype$
            if accttyp$ = "L" then L10410
                newinvbal = 0
                goto update_paymastr

L10410
*        Process CSHLINES for non-direct, non-frozen (liability) cash
*        disbursements applied to invoices
            newinvbal = invamt

            call "REDALT0" (#4, invoice$, 1%, f1%(4%))
L10460:         if f1%(4%) = 0% then update_paymastr
            get #4 using L10500, cshvendor$, cshcheck$, cshseq$,          ~
                                cshinvoice$, cshacct$, cshaccttyp$,      ~
                                cshamt, cshstatus$
L10500:         FMT CH(9), CH(8), CH(3), CH(16), CH(9), CH(1), PD(14,4), ~
                    POS(79), CH(1)

            if cshinvoice$ <> invoice$ then update_paymastr
            if cshvendor$  <> vendor$  then L10720
            if cshstatus$   = "F"      then L10720
            cshamt = round(cshamt, 2%)
            check% = 1%

            gosub'100 (cshacct$, cshaccttyp$)
                cshaccttyp$ = ttype$
            if cshaccttyp$ <> "L" then L10720

            gain_loss = 0
            if curr$ <> "Y" then L10700
                call "READ100" (#6, str(cshvendor$) & str(cshcheck$) &   ~
                                str(cshseq$), f1%(6%))
                     if f1%(6%) = 0% then L10700
                get #6 using L10670, gain_loss
L10670:              FMT POS(85), PD(14,4)
                gain_loss = round(gain_loss, 2%)

L10700:     newinvbal = newinvbal - (cshamt - gain_loss)

L10720:     call "READNEXT" (#4, f1%(4%))
            goto L10460

        update_paymastr
            if updatepaymst% <> 1% then L11020
            if oldinvbal = newinvbal then L11020
            if line% > 55% then gosub page_head
            call "DATEFMT" (invdate$)
            call "GLFMT" (payacct$)
            call "CONVERT" (invamt, 2.2, invamt$)
            call "CONVERT" (oldinvbal, 2.2, oldinvbal$)
            call "CONVERT" (newinvbal, 2.2, newinvbal$)

            remark$ = " "
            if oldaccttyp$ = "L" then L10855
                remark$ = "Error Correction"  :  goto L10910
L10855:     if accttyp$ <> "L" then L10895
                if oldinvbal = 0 and newinvbal <> 0 then L10880
                     if check% <> 0% then L10875
                          remark$ = "Classed as Direct"  :  goto L10910
L10875:              remark$ = "Rnding or Check?"  :  goto L10910
L10880:         if check% <> 0% then L10890
                     remark$ = "Classed as Open"  :  goto L10910
L10890:         remark$ = "Rnding or Check?"  :  goto L10910
L10895:     remark$ = "Classed as Direct"  :  goto L10910

L10910:     print using L60250, vendor$, vendordesc$, invoice$, invdate$, ~
                               payacct$, invamt$, oldinvbal$, newinvbal$,~
                               remark$
            let line% = line% + 1%

            call "READ101" (#2, str(vendor$) & str(invoice$), f1%(2%))
                if f1%(2%) <> 1% then L11020
            put #2 using L10990, newinvbal
L10990:         FMT POS(119), PD(14,4)
            rewrite #2

L11020:     venbal = venbal + newinvbal
            goto read_paymastr


*        Process CSHLINES for non-direct, non-frozen (liability) cash
*        disbursements that are unapplied.

        unapplied_search
            init (hex(00))  plowkey$
            str(plowkey$,,9%) = vendor$

L11270:     call "PLOWNEXT" (#4, plowkey$, 9%, f1%(4%))
                if f1%(4%) = 0% then update_vendor
            get #4 using L10500, cshvendor$, cshcheck$, cshseq$,          ~
                                cshinvoice$, cshacct$, cshaccttyp$,      ~
                                cshamt, cshstatus$

            if cshstatus$ = "F" then L11270
            cshamt = round(cshamt, 2%)

            gosub'100 (cshacct$, cshaccttyp$)
                cshaccttyp$ = ttype$
            if cshaccttyp$ <> "L" then L11270

            call "READ100" (#2, str(vendor$) & str(cshinvoice$), f1%(2%))
                if f1%(2%) <> 0% then L11270   /* Applied Check */

            gain_loss = 0
            if curr$ <> "Y" then L11510
                call "READ100" (#6, str(cshvendor$) & str(cshcheck$) &   ~
                                str(cshseq$), f1%(6%))
                     if f1%(6%) = 0% then L11510
                get #6 using L11480, gain_loss
L11480:              FMT POS(85), PD(14,4)
                gain_loss = round(gain_loss, 2%)

L11510:     venbal = venbal - (cshamt - gain_loss)
            goto L11270

        update_vendor             /* Et, voila! Update the VENDOR file */
            call "READ101" (#1, vendor$, f1%(1%))
                if f1%(1%) <> 1% then read_vendor  /* Shouldn't Happen */
            put #1 using L12040, round(venbal, 2)
L12040:         FMT POS(310), PD(14,4)
            rewrite #1
            goto read_vendor


        REM *************************************************************~
            *                CHECK A/P LIABILITY ACCOUNTS               *~
            * --------------------------------------------------------- *~
            * Check for Non-A/P General Ledger Liability Accounts       *~
            *                                                           *~
            *************************************************************
            deffn'100 (ttacct$,ttype$)
                if ttype$         <> "L" then return
                if ttacct$         = " " then return
                if nonapacct$(1%)  = " " then return
                search str(nonapacct$(), 1%, 9% * nonacctnum%)           ~
                         = str(ttacct$) to index%() step 9%
                if index%(1%) =  0% then return
                   ttype$ = "N"
            return

        page_head
            if page% > 0% then L15070
            report% = 1%
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            select printer
            runtime$ = " "  :  call "TIME" (runtime$)

L15070:     print page
            page% = page% + 1%
            print using L60040, date$,runtime$,company$,"VENBLNCE",rptid$
            print using L60070, rtitle1$, page%
            print
            print using L60120
            print using L60150
            print using L60180
            print using L60210
            line% = 7%
            return

        end_program
            if updatepaymst% = 0% then exit_program
            if report% <> 1% then report_message
                print skip(2)
                runtime$ = " "  :  call "TIME" (runtime$)
                print using L60300, runtime$
                close printer
                call "SETPRNT" (rptid$, " ", 0%, 1%)
                goto report_message

        report_message
            if report% = 1% then L16300
            u3% = 0%                            /* Widow in the middle */
            call "ASKUSER" (u3%, "*** NO INVOICE RECORDS MODIFIED ***",  ~
                "VENBLNCE DID NOT re-set any open invoice amounts.",     ~
                "A Report of Invoices Changes HAS NOT Been Produced.",   ~
                "Press any key to continue...")
            goto exit_program

L16300:     u3% = 0%                            /* Widow in the middle */
            call "ASKUSER" (u3%, "*** INVOICE RECORDS HAVE BEEN " &      ~
                 "MODIFIED ***", "VENBLNCE DID re-set open invoice " &   ~
                 "amounts.", "A Report of Invoices Changes HAS Been " &  ~
                 "Produced, Please Review It.", "Press any key to " &    ~
                 "continue...")
            goto exit_program

        REM *************************************************************~
            *     R E P O R T   F O R M A T   S T A T E M E N T S       *~
            *************************************************************

L60040: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

L60070: %                                  ##############################~
        ~##################################                    PAGE: ####


L60120: %                                                                ~
        ~                                Old        New

L60150: %Vendor                                                    Invoic~
        ~e                  Invoice    Invoice    Invoice

L60180: %Code      Vendor Name                    Invoice Number     Date~
        ~    G/L Account     Amount    Balance    Balance Remarks

L60210: %--------- ------------------------------ ---------------- ------~
        ~-- ------------ ---------- ---------- ---------- ----------------~
        ~---

L60250: %######### ############################## ################ ######~
        ~## ############ ########## ########## ########## ################~
        ~###

L60300: % * * * * * * * * * *   E N D   O F   R E P O R T   @ ########  *~
        ~ * * * * * * * * *


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
