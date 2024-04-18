        REM *************************************************************~
            *                                                           *~
            *   PPPP    AAA   Y   Y  TTTTT   OOO    SSS    SSS          *~
            *   P   P  A   A  Y   Y    T    O   O  S      S             *~
            *   PPPP   AAAAA   YYY     T    O   O   SSS    SSS          *~
            *   P      A   A    Y      T    O   O      S      S         *~
            *   P      A   A    Y      T     OOO    SSS    SSS          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYTOSS  - Updates  1. Vendor's 12 month payables balance *~
            *                     2. Invoice's outstanding balance      *~
            *            Replaces PAYMASTR Record with PAYBUFFR version *~
            *            tosses the PAYBUFF2 lines into PAYLINES        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 01/09/85 ! Original - subroutine clone of PAYFILE1  ! DSH *~
            * 01/09/86 ! Pass on 1099 code                        ! kab *~
            * 05/13/86 ! Invoice Files Format Change              ! HES *~
            * 09/23/86 ! Moved Vendor History to VENHSTRY file    ! ERN *~
            * 03/09/87 ! Serial Numbers added.                    ! ERN *~
            * 05/18/87 ! PAYBUF2 & PAYLINES mods for Std costs    ! JIM *~
            * 11/08/88 ! Now writes year of checkdate instead of  ! WPH *~
            *          !   post date to CSH1099 file              !     *~
            * 01/05/89 ! Corrected length of HEADER$() to 350     ! MJB *~
            * 12/17/93 ! Added EOD branch on write to CSH1099.    ! JDH *~
            * 12/30/93 ! Added Code Check for Non-A/P G/L         ! JBK *~
            *          !   Liability Accounts and treat as Direct !     *~
            * 01/19/94 ! Added Gain_Loss on M-C Checks to Invoice ! JBK *~
            *          !   Open Amount Calculation.               !     *~
            * 06/28/94 ! Puchasing Contracts- added update to     ! ERN *~
            *          !   VPCXREF                                !     *~
            * 01/16/95 ! PRR 13343. Non-A/P Lia. Accts now to 50. ! JDH *~
            * 07/18/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

            sub "PAYTOSS"  (paydate$,   /* A/P SYSTEM DATE             */~
                            month%,     /* POSTING MONTH               */~
                            #5, #6,     /* PAYMASTR, PAYLINES          */~
                            #9, #10,    /* PAYBUFFR, PAYBUF2           */~
                            #3,         /* VENDOR                      */~
                            #8, #29,    /* CSHLINES, CSHMASTR          */~
                            #30,        /* CSH1099                     */~
                            #18, #16,   /* SERTIF, SERMASTR            */~
                            #19,        /* SERHOLD                     */~
                            #37 )       /* VPCXREF                     */

        dim apacct$9,                    /* A/P Account                */~
            apkey$30,                    /* SYSFILE2 Key Field         */~
            checkkey$20,                 /* Key to CSHLNCUR, if needed */~
            contract$20,                 /* Purch Contract & Line #    */~
            cshaccttype$1,               /* CSHLINES Acct Type         */~
            curr$1,                      /* Multiple Currancy Switch   */~
            datetime$7,                  /* Date/Time Stamp            */~
            fill$93,                     /* Filler for VENHSTRY record */~
            header$(10)35,               /* Invoice header record      */~
            headerkey$10,                /* Key used for PAYBUFFR      */~
            index%(1),                   /* Search Receiver            */~
            invoice$50,                  /* Invoice number             */~
            linea$250, lineb$250, linec$41,  /* Invoice line item      */~
            nonapacct$(50)16,            /* Valid Non-AP Liability Acct*/~
            oldapacct$9,                 /* A/P Acct of old invoice    */~
            oldpayaccttype$1,            /* Account type of old inv.   */~
            payaccttype$1,               /* Account type of invoice    */~
            paydate$6,                   /* Payables date              */~
            purchase(12),                /* 12 month purchases history */~
            readkey$99, plowkey$99,      /* Key for most plow routines */~
            sn_status$1,                 /* SN_Status                  */~
            sn_loc$30, sn_trankey$42,    /* Serial Number Loc / Trans  */~
            status$1,                    /* Status of check line       */~
            tdate$8,                     /* Temporary Date             */~
            tempapacct$9,                /* A/P Account for CSHLINES   */~
            tempaccttype$1,              /* Account type of payment    */~
            tempcheck$8,                 /* Check # of payment         */~
            tempvencode$9,               /* Vendor code of payment     */~
            tempinvoice$50,              /* Invoice number of payment  */~
            tempseq$3,                   /* Sequence # of payment      */~
            ten99key$20,                                                 ~
            ten99acct$9,                                                 ~
            ten99cacct$9,                                                ~
            ten99dacct$9,                                                ~
            ten99posted$6,                                               ~
            ten99date$8,                                                 ~
            thisinvoicekey$25,           /* Key of Invoice (VEN+INV#)  */~
            ttacct$16,                   /* Subroutine Acct Arguement  */~
            ttype$1,                     /* Subroutine Type Arguement  */~
            oldten99$4,                  /* 1099 Category              */~
            newten99$4,                  /* 1099 Category              */~
            userid$3,                    /* USERID                     */~
            vencode$9                    /* Vendor code                */

        dim f1%(64)                      /* Record-on-file flags       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! VENHSTRY ! Vendor History File                      *~
            * # 3 ! VENDOR   ! Vendor master file (update balance)      *~
            * # 5 ! PAYMASTR ! Payables master file.                    *~
            * # 6 ! PAYLINES ! Payables line items file                 *~
            * # 8 ! CSHLINES ! Cash disbursements check detail file     *~
            * # 9 ! PAYBUFFR ! Payables header buffer                   *~
            * #10 ! PAYBUFF2 ! Payables line items buffer               *~
            * #18 ! SERTIF   ! Serial Number Transaction File           *~
            * #16 ! SERMASTR ! Serial Number Master File                *~
            * #19 ! SERHOLD  ! Serial Number Buffer Delete Holding File *~
            * #20 ! SYSFILE2 ! System Catch All File                    *~
            * #25 ! TXTFILE  ! System Text File                         *~
            *************************************************************

            select #1,  "VENHSTRY",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 11

            select #25, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #20, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #21, "CSHLNCUR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos =    5, keylen = 20,                      ~
                        alt key  1, keypos =  1, keylen =  24

            if beenherebefore% = 1% then L10000
            call "OPENCHCK" (#1,  fs%, f2%, 100%, " ")
            call "OPENCHCK" (#20, fs%, f2%,   0%, " ")

            init (" ")  nonapacct$()
            apkey$ = "APACCOUNTS-NON" & " "
            call "READ100" (#20, apkey$ , f1%(20%))
                if f1%(20%) = 0% then L02600
            get #20 using L02500, nonacctnum%, nonapacct$()
L02500:         FMT XX(20), BI(2), 50*CH(09)

L02600:     curr$ = "N"
            apkey$ = "SWITCHS.CUR" & " "
            call "READ100" (#20, apkey$ , f1%(20%))
                if f1%(20%) = 0% then L02700
            get #20 using L02650, curr$
L02650:         FMT POS(21), CH(1)
            if curr$ = "Y" then call "OPENCHCK" (#21, fs%, f2%,   0%, " ")
            if curr$ = "Y" then L02700
            curr$ = "N"

L02700:     beenherebefore% = 1%

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *-----------------------------------------------------------*~
            * Reads the invoice and posts it to the payables file.      *~
            * Update vendor balance for invoices against payables       *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

        REM Load the new invoice header from PAYBUFFR.  This is a "GET"  ~
            without a read.  It relies on the calling program to do the  ~
            READ.

             get #9, using L10140, headerkey$, str(header$())
L10140:      FMT CH(10), CH(340)
             get #9, using L10170, thisinvoicekey$, apacct$, payaccttype$,~
                                  invoiceamount, newten99$
L10170:         FMT XX(10), CH(25), XX(31), CH(9), CH(1), XX(44),        ~
                    PD(14,4), XX(44),CH(4)
             gosub'100 (apacct$, payaccttype$)
             payaccttype$ = ttype$
             vencode$ = str(thisinvoicekey$,,9)

             plowkey$ = thisinvoicekey$
             call "DELETE" (#19, plowkey$, 25%)    /* Clear SERHOLD    */

                REM Set info assuming that this is the original invoice
                    oldinvoiceamount, oldopenamount = 0
                    oldapacct$, oldpayaccttype$ = " "
                    oldten99$ = newten99$

                REM Check to see if invoice is on file already
                    call "READ100" (#5, thisinvoicekey$, f1%(5))
                         if f1%(5) = 0 then L10400

                    get #5, using L10320, oldapacct$, oldpayaccttype$,    ~
                            oldinvoiceamount, oldopenamount, oldten99$

                    gosub'100 (oldapacct$, oldpayaccttype$)
                    oldpayaccttype$ = ttype$

L10320:             FMT XX(56),          /* Skip to account type       */~
                        CH(9),           /* Old Payables Account       */~
                        CH(1),           /* Old payables acct type     */~
                        XX(44),          /* Skip to date               */~
                        PD(14,4),        /* Gross invoice amount       */~
                        PD(14,4),        /* Outstanding balance        */~
                        XX(36),          /* Skip freetext, disc stuff  */~
                        CH(4)            /* 1099 category              */

L10400:         REM Compute outstanding amount of invoice
                    if payaccttype$ = "L" then L10450
                       openamount = 0
                       goto L10460

L10450:         gosub L20000
L10460:         gosub L30000

            REM Now update the vendor balance with change in amounts
                call"READ101"(#3, vencode$, f1%(3))
                     if f1%(3) = 0 then L10850  /* should never happen */
                        get #3, using L10520, balance
L10520:                     FMT POS(310), PD(14,4)

                        if payaccttype$ = "L" then                       ~
                                 balance = round(balance + openamount, 2)
                        if oldpayaccttype$ = "L" then                    ~
                              balance = round(balance - oldopenamount, 2)

                        put #3, using L10520, balance
                        rewrite #3

                /* Update vendor sales history                     */
                     mat purchase = zer : fill$ = " "

                     tdate$ = paydate$
                     call "DATEFMT" (tdate$, 0%, paydate$)
                     convert str(paydate$,5%,2%) to m%  /* Month     */
                     convert str(paydate$,1%,4%) to yr%
                     readkey$ = str(vencode$,,9) & bin( yr%, 2)
                     call "DATECONV" (paydate$)   /* to internal fmt */

                     call "READ101" (#1, readkey$, f1%(1))
                     if f1%(1) = 0% then L10660
                          get #1 using L10647, readkey$, purchase(), fill$
L10647:                        FMT CH(11), 12*PD(14,4), CH(93)

L10660:              purchase(m%) = purchase(m%)                         ~
                                       + invoiceamount - oldinvoiceamount

                     put #1 using L10647, readkey$, purchase(), fill$
                     if f1%(1) = 0% then write #1 else rewrite #1

L10850:     REM Now delete the entire invoice from the payables buffer
                call "DELETE" ( #9, headerkey$, 10%)
                call "DELETE" (#10, thisinvoicekey$, 25%)
                goto L65000

L20000: REM *************************************************************~
            *                                                           *~
            *     Common update for CSHLINES and CSH1099 files          *~
            *                                                           *~
            *************************************************************

                REM Find all checks paying this invoice
                        openamount = invoiceamount
                        invoice$ = str(thisinvoicekey$,10%)
                        call "REDALT1" (#8, invoice$, 1%, f1%(8%))
L20100:                      if f1%(8%) = 0% then return
                        get #8, using L20150, tempvencode$, tempcheck$,   ~
                                tempseq$, tempinvoice$, tempapacct$,     ~
                                tempaccttype$, amountpaid, oldten99$,    ~
                                status$

L20150:                     FMT CH(9),          /* Vendor code         */~
                                CH(8),          /* Check number        */~
                                CH(3),          /* Sequence Number     */~
                                CH(16),         /* Invoice number      */~
                                CH(9),          /* Account number      */~
                                CH(1),          /* Account type        */~
                                PD(14,4),       /* Amount paid         */~
                                XX(6),          /* Check date          */~
                                XX(8),          /* Discount taken      */~
                                XX(6),          /* Inv date as of pay  */~
                                CH(4)           /* 1099 category       */~

                    REM Is this check paying this invoice?
                        if tempinvoice$ <> invoice$ then return
                        if tempvencode$ <> vencode$ then L20830
                        cshaccttype$ = tempaccttype$
                        gosub'100 (tempapacct$, cshaccttype$)
                        cshaccttype$ = ttype$
                        if cshaccttype$ <> "L" then L20830

                    REM Check for Multiple Currency and get gain_loss
                    gain_loss = 0
                    if curr$ <> "Y" then L20310
                          checkkey$ = str(tempvencode$) &                ~
                                      str(tempcheck$)   & str(tempseq$)
                          call "READ100" (#21, checkkey$, f1%(21%))
                               if f1%(21%) <> 1% then L20310
                          get #21 using L20305, gain_loss
L20305:                        FMT POS(85), PD(14,4)
                          gain_loss = round(gain_loss, 2%)

L20310:             REM Total up & deduct amount of check from invoice
                        openamount = openamount - (amountpaid - gain_loss)
                        openamount = round(openamount, 2%)

                    if status$ <> " " then L20370
                    if oldten99$ = newten99$ then L20830
L20370:                put #8, using L20380, newten99$, " "
L20380:                    FMT POS(75), CH(4), CH(1)
                       rewrite #8

                    if oldten99$ = newten99$ then L20830
                    if f30% > 0% then L20470
                    if f30% < 0% then L20830
                       call "OPENCHCK"(#30, f30%, f1%(30), 1%, " ")
                          if f30% < 0% then L20830

L20470:                get #8, using L20480, ten99key$
L20480:                    FMT CH(20)
                       call "READ101" (#30, ten99key$, f1%(30))
                          if f1%(30) = 0% then L20600
                          if newten99$ <> " " then L20550
                             delete #30
                             goto L20830

L20550:                put #30, using L20560, newten99$
L20560:                    FMT POS(3), CH(4)
                       rewrite #30
                       goto L20830

L20600:                if newten99$ = " " then L20830
                       get #8, using L20630, ten99acct$, ten99posted$,    ~
                                            ten99disc
L20630:                    FMT XX(36), CH(9), XX(9), CH(6), PD(14,4)

                       call "READ100" (#29, str(ten99key$,1,17), f1%(29))
                          if f1%(29) = 0% then L20830
                       get #29, using L20690, ten99date$, ten99dacct$,    ~
                                             ten99cacct$
L20690:                    FMT XX(17), CH(6), XX(8), 2*CH(9)

L20710:                call "GETDTTM" addr(datetime$)

                       tdate$ = ten99date$
                       call "DATEFMT" (tdate$, 0%, ten99date$)
                       convert str(ten99date$,1%,4%) to yr1099%
                       call "DATECONV" (ten99date$)    /* to internal fmt */

                       write #30, using L20790, yr1099%,      ~
                             newten99$, ten99key$, str(ten99key$,1,9),   ~
                             invoice$, datetime$, ten99date$,            ~
                             ten99posted$, amountpaid, ten99disc,        ~
                             ten99cacct$, ten99dacct$, ten99acct$,       ~
                             tempaccttype$, " ", eod goto L20710
L20790:                   FMT BI(2), CH(4), CH(20), CH(9), CH(16), CH(7),~
                              CH(6), CH(6), PD(14,4), PD(14,4),          ~
                              CH(9), CH(9), CH(9), CH(1), CH(6)

L20830:             REM Load next payment into buffer
                        call "READNXT1" (#8, f1%(8))
                        goto L20100             /* NEXT PAYMENT */

L30000:     REM *********************************************************~
                *       W R I T E   I N V O I C E   T O   F I L E       *~
                *-------------------------------------------------------*~
                * Write the invoice to the file.  Line items are loaded *~
                * as a data string from the buffer, and then are resaved*~
                * in the same type of data string in PAYLINES.          *~
                *********************************************************

            REM Transfer line items from PAYBUF2 to PAYLINES
                flag% = 0               /* flag for number transfered */

            REM Clear way for save... (no file status bytes 22)
                readkey$ = thisinvoicekey$
                call "DELETE" (#5, readkey$, 25%)
                call "DELETE" (#6, readkey$, 25%)
                call "TXTFUTIL" (#25, 0%, "DELE", str(header$(),169,4))

            REM Toss Lines...
L30180:         call "PLOWNEXT" (#10, readkey$, 25%, f1%(10))
                     if f1%(10) = 0 then L30240
                get #10 using L30201, linea$, lineb$, linec$
L30201:              FMT CH(250), CH(250), CH(41)
                write #6 using L30201, linea$, lineb$, linec$
                get #6 using L30204, contract$
L30204:              FMT POS(296), CH(20)
                if contract$ = " " then L30210
                     write #37 using L30209, contract$,  "I",             ~
                               str(linea$,36,9), str(linea$,45,19),      ~
                               contract$, " "
L30209:                   FMT CH(20), CH(1), CH(9), CH(19), CH(20), CH(64)
L30210:         flag% = 1
                goto L30180

L30240:     REM Toss Header...
                gosub serial_numbers
                if flag% = 0 and invoiceamount = 0 then return
                put header$(), using L30320, paydate$, openamount
                write #5, str(header$())
                call "TXTFUTIL" (#25, 0%, "TOSS", str(header$(),169,4))
                return

L30320:         FMT POS( 87), CH(6),      /* Date posted               */~
                    POS(119), PD(14,4)    /* Outstanding amount        */


        serial_numbers    /* Update status in master file              */
            plowkey$ = "VT" & thisinvoicekey$

L30390:     call "PLOWNEXT" (#18, plowkey$, 27%, f1%(18))
            if f1%(18) = 0% then return
                get #18 using L30420, str(readkey$,26), str(readkey$,,25)
L30420:              FMT POS(43), CH(20), CH(25)
                call "READ101" (#16, readkey$, f1%(16))
                if f1%(16) = 0% then L30390
                get #16 using L30460, sn_status$, sn_loc$, sn_trankey$
L30460:              FMT POS(1),CH(1), POS(183), CH(16), POS(216), CH(42)
                if str(sn_trankey$,,27) <> str(plowkey$,,27) then L30390
                if sn_status$ = "p" then sn_status$ = "0"
                if sn_status$ = "r" then sn_status$ = "2"
                if sn_status$ = "7" then sn_status$ = "2"
                put #16 using L30510, sn_status$
L30510:              FMT POS(1), CH(1)
L30520:              FMT POS(2), CH(30)
                if sn_status$ = "2" then put #16 using L30520, sn_loc$
                rewrite #16
                goto L30390


        REM Routine to handle valid A/P accounts...

            deffn'100 (ttacct$,ttype$)
                if ttype$         <> "L" then return
                if ttacct$         = " " then return
                if nonapacct$(1%)  = " " then return
                search str(nonapacct$(), 1%, 16% * nonacctnum%)          ~
                         = str(ttacct$) to index%() step 16%
                if index%(1%) =  0% then return
                   ttype$ = "N"
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

            end
