        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   IIIII  RRRR    CCC   RRRR   M   M  V   V   *~
            *  A   A  R   R    I    R   R  C   C  R   R  MM MM  V   V   *~
            *  AAAAA  RRRR     I    RRRR   C      RRRR   M M M  V   V   *~
            *  A   A  R   R    I    R   R  C   C  R   R  M   M   V V    *~
            *  A   A  R   R  IIIII  R   R   CCC   R   R  M   M    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIRCRMV - Creates new invoices from recurring masters    *~
            *            in ARIMASTR.  Also prints audit of what is     *~
            *            accomplished.                                  *~
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
            * 10/06/86 ! Original                                 ! ERN *~
            * 11/26/86 ! Added Settlement Number Logic            ! ERN *~
            * 12/14/87 ! Added Multi-currency, CURCONVR, ARILNCUR.! JIM *~
            * 08/24/88 ! PRR 10117- Delete ARILNCUR w/ MASTR&LINES! JIM *~
            * 10/06/88 ! Changed to copy text to new invoice      ! JDH *~
            * 05/31/89 ! Fixed deletion of ARIMASTR record        ! DWT *~
            * 10/10/89 ! Fixed Multi-Currency amounts being       ! JDH *~
            *          !  written to files.                       !     *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 10/07/92 ! Swap Generated Inv # for Recurring Inv #.! JDH *~
            * 03/29/95 ! PRR - 13187 Track Cross-Reference Parts  ! RJH *~
            *          !   for future reference and printing on   !     *~
            *          !   acknowledgements, and invoices, etc.   !     *~
            * 04/25/95 ! PRR - 13283 Additional Key to ARIBUFFR.  ! RJH *~
            * 07/24/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$9,                   /* Account X-Ref              */~
            billto$9,                    /* Bill-to X-Ref              */~
            company$60,                  /* Company Name               */~
            convdate$6,                  /* Currency conversion date   */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdflt$4,      /* Currency codes             */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9, cusname$30,       /* Ship-to Customer Number    */~
            date$8,                      /* Date for screen display    */~
            descrs$(8)30,                /* Group Descriptions         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            groups$(8)4,                 /* Recurring Group Codes      */~
            hdg$(3,2)30,                 /* Screen Headings            */~
            hdr$(10)200,                 /* Invoice Header             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdate$8,                   /* Invoice Date               */~
            invnr$8,                     /* New Invoice Number         */~
            last$(8)8,                   /* Last Cutover Dates         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(5)150,                 /* Invoice Line Items         */~
            line2$79,                    /* Second Screen Line         */~
            msg$50,                      /* Report Message             */~
            p%(1),                       /* Receiver for SEARCH        */~
            pf$(3)79, pfkey$20,          /* PF Keys                    */~
            plowkey$99,                  /* Misc Plow Key              */~
            postdate$8,                  /* Posting Date               */~
            rcrid$8,                     /* Recurring ID (Invoice)     */~
            readkey$100,                 /* Misc Read Key              */~
            runtime$8,                   /* Run Time                   */~
            session$6,                   /* Which Session to Post to   */~
            seq$3,                       /* ARILINES Sequence #        */~
            statutory$4,                 /* Statutory currency code    */~
            store$3,                     /* Store Number               */~
            termsdue(30),                /* Payment terms array        */~
            textid$4,                    /* Text ID                    */~
            userid$3,                    /* Current User Id            */~
            xrefdesc$32,                 /* Customer Xref Part Descr   */~
            xrefpart$25,                 /* Customer Xref Part         */~
            xreftype$1                   /* Xref Type -Cust or Manufctr*/~

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
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! STORNAME ! Store Master File                        *~
            * #4  ! GENCODES ! General Codes File                       *~
            * #5  ! ARIMASTR ! Invoice Master File                      *~
            * #6  ! ARILINES ! Invoice Line Items File                  *~
            * #7  ! ARINUMBR ! Invoice Number File                      *~
            * #9  ! ARIBUFFR ! Invoice Buffer File- Headers             *~
            * #10 ! ARIBUF2  ! Invoice Buffer File- Line Items          *~
            * #11 ! ARMTRIAL ! A/R Trial Balance                        *~
            * #12 ! TXTFILE  ! Text File                                *~
            * #41 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #42 ! ARIMSCUR ! Currency-specific ARI Master             *~
            * #43 ! ARILNCUR ! Currency-specific ARI lines              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alternate key 1, keypos = 10, keylen =  8, dup,  ~
                                  key 2, keypos = 18, keylen = 16, dup,  ~
                                  key 3, keypos = 34, keylen = 16, dup

            select #6,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #7,  "ARINUMBR",                                      ~
                        varc,     indexed,  recsize =   17,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =    10, keylen =  8, dup

            select #9,  "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos = 2001, keylen =   24,        ~
                            key  2, keypos =   34, keylen =   16, dup

            select #10, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #11, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

            select #12, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #41, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #42, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5,  keylen = 17,                      ~
                        alt key  1, keypos =   1, keylen =  21

            select #43, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   5,  keylen = 20,                      ~
                        alt key  1, keypos =   1, keylen =  24

        call "SHOSTAT" ("Opening Files, One Moment Please")

            rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            rslt$(5 ) = "REQUIRED"
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            rslt$(6 ) = "REQUIRED"
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 100%, rslt$(7 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 200%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 400%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 100%, rslt$(11))

            if min(fs%()) < 0% then exit_program

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
                if f1%(2) = 0% then L09000
            get #02 using L03120, curr$, statutory$, currtype$
L03120:         FMT POS(21), CH(1), CH(4), POS(29), CH(1)
            if curr$ <> "Y" then statutory$ = " "
            if curr$ <> "Y" then goto L09000
                call "OPENCHCK"(#41, fs%(41), f2%(41), f1%(41), rslt$(41))
                call "OPENCHCK"(#42, fs%(42), f2%(42),    200%, rslt$(42))
                call "OPENCHCK"(#43, fs%(43), f2%(43),    400%, rslt$(43))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        Check and see that some Recurring Codes do exist.
            readkey$ = "RECURGRPS" & hex(00)
            call "PLOWNEXT" (#4, readkey$, 9%, f1%(4))
            if f1%(4) = 1% then L09200
                call "ASKUSER" (0%, "NO GROUP CODES",                    ~
                                "There are no Recurring Groups on file.",~
                                " ", "Press any PF key to continue.")
                goto L65000

L09200
*        Now find out which session the new invoices go into
            u3% = 1%
            call "UPDUSRLG" ("ARIUPDTE", "ARIRCRMV",                     ~
                             "Recurring Invoice Cutover",                ~
                             "1", session$, u3%, postdate$, " ")
            if u3% <> 0% then L65000


*        Set up some misc variables
            line2$ = "Session: " & session$
            str(line2$,62) = "ARIRCRMV: " & str(cms2v$,,8)

            hdg$(1,1) = "Recurring"
            hdg$(1,2) = "Group Code"

            hdg$(2,2) = "Code Description"

            hdg$(3,1) = " Last  "
            hdg$(3,2) = "Cutover"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, groups$(), descrs$(),      ~
                      last$()
            invdate$ = date$

            for fieldnr% = 1% to 9%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10190
L10140:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  7 then       L10190
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10140
L10190:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
                      if keyhit% = 7% then edtpg1
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       process_invoices
                  if keyhit% <>  0 then       edtpg1
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1% or fieldnr% > 12% then edtpg1
            if fieldnr% = 1% then L11160
                fieldnr% = fieldnr% - 3%
                if fieldnr% < 2% or fieldnr% > 9% then edtpg1
L11160:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
            goto edtpg1

        REM *************************************************************~
            *             P R O C E S S   I N V O I C E S               *~
            *-----------------------------------------------------------*~
            * Cutover Invoices that meet the criteria                   *~
            *************************************************************
        process_invoices
            if str(groups$()) <> " " then L12100
                errormsg$ = "You must enter at least one Group Code."
                goto edtpg1

L12100
*        Do it to it!
            call "SHOSTAT" ("Recurring Invoice Cutover...")

*        Set up for reporting
            line% = 857%
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            select printer(134)
            call "SETPRNT" ("ARI002", " ", 0%, 0%)

*        And any other setups
            call "DATUNFMT" (postdate$)
            msg$ = "Invoice Date = " & invdate$
            gosub print_line
            call "DATUNFMT" (invdate$)

        invoice_loop
            call "READNXT1" (#5, f1%(5))
            if f1%(5) = 0% then end_processing

            get #5 using L12310, str(hdr$())
L12310:         FMT CH(2000)

*        Test and see if we have a candidate
            rcrid$ = str(hdr$(),10,8)
            if str(rcrid$,,4)    <> "RCR-" then invoice_loop
            if str(hdr$(),891,1) <> "R"    then invoice_loop
            search str(groups$()) = str(rcrid$,5,4) to p%() step 4
            if p%(1) = 0% then invoice_loop

*        Got One!!!! Either Kill it, Reject it, or Toss it.
            cuscode$ = str(hdr$(),  1, 9)
            cusname$ = str(hdr$(), 53,30)
            store$   = str(hdr$(),870, 3)
            invnr$   = " "
            currency$= " "
            if curr$ = "Y" then currency$= str(hdr$(),1779, 4)

            if str(hdr$(),527,6) >= date then L12540
                readkey$ = str(hdr$(),,17)
                call "DELETE" (#43, readkey$, 17%)
L12470:         call "PLOWNXT1" (#6, readkey$, 17%, f1%(6))
                if f1%(6) = 0% then L12505
                     get #6 using L12485, textid$
L12485:                   FMT POS(190), CH(4)
                     delete #6
                     call "TXTFUTIL" (#12, f2%(12), "DELE", textid$)
                     goto L12470
L12505:         call "TXTFUTIL" (#12, f2%(12), "DELE", str(hdr$(),901,4))
                call "READ101" (#5, key(#5), f1%(5))
                delete #5                     /* Kill expired master */
                call "DELETE" (#42, key(#5), 17%)
                call "PTUSEDSB" ("D", "ARI ", str(key(#5),,17%), "ALL",  ~
                                 " ", " ", " ", ret%)/* Del Xref Shadow */
                msg$ = "Expired Master Deleted"
                gosub print_line
                goto invoice_loop

L12540:     cuscode$ = str(hdr$(),,9)
                call "READ100" (#1, cuscode$, f1%(1))
                if f1%(1) = 1% then L12610
                     msg$ = "Rejected: Customer not on file."
L12580:              rejected% = rejected% + 1%
                     gosub print_line
                     goto invoice_loop
L12610:         get #1 using L12620, account$, billto$, currdflt$
L12620:              FMT POS(771), 2*CH(9), POS(1045), CH(4)
                if account$ = cuscode$ then L12690
                     call "READ100" (#1, account$, f1%(1))
                     if f1%(1) = 1% then L12690
                          msg$ = "Rejected: Account no longer on file: "&~
                                 account$
                          goto L12580
L12690:         if billto$ = cuscode$ then L12760
                     call "READ100" (#1, billto$, f1%(1))
                     if f1%(1) = 1% then L12760
                          msg$ = "Rejected: Bill-to no longer on file: "&~
                                 billto$
                          goto L12580

L12760:         call "READ100" (#3, store$, f1%(3))
                if f1%(3) = 1% then L12830
                     msg$ = "Rejected: Store no longer on file: " &      ~
                             store$
                     goto L12580


L12830
*        Passes Tests- Update and Toss
            call "ARINEXT" (#3, #2, #7, #11, "R", store$, cuscode$,      ~
                            billto$, invnr$)

*          IF CURRENCY$ = " " THEN CURRENCY$ = CURRDFLT$
            if currency$ = " " then currency$ = statutory$
            str(hdr$(),  10, 8) = invnr$           /* Invoice Number   */
            str(hdr$(), 521, 6) = invdate$         /* Invoice Date     */
            str(hdr$(), 527, 6) = " "              /* Expiration Date  */
            str(hdr$(), 533, 6) = postdate$        /* Posting Date     */
            str(hdr$(), 539, 6) = date             /* Date Entered     */
            str(hdr$(), 545, 3) = userid$          /* Entered By       */
            str(hdr$(), 849, 9) = billto$          /* Bill-to Number   */
            str(hdr$(), 858,12) = " "              /* Settlement #     */
            str(hdr$(), 891, 1) = "G"              /* Invoice Type     */
            str(hdr$(),1770, 9) = account$         /* Account Number   */
            str(hdr$(),1779, 4) = currency$        /* Currency code    */
            str(hdr$(),1783, 9) = billto$          /* Bill-to Number   */
            str(hdr$(),1801, 8) = invnr$           /* Invoice Number   */
            textid$ = str(hdr$(),901,4)
            call "TXTFUTIL" (#12, f2%(12), "COPY", textid$)
            call "TXTFUTIL" (#12, f2%(12), "SAVE", textid$)
            str(hdr$(),901,4) = textid$

            gosub header_currency_test

            put #9 using L12990, str(hdr$()), session$, "G", cuscode$,    ~
                                invnr$
L12990:         FMT CH(2000), CH(6), CH(1), CH(9), CH(8)
            write #9

            plowkey$ = str(cuscode$) & rcrid$
L13030:     call "PLOWNEXT" (#6, plowkey$, 17%, f1%(6))
            if f1%(6) = 0% then L13110
                get #6 using L13060, str(line$())
L13060:              FMT CH(750)
                gosub line_currency_test
                gosub get_xref_part_data
                str(line$(), 10,8) = invnr$         /* Text ID */
                textid$ = str(line$(),190,4)
                call "TXTFUTIL" (#12, f2%(12), "COPY", textid$)
                call "TXTFUTIL" (#12, f2%(12), "SAVE", textid$)
                str(line$(),190,4) = textid$
                write #10 using L13060, str(line$())
                gosub put_xref_part_data
                goto L13030

L13110:     msg$ = " "
            gosub print_line
            created% = created% + 1%
            invnr$ = " "
            goto invoice_loop


        end_processing
            if line% > 52% then gosub page_heading
            print
            print using L15130, created%, rejected%
            print
            print "***  END OF REPORT ***"
            close printer
            call "SETPRNT" ("ARI002", " ", 0%, 1%)
            for g% = 1% to 8%
                if groups$(g%) = " " then L13330
                     readkey$ = "ARIRCRMV." & groups$(g%)
                     call "READ101" (#2, readkey$, f1%(2))
                     put #2 using L13310, readkey$, date, " "
L13310:                   FMT CH(20), CH(250), CH(230)
                     if f1%(2) = 0% then write #2 else rewrite #2
L13330:     next g%
            goto exit_program



        print_line
            if line% > 55% then gosub page_heading
            print using L15100, cuscode$, cusname$, rcrid$, invnr$, msg$
            line% = line% + 1%
            return


        page_heading
            page% = page% + 1%  :  line% = 7%
            print page
            print using L15000, date$, runtime$, company$
            print using L15020, session$, page%
            print
            print using L15040
            print using L15060
            print using L15080
            return

        get_xref_part_data
            call "PTUSEDSB" ("R", "ARI ", str(line$(),,17%),             ~
                              str(line$(),18%,3%), xrefpart$, xrefdesc$, ~
                              xreftype$, ret%)
            if ret% = 0% then xrefpart$, xrefdesc$, xreftype$ = " "
            return

        put_xref_part_data
            if xrefpart$ = " " then return
            call "PTUSEDSB" ("W", "ARI ", str(line$(),,17%),             ~
                              str(line$(),18%,3%), xrefpart$, xrefdesc$, ~
                              xreftype$, ret%)
            return

        line_currency_test
            if curr$ <> "Y" then return
            if currency$ = statutory$ then return

            call "READ100" (#43, key(#6), f1%(43))
                if f1%(43) = 0% then return

            get #43 using L14050, seq$, upstk, uprce, dscnt, extsn
L14050:         FMT POS(22), CH(3), 4*PD(14,4)

*        Write line item to ARIBUF2 'shadow' file ARILNCUR ...
            write #43 using L14120, currency$, cuscode$, invnr$, seq$,    ~
                upstk, uprce, dscnt, extsn, convdate$, conveqv, convunt, ~
                " "
L14120:         FMT CH(4), CH(9), CH(8), CH(3), 4*PD(14,4), CH(6),       ~
                    2*PD(14,7), CH(22)

            upstk = upstk * conveqv
            upric = upric * conveqv
            dscnt = dscnt * conveqv
            extsn = extsn * conveqv
            put str(line$()) using L14195, seq$, upstk, upric, dscnt, extsn
L14195:         FMT POS(18), CH(3), POS(109), PD(14,4), POS(133),        ~
                    PD(14,4), POS(149), 2*PD(14,4)
            return

        header_currency_test
            convdate$ = " " : conveqv, convunt = 1
            if curr$ <> "Y" then return
            if currency$ = statutory$ then return

            call "DATREVRS" ( invdate$, rev_date$, errormsg$ )
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then get #41 using L14350, convdate$,        ~
                conveqv, convunt
L14350:         FMT POS(12), CH(6), 2*PD(14,7)

            call "READ100" (#42, key(#5), f1%(42))
                if f1%(42) = 0% then return

            get #42 using L14390, gross, dscnt, frt, tax, net, bal,       ~
                    termsdue()
L14390:         FMT POS(22), 6*PD(14,4), POS(92), 30*PD(14,4)

*        Write Invoice Header to ARIBUFFR 'shadow' file ARIMSCUR ...
            write #42 using L14460, currency$, cuscode$, invnr$, gross,   ~
                dscnt, frt, tax, net, bal, convdate$, conveqv, convunt,  ~
                termsdue(), " "
L14460:         FMT CH(4), CH(9), CH(8), 6*PD(14,4), CH(6), 2*PD(14,7),  ~
                    30*PD(14,4), CH(69)
            gross = gross * conveqv
            dscnt = dscnt * conveqv
            frt   = frt   * conveqv
            tax   = tax   * conveqv
            net   = net   * conveqv
            bal   = bal   * conveqv
            mat termsdue = (conveqv) * termsdue

            put str(hdr$()) using L14565, gross, dscnt, frt, tax, net,    ~
                bal, termsdue()
L14565:         FMT POS(793), PD(14,4), POS(809), 5*PD(14,4), POS(928),  ~
                    30*PD(14,4)
            return

L15000: %RUN DATE: ######## ########       ##############################~
        ~##############################     ARIRCRMV-ARI002
L15020: % SESSION: ######                             RECURRING INVOICE C~
        ~UTOVER AUDIT LISTING                    PAGE: ####
L15040: %                                           RECURRING   INVOICE  ~

L15060: % CUSTOMER  CUSTOMER NAME                   ID NUMBER   NUMBER   ~
        ~C O M M E N T S
L15080: %---------  ------------------------------  ---------  --------  ~
        ~--------------------------------------------------
L15100: %#########  ##############################   ########  ########  ~
        ~##################################################

L15130: %             ##### INVOICES CREATED;  #### REJECTED.

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20190,         /* Invoice Date     */~
                                    L20230,         /* Recurring Group  */~
                                    L20230,         /* Recurring Group  */~
                                    L20230,         /* Recurring Group  */~
                                    L20230,         /* Recurring Group  */~
                                    L20230,         /* Recurring Group  */~
                                    L20230,         /* Recurring Group  */~
                                    L20230,         /* Recurring Group  */~
                                    L20230          /* Recurring Group  */
                     return

L20190
*        Invoice Date                          INVDATE$
            inpmessage$ = "Enter Invoice Date"
            return

L20230
*        Recurring Group Code
            inpmessage$ = "Enter Group Code to Cutover."
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
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            gosub setpfkeys
            on fieldnr% gosub       L40240,         /* Invoice Date     */~
                                    L40240,         /* Recurring Group  */~
                                    L40240,         /* Recurring Group  */~
                                    L40240,         /* Recurring Group  */~
                                    L40240,         /* Recurring Group  */~
                                    L40240,         /* Recurring Group  */~
                                    L40240,         /* Recurring Group  */~
                                    L40240,         /* Recurring Group  */~
                                    L40240          /* Recurring Group  */
            goto L40310

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40240:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40310:     accept                                                       ~
               at (01,02),                                               ~
                  "Cutover Recurring Invoices",                          ~
               at (01,62), "Post Date: ",                                ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,13), "Invoice Date",                               ~
               at (06,30), fac(lfac$( 1)), invdate$             , ch(08),~
                                                                         ~
               at (08,13), fac(hex(8c)), hdg$(1,1)              , ch(10),~
               at (09,13), fac(hex(ac)), hdg$(1,2)              , ch(10),~
               at (09,25), fac(hex(ac)), hdg$(2,2)              , ch(30),~
               at (08,57), fac(hex(8c)), hdg$(3,1)              , ch(08),~
               at (09,57), fac(hex(ac)), hdg$(3,2)              , ch(08),~
                                                                         ~
               at (10,10), "1",  at (11,10), "2",  at (12,10), "3",      ~
               at (13,10), "4",  at (14,10), "5",  at (15,10), "6",      ~
               at (16,10), "7",  at (17,10), "8",                        ~
                                                                         ~
               at (10,16), fac(lfac$( 2)), groups$( 1)          , ch(04),~
               at (10,25), fac(hex(8c))  , descrs$( 1)          , ch(30),~
               at (10,57), fac(hex(8c))  , last$  ( 1)          , ch(08),~
                                                                         ~
               at (11,16), fac(lfac$( 3)), groups$( 2)          , ch(04),~
               at (11,25), fac(hex(8c))  , descrs$( 2)          , ch(30),~
               at (11,57), fac(hex(8c))  , last$  ( 2)          , ch(08),~
                                                                         ~
               at (12,16), fac(lfac$( 4)), groups$( 3)          , ch(04),~
               at (12,25), fac(hex(8c))  , descrs$( 3)          , ch(30),~
               at (12,57), fac(hex(8c))  , last$  ( 3)          , ch(08),~
                                                                         ~
               at (13,16), fac(lfac$( 5)), groups$( 4)          , ch(04),~
               at (13,25), fac(hex(8c))  , descrs$( 4)          , ch(30),~
               at (13,57), fac(hex(8c))  , last$  ( 4)          , ch(08),~
                                                                         ~
               at (14,16), fac(lfac$( 6)), groups$( 5)          , ch(04),~
               at (14,25), fac(hex(8c))  , descrs$( 5)          , ch(30),~
               at (14,57), fac(hex(8c))  , last$  ( 5)          , ch(08),~
                                                                         ~
               at (15,16), fac(lfac$( 7)), groups$( 6)          , ch(04),~
               at (15,25), fac(hex(8c))  , descrs$( 6)          , ch(30),~
               at (15,57), fac(hex(8c))  , last$  ( 6)          , ch(08),~
                                                                         ~
               at (16,16), fac(lfac$( 8)), groups$( 7)          , ch(04),~
               at (16,25), fac(hex(8c))  , descrs$( 7)          , ch(30),~
               at (16,57), fac(hex(8c))  , last$  ( 7)          , ch(08),~
                                                                         ~
               at (17,16), fac(lfac$( 9)), groups$( 8)          , ch(04),~
               at (17,25), fac(hex(8c))  , descrs$( 8)          , ch(30),~
               at (17,57), fac(hex(8c))  , last$  ( 8)          , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L40930
                  call "MANUAL" ("ARIRCRMV") : goto L40310

L40930:        if keyhit% <> 15 then L40960
                  call "PRNTSCRN" : goto L40310

L40960:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpfkeys
        if edit% = 2% then L41110         /* Input Mode                 */
           pf$(1) = "(1)Start Over      (7)Proceed to Edit Mode        "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkey$ = hex(01ffffffffff07ffffffffff0dff0f10ffffff00)
           return

L41110:  if fieldnr% > 0% then L41220     /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)PROCEED     "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           return

                                         /* Edit Mode- Field Enabled   */
L41220:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50190,         /* Invoice Date     */~
                                    L50230,         /* Recurring Group  */~
                                    L50230,         /* Recurring Group  */~
                                    L50230,         /* Recurring Group  */~
                                    L50230,         /* Recurring Group  */~
                                    L50230,         /* Recurring Group  */~
                                    L50230,         /* Recurring Group  */~
                                    L50230,         /* Recurring Group  */~
                                    L50230          /* Recurring Group  */
                  return

L50190
*        Invoice Date                          INVDATE$
            call "DATEOK" (invdate$, u3%, errormsg$)
            return

L50230
*        Recurring Group Code                  GROUPS$()
            f% = fieldnr% - 1%
            if groups$(f%) <> " " then L50280
                descrs$(f%), last$(f%) = " "
                return
L50280:     readkey$ = "RECURGRPS" & groups$(f%)
            call "PLOWCODE" (#4, readkey$, descrs$(f%), 9%, 0.30, f1%(4))
            if f1%(4) = 1% then L50330
                errormsg$ = "Recurring Group Code Not on file."
                return
L50330:     groups$(f%) = str(readkey$,10,4)
            readkey$ = "ARIRCRMV." & groups$(f%)
            call "READ100" (#2, readkey$, f1%(2))
            last$(f%) = " "
            if f1%(2) = 1% then get #2 using L50380, last$(f%)
L50380:         FMT XX(20), CH(6)
            call "DATEFMT" (last$(f%))
*        Check that code is not already entered.
            temp$ = groups$(f%)
            groups$(f%) = " "
            search str(groups$()) = str(temp$,,4) to p%() step 4
            groups$(f%) = temp$
            if p%(1) = 0% then return
                errormsg$ = "Group Code already entered"
                return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
            call "SHOSTAT" ("One Moment, Please")
            u3% = 2%
            call "UPDUSRLG" ("ARIUPDTE", " ", " ", " ", session$, u3%,   ~
                                                                " ", " ")
            end
