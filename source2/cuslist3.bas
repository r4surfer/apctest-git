        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U   SSS   L      IIIII   SSS   TTTTT 3333     *~
            *  C   C  U   U  S      L        I    S        T       3    *~
            *  C      U   U   SSS   L        I     SSS     T    333     *~
            *  C   C  U   U      S  L        I        S    T       3    *~
            *   CCC    UUU    SSS   LLLLL  IIIII   SSS     T   3333     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSLIST3 - Lists Customer Master File.                    *~
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
            * 06/16/86 ! Original                                 ! ERN *~
            * 04/28/87 ! Fixed problem with duplicate sort names  ! ERN *~
            * 11/16/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/30/92 ! Added Run Time to End of Report.         ! JIM *~
            * 12/23/92 ! QC Rework- CCRMASTR dynamic data deleted.! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12,                     /* Acct # for test/descr rtn  */~
            acctdescr$32,                /* Descr  for test/descr rtn  */~
            accttype$1,                  /* Type   for test/descr rtn  */~
            acctxref$9,                  /* Account Cross Reference    */~
            aracct$12,                   /* Net Invoice Distribution   */~
            aracctdescr$32,              /* Net Invoice Distribution   */~
            baldate$8,                   /* High A/R Balance Date      */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            bo$1,                        /* Allow Backorders?          */~
            cashacct$12,                 /* Cash-in-Bank Account       */~
            cashacctdescr$32,            /* Cash-in-Bank Account       */~
            comm%(3), comm$(3)3,         /* Commission Split %s        */~
            company$60,                  /* Company Name               */~
            contact$20,                  /* Customer Contact           */~
            crchng$8, cruser$3,          /* CR Limit change audit      */~
            cursor%(2),                  /* Cursor location for edit   */~
            customer$9,                  /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            dfrom$30, dto$30,            /* Range (Display)            */~
            disc$5,                      /* Standard Order Discount %  */~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctdescr$32,            /* Sales Discounts Account    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            exempt$25,                   /* Tax Exemption Number       */~
            fcacct$12,                   /* Finance Charge Account     */~
            fcacctdescr$32,              /* Finance Charge Account     */~
            fctable$1,                   /* Finance Charge Table       */~
            fctabledescr$32,             /* Finance Charge Table       */~
            fob$20,                      /* FOB                        */~
            from$30, to$30,              /* Print Range                */~
            frtacct$12,                  /* Freight Account            */~
            frtacctdescr$32,             /* Freight Account            */~
            howship$20,                  /* How Ship                   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Messages     */~
            lastcash$8,                  /* Last Cash Receipts Date    */~
            lastchng$8,                  /* Date record last changed   */~
            lastinv$8,                   /* Date of last invoice       */~
            lastused$8,                  /* Date record last update    */~
            lastuser$3,                  /* User who made last change  */~
            late$1,                      /* Print Late Notices?        */~
            lateship$1,                  /* Allow Late Shipments?      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            limit$10,                    /* Credit Limit               */~
            line2$79,                    /* Second Line of Screen Headr*/~
            main$9 , mainprt$9 ,         /* Account Printing           */~
            name$30, nameprt$30,         /* Account Printing           */~
            opened$8,                    /* Date account opened        */~
            pc$1,                        /* Standard Price Code        */~
            pcdescr$32,                  /* Standard Price Code        */~
            pf4$17, pf16$16,             /* PF Descriptors             */~
            phone$12,                    /* Phone Number               */~
            plowkey$40,                  /* Plowkey                    */~
            poreqd$1,                    /* PO Required?               */~
            readkey$50,                  /* Misc. Purpose Read Key     */~
            region$4,                    /* Sales Region Code          */~
            regiondescr$32,              /* Sales Region Code          */~
            runtime$8,                   /* Report run time            */~
            sales$12,                    /* Sales Distribution Account */~
            salesdescr$32,               /* Sales Distribution Account */~
            salesmen$(3)4,               /* Salesmen/ Commission Splits*/~
            salesmendescr$(3)32,         /* Salesmen/ Commission Splits*/~
            seq$1,                       /* Output Sequence (C/S)      */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shiprgn$9,                   /* Shipping Region Code       */~
            shiprgndescr$32,             /* Shipping Region Code       */~
            shipto$(6)30,                /* Ship-to Name and Address   */~
            soldto$(6)30,                /* Sold-to Name and Address   */~
            sort$30,                     /* Customer Sort Name         */~
            status$1,                    /* Customer Status            */~
            statusdescr$32,              /* Customer Status            */~
            stmnt$1,                     /* Statement Printing Flag    */~
            taxable$1,                   /* Customer Taxable?          */~
            taxacct$12,                  /* Sales Tax Account          */~
            taxacctdescr$32,             /* Sales Tax Account          */~
            taxcode$10,                  /* Sales Tax Code             */~
            taxcodedescr$32,             /* Sales Tax Code             */~
            terms$20, termsdescr$30,     /* Payment Terms (Code)       */~
            textid$4,                    /* Text ID                    */~
            textopt$1,                   /* Include Test? (Y/N)        */~
            type$2,                      /* Customer Type Code         */~
            typedescr$32,                /* Customer Type Code         */~
            vf$200                       /* Variable Fields            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! TXTFILE  ! System Text File                         *~
            * #9  ! CUSTOMER ! Customer Master File                     *~
            * #10 ! GENCODES ! General Codes File                       *~
            * #11 ! STXCODES ! Sales Tax Codes                          *~
            * #12 ! SLMMASTR ! Salesman master file                     *~
            * #13 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #14 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #15 ! GLMAIN   ! General Ledger Main File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select #2,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #9,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select #10, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #11, "STXCODES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  10                      ~

            select #12, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4                      ~

            select #13, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20                      ~

            select #14, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #15, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#2,  fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#9,  fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62) = "CUSLIST3: " & str(cms2v$,,8)
            rlen% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            page% = -1%
            pf4$="(4)Previous Field" : pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$,                            ~
                      seq$                        ,/* Output Sequence  */~
                      dfrom$                      ,/* From Range       */~
                      dto$                        ,/* TO Range         */~
                      textopt$                     /* Include Test?    */

            for fieldnr% = 1% to  2%
L10310:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10430
L10330:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10410
L10360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10330
                         if fieldnr% = 1% then L10310
                         goto L10360
L10410:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " "
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       edtpg1
            fieldnr% = cursor%(1) - 5
            if fieldnr% > 2% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1 or fieldnr% >  4 then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf16$ = " "
L11190:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
            goto edtpg1


        REM *************************************************************~
            *               P R I N T   R E P O R T                     *~
            *-----------------------------------------------------------*~
            * Print listing as requested.                               *~
            *************************************************************
        print_report
            call "SHOSTAT" ("Printing Report")
            runtime$ = " " : call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            line% = 857%
            select printer(134)
            call "SETPRNT" ("CUS003", " ", 0%, 0%)
            plowkey$ = from$
            call "PLOWALTS" (#1, plowkey$, key%, 0%, f1%(1))
            goto L12160

        report_loop
            call "READNEXT" (#1, f1%(1))
L12160:     if f1%(1)   = 0%  then end_of_report
            plowkey$  = key(#1, key%)
            if plowkey$ > to$ then end_of_report

*        See if this is an Account Record
            get #1 using L12210, main$, name$, acctxref$
L12210:         FMT CH(9), CH(30), POS(771), CH(9)
            if acctxref$ <> main$ then report_loop
            mainprt$ = main$  :  nameprt$ = name$
            call "REDALT0" (#9, main$, 3%, f1%(9))
            if f1%(9) = 0% then report_loop /* Should get himself!?!?  */
            goto L12340

        account_loop
            call "READNEXT" (#9, f1%(9))
            if f1%(9) > 0% then L12340
L12310:         print
                line% = line% + 1%
                goto report_loop
L12340:     get #9 using L12350, acctxref$
L12350:         FMT POS(771), CH(9)
            if acctxref$ <> main$ then L12310

            count% = count% + 1%
            gosub load_record
            if line% > 55% then gosub page_heading
            print using L14120, mainprt$, nameprt$,                       ~
                               customer$, shipto$(1), billxref$,         ~
                               contact$, phone$, salesmen$(1)
            line% = line% + 1%
            mainprt$, nameprt$ = " "
            goto account_loop


        page_heading
            page% = page% + 1%
            if page% = 0% then gosub print_params
            print page
            print using L14000, date$, runtime$, company$
            print using L14030, page%
            print
            print
            print using L14060, "#", "#", "#"
            print using L14090
            line% = 7%
            mainprt$ = main$ : nameprt$ = name$
            return


        end_of_report
            if line% > 55% then gosub page_heading
            print
            print using L14180, count%
            print
            runtime$ = " " : call "TIME" (runtime$)
            print "END OF REPORT", runtime$
            close printer
            call "SETPRNT" ("CUS003", " ", 0%, 1%)
            goto inputmode


        print_params
            print page
L12750:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L12790
                str(i$(), i%, 1%) = hex(20)
                goto L12750
L12790:     print using L14000, date$, runtime$, company$
            print using L14030, page%
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26)
            print "------------------------------------------------------~
        ~--------------------------"
            page% = page% + 1%
            return

L14000: %RUN DATE: ######## ########            #########################~
        ~###################################               CUSLIST3-CUS003

L14030: %                                                     SHIP-TO LIS~
        ~TING BY MAIN ACCOUNT                                  PAGE: ###

L14060: %ACCOUNT # ACCOUNT SORT NAME              SHIP-TO # SHIP-TO NAME ~
        ~                 BILL-TO # CONTACT              PHONE NUMBER SLMN

L14090: %--------- ------------------------------ --------- -------------~
        ~---------------- --------- -------------------- ------------ ----

L14120: %######### ############################## ######### #############~
        ~################ ######### #################### ############ ####


L14180: %   ###,### CUSTOMERS LISTED


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Output Sequence  */~
                                    L20200          /* From Range       */
                     return

L20100
*        Output Sequence                       SEQ$
            inpmessage$ = "Enter 'C' for Customer Code -OR- 'S' for Sor"&~
                "t Name."
            return

L20200
*        Customer Range                        DFROM$ / DTO$
            inpmessage$ = "Enter Range of Customers to Print."
            if dfrom$  = " " then dfrom$ = "ALL"
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
            *                 L O A D   R E C O R D                     *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_record

            get #9 using L35060, customer$, sort$, soldto$(), opened$,    ~
                lastuser$, shipto$(), contact$, phone$, sales$, aracct$, ~
                cashacct$, frtacct$, discacct$, taxacct$, disc, pc$,     ~
                limit, crchng$, cruser$, terms$, howship$, fob$,         ~
                shiprgn$, bo$, lateship$, shipinstr$(), salesmen$(),     ~
                comm%(), region$, acctxref$, billxref$, textid$, status$,~
                taxable$, exempt$, vf$, poreqd$, stmnt$, late$, type$,   ~
                taxcode$, fcacct$, fctable$
            if soldto$(1) = "BILLTO" then                                ~
                soldto$(1) = "-- SAME AS BILL-TO ADDRESS --"
            if soldto$(1) = " " then                                     ~
                soldto$(1) = "-- SAME AS SHIP-TO ADDRESS --"
            if phone$ <> " " then                                        ~
                phone$ = str(phone$,,3) & "-" & str(phone$,4,3) & "-" &  ~
                         str(phone$, 7)
            call "LINSMASH" (shipto$())
            call "LINSMASH" (soldto$())
            call "DATEFMT" (opened$)
            call "DATEFMT" (lastused$)
            call "DATEFMT" (lastinv$)
            call "DATEFMT" (lastcash$)
            call "DATEFMT" (lastchng$)
            call "DATEFMT" (crchng$)
            call "DATEFMT" (baldate$)
            call "CONVERT" (disc   , 2.2, disc$   )
            call "CONVERT" (limit  , 0.0, limit$  )
            for i% = 1% to 3%
                if salesmen$(i%) = " " then L30390
                     call "DESCRIBE" (#12, salesmen$(i%),                ~
                                         salesmendescr$(i%), 0%, f1%(12))
                     convert comm%(i%) to comm$(i%), pic(###)
L30390:     next i%
            acct$ =  sales$    : gosub describe_acct
                sales$    = acct$ : salesdescr$ = acctdescr$
            acct$ =  discacct$ : gosub describe_acct
                discacct$ = acct$ : discacctdescr$ = acctdescr$
            acct$ =  aracct$   : gosub describe_acct
                aracct$   = acct$ : aracctdescr$ = acctdescr$
            acct$ =  cashacct$ : gosub describe_acct
                cashacct$ = acct$ : cashacctdescr$ = acctdescr$
            acct$ =  frtacct$  : gosub describe_acct
                frtacct$  = acct$ : frtacctdescr$ = acctdescr$
            acct$ =  taxacct$  : gosub describe_acct
                taxacct$  = acct$ : taxacctdescr$ = acctdescr$
            acct$ =  fcacct$   : gosub describe_acct
                fcacct$   = acct$ : fcacctdescr$ = acctdescr$
            if billxref$  = customer$ then billxref$ = " "
            if pc$ = " " then L30630
                readkey$ = "PRICECODE" & pc$
                call "DESCRIBE" (#10, readkey$, pcdescr$, 0%, f1%(10))
L30630:     if shiprgn$ = " " then L30660
                readkey$ = "SHPREGION" & shiprgn$
                call "DESCRIBE" (#10, readkey$, shiprgndescr$, 0%,f1%(10))
L30660:     if region$ = " " then L30690
                readkey$ = "REGIONS  " & region$
                call "DESCRIBE" (#10, readkey$, regiondescr$, 0%,f1%(10))
L30690:     if type$ = " " then L30720
                readkey$ = "CUS TYPES" & type$
                call "DESCRIBE" (#10, readkey$, typedescr$, 0%, f1%(10))
L30720:     if status$ = "A" then statusdescr$ = "ACTIVE"
            if status$ = "H" then statusdescr$ = "ON HOLD"
            if status$ = "I" then statusdescr$ = "INACTIVE"
            if status$ = "D" then statusdescr$ = "TO BE DELETED"
            call "DESCRIBE" (#11, taxcode$, taxcodedescr$, 0%, f1%(11))
            fctabledescr$ = "No Finance Charges"
            if fctable$ = " " then L30810
                readkey$ = "FINANCECHARGETABLE" & fctable$
                call "DESCRIBE" (#14, readkey$, fctabledescr$, 0%,f1%(14))
L30810:     termsdescr$ = "0% 0 Days, Net 0 Days"
            if terms$ = " " then L30840
            call "DESCRIBE" (#13, terms$, termsdescr$, 0%, f1%(13))
L30840:     return



        describe_acct    /* Describe account number                    */
            call "DESCRIBE" (#15, acct$, acctdescr$, 0%, f1%(15))
            if f1%(15) = 1% then L30940
                acctdescr$ = "[Use System Default]"
                accttype$  = " "
                return
L30940:     call "GLFMT" (acct$)
            get #15 using L30960, accttype$
L30960:              FMT XX(39), CH(1)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: CUSTOMER                          */~
            CH(9),          /* Customer Code                           */~
            CH(30),         /* Sort Name                               */~
            6*CH(30),       /* Sold-to Name and Address                */~
            CH(6),          /* Date account opened                     */~
            XX(24),         /* Used/Inv'd/Cash/Modified  (CCRMASTR)    */~
            CH(3),          /* User ID of Last Modification            */~
            6*CH(30),       /* Ship-to Name and Address                */~
            CH(20),         /* Contact                                 */~
            CH(10),         /* Phone number                            */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Receivables Account Number              */~
            CH(9),          /* Cash-in-Bank Account                    */~
            CH(9),          /* Freight Account                         */~
            CH(9),          /* Sales Discounts Account                 */~
            CH(9),          /* Sales tax account                       */~
            PD(14,4),       /* Order Discount Percent                  */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Credit Limit                            */~
            CH(6),          /* Date CR Limit last modified             */~
            CH(3),          /* User ID of Last Modification            */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* FOB Information                         */~
            CH(9),          /* Shipping Region Code                    */~
            CH(1),          /* Allow Backorders Flag                   */~
            CH(1),          /* Allow Late Shipments flag               */~
            2*CH(50),       /* Shipping Instructions                   */~
            3*CH(4),        /* Salesman codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Sales Region Code                       */~
            XX(32),         /* O/O, Rlsed, A/R, High A/R (CCRMASTR)    */~
            XX(6),          /* High A/R Balance Date     (CCRMASTR)    */~
            CH(9),          /* Account Xref                            */~
            CH(9),          /* Bill-to Xref                            */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Customer Status                         */~
            CH(1),          /* Customer Taxable?(Y/N)                  */~
            CH(25),         /* Tax Exemption Number                    */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(1),          /* Purchase Order Required?                */~
            CH(1),          /* Statement Print Flag                    */~
            CH(1),          /* Print Late Notices?                     */~
            CH(2),          /* Customer Type                           */~
            CH(10),         /* Sales Tax Code                          */~
            CH(9),          /* Finance Charge G/L Account Number       */~
            CH(1),          /* Finance Charge Code                     */~
            CH(156)         /* Filler                                  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40210,         /* Output Sequence  */~
                                    L40210          /* Range            */
                  goto L40280

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40210:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Customer Master Listing - Ship-to by Account",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Output Sequence",                            ~
               at (06,30), fac(lfac$( 1)), seq$                 , ch(01),~
                                                                         ~
               at (07,02), "Customer Range- From",                       ~
               at (07,30), fac(lfac$( 2)), str(dfrom$,,rlen%),           ~
                                                                         ~
               at (08,02), "                  To",                       ~
               at (08,30), fac(lfac$( 2)), str(dto$  ,,rlen%),           ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("CUSLIST3")
                  goto L40280

L40650:        if keyhit% <> 15 then L40690
                  call "PRNTSCRN"
                  goto L40280

L40690:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Output Sequence  */~
                                    L50200          /* From Range       */
                  return

L50100
*        Output Sequence                       SEQ$
            if seq$ = "C" or seq$ = "S" then L50130
                errormsg$ = "Enter 'C' or 'S'."  :  return
L50130:     rlen% = 9%  :  key% = 0%          /* Set for Customer Code */
            if seq$ = "C" then return
                rlen% = 30%  :  key% = 1%/* Set for Customer Sort Name */
                return

L50200
*        Customer Range- From                  DFROM$ / DTO$
            call "TESTRNGE" (str(dfrom$,,rlen%), str(dto$,,rlen%),       ~
                             str( from$,,rlen%), str( to$,,rlen%),       ~
                             errormsg$ )
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

            end
