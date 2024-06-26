        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   PPPP    CCC   L      IIIII   SSS   TTTTT  222     *~
            *  A   A  P   P  C   C  L        I    S        T   2  2     *~
            *  AAAAA  PPPP   C      L        I     SSS     T     2      *~
            *  A   A  P      C   C  L        I        S    T    2       *~
            *  A   A  P       CCC   LLLLL  IIIII   SSS     T   22222    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCLIST2 - Lists Customer Master File (with Balcances)    *~
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
            * 04/12/88 ! Fixed truncating zip code                ! MJB *~
            * 06/02/88 ! Added Page Zero                          ! MJB *~
            * 02/20/89 ! Formatted ZIP Code on listing xxxxx-xxxx ! MJB *~
            * 08/26/91 ! SPECIAL TEXT FOR SALES  - LINE 30622     ! RHH *~
            * 10/04/91 ! Add Customer Balcnces to Report          ! RHH *~
            *          !                                          !     *~
            * 11/11/97 ! Revision Update For 60403                ! DJD *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12,                     /* Acct # for test/descr rtn  */~
            acctdescr$32,                /* Descr  for test/descr rtn  */~
            accttype$1,                  /* Type   for test/descr rtn  */~
            acctxref$9,                  /* Account Cross Reference    */~
            acctxrefdescr$32,            /* Account Cross Reference    */~
            aracct$12,                   /* Net Invoice Distribution   */~
            aracctdescr$32,              /* Net Invoice Distribution   */~
            baldate$8,                   /* High A/R Balance Date      */~
            bals(4),                     /* Summary Balance Info       */~
            bals$(4)15,                  /* Summary Balance Info       */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            billxrefdescr$32,            /* Bill-to Cross Reference    */~
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
            linemask$132,                /* Text printing mask         */~
            opened$8,                    /* Date account opened        */~
            pc$1,                        /* Standard Price Code        */~
            pcdescr$32,                  /* Standard Price Code        */~
            rhh$32,                      /* SPECIAL TEXT               */~
            pf4$17, pf16$16,             /* PF Descriptors             */~
            phone$12,                    /* Phone Number               */~
            plowkey$40,                  /* Plow key                   */~
            poreqd$1,                    /* PO Required?               */~
            readkey$50,                  /* Misc. Purpose Read Key     */~
            region$4,                    /* Sales Region Code          */~
            regiondescr$32,              /* Sales Region Code          */~
            runtime$8,                   /* Report run time            */~
            sales$12,                    /* Sales Distribution Account */~
            salesdescr$32,               /* Sales Distribution Account */~
            salesmen$(3)4,               /* Salesmen/ Commission Splits*/~
            salesmendescr$(3)32,         /* Salesmen/ Commission Splits*/~
            seq$1,                       /* Output Sequence (N/S)      */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shiprgn$9,                   /* Shipping Region Code       */~
            shiprgndescr$32,             /* Shipping Region Code       */~
            shipto$(6)31,                /* Ship-to Name and Address   */~
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
            terms$20,                    /* Payment Terms (Code)       */~
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
            cms2v$ = "R6.04.03 11/11/97 CMS2 / CMS-I Merge              "
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
            str(line2$,62) = "APCLIST2: " & str(cms2v$,,8)
            rlen% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            page% = -1%
            pf5$ = " "  :  pf16$ = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, runtime$,                  ~
                      seq$                        ,/* Output Sequence  */~
                      dfrom$                      ,/* From Range       */~
                      dto$                        ,/* TO Range         */~
                      textopt$                     /* Include Test?    */

            for fieldnr% = 1% to  3%
                if fieldnr% = 1% then pf4$ = " "                         ~
                                 else pf4$ = "(4)Previous Field"
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
            pf5$  = " "
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
                  pf4$, pf5$, pf16$ = " "
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
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            line% = 857%
            select printer(134)
            call "SETPRNT" ("CUS002", " ", 0%, 0%)
            plowkey$ = from$
            call "PLOWALTS" (#1, plowkey$, key%, 0%, f1%(1))
            goto L12160

        report_loop
            call "READNEXT" (#1, f1%(1))
L12160:     if f1%(1)   = 0%  then end_of_report
            plowkey$  = key(#1, key%)
            if plowkey$ > to$ then end_of_report

            count% = count% + 1%
            gosub load_record
            if line% > 56% then gosub page_heading
            print using L14120, customer$, sort$, shipto$(1), acctxref$,  ~
                               billxref$, contact$, phone$, salesmen$(1)
            line% = line% + 1%
            for i% = 2% to 6%
                if shipto$(i%) = " " then L12300
                     print using L14150, shipto$(i%)
                     line% = line% + 1%
            next i%
L12300:
            print
            print using L14220, pc$, str(pcdescr$,1%,30%), type$,         ~
                                    str(typedescr$,1%,30%),              ~
                                    str(salesmendescr$(1),1%,30%)
            print using L14250, bals$(1%), bals$(3%)
            print using L14270, bals$(2%), bals$(4%)

            line% = line% + 3%

*        Print text if so requested
            prt% = 0%
L12330:     if line% > 56% then gosub page_heading
            if textopt$ = "N" then L12380
                call "TXTPRINT" (#2, f2%(2), 134%, textid$, "CUS002",    ~
                                 30%, line%, 55%, "Y", linemask$, prt%)
            if prt% <> 0% then L12330
L12380:     print
            line% = line% + 1%

            goto report_loop


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
            return


        end_of_report
            if line% > 55% then gosub page_heading
            print
            print using L14180, count%
            print
            print "END OF REPORT"
            close printer
            call "SETPRNT" ("CUS002", " ", 0%, 1%)
            goto inputmode

        print_params
            print page
            tran(i$(), hex(208c2084208620ac))replacing
            print using L14200, "Customer Listing"
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
        ~###################################               APCLIST2-CUS002

L14030: %                                                     C U S T O M~
        ~ E R   L I S T I N G                                  PAGE: ###

L14060: %CUSTOMER# CUSTOMER SORT NAME             SHIP-TO NAME AND ADDRES~
        ~S       ACCOUNT # BILL-TO # CONTACT              PHONE NUMBER SLM~
        ~N

L14090: %--------- ------------------------------ -----------------------~
        ~------- --------- --------- -------------------- ------------ ---~
        ~-

L14120: %######### ############################## #######################~
        ~####### ######### ######### #################### ############ ###~
        ~#

L14150: %                                         #######################~
        ~#######

L14180: %   ###,### CUSTOMERS LISTED

L14200: %############################################################

L14220: %PRICE CODE: (#) ##############################  TYPE: (##) #####~
        ~#########################   SLSMN: ##############################

L14250: %                                                BILL-TO OPEN ORD~
        ~ER $'S: ###############   OPEN A/R $'S     : ###############
L14270: %                                                SHIP-TO OPEN ORD~
        ~ER $'S: ###############   HIGH A/R BALANCE : ###############

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Output Sequence  */~
                                    L20200,         /* From Range       */~
                                    L20400          /* Include Test?    */
                     return

L20100
*        Output Sequence                       SEQ$
            inpmessage$ = "Enter 'N' for Numeric or 'S' for Sort Name."
            return

L20200
*        Customer Range                        DFROM$ / DTO$
            inpmessage$ = "Enter Range of Customers to Print."
            if dfrom$  = " " then dfrom$ = "ALL"
            return

L20400
*        Include Test? (Y/N)                   TEXTOPT$
            if textopt$ = " " then textopt$ = "N"
            inpmessage$ = "Enter 'Y' to print text on the report."
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

            get #1 using L35060, customer$, sort$, soldto$(), opened$,    ~
                lastused$, lastinv$, lastcash$, lastchng$, lastuser$,    ~
                shipto$(), contact$, phone$, sales$, aracct$, cashacct$, ~
                frtacct$, discacct$, taxacct$, disc, pc$, limit,         ~
                crchng$, cruser$, terms$, howship$, fob$, shiprgn$, bo$, ~
                lateship$, shipinstr$(), salesmen$(), comm%(), region$,  ~
                bals(), baldate$, acctxref$, billxref$, textid$, status$,~
                taxable$, exempt$, vf$, poreqd$, stmnt$, late$, type$,   ~
                taxcode$, fcacct$, fctable$
            if soldto$(1) = "BILLTO" then                                ~
                soldto$(1) = "-- SAME AS BILL-TO ADDRESS --"
            if soldto$(1) = " " then                                     ~
                soldto$(1) = "-- SAME AS SHIP-TO ADDRESS --"
            if phone$ <> " " then                                        ~
                phone$ = str(phone$,,3) & "-" & str(phone$,4,3) & "-" &  ~
                         str(phone$, 7)
            if str(shipto$(6),17,1) <> " " or str(shipto$(6),16,1) <> " "~
                or pos(str(shipto$(6),27,4) = " ") > 0% then L30260
                    temp$ = str(shipto$(6),27,4)
                    str(shipto$(6),28,4) = temp$
                    str(shipto$(6),27,1) = "-"
L30260:     call "SPCESMSH" (shipto$(6), 2%)

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
            call "DESCRIBE" (#9 , acctxref$, acctxrefdescr$, 0%, f1%( 9))
                if acctxref$ = customer$ then                            ~
                                         acctxref$, acctxrefdescr$ = " "
            call "DESCRIBE" (#9 , billxref$, billxrefdescr$, 0%, f1%( 9))
                if billxref$ = customer$ then                            ~
                                         billxref$, billxrefdescr$ = " "
            if pc$ = " " then L30622
                readkey$ = "PRICECODE" & pc$
                call "DESCRIBE" (#10, readkey$, pcdescr$, 0%, f1%(10))
        REM APC MOD 08/26/91
L30622:     readkey$ = "CUST RPT " & customer$
                call "DESCRIBE" (#10, readkey$, rhh$, 0%, f1%(10))
                if f1%(10) <> 0% then pcdescr$ = rhh$

            if shiprgn$ = " " then L30660
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
L30840: REM ( APC MOD - 10/04/91 CONVERT CUSTOMER BALANCES )
            for j% = 1% to 4%
                bals(j%) = round( bals(j%), 2)
                convert bals(j%) to bals$(j%), pic($##,###,###.##-)
            next j%
        return



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
            4*CH(6),        /* Used / Invoiced / Cash / Modified       */~
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
            4*PD(14,4),     /* O/O, Rlsed, A/R, High A/R               */~
            CH(6),          /* High A/R Balance Date                   */~
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
                                    L40210,         /* Range            */~
                                    L40210          /* Include Test?    */
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
                  "Customer Master Listing",                             ~
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
               at (09,02), "Include Text? (Y/N)",                        ~
               at (09,30), fac(lfac$( 3)), textopt$             , ch(01),~
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
                  call "MANUAL" ("CUSLIST2")
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
                                    L50200,         /* From Range       */~
                                    L50400          /* Include Test?    */
                  return

L50100
*        Output Sequence                       SEQ$
            if seq$ = "N" or seq$ = "S" then L50130
                errormsg$ = "Enter 'N' or 'S'."  :  return
L50130:     rlen% = 9%  :  key% = 0%     /* Set for Customer Number    */
            if seq$ = "N" then return
                rlen% = 30%  :  key% = 1%
                return

L50200
*        Customer Range- From                  DFROM$ / DTO$
            call "TESTRNGE" (str(dfrom$,,rlen%), str(dto$,,rlen%),       ~
                             str( from$,,rlen%), str( to$,,rlen%),       ~
                             errormsg$ )
            return

L50400
*        Include Test? (Y/N)                   TEXTOPT$
            if textopt$ = "Y" or textopt$ = "N" then return
                errormsg$ =  "Enter 'Y' or 'N'."
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
