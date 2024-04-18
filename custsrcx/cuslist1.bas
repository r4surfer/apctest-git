        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U   SSS   L      IIIII   SSS   TTTTT   1      *~
            *  C   C  U   U  S      L        I    S        T    11      *~
            *  C      U   U   SSS   L        I     SSS     T     1      *~
            *  C   C  U   U      S  L        I        S    T     1      *~
            *   CCC    UUU    SSS   LLLLL  IIIII   SSS     T   11111    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSLIST1 - Lists Customer Master File.                    *~
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
            * 06/02/88 ! Added Page zero                          ! MJB *~
            * 02/20/89 ! Formatted ZIP Code xxxxx-xxxx            ! MJB *~
            * 02/27/89 ! Corrected Column Heading Printing        ! MJB *~
            * 09/07/89 ! Minor tweek of ZIP format.               ! JDH *~
            *          !   Added Credit Parent, Credit # of days, !     *~
            *          !   and Default Currency if MC is on.      !     *~
            * 11/01/90 ! Added ICC & ICOC.  Also, SIC & Country.  ! JDH *~
            * 04/29/91 ! Minor Format changes for ICC & ICOC codes! JBK *~
            * 05/29/91 ! Fixed PRR 11716 - Some descriptions hang ! JBK *~
            *          !  from previous customer.  Customer count !     *~
            *          !  corrected.                              !     *~
            * 11/16/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/30/92 ! Added Run Time to End of Report.         ! JIM *~
            * 12/23/93 ! Added Shipping Priority Code To Report.  ! MLJ *~
            * 03/17/95 ! Added Export, Precious Metal Flags, &    ! RJH *~
            *          !  Part Xref Print Flags.                  !     *~
            * 03/21/95 ! Added Fax # and Print Acknowledgements.  ! JDH *~
            * 12/15/98 ! (EWD001) Added Customer Status selection.! BWS *~
            * 05/11/06 ! Added TXTCUST file.                      ! DES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12,                     /* Acct # for test/descr rtn  */~
            acctdescr$32,                /* Descr  for test/descr rtn  */~
            accttype$1,                  /* Type   for test/descr rtn  */~
            acctxref$9,                  /* Account Cross Reference    */~
            acctxrefdescr$32,            /* Account Cross Reference    */~
            acks$3,                      /* Print Acknowledgements Flag*/~
            aracct$12,                   /* Net Invoice Distribution   */~
            aracctdescr$32,              /* Net Invoice Distribution   */~
            baldate$8,                   /* High A/R Balance Date      */~
            bals(4),                     /* Summary Balance Info       */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            billxrefdescr$32,            /* Bill-to Cross Reference    */~
            bo$1,                        /* Allow Backorders?          */~
            cashacct$12,                 /* Cash-in-Bank Account       */~
            cashacctdescr$32,            /* Cash-in-Bank Account       */~
            col1$25, col2$30, col3$30,   /* Columnar Printing          */~
            col2l$50,                    /* Columnar Printing - Long   */~
            comm%(3), comm$(3)3,         /* Commission Split %s        */~
            company$60,                  /* Company Name               */~
            contact$20,                  /* Customer Contact           */~
            country$3,                   /* Country Code               */~
            cntry_desc$30,               /* Country                    */~
            crchng$8, cruser$3,          /* CR Limit change audit      */~
            crdays$3,                    /* Credit number of days      */~
            crdayschng$8, crdaysuser$3,  /* CR Days change audit       */~
            crparent$9,                  /* Credit parent              */~
            crparent_descr$30,           /* Credit parent description  */~
            cursor%(2),                  /* Cursor location for edit   */~
            curr$4,                      /* Default currency           */~
            curr_descr$30,               /* Default currency descriptio*/~
            customer$9,                  /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            dfrom$30, dto$30,            /* Range (Display)            */~
            disc$5,                      /* Standard Order Discount %  */~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctdescr$32,            /* Sales Discounts Account    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            exempt$25,                   /* Tax Exemption Number       */~
            export$1,                    /* Normally an Export Cust?   */~
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
            icc$6,                       /* Intercompany Corporation Cd*/~
            icc_desc$30,                 /* Intercompany Corporation   */~
            icoc$9,                      /* Intercompany Ownership Code*/~
            icoc_desc$30,                /* Intercompany Owner         */~
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
            mcon$1,                      /* Multi-currency on flag     */~
            mvon$1,                      /* Management Values on flag  */~
            opened$8,                    /* Date account opened        */~
            pco$1,                       /* Price code for options     */~
            pc$1,                        /* Standard Price Code        */~
            pcdescr$32,                  /* Standard Price Code        */~
            pf4$17, pf16$16,             /* PF Descriptors             */~
            phone$(2)10,                 /* Phone Number - Fax Number  */~
            phone$30,                    /* Phone Number - Fax Number  */~
            plowkey$40,                  /* Plow key                   */~
            poreqd$1,                    /* PO Required?               */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            readkey$50,                  /* Misc. Purpose Read Key     */~
            region$4,                    /* Sales Region Code          */~
            regiondescr$32,              /* Sales Region Code          */~
            runtime$8,                   /* Report run time            */~
            sales$12,                    /* Sales Distribution Account */~
            salesdescr$32,               /* Sales Distribution Account */~
            salesmen$(3)4,               /* Salesmen/ Commission Splits*/~
            salesmendescr$(3)32,         /* Salesmen/ Commission Splits*/~
            seq$1,                       /* Output Sequence (C/S)      */~
            shipcode$1,                  /* Shipping Priority Code     */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shiprgn$9,                   /* Shipping Region Code       */~
            shiprgndescr$32,             /* Shipping Region Code       */~
            shipto$(6)31,                /* Ship-to Name and Address   */~
            sic$4,                       /* SIC Code                   */~
            sic_desc$30,                 /* SIC Description            */~
            soldto$(6)31,                /* Sold-to Name and Address   */~
            sort$30,                     /* Customer Sort Name         */~
            status$1,                    /* Customer Status            */~
/*EWD001*/  status_sel$1,                /* Customer Status Selection  */~
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
            vf$(10)20,                   /* Variable Fields - Labels   */~
            vf_d$(10)20,                 /* Variable Fields - Data     */~
            vf_print$1,                  /* Variable Fields - Option   */~
            vf_prompt$22,                /* Variable Fields - Prompt   */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1                   /* Print Manufactur Xref Part */

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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #2  ! TXTFILE  ! System Text File (old file)              *~
            * #2  ! TXTCUST  ! System Text File (new file)              *~
            * #9  ! CUSTOMER ! Customer Master File                     *~
            * #10 ! GENCODES ! General Codes File                       *~
            * #11 ! STXCODES ! Sales Tax Codes                          *~
            * #12 ! SLMMASTR ! Salesman master file                     *~
            * #13 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #14 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #15 ! GLMAIN   ! General Ledger Main File                 *~
            * #16 ! CURMASTR ! Currency Master File                     *~
            * #31 ! CCRMASTR ! Customer Credit Master file              *~
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

        /*  select #2,  "TXTFILE", */ 
            select #2,  "TXTCUST",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #9,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

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

            select #16, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #31, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

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
            call "OPENCHCK" (#31, fs%(31), f2%(31), 0%, rslt$(31))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62) = "CUSLIST1: " & str(cms2v$,,8)
            rlen% = 1%

            mcon$ = "N"
            call "READ100" (#14, "SWITCHS.CUR", f1%(14))
               if f1%(14) = 0% then goto L09270
            get #14 using L09240, mcon$
L09240:         FMT POS(21), CH(1)
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, rslt$(16))

L09270:     mvon$ = "N"
            call "READ100" (#14, "SWITCHS.GL", f1%(14))
               if f1%(14) = 0% then goto L09330
            get #14 using L09310, mvon$
L09310:         FMT POS(59), CH(1)

L09330:     vf_prompt$ = " " : end% = 4%    /*EWD001*/
            plowkey$ = "VF1:CUSTOMER"
            call "READ100" (#14, plowkey$, vf%)
            if vf% <> 1% then L10000
                get #14 using L09370, vf$()
L09370:              FMT POS(82), 10*CH(20)
                vf_prompt$ = "Incl. Variable Fields?"
                end% = 5%                   /*EWD001*/

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            page% = -1% :  count% = 0%
            pf16$ = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, runtime$,                  ~
                      seq$                        ,/* Output Sequence  */~
                      dfrom$                      ,/* From Range       */~
                      dto$                        ,/* TO Range         */~
                      vf_print$                   ,/* Include VFs ?    */~
                      textopt$                    ,/* Include Text?    */~
                      status_sel$                  /* Status Selection */

            for fieldnr% = 1% to end%
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
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       edtpg1
            fieldnr% = cursor%(1) - 5
            if fieldnr% > 2% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1% or fieldnr% > end% then edtpg1
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
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            line% = 857%
            select printer(134)
            call "SETPRNT" ("CUS001", " ", 0%, 0%)
            plowkey$ = from$
            call "PLOWALTS" (#1, plowkey$, key%, 0%, f1%(1))
            goto L12160

        report_loop
            call "READNEXT" (#1, f1%(1))
L12160:     if f1%(1) = 0% then end_of_report
            plowkey$  = key(#1, key%)
            if plowkey$ > to$ then end_of_report

            count% = count% + 1%
            gosub load_record
            gosub page_heading
            print using L14150, customer$, sort$, shipto$(1), soldto$(1), ~
                               opened$,  lastchng$
            line% = line% + 1%
            for i% = 2% to 6%
                if shipto$(i%) = " " and soldto$(i%) = " "  then L12300
                     print using L14180, shipto$(i%), soldto$(i%)
                     line% = line% + 1%
            next i%
L12300:     print  : print using L14210 : print using L14240
            line% = line% + 3%

            if mvon$ <> "Y" then L12316
            col1$ = "Intercompany Corporation "
                col2$ = icc$      :  col3$ = icc_desc$  :  gosub col_print
            col1$ = "Intercompany Owner       "
                col2$ = icoc$     :  col3$ = icoc_desc$ :  gosub col_print
L12316:     col1$ = "SIC Code                 "
                col2$ = sic$      :  col3$ = sic_desc$  :  gosub col_print
            col1$ = "Country                  "
                col2$ = country$  :  col3$ = cntry_desc$:  gosub col_print
            col1$ = "Contact & Phone/Fax #s   "
                col2$ = contact$  :  col3$ = phone$     :  gosub col_print
            col1$ = "Account Cross Reference  "
                col2$ = acctxref$ :  col3$ = acctxrefdescr$
                gosub col_print
            col1$ = "Bill-To Cross Reference  "
                col2$ = billxref$ :  col3$ = billxrefdescr$
                gosub col_print
            col1$ = "Credit Parent X-Ref      "
                col2$ = crparent$ :  col3$ = crparent_descr$
                gosub col_print
            col1$ = "Purchase Order Required? "
                col2$ = poreqd$   :  col3$ = " "        : gosub col_print
            col1$ = "Standard Price Code      "
                col2$ = pc$       :  col3$ = pcdescr$   : gosub col_print
            col1$ = "Price Code for Options   "
                col2$ = pco$      :  col3$ = " "        : gosub col_print
            col1$ = "Standard Order Discount  "
                if str(disc$,,1) = " " then disc$ = str(disc$,2)
                col2$ = disc$ & "%"     :  col3$ = " "  : gosub col_print
            col1$ = "Shipping Priority Code   "
                if shipcode$ = " " then shipcode$ = "3"
                col2$ = shipcode$ :  col3$ = " "        : gosub col_print
            col1$ = "How Ship Instructions    "
                col2$ = howship$  :  col3$ = " "        : gosub col_print
            col1$ = "Standard Fob Terms       "
                col2$ = fob$      :  col3$ = " "        : gosub col_print
            col1$ = "Shipping Region          "
                col2$ = shiprgn$  :  col3$ = shiprgndescr$
                                                          gosub col_print
            col1$ = "Allow Backorders?        "
                col2$ = "YES" : if bo$ = "N" then col2$ = "NO"
                col3$ = " "   : gosub col_print
            col1$ = "Allow Late Shipments?    "
                col2$ = "YES" : if lateship$ = "N" then col2$ = "NO"
                col3$ = " "   : gosub col_print

            col1$ = "Customer Status          "
                col2$ = status$   :  col3$ = statusdescr$
                gosub col_print
            if mcon$ = "N" then L12650
            col1$ = "Default Currency         "
                col2$ = curr$   :  col3$ = curr_descr$
                gosub col_print
L12650:     col1$ = "Sales Tax Exemption #    "
                col2$ = exempt$   :  col3$ = " "        : gosub col_print
            col1$ = "Sales Tax Code           "
                col2$ = taxcode$  :  col3$ = taxcodedescr$
                gosub col_print
            col1$ = "Customer Taxable?        "
                col2$ = "YES" : if taxable$ = "N" then col2$ = "NO"
                col3$ = " "   : gosub col_print
            col1$ = "Customer Type Code       "
                col2$ = type$     :  col3$ = typedescr$ : gosub col_print
            col1$ = "Sales Region Code        "
                col2$ = region$ : col3$ = regiondescr$  : gosub col_print
            col1$ = "Salesmen (Split%)        "
            for i% = 1% to 3%
                if salesmen$(i%) = " " then L12830
                     col2$ = salesmen$(i%) & "  (" & comm$(i%) & "%)"
                     col3$ = salesmendescr$(i%)
                     gosub col_print
L12830:         col1$ = " "
            next i%
            col1$ = "Payment Terms (Code)     "
                col2$ = terms$ : col3$ = termsdescr$  : gosub col_print
            col1$ = "Credit Limit             "
                col2$ = limit$
                col3$ = "Last Changed " & crchng$ & " By " & cruser$
                gosub col_print
            col1$ = "# of Days late - CR Hold "
                col2$ = crdays$
                col3$ = "Last Changed " & crdayschng$ & " By " &         ~
                        crdaysuser$
                gosub col_print
            col1$ = "Finance Charge Table     "
                col2$ = fctable$ : col3$ = fctabledescr$
                gosub col_print
            col1$ = "Statement Print Flag     "
                                     col2$ = " "
                if stmnt$ = "O" then col2$ = "OPEN ITEM"
                if stmnt$ = "B" then col2$ = "BALANCE FORWARD"
                if stmnt$ = "N" then col2$ = "NO STATEMENTS"
                col3$ = " " : gosub col_print
            col1$ = "Doc. Printing - A/R:     "
                if late$ = "N" then col3$ = "Neither"
                if late$ = "L" then col3$ = "Late Notes Only"
                if late$ = "S" then col3$ = "Statements Only"
                if late$ = "B" then col3$ = "Both Statements & Late Notes"
                col2$ = late$    : gosub col_print
            col1$ = "G/L- Sales Distribution  "
                col2$ = sales$    : col3$ = salesdescr$  : gosub col_print
            col1$ = "     Sales Discounts     "
                col2$ = discacct$ : col3$ = discacctdescr$
                gosub col_print
            col1$ = "     Net Invoice Distr.  "
                col2$ = aracct$ : col3$ = aracctdescr$  : gosub col_print
            col1$ = "     Cash-In-Bank Account"
                col2$ = cashacct$ : col3$ = cashacctdescr$
                gosub col_print
            col1$ = "     Freight Account     "
                col2$ = frtacct$ : col3$ = frtacctdescr$ : gosub col_print
            col1$ = "     Sales Tax Account   "
                col2$ = taxacct$ : col3$ = taxacctdescr$ : gosub col_print
            col1$ = "     Finance Charges     "
                col2$ = fcacct$ : col3$ = fcacctdescr$ : gosub col_print
            col1$ = "Shipping Instructions"
                col2l$ = shipinstr$(1%) : gosub col_print_long
            col1$ = " "
                col2l$ = shipinstr$(2%) : gosub col_print_long
            col1$ = "Normally Export Customer"
                if export$ = "Y" then col2$ = "YES" else col2$ = "NO "
                col3$ = " " : gosub col_print
            col1$ = "Print Cust Xref Part "
                if xref_cus$ = "Y" then col2$ = "YES" else col2$ = "NO "
                if xref_cus$ = "B" then col2$ = "BOTH"
                col3$ = " " : gosub col_print
            col1$ = "Print Mfg Xref Part "
                if xref_mnf$ = "Y" then col2$ = "YES" else col2$ = "NO "
                if xref_mnf$ = "B" then col2$ = "BOTH"
                col3$ = " " : gosub col_print
            col1$ = "Prec Metal at Sales Order"
                if pm_so$  = "Y" then col2$ = "YES" else col2$ = "NO "
                col3$ = " " : gosub col_print
            col1$ = "Prec Metal at Invoice"
                if pm_inv$ = "Y" then col2$ = "YES" else col2$ = "NO "
                col3$ = " " : gosub col_print
            col1$ = "Print Acknowledgements"
                if acks$ = "N" then col2$ = "NO " else col2$ = "YES"
                col3$ = " " : gosub col_print

*        Print text if so requested
            prt% = 0%
            if textopt$ = "N" then L13310
L13270:     if line% > 50% then gosub page_heading2
                call "TXTPRINT" (#2, f2%(2), 134%, textid$, "CUS001",    ~
                                 43%, line%, 55%, "Y", linemask$, prt%)
            if prt% <> 0% then L13270
L13310:     print
            line% = line% + 1%

*        Print VFs if so requested
            if vf_print$ <> "Y" then L13350
                if line% > 45% then gosub page_heading2
                     print "Variable Fields:"
                     for i% = 1% to 10%
                          col1$ = vf$(i%) : col2$ = vf_d$(i%) : col3$ =" "
                          if col1$ = " " and col2$ = " " then L13340
                          gosub col_print
L13340:              next i%

L13350:     goto report_loop

        page_heading
            page% = page% + 1%
            if page% = 0% then gosub print_params
            print page
            print using L14000, date$, runtime$, company$
            print using L14030, page%
            print
            if heading2% = 1% then return
            print using L14060
            print using L14090, "#"
            print using L14120
            line% = 7%
            return

        page_heading2     /* Continuation Pages    */
            heading2% = 1%
            gosub page_heading
            print using L14350, customer$, sort$
            print
            line% = 7%  :  heading2% = 0%
            return



        end_of_report
            gosub page_heading
            print : print
            print using L14330, count%
            print
            runtime$ = " " : call "TIME" (runtime$)
            print "END OF REPORT", runtime$
            close printer
            call "SETPRNT" ("CUS001", " ", 0%, 1%)
            goto inputmode


        col_print
            print using L14270, col1$, col2$, col3$
            line% = line% + 1%
            return

        col_print_long
            print using L14300, col1$, col2l$
            line% = line% + 1%
            return

        print_params
            print page
L13772:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L13792
                str(i$(), i%, 1%) = hex(20)
                goto L13772
L13792:     print using L14000, date$, runtime$, company$
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

L14000: %Run Date: ######## ########            #########################~
        ~###################################               CUSLIST1-CUS001

L14030: %                                             C U S T O M E R   L~
        ~ I S T I N G  :  D E T A I L                          PAGE: ###

L14060: %                                                                ~
        ~                                           Date     Last

L14090: %Customer# Customer Sort Name              Ship-To Name And Addre~
        ~ss        Sold-To Name And Address        Created   Changed

L14120: %--------- ------------------------------  ----------------------~
        ~--------  ------------------------------  --------  --------

L14150: %######### ##############################  ######################~
        ~########  ##############################  ########  ########

L14180: %                                          ######################~
        ~########  ##############################

L14210: %              Field Definition            Field Contents        ~
        ~         Description / Comments

L14240: %              -------------------------   ----------------------~
        ~-------  ------------------------------

L14270: %              #########################   ######################~
        ~######## ##############################

L14300: %              #########################   ######################~
        ~############################

L14330: %   ###,### Customers Listed

L14350: %Customer: #########  ##############################

        %############################################################
        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Output Sequence  */~
                                    L20200,         /* From Range       */~
                                    L20400,         /* Include Test?    */~
/*EWD001*/                          L20450,         /* Status Selection */~
                                    L20500          /* Include VFs ?    */ 
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

L20400
*        Include Test? (Y/N)                   TEXTOPT$
            if textopt$ = " " then textopt$ = "N"
            inpmessage$ = "Enter 'Y' to print text on the report."
            return

L20450
*        Status Selection                      STATUS_SEL$      /*EWD001*/
            if status_sel$ = " " then status_sel$ = "A"
            inpmessage$ = "Enter 'A'ctive,'H'old,'I'nactive,'D'elete or "~
                & "'X' for ALL."
            return

L20500
*        Include Variable Fields?              VF_PRINT$
            if vf_print$ = " " then vf_print$ = "N"
            inpmessage$ = "Enter 'Y' to print Variable Fields."
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
                pm_so$, pm_inv$, phone$(2%), acks$,                      ~
                lastuser$, shipto$(), contact$, phone$(1%), sales$,      ~
                aracct$, cashacct$, frtacct$, discacct$, taxacct$, disc, ~
                pc$, limit, crchng$, cruser$, terms$, howship$, fob$,    ~
                shiprgn$, bo$, lateship$, shipinstr$(), salesmen$(),     ~
                comm%(), region$, shipcode$, pco$,                       ~
                acctxref$, billxref$, textid$, status$,                  ~
                taxable$, exempt$,vf_d$(), poreqd$, stmnt$, late$, type$,~
                taxcode$, fcacct$, fctable$, curr$, crparent$, crdays,   ~
                crdayschng$, crdaysuser$, sic$, country$, icc$, icoc$,   ~
                export$, xref_cus$, xref_mnf$

/*EWD001*/  if status$ = status_sel$ or status_sel$ = "X" then L30150
/*   |  */      count% = count% - 1%
/*   |  */      return clear all
/*EWD001*/      goto report_loop
L30150:     lastused$, lastinv$, lastcash$, lastchng$, baldate$ = " "
            mat bals = zer
            call "READ100" (#31, customer$, f1%(31%))
            if f1%(31%) <> 0% then get #31 using L30156, lastused$,       ~
                lastinv$, lastcash$, lastchng$, baldate$, bals()
L30156:         FMT POS(84), 5*CH(6), 4*PD(14,4)

            gosub init_descr
            if soldto$(1) = "BILLTO" then                                ~
                soldto$(1) = "-- SAME AS BILL-TO ADDRESS --"
            if soldto$(1) = " " then                                     ~
                soldto$(1) = "-- SAME AS SHIP-TO ADDRESS --"
            phone$ = " "
            if phone$(1%) = " " then L30232
                str(phone$,,3%)     = str(phone$(1%),,3%)
                str(phone$, 5%, 3%) = str(phone$(1%), 4%, 3%)
                str(phone$, 9%, 4%) = str(phone$(1%), 7%, 4%)
                str(phone$, 4%, 1%) = "-"
                str(phone$, 8%, 1%) = "-"
L30232:     if phone$(2%) = " " then L30250
                str(phone$,19%, 3%) = str(phone$(2%),,3%)
                str(phone$,23%, 3%) = str(phone$(2%), 4%, 3%)
                str(phone$,27%, 4%) = str(phone$(2%), 7%, 4%)
                str(phone$,15%, 3%) = "Fax"
                str(phone$,22%, 1%) = "-"
                str(phone$,26%, 1%) = "-"
L30250:     if str(shipto$(6),17,1) <> " " or str(shipto$(6),16,1) <> " "~
                or pos(str(shipto$(6),27,4) = " ") > 0% then L30310
                  temp$ = str(shipto$(6),27,4)
                  str(shipto$(6),28,4) = temp$
                  str(shipto$(6),27,1) = "-"

L30310:     if str(soldto$(6),17,1) <> " " or str(soldto$(6),16,1) <> " "~
                or pos(str(soldto$(6),27,4) = " ") > 0% then L30370
                  temp$ = str(soldto$(6),27,4)
                  str(soldto$(6),28,4) = temp$
                  str(soldto$(6),27,1) = "-"

L30370:     call "SPCESMSH" (shipto$(6), 2%)
            call "SPCESMSH" (soldto$(6), 2%)
            call "LINSMASH" (shipto$())
            call "LINSMASH" (soldto$())
            call "DATEFMT" (opened$)
            call "DATEFMT" (lastused$)
            call "DATEFMT" (lastinv$)

            call "DATEFMT" (lastcash$)
            call "DATEFMT" (lastchng$)
            call "DATEFMT" (crchng$)
            call "DATEFMT" (crdayschng$)
            call "DATEFMT" (baldate$)
            call "CONVERT" (disc   , 2.2, disc$   )
            convert limit to limit$, pic(##,###,##0)
            convert crdays to crdays$, pic(##0)
                call "SPCSMASH" (limit$, 0%)
                call "SPCSMASH" (crdays$, 0%)
            for i% = 1% to 3%
                if salesmen$(i%) = " " then L30600
                     call "DESCRIBE" (#12, salesmen$(i%),                ~
                                         salesmendescr$(i%), 0%, f1%(12))
                     convert comm%(i%) to comm$(i%), pic(###)
L30600:     next i%
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
            if mvon$ <> "Y" then L30800
              call "DESCRIBE" (#9 , icoc$, icoc_desc$, 0%, f1%( 9))
              if icc$ = " " then L30800
                readkey$ = "CORPORATE" & icc$
                call "DESCRIBE" (#10, readkey$, icc_desc$, 0%, f1%(10))
L30800:     if sic$ = " " then L30830
                readkey$ = "INDUSTRY " & sic$
                call "DESCRIBE" (#10, readkey$, sic_desc$, 0%, f1%(10))
L30830:     if country$ = " " then L30860
                readkey$ = "COUNTRYCD" & country$
                call "DESCRIBE" (#10, readkey$, cntry_desc$, 0%, f1%(10))
L30860:     call "DESCRIBE" (#9 , acctxref$, acctxrefdescr$, 0%, f1%( 9))
                if acctxref$ = customer$ then                            ~
                                         acctxref$, acctxrefdescr$ = " "
            call "DESCRIBE" (#9 , billxref$, billxrefdescr$, 0%, f1%( 9))
                if billxref$ = customer$ then                            ~
                                         billxref$, billxrefdescr$ = " "
            call "DESCRIBE" (#9, crparent$, crparent_descr$, 0%, f1%( 9))
            if pc$ = " " then L30960
                readkey$ = "PRICECODE" & pc$
                call "DESCRIBE" (#10, readkey$, pcdescr$, 0%, f1%(10))
L30960:     if shiprgn$ = " " then L30990
                readkey$ = "SHPREGION" & shiprgn$
                call "DESCRIBE" (#10, readkey$, shiprgndescr$, 0%,f1%(10))
L30990:     if region$ = " " then L31020
                readkey$ = "REGIONS  " & region$
                call "DESCRIBE" (#10, readkey$, regiondescr$, 0%,f1%(10))
L31020:     if type$ = " " then L31050
                readkey$ = "CUS TYPES" & type$
                call "DESCRIBE" (#10, readkey$, typedescr$, 0%, f1%(10))
L31050:     if status$ = "A" then statusdescr$ = "ACTIVE"
            if status$ = "H" then statusdescr$ = "ON HOLD"
            if status$ = "I" then statusdescr$ = "INACTIVE"
            if status$ = "D" then statusdescr$ = "TO BE DELETED"
            if mcon$ = "N" then L31120
                curr_descr$ = "No Default Currency"
            call "DESCRIBE" (#16, curr$, curr_descr$, 0%, f1%(16))
L31120:     call "DESCRIBE" (#11, taxcode$, taxcodedescr$, 0%, f1%(11))
            fctabledescr$ = "No Finance Charges"
            if fctable$ = " " then L31170
                readkey$ = "FINANCECHARGETABLE" & fctable$
                call "DESCRIBE" (#14, readkey$, fctabledescr$, 0%,f1%(14))
L31170:     termsdescr$ = "0% 0 Days, Net 0 Days"
            if terms$ = " " then L31200
            call "DESCRIBE" (#13, terms$, termsdescr$, 0%, f1%(13))
L31200:     return



        describe_acct    /* Describe account number                    */
            call "DESCRIBE" (#15, acct$, acctdescr$, 0%, f1%(15))
            if f1%(15) = 1% then L31300
                acctdescr$ = "[Use System Default]"
                accttype$  = " "
                return
L31300:     call "GLFMT" (acct$)
            get #15 using L31320, accttype$
L31320:              FMT XX(39), CH(1)
            return

        REM Initialize descriptions so they're nice and fresh
        init_descr
            init (" ")  icc_desc$, icoc_desc$, pcdescr$, shiprgndescr$,  ~
                        regiondescr$, typedescr$, curr_descr$, sic_desc$,~
                        cntry_desc$
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
            CH(1),          /* Precious Metal SO entry Flag            */~
            CH(1),          /* Precious Metal Invoicing Flag           */~
            CH(10),         /* Fax number                              */~
            CH(1),          /* Print Acknowledgements Flag             */~
            XX(11),         /* Used/Inv'd/Cash/Modified  (CCRMASTR)    */~
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
            CH(1),          /* Shipping Priority Code                  */~
            CH(1),          /* Price code for options                  */~
            XX(30),         /* O/O, Rlsed, A/R, High A/R (CCRMASTR)    */~
            XX(6),          /* High A/R Balance Date     (CCRMASTR)    */~
            CH(9),          /* Account Xref                            */~
            CH(9),          /* Bill-to Xref                            */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Customer Status                         */~
            CH(1),          /* Customer Taxable?(Y/N)                  */~
            CH(25),         /* Tax Exemption Number                    */~
            10*CH(20),      /* Variable Fields Data Area               */~
            CH(1),          /* Purchase Order Required?                */~
            CH(1),          /* Statement Print Flag                    */~
            CH(1),          /* Print Late Notices?                     */~
            CH(2),          /* Customer Type                           */~
            CH(10),         /* Sales Tax Code                          */~
            CH(9),          /* Finance Charge G/L Account Number       */~
            CH(1),          /* Finance Charge Code                     */~
            CH(4),          /* Currency code                           */~
            CH(9),          /* Credit Parent                           */~
            BI(2),          /* Credit number of days                   */~
            CH(6),          /* Date # days last modified               */~
            CH(3),          /* User ID of Last Modification            */~
            CH(4),          /* SIC Code                                */~
            CH(3),          /* Country Code                            */~
            CH(6),          /* Intercompamy Corporation Code           */~
            CH(9),          /* Intercompamy Ownership Code             */~
            CH(1),          /* Customer Normally an Export Customer?   */~
            CH(1),          /* Print Customer Xref Part Flag           */~
            CH(1),          /* Print Manufactr Xref Part Flag          */~
            CH(107)         /* Filler                                  */

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
                                    L40210,         /* Include Test?    */~
/*EWD001*/                          L40210,         /* Status Selection */~
                                    L40210          /* Include VFs ?    */ 
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
                  "Customer Master Listing - Detail Report",             ~
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
/*EWD001*/     at (10,02), "Status Selection:",                          ~
/*EWD001*/     at (10,30), fac(lfac$( 4)), status_sel$          , ch(01),~
                                                                         ~
/*EWD001*/     at (11,02), fac(hex(8c)), vf_prompt$             , ch(22),~
/*EWD001*/     at (11,30), fac(lfac$(5%)), vf_print$            , ch(01),~
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
                  call "MANUAL" ("CUSLIST1")
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
                                    L50400,         /* Include Text?    */~
/*EWD001*/                          L50450,         /* Status Selection */~
                                    L50500          /* Include VFs ?    */ 
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

L50400
*        Include Test? (Y/N)                   TEXTOPT$
            if textopt$ = "Y" or textopt$ = "N" then return
                errormsg$ =  "Enter 'Y' or 'N'."
                return

L50450
*       Status Selection                       STATUS_SEL$      /*EWD001*/
            sel% = pos("AHIDX" = status_sel$)
            if sel% = 0% then errormsg$ = "Enter 'A','H','I','D' or 'X'."
            return

L50500
*        Include VFs ? (Y/N)                   VF_PRINT$
            if vf_print$ = "Y" or vf_print$ = "N" then return
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


