        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT  X   X  RRRR   EEEEE  PPPP   RRRR   TTTTT   *~
            *  S        T     X X   R   R  E      P   P  R   R    T     *~
            *   SSS     T      X    RRRR   EEEE   PPPP   RRRR     T     *~
            *      S    T     X X   R   R  E      P      R   R    T     *~
            *   SSS     T    X   X  R   R  EEEEE  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STXREPRT - Prints the Sales Tax report. Report is produced*~
            *            for a given date range, which must not conflict*~
            *            with the last purge date of the ARIMASTR and   *~
            *            ARILINES files. The last purge date is found in*~
            *            SYSFILE2.  Replaces SATAXRPT.                  *~
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
            * 10/17/86 ! Original                                 ! JIM *~
            * 03/28/88 ! Corrected taxable base amount (PRR 5969).! JIM *~
            * 02/07/89 ! Added Adjustment Amount & Net Taxable    ! MJB *~
            * 03/09/90 ! Fixed (PRR 11337).                       ! SID *~
            *          ! Excluded any invoice type "R" i.e.       !     *~
            *          ! recurring control invoices from report.  !     *~
            * 06/07/91 ! PRR 11427.  No longer writes short recrd.! JDH *~
            *          ! PRR 11474.  Now uses posting date rather ! JDH *~
            *          !   than invoice date.  Variable name same.!     *~
            * 06/17/91 ! Added call to ALLFREE.                   ! JDH *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            arilines_key$20,             /* Key to ARILINES file       */~
            ast$7,                       /* Total line asterisks       */~
            company_name$60,             /* Company name form COMPNAME */~
            cust_code$9,                 /* Customer code from ARIMASTR*/~
            cust_name$30,                /* Customer name from ARIMASTR*/~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            grtxbl$12,                   /* Gross Taxable for print    */~
            hdr$40,                      /* ASKUSER constant           */~
            high_date$10,                /* High date for capture      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inv_date$8,                  /* Posting date from ARIMASTR */~
            inv_hdr$40,                  /* Dates edited for header    */~
            inv_type$1,                  /* ID for a type of Invoice   */~
            invoice$8,                   /* Invoice number fr ARIMASTR */~
            invamt(200),                 /* Invoice amounts by tax code*/~
            invamt$12,                   /* Invoice amount for print   */~
            lastpur$24,                  /* Last purged description    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            low_date$10,                 /* Low date for capture       */~
            lseg%(5),                    /* Tax code segment lengths   */~
            msg$(3)80,                   /* ASKUSER constant           */~
            nettxbl$12,                  /* Net Taxable for print      */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pseg%(5),                    /* Tax code segment positions */~
            purge_date$8,                /* Date last purged (SYSFILE2)*/~
            rptid$6,                     /* Report ID                  */~
            seg_key$20,                  /* Key to SYSFILE2            */~
            sub_hdr$70,                  /* Report sub-header          */~
            tax_brk$10,                  /* Controls tax code breaks   */~
            tax_code$10,                 /* Tax code from ARIMASTR     */~
            tax_desc$32,                 /* Tax code description       */~
            grtaxabl(200),               /* Taxable amounts by tax code*/~
            nettaxabl(200),              /* Net Taxable amt by tax code*/~
            txbldisc(200),               /* Taxable Dsc by tax code    */~
            taxamt(200),                 /* Tax amounts by tax code    */~
            taxamt$10,                   /* Tax amount for print       */~
            taxcode$(200)10,             /* Tax codes by tax code      */~
            taxdesc$(200)32,             /* Tax descriptions by tax cd */~
            taxhold$10,                  /* Temp storage- tax code segs*/~
            taxpct$4,                    /* Edited sales tax percent   */~
            time$8,                      /* Time of day stamp          */~
            tot_desc$61,                 /* Total description          */~
            totinvamt(6),                /* Report total invoice amount*/~
            tottaxabl(6),                /* Report total invoice taxabl*/~
            tottaxamt(6),                /* Report total invoice tax   */~
            tottxbdsc(6),                /* Report total discount      */~
            tottxbnet(6),                /* Report total net amount    */~
            tstpurgdt$8,                 /* Test purge date            */~
            txbl$1,                      /* Taxable (Y/N) code-ARILINES*/~
            txbldsc$8,                   /* Taxable discount for print */~
            userid$3,                    /* Current User Id            */~
            work_fil$8,                  /* WORKFILE name              */~
            work_lib$8,                  /* WORKFILE library           */~
            work_vol$6,                  /* WORKFILE volume            */~
            xseg%(5)                     /* Tax code segment lengths   */

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
            * #3  ! ARIMASTR ! Invoice Master File                      *~
            * #4  ! ARILINES ! Invoice Line Items File                  *~
            * #5  ! SYSFILE2 ! Caelus Management System Information     *~
            * #6  ! STXCODES ! Sales Tax Codes                          *~
            * #9  ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17

            select #4,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #5,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #6,  "STXCODES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  10

            select #9,  "WORKFILE", consec, recsize =  113

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            get rslt$(3) using L02380, nbr_recs%
L02380:         FMT  POS(17), BI(4)
                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
                rslt$(5 ) = "REQUIRED"
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
                rslt$(6 ) = "REQUIRED"
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if nbr_recs% > 0% then goto initializer
L09070:         comp% = 2%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Invoice Sales Tax data to print"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09070
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */

        initializer
        REM *************************************************************~
            * DATE RANGE ENTERED BY THE OPERATOR (IF ANY), MAY NOT IM-  *~
            * PINGE UPON THE DATE LAST PURGED AS RECORDED IN 'SYSFILE2'.*~
            *************************************************************
            purge_date% = 0% : init (" ") purge_date$, errormsg$
            call "READ100" (#5, "LAST PURGE DATES", f1%(5))
            if f1%(5) = 0% then goto initial_continue_1
            get #5 using L09240, purge_date$
L09240:         FMT  POS(21), CH(6)
            tstpurgdt$ = "19010101"
            call "DATECONV" (tstpurgdt$)
            if not (purge_date$ > tstpurgdt$) then goto initial_continue_1
            call "DATEOK" (purge_date$, purge_date%, errormsg$)
            if errormsg$ <> " " then goto exit_program
            lastpur$ = "Last Purged: " & purge_date$

        initial_continue_1
        REM *************************************************************~
            * READ AND SET THE CONSTANTS THAT CONTROL THE POSSIBLE      *~
            * BREAKDOWN OF THE TAX CODE INTO DIFFERENT SEGMENTS FOR     *~
            * TAX CODE TOTALLING PURPOSES --                            *~
            *   LSEG%() = THE LENGTHS OF THE (UP TO) 5 SEGMENTS OF THE  *~
            *        TAX CODE ... AS ENTERED BY THE OPERATOR.           *~
            *   PSEG%() = THE BEGINNING POSITION WITHIN THE TAX CODE OF *~
            *        EACH SEGMENT. COMPUTED.                            *~
            *   XSEG%() = THE LENGTH OF EACH SEGMENT FROM THE BEGINNING *~
            *        (POSITION 1) OF THE TAX CODE. COMPUTED.            *~
            *   NR% = THE TOTAL NUMBER OF TAX CODE SEGMENTS. COMPUTED.  *~
            * THE DEFAULT TAX CODE BREAKDOWN IS 1 SEGMENT, 10 BYTES LONG*~
            *************************************************************
            mat lseg% = zer : mat pseg% = zer : mat xseg% = zer
            xseg%(1), lseg%(1) = 10% : nr%, pseg%(1) = 1%
            seg_key$ = "TAX CODE SEGMENTS"
            call "READ100" (#5, seg_key$, f1%(5))
            if f1%(5) = 0% then goto initial_continue_2
            get #5 using L09500, seg_key$, lseg%(), pseg%(), xseg%(), nr%
L09500:         FMT  CH(20), 5*BI(1), 5*BI(1), 5*BI(1), BI(1)

        initial_continue_2
            call "COMPNAME" (12%, company_name$, comp%)
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            rptid$ = "STX001"
            max_lines% = 56%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, high_date$
            low_date$ = "ALL"
            call "ALLFREE"

            for fieldnr% = 1% to  2%
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1% or fieldnr% > 2% then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                if cursor%(1) - 5% <> fieldnr% then goto L11150
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
        REM Format The Sub-Header Printed At The Top Of All Pages *******
            if low_date$ <> "ALL" then goto L14100
                inv_hdr$ = "ALL INVOICES" : goto L14150
L14100:     inv_hdr$ = "INVOICES POSTED " & low_date$ & " TO " &         ~
                       high_date$

        REM *************************************************************~
            * The Following Computes The Position(S) Of Each Segment Of *~
            * The Tax Code In PSEG%().                                  *~
            *************************************************************
L14150:     if lseg%(2) <> 0% then pseg%(2) = pseg%(1) + lseg%(1)
            if lseg%(3) <> 0% then pseg%(3) = pseg%(2) + lseg%(2)
            if lseg%(4) <> 0% then pseg%(4) = pseg%(3) + lseg%(3)
            if lseg%(5) <> 0% then pseg%(5) = pseg%(4) + lseg%(4)
        REM *************************************************************~
            * The Following Computes The Length Of The Various Segments *~
            * Of The Tax Code (From It's Beginning).                    *~
            *************************************************************
            xseg%(1)=lseg%(1)
            xseg%(2)=lseg%(1) + lseg%(2)
            xseg%(3)=lseg%(1) + lseg%(2) + lseg%(3)
            xseg%(4)=lseg%(1) + lseg%(2) + lseg%(3) + lseg%(4)
            xseg%(5)=lseg%(1) + lseg%(2) + lseg%(3) + lseg%(4) + lseg%(5)
        REM *************************************************************~
            * (Re)Write The Tax Code Segmentation Control Fields Back   *~
            * To 'SYSFILE2'. NR% Is The Number Of Tax Code Segments.    *~
            *************************************************************
            call "READ101" (#5, seg_key$, f1%(5))
            if f1%(5) = 0% then                                          ~
                write #5 using L14385, seg_key$, lseg%(), pseg%(),        ~
                     xseg%(), nr%, " "                                   ~
            else                                                         ~
                rewrite #5 using L14385, seg_key$, lseg%(), pseg%(),      ~
                     xseg%(), nr%, " "
L14385:            FMT  CH(20), 5*BI(1), 5*BI(1), 5*BI(1), BI(1), CH(464)
            call "SHOSTAT"                                               ~
                ("Records are being selected ... Please stand by")
            call "WORKOPN2" (#9, "OUTPT", nbr_recs%, f2%(9))
            call "GETNAMES" addr(#9, work_fil$, work_lib$, work_vol$)
            plowkey$ = xor plowkey$
            nbr_recs% = 0%   /* Initialize count of records selected */

        plow_thru_arimastr
            call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
            if f1%(3) = 0% then goto sort_work_file
            get #3 using L14530, cust_code$, invoice$, cust_name$,        ~
                inv_date$, invdiscpct, invtaxamt, invnetamt, tax_code$,  ~
                invtaxpct, inv_type$

        REM PARTIAL RECORD LAYOUT FOR FILE 'ARIMASTR' *******************
L14530:         FMT  CH(9),              /* Customer code              */~
                     CH(8),              /* Invoice number             */~
                     POS(53), CH(30),    /* Ship-to name               */~
                     POS(533), CH(6),    /* Invoice Posting Date       */~
                     POS(801), PD(14,4), /* Invoice discount percent   */~
                     POS(825), PD(14,4), /* Invoice sales tax amount   */~
                     POS(833), PD(14,4), /* Invoice net amount         */~
                     POS(873), CH(10),   /* Sales tax code             */~
                     POS(883), PD(14,4), /* Sales tax rate             */~
                     POS(891), CH(1)     /* ID for a type of Invoice   */

        REM Check for a Recurring Invoice Number
            if inv_type$ = "R" then goto plow_thru_arimastr
            errormsg$ = " "
            call "DATEOK" (inv_date$, inv_date%, errormsg$)
            if errormsg$ <> " " then goto plow_thru_arimastr
            if inv_date% < fr_date% then goto plow_thru_arimastr
            if inv_date% > to_date% then goto plow_thru_arimastr

        REM Read Associated Line Items To Get Taxable Amount ************
            linetotext, linetotdisc = 0
            arilines_key$ = str(cust_code$,,9) & invoice$
            str(arilines_key$,18) = all(hex(00))

        plow_thru_arilines
            call "PLOWNEXT" (#4, arilines_key$, 17%, f1%(4))
            if f1%(4) = 0% then goto write_work_file
            get #4 using L14785, linediscamt, linextension, txbl$

        REM Partial Record Layout of File 'ARILINES' ********************
L14785:         FMT  POS(149), PD(14,4), /* Line item disc amount      */~
                     POS(157), PD(14,4), /* Line item extension        */~
                     POS(165), CH(1)     /* Taxable code (Y/N)         */

            if txbl$ <> "Y" then goto plow_thru_arilines
            linetotdisc = linetotdisc + linediscamt
            linetotext = linetotext + linextension
            goto plow_thru_arilines

        write_work_file
        REM Have Found An Eligible Invoice ... Record it in 'workfile'.
            grtaxable = linetotext - linetotdisc
            hdrdiscamt = round(linetotext * invdiscpct * .01, 2)
            invdiscamt = hdrdiscamt - linetotdisc
            nettaxable = round(grtaxable - invdiscamt, 2)
            invnetamt = round(invnetamt, 2)
            invtaxamt = round(nettaxable * invtaxpct * .01, 2)
            write #9 using L15110, tax_code$, invoice$, inv_date$,        ~
                cust_code$, cust_name$, invnetamt, grtaxable, invtaxpct, ~
                invtaxamt, invdiscamt, nettaxable

        REM RECORD LAYOUT FOR 'WORKFILE' ********************************
L15110:         FMT  CH(10), CH(8), CH(8), CH(9), CH(30), 6*PD(14,4)

            nbr_recs% = 1% /* INDICATE 'RECORD SELECTED'   */
            goto plow_thru_arimastr

        sort_work_file
            if nbr_recs% > 0% then sort_continue
L15180:         comp% = 2%
                hdr$ = "*** NULL SET SELECTED ***"
                msg$(1) = "There are no records that satisfy your" &     ~
                     " criteria"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L15180
                goto inputmode
        sort_continue
            call "SLCTSORT" (#9, 18%)
            call "SHOSTAT" ("Sales Tax Report in process")
            page_nbr% = 0% : nbr_lines% = 99%
            init (" ") taxcode$(), taxdesc$()
            mat grtaxabl  = zer : mat taxamt    = zer : mat invamt = zer
            mat nettaxabl = zer : mat txbldisc  = zer
            mat tottaxabl = zer : mat tottaxamt = zer : mat totinvamt=zer
            mat tottxbdsc = zer : mat tottxbnet = zer
            time$ = time
            call "TIME" (time$)
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            sub_hdr$ = "SALES TAX REPORT:"
            call "FMTTITLE" (sub_hdr$, inv_hdr$, 2%)
            one_time%, eodsw%, taxsw%, colsw% = 0%
            ptr% = 1%

        REM Print 'PAGE ZERO' -- The Selection Screen *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
L15442:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L15450
                str(i$(), i%, 1%) = " "
                goto L15442
L15450:     print using L60100, "    ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60100, i$(n%)
            next n%
            pagesw% = 0%

        read_sorted_work_file
            read #9 using L15110, tax_code$, invoice$, inv_date$,         ~
                cust_code$, cust_name$, invnetamt, grtaxable, invtaxpct, ~
                invtaxamt, invdiscamt, nettaxable, eod goto end_of_report

            if one_time% = 0% then gosub tax_code_reset
        REM Find The Highest Level Of Break In The Tax Code. ************
            for nt% = 1% to nr% /* COUNT UP TO LOWEST LEVEL            */
                if str(tax_code$, pseg%(nt%), lseg%(nt%)) <>             ~
                     str(tax_brk$, pseg%(nt%), lseg%(nt%)) then          ~
                     goto print_totals
            next nt%
            goto totals_done   /* No Break (Tax_code$ = Tax_brk$)      */
        print_totals
        REM *************************************************************~
            * Print Tax-Code-Break Totals From The Lowest To The Highest*~
            * Level.                                                    *~
            *************************************************************
            ast$ = " "
            for nx% = nr% to nt% step -1% /*COUNT DOWN TO HIGHEST LEVEL*/
                gosub tax_code_total
            next nx%
            print
            nbr_lines% = nbr_lines% + 1%
        totals_done
            if nbr_lines% > max_lines% then gosub page_heading
            if taxsw% <> 0% then goto detail_print
                print using L60120, tax_code$, tax_desc$
                nbr_lines% = nbr_lines% + 1%
        detail_print
            invamt$, grtxbl$, txbldsc$, taxpct$, nettxbl$, taxamt$ = " "
            if invtaxpct <> 0 then call "CONVERT" (invtaxpct, 1.4,       ~
                taxpct$)
            call "CONVERT" (invnetamt, 2.2, invamt$)
            call "CONVERT" (grtaxable, 2.2, grtxbl$)
            call "CONVERT" (invdiscamt, 2.2, txbldsc$)
            call "CONVERT" (nettaxable, 2.2, nettxbl$)
            call "CONVERT" (invtaxamt, 2.2, taxamt$)
            print using L60190, invoice$, inv_date$, cust_code$,          ~
                  cust_name$, invamt$, grtxbl$, txbldsc$, nettxbl$,      ~
                  taxpct$, taxamt$
            nbr_lines% = nbr_lines% + 1%
            taxsw% = 1%   /* Turn Off Tax Code & Description Header */

        REM Add Detail Totals Into Lowest Level Of Total In The Arrays.
            totinvamt(nr% + 1%) = totinvamt(nr% + 1%) + invnetamt
            tottaxabl(nr% + 1%) = tottaxabl(nr% + 1%) + grtaxable
            tottxbdsc(nr% + 1%) = tottxbdsc(nr% + 1%) + invdiscamt
            tottxbnet(nr% + 1%) = tottxbnet(nr% + 1%) + nettaxable
            tottaxamt(nr% + 1%) = tottaxamt(nr% + 1%) + invtaxamt
            goto read_sorted_work_file

        tax_code_total
            ast$ = ast$ & "*" : ns% = nx% + 1%
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60220 /* UNDERSCORE */
            taxhold$ = str(tax_brk$,,xseg%(nx%))
            if nx% = nr% then  /* Lowest Level Of Total Being Printed  */~
                tot_desc$ = ast$ & " TAX CODE " & taxhold$ & " " &       ~
                     tax_desc$ & " TOTALS:"                              ~
            else                                                         ~
                tot_desc$ = ast$ & " " & taxhold$ & " TOTALS:"
            call "FMTTITLE" (tot_desc$, " ", 1%)
            call "CONVERT" (totinvamt(ns%), 2.2, invamt$)
            call "CONVERT" (tottaxabl(ns%), 2.2, grtxbl$)
            call "CONVERT" (tottxbdsc(ns%), 2.2, txbldsc$)
            call "CONVERT" (tottxbnet(ns%), 2.2, nettxbl$)
            call "CONVERT" (tottaxamt(ns%), 2.2, taxamt$)
            print using L60250, tot_desc$, invamt$, grtxbl$, txbldsc$,    ~
                               nettxbl$, taxamt$
            nbr_lines% = nbr_lines% + 2%
        REM *************************************************************~
            * Roll Totals Into Next-Highest Level Of Total In The Arrays*~
            * The Highest Level Is 1 (Run Totals).                      *~
            *************************************************************
            totinvamt(nx%) = totinvamt(nx%) + totinvamt(ns%)
            tottaxabl(nx%) = tottaxabl(nx%) + tottaxabl(ns%)
            tottxbdsc(nx%) = tottxbdsc(nx%) + tottxbdsc(ns%)
            tottxbnet(nx%) = tottxbnet(nx%) + tottxbnet(ns%)
            tottaxamt(nx%) = tottaxamt(nx%) + tottaxamt(ns%)
            if nx% <> nr% then goto L16300
        REM Add Tax Code Total To The Summary Total Arrays. *************
            if ptr% > 200% then goto L16300
                taxcode$(ptr%) = tax_brk$
                taxdesc$(ptr%) = tax_desc$
                invamt(ptr%) = totinvamt(ns%)
                grtaxabl(ptr%) = tottaxabl(ns%)
                txbldisc(ptr%) = tottxbdsc(ns%)
                nettaxabl(ptr%) = tottxbnet(ns%)
                taxamt(ptr%) = tottaxamt(ns%)
                ptr% = ptr% + 1%
L16300:     totinvamt(ns%), tottaxabl(ns%), tottaxamt(ns%) = 0
            tottxbdsc(ns%), tottxbnet(ns%) = 0
            if nx% <> nt% then return
        tax_code_reset
            if eodsw% = 1% then return
            one_time% = 1%
            tax_brk$ = tax_code$
            call "DESCRIBE" (#6, tax_code$, tax_desc$, 1%, f1%(6))
            if tax_desc$ = " " then tax_desc$ = "(NO TAX CODE)"
            taxsw% = 0%
            return

        end_of_report
            eodsw% = 1%
            ast$ = " "
        REM Total Out The Final Tax Code.
            for nx% = nr% to 1% step -1%
                gosub tax_code_total
            next nx%
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60220 /* Underscore */
            ast$ = ast$ & "*"
            tot_desc$ = ast$ & ast$ & ast$ & " RUN TOTALS:"
            call "FMTTITLE" (tot_desc$, " ", 1%)
            call "CONVERT" (totinvamt(1%), 2.2, invamt$)
            call "CONVERT" (tottaxabl(1%), 2.2, grtxbl$)
            call "CONVERT" (tottxbdsc(1%), 2.2, txbldsc$)
            call "CONVERT" (tottxbnet(1%), 2.2, nettxbl$)
            call "CONVERT" (tottaxamt(1%), 2.2, taxamt$)
            print using L60250, tot_desc$, invamt$, grtxbl$, txbldsc$,    ~
                               nettxbl$, taxamt$
        REM Print The Summary Total Page. *******************************
            sub_hdr$ = "SALES TAX REPORT SUMMARY:"
            call "FMTTITLE" (sub_hdr$, inv_hdr$, 2%)
            nbr_lines% = 99% : colsw% = 1%
            for nx% = 1% to min(ptr%-1%, 200%)
                if nbr_lines% > max_lines% then gosub page_heading
                call "CONVERT" (invamt(nx%), 2.2, invamt$)
                call "CONVERT" (grtaxabl(nx%), 2.2, grtxbl$)
                call "CONVERT" (txbldisc(nx%), 2.2, txbldsc$)
                call "CONVERT" (nettaxabl(nx%), 2.2, nettxbl$)
                call "CONVERT" (taxamt(nx%), 2.2, taxamt$)
                print using L60320, taxcode$(nx%), taxdesc$(nx%), invamt$,~
                                   grtxbl$, txbldsc$, nettxbl$, taxamt$
                nbr_lines% = nbr_lines% + 1%
            next nx%
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60340
            tax_desc$ = ast$ & ast$ & ast$ & ast$ & " SUMMARY TOTALS:"
            call "FMTTITLE" (tax_desc$, " ", 1%)
            call "CONVERT" (totinvamt(1%), 2.2, invamt$)
            call "CONVERT" (tottaxabl(1%), 2.2, grtxbl$)
            call "CONVERT" (tottxbdsc(1%), 2.2, txbldsc$)
            call "CONVERT" (tottxbnet(1%), 2.2, nettxbl$)
            call "CONVERT" (tottaxamt(1%), 2.2, taxamt$)
            print using L60320, " ", tax_desc$, invamt$, grtxbl$,         ~
                               txbldsc$, nettxbl$, taxamt$
            print
            print using L60360   /* END OF REPORT */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

        REM Scratch The Work File Holding Current Line Items ************
            close #9
            call "SCRATCH" addr("F", work_fil$, work_lib$, work_vol$,    ~
                "B", " ", comp%)
            goto inputmode

        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 6%
        page_0_heading
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, sub_hdr$, page_nbr%
            if pagesw% <> 0% then return
            print
            if colsw% = 0% then goto L17530
                print using L60280  /* Column Headers */
                print using L60300  /* Underscores    */
                goto L17550
L17530:     print using L60130, "#", "#" /* Column Headers */
            print using L60160           /* Underscores    */
L17550:     print
            taxsw% = 0%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20110,         /* Range of Dates   */~
                                    L20160          /* Segment lengths  */
                     return
L20110:     REM Range of Invoice Dates
                inpmessage$ = "Enter range of Posting Dates, 'FIRST', "& ~
                     "'LAST', or 'ALL'"
                return

L20160:     REM Tax Code Segment lengths
                inpmessage$ = "Enter Tax Code Segment lengths (MUST " &  ~
                     "equal 10)"
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

            deffn'101(fieldnr%)
                  str(line2$,62%) = "STXREPRT: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40190,         /* Range of Dates   */~
                                    L40220          /* Segment lengths  */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40260

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40220:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Tax Report",                                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Invoice Posting Dates: From             To", ~
               at (06,30), fac(lfac$( 1)), low_date$            , ch(10),~
               at (06,45), fac(lfac$( 1)), high_date$           , ch(10),~
               at (06,56), fac(hex(8c)), lastpur$               , ch(24),~
                                                                         ~
                at (07,02), "Tax Code Segment sizes:",                   ~
                at (07,30), fac(lfac$( 2)), lseg%(1), pic (##),          ~
                at (07,35), fac(lfac$( 2)), lseg%(2), pic (##),          ~
                at (07,40), fac(lfac$( 2)), lseg%(3), pic (##),          ~
                at (07,45), fac(lfac$( 2)), lseg%(4), pic (##),          ~
                at (07,50), fac(lfac$( 2)), lseg%(5), pic (##),          ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40630
                  call "MANUAL" ("STXREPRT")
                  goto L40260

L40630:        if keyhit% <> 15 then L40670
                  call "PRNTSCRN"
                  goto L40260

L40670:        if fieldnr% > 0% then return
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
                  on fieldnr% gosub L50120,         /* Range of Dates   */~
                                    L50350          /* Segment lengths  */
                  return

L50120: REM Invoice Dates                          LOW_DATE$/HIGH_DATE$
            fr_date% = 0% :  to_date% = 99999999%
            if low_date$ = " " then low_date$ = "FIRST"
            if low_date$ = "ALL" then L50310
            if low_date$ = "FIRST" then L50270
                call "DATEOKC" (low_date$, fr_date%, errormsg$)
                if errormsg$ <> " " then return
                if not (fr_date% < purge_date%) then goto L50230
                     errormsg$ = "The FROM date conflicts with the Date"&~
                          " Last Purged"
                     return
L50230:         if high_date$ = "LAST" then L50310
                if high_date$ <> " " then L50290
                     high_date$ = low_date$ : to_date% = fr_date%
                     goto L50310
L50270:     if high_date$ = " " then high_date$ = "LAST"
            if high_date$ = "LAST" then L50310
L50290:     call "DATEOKC" (high_date$, to_date%, errormsg$)
            if errormsg$ <> " " then return
L50310:     if fr_date% > to_date% then errormsg$ =                      ~
                "The FROM Date must be EARLIER than the TO Date"
                return

L50350: REM Segment lengths
        REM *************************************************************~
            * VALIDATES THE OPERATOR-ENTERED SEGMENT LENGTHS. THEY MUST *~
            * EQUAL 10, AND ANY ZERO-LENGTH SEGMENTS MUST BE AT THE END.*~
            * IN THE PROCESS, THIS ROUTINE COMPUTES NR%, THE TOTAL NUM- *~
            * BER OF TAX CODE SEGMENTS (RECORDED IN 'SYSFILE2').        *~
            *************************************************************
            nr%, nt%, sw% = 0%
            for nx% = 1% to 5%
                nt% = nt% + lseg%(nx%)
                if sw% <> 0% then goto L50510
                     if lseg%(nx%) <> 0% then goto L50490
                          sw% = 1%
                          goto L50550
L50490:              nr% = nr% + 1%
                     goto L50550
L50510:         if lseg%(nx%) = 0% then goto L50550
                     errormsg$ = "Zero-length segments must all be at "& ~
                          "the end"
                     return
L50550:     next nx%
            if nt% <> 10% then errormsg$ = "Sum of segment lengths MUS"& ~
                "T equal 10"
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %Run Date: ######## @ ########      #############################~
        ~###############################                      STXREPRT####~
        ~###
L60070: %                              ##################################~
        ~####################################                      Page: #~
        ~###
L60100: %                          ######################################~
        ~##########################################
L60120: % *****  Tax Code: ########## ################################
L60130: %Invoice# Post Date  Customer#  Customer Name                    ~
        ~Invoice Amt    Gr Taxable    Adjust   Net Taxable Tax %  Tax Amou~
        ~nt
L60160: %-------- ---------  ---------  ------------------------------  -~
        ~-----------  ------------  --------  ------------  ----  --------~
        ~--
L60190: %########  ########  #########  ##############################  #~
        ~###########- ############- ########- ############- ####  ########~
        ~##-
L60220: %                                                               -~
        ~-----------  ------------  --------  ------------        --------~
        ~--
L60250: %#############################################################  #~
        ~###########- ############- ########- ############-       ########~
        ~##-
L60280: %           Tax Code   Tax Code Description              Invoice ~
        ~Amt    Grs Taxable   -Adjust   Net Taxable  Tax Amount
L60300: %           ---------- --------------------------------  --------~
        ~----  ------------  --------  ------------  ----------
L60320: %           ########## ################################  ########~
        ~####  ############  ########  ############  ##########
L60340: %                                                        --------~
        ~----  ------------  --------  ------------  ----------
L60360: %                                                        ** END O~
        ~F REPORT **

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
