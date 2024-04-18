        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K   QQQ   U   U  EEEEE  RRRR   Y   Y   *~
            *  B   B  C   C  K  K   Q   Q  U   U  E      R   R  Y   Y   *~
            *  BBBB   C      KKK    Q   Q  U   U  EEEE   RRRR    YYY    *~
            *  B   B  C   C  K  K   Q Q Q  U   U  E      R   R    Y     *~
            *  BBBB    CCC   K   K   QQQ    UUU   EEEEE  R   R    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKQUERY - Allows the operator to inquire into and/or     *~
            *            print Open Order activity either by Part number*~
            *            range or by Project Code range.  This program  *~
            *            combines the former BCKBYPRT & BCKBYJOB.       *~
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
            * 08/22/86 ! Original                                 ! JIM *~
            * 02/27/89 ! Changed Column Header from QTY SHIP'D    ! MJB *~
            *          !  to QTY INVC'D                           ! MJB *~
            * 05/07/91 ! PRR 11965 Allow users to Print/Display   ! SID *~
            *          !  open SO qty based on Invoiced/Shipping  !     *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 11/09/92 ! PRR 12436- Option to include Closed Lines! JIM *~
            * 07/31/96 ! Changes for the year 2000.               ! DXL *~
            * 04/04/97 ! Change PUTPARM call for NT Compatibility ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bck_cust$9,                  /* Customer code from BCKLINES*/~
            bck_dat8$8,                  /* 8-char copy of BCK_DATE$   */~
            bck_date$6,                  /* Due Date from BCKLINES     */~
            bck_desc$32,                 /* Part Descript from BCKLINES*/~
            bck_lot$6,                   /* Lot number from BCKLINES   */~
            bck_part$25,                 /* Part number from BCKLINES  */~
            bck_proj$8,                  /* Project # from BCKLINES    */~
            bck_sord$16,                 /* Sales Order # from BCKLINES*/~
            bck_stor$3,                  /* Store number from BCKMASTR */~
            bckmastr_key$25,             /* Key to BCKMASTR file       */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            closed$1,                    /* Include closed Line Items? */~
            code$1,                      /* '1'= Part #; '2'= Project  */~
            codedescr$32,                /* Description of above field */~
            company_name$60,             /* Company name from COMPNAME */~
            cursor%(2),                  /* Cursor location for edit   */~
            cust_brk$9,                  /* Customer code for breaks   */~
            cust_name$30,                /* Customer name from CUSTOMER*/~
            date$8,                      /* Date for screen display    */~
            desc_brk$32,                 /* Holds Part descr for totals*/~
            dummy$18,                    /* Dummy Variable             */~
            edtmessage$79,               /* Edit screen message        */~
            eodsw$1,                     /* End of data on SORTWORK    */~
            errormsg$79,                 /* Error message              */~
            fr_cust$9,                   /* Low Customer for compare   */~
            fr_part$25,                  /* Low Part # for compare     */~
            fr_proj$8,                   /* Low Project # for compare  */~
            fr_sord$16,                  /* Low Sales Order for compare*/~
            hdr$40,                      /* ASKUSER constant           */~
            high_cust$9,                 /* High Customer Code         */~
            high_date$10,                /* High Due Date              */~
            high_part$25,                /* High Part Number           */~
            high_proj$8,                 /* High Project Number        */~
            high_sord$16,                /* High Sales Order Number    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            low_cust$9,                  /* Low Customer Code          */~
            low_date$10,                 /* Low Due Date               */~
            low_part$25,                 /* Low Part Number            */~
            low_proj$8,                  /* Low Project Number         */~
            low_sord$16,                 /* Low Sales Order Number     */~
            msg$(3)80,                   /* ASKUSER messages           */~
            one_time$1,                  /* 1-time switch for print    */~
            part_brk$25,                 /* Part # for report breaks   */~
            pf14$16,                     /* PF 14 Screen Literal       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf16fac$1,                   /* PF 16 Screen Attribute     */~
            pf30$30,                     /* PF 30 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prnt_disp$1,                 /* Print or Display flag      */~
            prnt_opts$1,                 /* Print Options I,S          */~
            prnt_opts_desc$35,           /* Print Options I,S Descr.   */~
            proj_brk$8,                  /* Holds current Project #    */~
            proj_desc$32,                /* Project code description   */~
            prt_fil$8,                   /* Print file name            */~
            prt_lib$8,                   /* Print file library         */~
            prt_vol$6,                   /* Print file volume          */~
            rptid$6,                     /* Report ID                  */~
            skipsw$1,                    /* Skip a line after totals   */~
            sortkey$70,                  /* Sort on these values       */~
            time$8,                      /* Time of day stamp          */~
            to_cust$9,                   /* High Customer for compare  */~
            to_part$25,                  /* High Part # for compare    */~
            to_proj$8,                   /* High Project # for compare */~
            to_sord$16,                  /* High Sales Ord for compare */~
            userid$3,                    /* Current User Id            */~
            work_fil$8,                  /* WORKFILE name              */~
            work_lib$8,                  /* WORKFILE library           */~
            work_vol$6                   /* WORKFILE volume            */~

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
            * #3  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #4  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! JOBMASTR ! WORK IN PROCESS MASTER FILE              *~
            * #7  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #9  ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select #4,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #6,  "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =   8                      ~

            select #7,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16          ~

            select #9,  "WORKFILE", consec, recsize =  142

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            get rslt$(4), using L02552, bck_recs%
L02552:         FMT POS(17), BI(4)
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
                rslt$(7 ) = "REQUIRED"
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            if bck_recs% > 0% then L09140
L09070:         comp% = 2%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Sales Order data to analyze"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09070
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
L09140:     bck_recs% = max(50%, (bck_recs% * .3))
            call "COMPNAME" (12%, company_name$, return%)
            rptid$ = "BCK004"
            if return% = 1% then company_name$ = " "
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16fac$ = hex(84)
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, code$, high_part$,         ~
                      high_proj$, high_date$, high_cust$, high_sord$,    ~
                      pf14$, pf30$, codedescr$, prnt_opts$, closed$
            prnt_opts$ = "I" : code$ = "1" : closed$ = "N"
            low_part$,low_proj$,low_date$,low_cust$,low_sord$="ALL"

            for fieldnr% = 1% to 8%
L10150:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10270
L10170:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10250
L10200:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10170
                         if fieldnr% = 1% then L10150
                         goto L10200
L10250:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10170
L10270:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10170
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
            pf30fac$ = " "
            pf30$ = "(30)Display Report On Screen" : pf30fac$ = hex(84)
            pf16$ = "(16)Print Report" : pf16fac$ = hex(84)
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit% =  1 then gosub startover
                  if keyhit% = 16 then report_print
                  if keyhit% = 30 then report_display
                  if keyhit% <>  0 then       edtpg1
L11180:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  8 then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11230:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
            if cursor%(1) - 5 <> fieldnr% then L11180
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        report_print
            prnt_disp$ = "P" : goto datasave
        report_display
            prnt_disp$ = "D"
        datasave
            call "SHOSTAT"                                               ~
                ("Selecting Records ... Please stand by")
            plowkey$ = fr_sord$
            str(plowkey$,len(fr_sord$)+1) = all(hex(00))
            nbr_recs% = 0%   /* Initialize count of records selected */
            call "WORKOPN2" (#9, "OUTPT", bck_recs%, f2%(9))
            call "GETNAMES" addr(#9, work_fil$, work_lib$, work_vol$)

        plow_thru_bcklines
            call "PLOWNEXT" (#4, plowkey$, 0%, f1%(4))
            if f1%(4) = 0% then sort_work_file
            if str(plowkey$,,len(to_sord$)) > to_sord$                   ~
                then sort_work_file
            get #4 using L13290, bck_cust$, bck_sord$, bck_part$,         ~
                bck_desc$, bck_qord, bck_qshp, bck_qopn, bck_preinv,     ~
                bck_uprc, bck_date$, bck_lot$, bck_proj$

        REM PARTIAL RECORD LAYOUT FOR FILE 'BCKLINES' *******************
L13290:         FMT CH(9),              /* Customer Code               */~
                    CH(16),             /* Sales Order Number          */~
                    POS(32), CH(25),    /* Part Number                 */~
                    CH(32),             /* Part Description            */~
                    POS(93), 3*PD(14,4),/* Qtys: Ordered, Shipped, Open*/~
                    POS(133), PD(14,4), /* Qtys: Pre-Invoiced Qty      */~
                    POS(141), PD(14,4), /* Unit Price                  */~
                    POS(206), CH(6),    /* Due Date                    */~
                    POS(218), CH(6),    /* Lot Number                  */~
                    POS(224), CH(6)     /* Project Code                */
            if bck_cust$ < fr_cust$ then plow_thru_bcklines
            if bck_cust$ > to_cust$ then plow_thru_bcklines
            if bck_part$ < fr_part$ then plow_thru_bcklines
            if bck_part$ > to_part$ then plow_thru_bcklines
            if bck_proj$ < fr_proj$ then plow_thru_bcklines
            if bck_proj$ > to_proj$ then plow_thru_bcklines
            bck_dat8$ = bck_date$
            call "DATEOK" (bck_dat8$, bck_date%, errormsg$)
            if bck_date% < fr_date% then plow_thru_bcklines
            if bck_date% > to_date% then plow_thru_bcklines
            if prnt_opts$ <> "I" then L13477
               if closed$ <> "Y" and bck_qopn <= 0 then plow_thru_bcklines
               goto L13480
L13477:        bck_qopn = max(0, (bck_qopn - bck_preinv))
               if closed$ <> "Y" and bck_qopn <= 0 then plow_thru_bcklines

L13480:     nbr_recs% = nbr_recs% + 1%   /* Count records selected */

        REM READ THE BACKLOG MASTER FILE TO GET THE STORE NUMBER ********
            bckmastr_key$ = str(bck_cust$,,9) & bck_sord$
            call "READ100" (#7, bckmastr_key$, f1%(7))
            bck_stor$ = " "
            if f1%(7) = 0 then build_sort_key               /* NO HIT */
                get #7, using L13560, bck_stor$
L13560:              FMT POS(803), CH(3)
        build_sort_key
            if code$ <> "1" then sort_by_proj
        REM OTHERWISE, SORT BY PART
            str(sortkey$,  1, 25) = bck_part$
            str(sortkey$, 26,  9) = bck_cust$
            str(sortkey$, 35,  6) = bck_date$
            str(sortkey$, 41, 16) = bck_sord$
            str(sortkey$, 57,  3) = bck_stor$
            goto output_workfile
        sort_by_proj
            str(sortkey$,  1,  8) = bck_proj$
            str(sortkey$,  9, 25) = bck_part$
            str(sortkey$, 34,  9) = bck_cust$
            str(sortkey$, 43,  6) = bck_date$
            str(sortkey$, 49, 16) = bck_sord$
            str(sortkey$, 65,  3) = bck_stor$
        output_workfile
            write #9 using L13760, sortkey$, bck_qord, bck_qshp, bck_qopn,~
                bck_uprc, bck_dat8$, bck_desc$
L13760:         FMT CH(70), 4*PD(14,4), CH(8), CH(32)
            goto plow_thru_bcklines

        sort_work_file
            if nbr_recs% > 0% then sort_continue
L13810:         comp% = 2%
                hdr$ = "*** NULL SET SELECTED ***"
                msg$(1) = "There are no records that satisfy your" &     ~
                     " criteria"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L13810
                goto inputmode
        sort_continue
            call "SLCTSORT" (#9, 70%)

        REM PRINT/DISPLAY LOGIC BEGINS HERE ****************************
            one_time$, eodsw$ = "0"
            call "SHOSTAT" ("Report Generation In Process")
            page_nbr% = 0% : line_nbr% = 99%
            t1_qord, t1_qshp, t1_qopn, t1_extn, t2_qord, t2_qshp,        ~
                t2_qopn, t2_extn = 0
            init (" ") bck_part$, bck_stor$, bck_date$, bck_cust$,       ~
                bck_sord$, bck_dat8$, sortkey$
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)
        REM PRINT 'PAGE ZERO' -- THE SELECTION SCREEN *******************
            pagesw% = 1%
            gosub page_0_heading
L14222:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L14230
                str(i$(), i%, 1%) = " "
                goto L14222
L14230:     print skip (4)
            print using L60370, "   ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60370, i$(n%)
            next n%
            pagesw% = 0%

        read_sorted_workfile
            read #9 using L13760, sortkey$, bck_qord, bck_qshp, bck_qopn, ~
                bck_uprc, bck_dat8$, bck_desc$, eod goto report_total
            if code$ = "1" then                                          ~
                get #9 using L14400, bck_part$, bck_cust$, bck_sord$,     ~
                     bck_stor$                                           ~
            else                                                         ~
                get #9 using L14410, bck_proj$, bck_part$, bck_cust$,     ~
                     bck_sord$, bck_stor$
L14400:         FMT CH(25), CH(9), POS(41), CH(16), CH(3) /* PART # */
L14410:         FMT CH(8), CH(25), CH(9), POS(49), CH(16), CH(3) /*PROJ*/
            if one_time$ = "1" then data_print_setup
                if code$="1" then gosub cust_reset else gosub pjpt_reset
                if code$="1" then gosub part_reset else gosub proj_reset
                one_time$ = "1"

        data_print_setup
            skipsw$ = "0"
            if code$= "1" and bck_part$<>part_brk$ then gosub part_total
            if code$= "1" and bck_cust$<>cust_brk$ then gosub cust_total
            if code$<>"1" and bck_proj$<>proj_brk$ then gosub proj_total
            if code$<>"1" and bck_part$<>part_brk$ then gosub pjpt_total
            if skipsw$ = "0" then L14550
                print : line_nbr% = line_nbr% + 1%
L14550:     if not(line_nbr% > 56%) then goto L14620
                gosub page_heading
                if code$ = "1" then                                      ~
                     print using L60100, bck_part$, bck_desc$             ~
                else                                                     ~
                     print using L60420, bck_proj$, proj_desc$
                line_nbr% = line_nbr% + 1%
L14620:     bck_qord = round(bck_qord, 2)
            bck_qshp = round(bck_qshp, 2)
            bck_qopn = round(bck_qopn, 2)
            bck_extn = round(bck_qopn * bck_uprc, 2)
            if code$ = "1" then                                          ~
                print using L60330, bck_cust$, cust_name$, bck_dat8$,     ~
                     bck_sord$, bck_stor$, bck_qord, bck_qshp, bck_qopn, ~
                     bck_uprc, bck_extn                                  ~
            else                                                         ~
                print using L60500, bck_part$, bck_cust$, bck_dat8$,      ~
                     bck_sord$, bck_stor$, bck_qord, bck_qshp, bck_qopn, ~
                     bck_uprc, bck_extn
            line_nbr% = line_nbr% + 1%
            t1_qord = t1_qord + bck_qord
            t1_qshp = t1_qshp + bck_qshp
            t1_qopn = t1_qopn + bck_qopn
            t1_extn = t1_extn + bck_extn
            goto read_sorted_workfile

        page_heading
            page_nbr% = page_nbr% + 1%  :  line_nbr% = 6%
        page_0_heading
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            if prnt_opts$ = "I" then dummy$ = "BASED ON INVOICING"       ~
                                else dummy$ = "BASED ON SHIPPING "
            if code$ = "1" then                                          ~
                print using L60070, dummy$, page_nbr%  /* PART HEADER */  ~
            else                                                         ~
                print using L60390, dummy$, page_nbr%  /* PROJECT HEADER */

            if pagesw% <> 0% then return
            print
            if code$ <> "1" then goto L15150
                print using L60150, "#" /*PART COLUMN HEADERS*/
                print using L60120      /*UNDERSCORES*/
                goto L15170
L15150:     print using L60440, "#"  /*PROJECT COLUMN HEADERS*/
            print using L60470       /*UNDERSCORES*/
L15170:     print
            return

        report_total
            eodsw$ = "1"
            if code$ = "1" then gosub part_total else gosub proj_total
            print
            print using L60570, time$                 /* END OF REPORT */
            call "GETPRTNM" addr(prt_fil$, prt_lib$, prt_vol$)
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

        REM SCRATCH THE WORK FILE HOLDING CURRENT LINE ITEMS ************
            close #9
            call "SCRATCH" addr("F", work_fil$, work_lib$, work_vol$,    ~
                "B", " ", return%)

        REM IF OPERATOR SELECTED 'PRINT', WE'RE DONE ********************
            if prnt_disp$ = "P" then goto inputmode

        REM OTHERWISE, LINK TO 'DISPLAY' TO SHOW RESULTS ON THE SCREEN **
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                "FILE    ", prt_fil$, 8%,                                ~
                "LIBRARY ", prt_lib$, 8%,                                ~
                "VOLUME  ", prt_vol$, 6%,                                ~
                "ACCESS  ", "PRINT ", 6%, "@",return%)

            call "LINK" addr("DISPLAY ","S"," "," ",0%," "," ",0%,"N",   ~
                comp%, return%)

        REM SCRATCH THE PRINT FILE JUST DISPLAYED ON THE TUBE ***********
            call "SCRATCH" addr("F", prt_fil$, prt_lib$, prt_vol$,       ~
                " ", " ", return%)
            goto inputmode

        part_total
            gosub cust_total_no_skip
            if line_nbr% > 56% then gosub page_heading
            print using L60300
            print using L60210, part_brk$, t2_qord, t2_qshp, t2_qopn,     ~
                t2_extn
            line_nbr% = line_nbr% + 2%
            t2_qord, t2_qshp, t2_qopn, t2_extn = 0
        part_reset
            if eodsw$ = "1" then return
            part_brk$ = bck_part$
            desc_brk$ = bck_desc$
            if line_nbr% > 56% then gosub page_heading
            if one_time$ = "0" then print else print using L60530
            print using L60100, bck_part$, bck_desc$
            line_nbr% = line_nbr% + 2%
            return

        proj_total
            gosub pjpt_total_no_skip
            if line_nbr% > 56% then gosub page_heading
            print using L60300
            print using L60240, proj_brk$, proj_desc$, t2_qord, t2_qshp,  ~
                t2_qopn, t2_extn
            line_nbr% = line_nbr% + 2%
            t2_qord, t2_qshp, t2_qopn, t2_extn = 0
        proj_reset
            if eodsw$ = "1" then return
            proj_brk$ = bck_proj$
            call "DESCRIBE" (#6, bck_proj$, proj_desc$, 0%, f1%(6))
            if f1%(6) = 0% then proj_desc$ = "Project code not in file"
            if line_nbr% > 56% then gosub page_heading
            if one_time$ = "0" then print else print using L60530
            print using L60420, bck_proj$, proj_desc$
            line_nbr% = line_nbr% + 2%
            return

        cust_total
            skipsw$ = "1"
        cust_total_no_skip
            if line_nbr% > 56% then gosub page_heading
            print using L60300
            print using L60180, cust_brk$, cust_name$, t1_qord, t1_qshp,  ~
                t1_qopn, t1_extn
            line_nbr% = line_nbr% + 2%
            t2_qord = t2_qord + t1_qord
            t2_qshp = t2_qshp + t1_qshp
            t2_qopn = t2_qopn + t1_qopn
            t2_extn = t2_extn + t1_extn
            t1_qord, t1_qshp, t1_qopn, t1_extn = 0
        cust_reset
            if eodsw$ = "1" then return
            cust_brk$ = bck_cust$
            call "DESCRIBE" (#3, bck_cust$, cust_name$, 0%, f1%(3))
            if f1%(3) = 0% then cust_name$ = "Customer not in file"
            return

        pjpt_total
            skipsw$ = "1"
        pjpt_total_no_skip
            if line_nbr% > 56% then gosub page_heading
            print using L60300
            print using L60270, part_brk$, t1_qord, t1_qshp, t1_qopn,     ~
                t1_extn
            line_nbr% = line_nbr% + 2%
            t2_qord = t2_qord + t1_qord
            t2_qshp = t2_qshp + t1_qshp
            t2_qopn = t2_qopn + t1_qopn
            t2_extn = t2_extn + t1_extn
            t1_qord, t1_qshp, t1_qopn, t1_extn = 0
        pjpt_reset
            if eodsw$ = "1" then return
            part_brk$ = bck_part$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20142,         /* I,S Open Quantity*/~
                                    L20150,         /* Part or Project  */~
                                    L20400,         /* Incl Closed Lns? */~
                                    L20190,         /* Part Number(s)   */~
                                    L20230,         /* Project Number(s)*/~
                                    L20270,         /* Due Date(s)      */~
                                    L20310,         /* Customer Code(s) */~
                                    L20350          /* Sales Order #(s) */
            return

L20142: REM 'I'nvoiced, 'S'hipped                 PRINT_OPT$
            inpmessage$ = "Print/Display Based on 'I'nvoiced OR 'S'hipp"&~
                          "ing Quantity."
            return

L20150: REM '1' for Part; '2' for Project         CODE$
            inpmessage$ = "Enter '1' to search on PART or '2' to search"&~
                          " on PROJECT (Default = '1')"
            return
L20190: REM Part Number(s)                        LOW_PART$/HIGH_PART$
            inpmessage$ = "Enter a range of PART #s, 'FIRST', 'LAST', "& ~
                          "or 'ALL'"
            return
L20230: REM Project Number(s)                     LOW_PROJ$/HIGH_PROJ$
            inpmessage$ = "Enter a range of PROJECT codes, 'FIRST', " &  ~
                          "'LAST', or 'ALL'"
            return
L20270: REM Due Date(s)                           LOW_DATE$/HIGH_DATE$
            inpmessage$ = "Enter a range of DUE DATES, 'FIRST'" &        ~
                          ", 'LAST', or 'ALL'"
            return
L20310: REM Customer Code(s)                      LOW_CUST$/HIGH_CUST$
            inpmessage$ = "Enter a range of CUSTOMER codes, 'FIRST', 'L"&~
                          "AST', or 'ALL'"
            return
L20350: REM Sales Order(s)                       LOW_SORD$/HIGH_SORD$
            inpmessage$ = "Enter a range of SALES ORDER #s, 'FIRST', 'L"&~
                          "AST', or 'ALL'"
            return

L20400: REM Include closed Line Items?           CLOSED$
            inpmessage$ = "Enter 'Y' (or 'N') to INCLUDE (or exclude) C"&~
                "losed Line Items."
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
                  str(line2$,62%) = "BCKQUERY: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40210,         /* Print Options I,S*/~
                                    L40210,         /* Part or Project  */~
                                    L40210,         /* Incl Closed Lns? */~
                                    L40210,         /* Part Number(s)   */~
                                    L40210,         /* Project Number(s)*/~
                                    L40210,         /* Due Date(s)      */~
                                    L40210,         /* Customer Code(s) */~
                                    L40210          /* Sales Order(s)   */
                     goto L40280

        REM Set FAC's for Upper/Lower Case Input
            lfac$(fieldnr%) = hex(80)
            return
L40210: REM SET FAC'S FOR UPPER CASE ONLY INPUT
            lfac$(fieldnr%) = hex(81)
            return
        REM SET FAC'S FOR NUMERIC ONLY INPUT
            lfac$(fieldnr%) = hex(82)
            return

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Order Inquiry by Part # or Project code",       ~
               at (01,66), "TODAY:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "'I'nvoiced; 'S'hipped"              ,        ~
               at (06,25), fac(lfac$( 1)), prnt_opts$           , ch(01),~
               at (06,28), fac(hex(8c)),   prnt_opts_desc$      , ch(35),~
                                                                         ~
               at (07,02), "'1' = Part; '2' = Project",                  ~
               at (07,25), fac(lfac$( 2)), code$                , ch(01),~
               at (07,28), fac(hex(8c)),   codedescr$           , ch(32),~
                                                                         ~
               at (08,02), "Include Closed Lines?",                      ~
               at (08,25), fac(lfac$( 3)), closed$              , ch(01),~
                                                                         ~
               at (09,02), "Part Numbers",                               ~
               at (09,25), fac(lfac$( 4)), low_part$            , ch(25),~
               at (09,52), fac(lfac$( 4)), high_part$           , ch(25),~
                                                                         ~
               at (10,02), "Project Numbers",                            ~
               at (10,25), fac(lfac$( 5)), low_proj$            , ch(08),~
               at (10,52), fac(lfac$( 5)), high_proj$           , ch(08),~
                                                                         ~
               at (11,02), "Due Dates",                                  ~
               at (11,25), fac(lfac$( 6)), low_date$            , ch(10),~
               at (11,52), fac(lfac$( 6)), high_date$           , ch(10),~
                                                                         ~
               at (12,02), "Customer Codes",                             ~
               at (12,25), fac(lfac$( 7)), low_cust$            , ch(09),~
               at (12,52), fac(lfac$( 7)), high_cust$           , ch(09),~
                                                                         ~
               at (13,02), "Sales Orders",                               ~
               at (13,25), fac(lfac$( 8)), low_sord$            , ch(16),~
               at (13,52), fac(lfac$( 8)), high_sord$           , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$,                           ~
               at (22,41), fac(hex(84)), pf14$,                          ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,21), fac(hex(84)), pf30$,                          ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(pf16fac$), pf16$,                         ~
                                                                         ~
               keys(hex(000104050dff0f101e)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40780
                  call "MANUAL" ("BCKQUERY")
                  goto L40280

L40780:        if keyhit% <> 15 then L40820
                  call "PRNTSCRN"
                  goto L40280

L40820:        if fieldnr% > 0% then return
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
                  on fieldnr% gosub L50151,         /* Print Opts I,S   */~
                                    L50165,         /* Part or Project  */~
                                    L50680,         /* Incl Closed Lns? */~
                                    L50250,         /* Part Number(s)   */~
                                    L50310,         /* Project Number(s)*/~
                                    L50380,         /* Due Date(s)      */~
                                    L50560,         /* Customer Code(s) */~
                                    L50620          /* Sales Order(s)   */
                  return

L50151: REM 'I'nvoiced OR 'S'hipped                PRNT_OPTS$
            if prnt_opts$ = " " and keyhit% = 0% then prnt_opts$ = "I"
            if prnt_opts$ <> "I" then L50156
               prnt_opts_desc$ = "(Orders Based on Invoiced Qty)"
               return
L50156:     if prnt_opts$ <> "S" then L50159
               prnt_opts_desc$ = "(Orders Based on Shipped Qty)"
               return
L50159:     errormsg$ = "You must enter either 'I' or 'S'"
            return



L50165: REM '1' for Part; '2' for Project          CODE$
            if code$ = " " and keyhit% = 0% then code$ = "1" /*DEFAULT*/
            if code$ <> "1" then L50200
                codedescr$ = "(Search by PART Number)" : return
L50200:     if code$ <> "2" then L50220
                codedescr$ = "(Search by PROJECT Code)" : return
L50220:     errormsg$ = "You must enter either '1' or '2'"
            return

L50250: REM Part Number(s)                         LOW_PART$/HIGH_PART$
            call "TESTRNGE" (low_part$, high_part$, fr_part$, to_part$,  ~
                             errormsg$)
            fr_part$ = addc (hex(01))
            return

L50310: REM Project Number(s)                      LOW_PROJ$/LOW_PROJ$
            call "TESTRNGE" (low_proj$, high_proj$, fr_proj$, to_proj$,  ~
                             errormsg$)
            fr_proj$ = addc (hex(01))
            return

        REM Due Date(s)                            LOW_DATE$/HIGH_DATE$
L50380:     fr_date% = 0% :  to_date% = 99999999%
            if low_date$ = " " or low_date$ = blankdate$ then ~
                                  low_date$ = "FIRST"
            if low_date$ = "ALL" then L50520
            if low_date$ = "FIRST" then L50480
                call "DATEOKC" (low_date$, fr_date%, errormsg$)
                if errormsg$ <> " " then return
                if high_date$ = "LAST" then L50520
                if high_date$ <> " " and high_date$ <> blankdate$ then L50500
                     high_date$ = low_date$ : to_date% = fr_date%
                     goto L50520
L50480:     if high_date$ = " " or high_date$ = blankdate$ ~
                              then high_date$ = "LAST"
            if high_date$ = "LAST" then L50520
L50500:     call "DATEOKC" (high_date$, to_date%, errormsg$)
            if errormsg$ <> " " then return
L50520:     if fr_date% > to_date% then errormsg$ =                      ~
                "The FROM Date must be EARLIER than the TO Date"
            return

L50560: REM Customer Code(s)                       LOW_CUST$/HIGH_CUST$
            call "TESTRNGE" (low_cust$, high_cust$, fr_cust$, to_cust$,  ~
                             errormsg$)
            fr_cust$ = addc (hex(01))
            return

L50620: REM Sales Order(s)                         LOW_DATE$/HIGH_DATE$
            call "TESTRNGE" (low_sord$, high_sord$, fr_sord$, to_sord$,  ~
                             errormsg$)
            fr_sord$ = addc (hex(01))
            return

L50680: REM Include Closed Line Items?             CLOSED$
            if closed$ = "Y" or closed$ = "N" then return
                errormsg$ = "You must enter 'Y' or 'N' for this code."
                return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                    BCKQUERY######~
        ~#
L60070: %                                      SALES ORDER REPORT BY PART~
        ~ NUMBER ##################                              PAGE: ###~
        ~#
L60100: %     PART: ######################### ###########################~
        ~#####
L60120: %--------- ------------------------------ -------- --------------~
        ~-- --- ----------- ----------- ----------- ----------- ----------~
        ~---
L60150: %CUSTOMER# CUSTOMER NAME                  DUE DATE SALES ORDER NO~
        ~.  STR   QTY ORD'D  QTY INVC'D    QTY OPEN  UNIT PRICE     OPEN E~
        ~XTN
L60180: %  ** CUSTOMER TOTAL: ######### ##############################   ~
        ~      -####,###.##-####,###.##-####,###.##            -##,###,###~
        ~.##
L60210: % *** PART TOTAL: #########################                      ~
        ~      -####,###.##-####,###.##-####,###.##            -##,###,###~
        ~.##
L60240: % *** PROJECT TOTAL: ######## ################################   ~
        ~      -####,###.##-####,###.##-####,###.##            -##,###,###~
        ~.##
L60270: %  ** PART TOTAL: #########################                      ~
        ~      -####,###.##-####,###.##-####,###.##            -##,###,###~
        ~.##
L60300: %                                                                ~
        ~       ----------- ----------- -----------             ----------~
        ~---
L60330: %######### ############################## ######## ##############~
        ~## ###-####,###.##-####,###.##-####,###.##-######.####-##,###,###~
        ~.##

L60370: %                          ######################################~
        ~##########################################
L60390: %                                     SALES ORDER REPORT BY PROJE~
        ~CT NUMBER ##################                             PAGE ###~
        ~#
L60420: %          PROJECT: ######## ################################

L60440: %    PART NUMBER               CUSTOMER#  DUE DATE SALES ORDER NO~
        ~.  STR   QTY ORD'D  QTY INVC'D    QTY OPEN  UNIT PRICE     OPEN E~
        ~XTN
L60470: %    ------------------------- ---------  -------- --------------~
        ~-- --- ----------- ----------- ----------- ----------- ----------~
        ~---
L60500: %    ######################### #########  ######## ##############~
        ~## ###-####,###.##-####,###.##-####,###.##-######.####-##,###,###~
        ~.##
L60530: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L60570: %                                                  ** END OF REPO~
        ~RT    ######## **

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
