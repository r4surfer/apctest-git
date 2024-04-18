        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   PPPP   RRRR   IIIII  N   N  TTTTT          *~
            *  S      A   A  P   P  R   R    I    NN  N    T            *~
            *   SSS   AAAAA  PPPP   RRRR     I    N N N    T            *~
            *      S  A   A  P      R   R    I    N  NN    T            *~
            *   SSS   A   A  P      R   R  IIIII  N   N    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAPRINT  - Prints Sales Analysis reports based on the     *~
            *            selections made in program SARPTSEL.           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/87 ! Original                                 ! JIM *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 05/17/88 ! PRR 6203 corrected totalling             ! JIM *~
            * 06/01/89 ! PRR 10964 corrected edit logic           ! DWT *~
            * 02/13/91 ! Summary report now PLOWs/Accums 2nd key. ! JIM *~
            * 06/06/91 ! PRR 11385.  Visibility into summaries    ! JDH *~
            *          !   even if no activity in current year.   !     *~
            *          ! PRR 11657.  Fixd pg break summary descrs.! JDH *~
            * 12/10/91 ! Delete out JIMs Sum rpt PLOWs ref above. ! JDH *~
            * 02/19/92 ! Added support for Including summary codes! JDH *~
            *          !   with no activity in specified years.   !     *~
            *          ! Opened workfile with better # of records.!     *~
            * 02/12/93 ! PRR 12669 distinct WORKFILn names.       ! JIM *~
            * 12/30/93 ! PRR 13028. Fixed Sub Header on sub total ! JDH *~
            *          !   page break.                            !     *~
            * 08/21/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim     /* Renumber 1000 - 2000 series by 10 */                  ~
            asnd_dsnd$1,                 /* Ascend or Descend          */~
            asof_date$8,                 /* Default As Of Date         */~
            bg_yorn$1,                   /* Print in Background (Y/N)?:*/~
            blankdate$8,                 /* blank unfmt date           */~
            cdescr$(7)45,                /* Report Column description  */~
            code1$25, code1desc$32,      /* Record code # 1            */~
            code1save$25,                /* Save area for Code 1       */~
            code2$25, code2desc$32,      /* Record code # 2            */~
            codefile%(8), codelen%(8),   /* Group Code Len, File#      */~
            codes%(10,2),                /* Summary File Group Codes   */~
            codes$(10,2)14,              /* Group Code Descriptors     */~
            codex$25, codexdesc$32,      /* For Description lookups    */~
            col$3,                       /* Column header              */~
            col$(7)1,                    /* Column designators A-G     */~
            col_desc$(8)32,              /* Column header              */~
            col_head$(8)25,              /* Column header              */~
            colerr%(7),                  /* Report column error flags  */~
            colahdr$(3)10,               /* Column A report header     */~
            colbhdr$(3)10,               /* Column B report header     */~
            colchdr$(3)10,               /* Column C report header     */~
            coldhdr$(3)10,               /* Column D report header     */~
            colehdr$(3)10,               /* Column E report header     */~
            colfhdr$(3)10,               /* Column F report header     */~
            colghdr$(3)10,               /* Column G report header     */~
            colxhdr$(3)10,               /* Column header work area    */~
            colyhdr$(3)20,               /* Ditto                      */~
            coldesc$18,                  /* Column header              */~
            coltot(7, 3),                /* Columnar computations      */~
            columns$(5)132,              /* Report Columns & underscore*/~
            coname$60,                   /* Company name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* Summary File Descriptions  */~
            edvalue$(1)20,               /* Edited column value        */~
            err$(15)79,                  /* Report error messages      */~
            errormsg$79,                 /* Error message              */~
            f$1,                         /* Field code work area       */~
            fc$3,                        /* Column header              */~
            fc1$(7)1,                    /* Field or Column 1          */~
            fc2$(7)1,                    /* Field or Column 2          */~
            ffmt$3,                      /* Field format (validation)  */~
            ffmt$(7)3,                   /* Field Format               */~
            fr1$25,                      /* Code 1 From value          */~
            fr2$25,                      /* Code 2 From value          */~
            grcd1$(2)25,                 /* 1st Grp Codes- SYSFILE2    */~
            grcd2$(2)25,                 /* 2nd Grp Codes- SYSFILE2    */~
            group1$14, group2$14,        /* Group Code Descriptors     */~
            grp1_desc$2,                 /* Group Code 1 Desc length   */~
            grp2_desc$2,                 /* Group Code 2 Desc length   */~
            i$(24)80,                    /* Screen Image               */~
            incl$1,                      /* Include groups outside yrs */~
            inpmessage$79,               /* Informational Message      */~
            key$30,                      /* Lookup key                 */~
            line1$79,                    /* First Line of Screen Header*/~
            line2$79,                    /* Second Line of Screen Headr*/~
            line16$79,                   /* Screen line 16             */~
            line17$79,                   /* Screen line 17             */~
            line18$79,                   /* Screen line 18             */~
            line19$79,                   /* Screen line 19             */~
            line20$79,                   /* Screen line 20             */~
            lname$(15)21,                /* Field/column names (long)  */~
            lv1tot(7, 3),                /* Level 1 report totals      */~
            max_print$3,                 /* Max to print               */~
            mega$(63)104,                /* 7 Years / 9 Arrays / 13*FP */~
            msg$79,                      /* Message                    */~
            op$4,                        /* Column header              */~
            oper$(7)1,                   /* Operation                  */~
            page_brk$1,                  /* Page break switch          */~
            periods$(13)6,               /* Period Start Dates by Year */~
            pf16$17, pf16fac$1,          /* PF(16) message & FAC       */~
            pline3$132,                  /* Print header 3             */~
            pline4$132,                  /* Print header 4             */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            posn_rem$4,                  /* Edited positions remaining */~
            prnames$(10)8,               /* Summary File PR Names      */~
            printline$132,               /* Formatted print line       */~
            prtval(7, 3),                /* Detail printed from here   */~
            rank_colm$1,                 /* Rank on Column             */~
            readkey1$99,                 /* Miscellaneous Read/Plow Key*/~
            readkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            rept$1,                      /* Code for current report    */~
            rept_date$8,                 /* Edited report date         */~
            rept_desc$30,                /* Report Description         */~
            rept_name$10,                /* Report Name                */~
            rept_type$1,                 /* Report Type                */~
            rptid$6,                     /* Report ID                  */~
            rpttot(7, 3),                /* Report totals              */~
            s_d$1,                       /* Currently printing Sum/Detl*/~
            seq$7,                       /* Edited sequence number     */~
            sname$(15)10,                /* Field/column names (short) */~
            sortkey$8,                   /* Sort field #1 -- WORKFIL1  */~
            sortsav$8,                   /* Sort field work area       */~
            spacer$26,                   /* Spaces                     */~
            sub_hdr$60,                  /* 'History' or 'Ranking'     */~
            summ_desc$60,                /* Summary file header descr  */~
            summ_detl$1,                 /* Summary or Detail          */~
            summ_file$1,                 /* Summary File #             */~
            summ_save$1,                 /* Last Summary File #        */~
            time$8,                      /* Time of day                */~
            to1$25,                      /* Code 1 to value            */~
            to2$25,                      /* Code 2 to value            */~
            tt$1,                        /* Task Type from EXTRACT     */~
            userid$3,                    /* Current User Id            */~
            values(13),                  /* 1 field / 1 year/ 13 values*/~
            wfil$8,                      /* WORKFIL1 name              */~
            wlib$8,                      /* WORKFIL1 library           */~
            wvol$6,                      /* WORKFIL1 volume            */~
            y$2,                         /* Year work area             */~
            years$(99,13)6,              /* All allowable report dates */~
            yr$2,                        /* Column header              */~
            yr1$(7)2,                    /* Data Year 1                */~
            yr2$(7)2,                    /* Data Year 2                */~
            yt$1,                        /* YTD work area              */~
            ytd$4,                       /* Column header              */~
            ytd1$(7)1,                   /* Year to Date 1             */~
            ytd2$(7)1                    /* Year to Date 2             */

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
            * #1  ! SASUMRY# ! Sales Anaylsis Summary File              *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! CATEGORY ! INVENTORY CATEGORY CODES FILE            *~
            * #6  ! GENCODES ! System General Codes file.               *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! STORNAME ! STORE INFORMATION FILE                   *~
            * #9  ! SAREPORT ! Sales Analysis Report file               *~
            * #10 ! WORKFIL1 ! Temporary System Workfile                *~
            * #11 ! WORKFIL2 ! Valid report date work area              *~
            * #12 ! WORKFIL3 ! No dup list of code1/code2 elements      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SASUMRY#",                                      ~
                        varc,     indexed,  recsize =  1048,             ~
                        keypos = 1,    keylen = 56,                      ~
                alt key 1, keypos =  993,    keylen = 56,                ~
                    key 2, keypos = 1024,    keylen = 25, dup

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #5,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #6,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #7,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #8,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #9,  "SAREPORT", varc,    indexed,   recsize =  463,  ~
                          keypos = 1, keylen = 7

            select #10, "WORKFIL1", consec, recsize = 233

            select #11, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize =  12,               ~
                        keypos =    1, keylen =  12

            select #12, "WORKFIL3",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos =    1, keylen =  50


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, rslt$(9 ))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr ("TT", tt$, "ID", userid$)
            call "SHOSTAT" ("Initializing ...")
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

*        Load S/A Summary File Descriptions
            plowkey$ = "SA.FILES.SASUMRY"
            str(plowkey$, len(plowkey$)+1) = all(hex(00))
L08120:     call "PLOWNEXT" (#2, plowkey$, 16%, f1%(2))
            if f1%(2) = 1% then goto L08200
                if file% <> 0% then goto L08260
                if tt$ = "F" then                                        ~
                     call "ASKUSER" (0%, "*** NO S/A FILES ***",         ~
                                "There are no S/A Summary Files Defined",~
                                " ", "Press RETURN to exit program...")
                goto exit_program
L08200:     convert str(plowkey$,17,1) to file% : file% = file% + 1%
            get #2 using L08230, prnames$(file%), descrs$(file%),          ~
                               codes%(file%, 1), codes%(file%, 2)
L08230:         FMT XX(9), CH(8), XX(3), CH(30), XX(2), 2*BI(1)
            goto L08120

L08260: REM Build a WORKFIL2 of the valid report dates
            call "WORKOPEN" (#11, "IO   ", 100%, f2%(11))
            if f2%(11) = 1% then goto exit_program
            readkey$ = "SWITCHS.SA"
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) <> 0% then goto L08380
            if tt$ = "F" then                                            ~
                call "ASKUSER" (0%, "*** NO SWITCHES RECORD ***",        ~
                     "The S/A Switches record does not exist", " ",      ~
                     "Press (RETURN) to exit program")
                goto exit_program

L08380:     get #2 using L08390, periods$() : gosub workfil2_outputter
L08390:         FMT POS(74), 13*CH(6)
            get #2 using L08410, periods$() : gosub workfil2_outputter
L08410:         FMT POS(152), 13*CH(6)
            get #2 using L08430, periods$() : gosub workfil2_outputter
L08430:         FMT POS(230), 13*CH(6)

            plowkey$ = "SA.YEARS.USED." & hex(00)
L08460:     call "PLOWNEXT" (#2, plowkey$, 14%, f1%(2))
            if f1%(2) = 0% then goto L08520
            get #2 using L08490, periods$() : gosub workfil2_outputter
L08490:         FMT POS(29), 13*CH(6)
            goto L08460

L08520: REM Read the WORKFIL2 back in and set up date tables
            init (" ") years$(), periods$() : plowkey$ = all(hex(00))
            y%, z% = 0%
L08550:     call "PLOWNEXT" (#11, plowkey$, 0%, f1%(11))
            if f1%(11) = 0% then goto L08670
            get #11 using L08580, periods$(1), periods$(2)
L08580:         FMT  CH(6), CH(6)
            if periods$(1) = periods$(3) then goto L08630
                periods$(3) = periods$(1)
                y% = y% + 1% : z% = 0%
                if y% > 99% then goto L08670
L08630:     z% = z% + 1%
            years$(y%, z%) = periods$(2)
            goto L08550

L08670: REM Set up static tables
            codes$(1,1) = "  Part Number:" : codes$(1,2) = "Part Number"
            codes$(2,1) = "Part Category:" : codes$(2,2) = "Part Category"
            codes$(3,1) = "      Account:" : codes$(3,2) = "Account"
            codes$(4,1) = "      Ship-to:" : codes$(4,2) = "Ship-to"
            codes$(5,1) = "Customer Type:" : codes$(5,2) = "Customer Type"
            codes$(6,1) = "        Store:" : codes$(6,2) = "Store"
            codes$(7,1) = " Sales Region:" : codes$(7,2) = "Sales Region"
            codes$(8,1) = "     Salesman:" : codes$(8,2) = "Salesman"

            codefile%(1) = 4%   :   codelen%(1) = 25%
            codefile%(2) = 5%   :   codelen%(2) =  4%
            codefile%(3) = 3%   :   codelen%(3) =  9%
            codefile%(4) = 3%   :   codelen%(4) =  9%
            codefile%(5) = 6%   :   codelen%(5) =  2%
            codefile%(6) = 8%   :   codelen%(6) =  3%
            codefile%(7) = 6%   :   codelen%(7) =  4%
            codefile%(8) = 7%   :   codelen%(8) =  4%

            call "COMPNAME" (12%, coname$, u3%)
            date$ = date : call "DATEFMT" (date$)
            col$(1) = "A" : col$(2) = "B" : col$(3) = "C" : col$(4) = "D"
            col$(5) = "E" : col$(6) = "F" : col$(7) = "G"
            sname$( 1) = "A.Bkng Qty" : sname$( 2) = "A.Bkng Val"
            sname$( 3) = "A.Ship Qty" : sname$( 4) = "A.Ship Val"
            sname$( 5) = "A.Ship Cst" : sname$( 6) = "T.Bkng Qty"
            sname$( 7) = "T.Bkng Val" : sname$( 8) = "T.Ship Qty"
            sname$( 9) = "T.Ship Val" : sname$(10) = "Column A  "
            sname$(11) = "Column B  " : sname$(12) = "Column C  "
            sname$(13) = "Column D  " : sname$(14) = "Column E  "
            sname$(15) = "Column F  "
            lname$( 1) = "Actual Booking Units "
            lname$( 2) = "Actual Booking Value "
            lname$( 3) = "Actual Shipping Units"
            lname$( 4) = "Actual Shipping Value"
            lname$( 5) = "Actual Shipping Cost "
            lname$( 6) = "Target Booking Units "
            lname$( 7) = "Target Booking Value "
            lname$( 8) = "Target Shipping Units"
            lname$( 9) = "Target Shipping Value"
            lname$(10) = "Column A             "
            lname$(11) = "Column B             "
            lname$(12) = "Column C             "
            lname$(13) = "Column D             "
            lname$(14) = "Column E             "
            lname$(15) = "Column F             "
            date$ = date : call "DATEFMT" (date$)
            col$ = "Col" : yr$ = "Yr" : ytd$ = "YTD?" : fc$ = "F/C"
            op$ = "Oper" : fmt$ = "Fmt" : coldesc$ = "Column Description"
            line1$ = "Sales Analysis: Print Reports"
            str(line1$,65%) = "Today: " & date$
            str(line2$,63%) = "SAPRINT: " & str(cms2v$,,8%)
            line16$ = "Years          ! Operators ! Field Codes"
            line17$ = "-n:n years ago ! +:Total   ! 1:Actl Bkng Qty  4:Ac~
        ~tl Ship Val  7:Trgt Bkng Val"
            line18$ = " 0:Current     ! -:Diff    ! 2:Actl Bkng Val  5:Ac~
        ~tl Ship Cst  8:Trgt Ship Qty"
            line19$ = "+n:Future yrs  ! @:Average ! 3:Actl Ship Qty  6:Tr~
        ~gt Bkng Qty  9:Trgt Ship Val"
            line20$ = " (n = 1-3)     ! %:Percent !     or, you may use C~
        ~olumn Designators A-F"
            rptid$ = "SAR001"
            col_head$(1) = "PART NUMBER":col_desc$(1) = "PART DESCRIPTION"
            col_head$(2) = "CATY"       :col_desc$(2) = "CATEGORY NAME"
            col_head$(3) = "ACCOUNT"    :col_desc$(3) = "ACCOUNT NAME"
            col_head$(4) = "CUSTOMER"   :col_desc$(4) = "CUSTOMER NAME"
            col_head$(5) = "TY"         :col_desc$(5) = "CUSTOMER TYPE"
            col_head$(6) = "STR"        :col_desc$(6) = "STORE NAME"
            col_head$(7) = "REGN"       :col_desc$(7) = "SALES REGION"
            col_head$(8) = "SMAN"       :col_desc$(8) = "SALESMAN NAME"
            max_lines% = 56%
            init (hex(84)) pf16fac$
            if tt$ = "B" then goto report_print_loop

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            summ_save$ = " "
            errormsg$ = " " : bg_yorn$ = "Y" : pf16$ = "(16)Exit Program"
            inpmessage$ = "Enter 'Y' to print in Background and return "&~
                "to menu; 'N' to print in Foreground."

        bg_fg_capture_input
            gosub'101
            if errormsg$ <> " " then goto bg_fg_capture_input
            if keyhit%  =  1% then gosub startover
            if keyhit%  = 16% then goto exit_program
            if keyhit% <>  0% then goto bg_fg_capture_input

        REM *************************************************************~
            *         E D I T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal edit for data entry screens.               *~
            *************************************************************

            errormsg$ = " " : pf16$ = "(16)Print Reports"
            inpmessage$ = "To Modify displayed value, Position Cursor" & ~
                " and press (RETURN)"

        bg_fg_capture_edit
            gosub'101
            if errormsg$ <> " " then goto bg_fg_capture_edit
            if keyhit%  =  1% then gosub startover
            if keyhit%  = 16% then goto bg_fg_test
            goto bg_fg_capture_edit

        REM *************************************************************~
            *          P R O G R A M   M A J O R   L O O P              *~
            *-----------------------------------------------------------*~
            * Reads SAREPORT, interprets the records therein (which are *~
            * S/A report format definitions selected in SARPTSEL), com- *~
            * putes the required values and prints the reports.         *~
            *************************************************************

        bg_fg_test
            if bg_yorn$ = "N" then goto report_print_loop
                close ws
                call "TASKUP" ("ME", 0%)
                goto exit_program

        report_print_loop
            plowkey$ = xor plowkey$
            str(plowkey$,1,3) = userid$
        read_sa_report_file
            call "PLOWNEXT" (#9, plowkey$, 3%, f1%(9))
            if f1%(9) = 0% then goto end_of_job
            gosub get_report_def_data
            if rept$ <> "H" then goto report_read_loop
                gosub form_column_headers
                gosub print_params_page_0
                if abort% = 0% then goto report_read_loop
                    gosub print_end_of_report
                    goto read_sa_report_file
        report_read_loop
            gosub read_report_records
            if eorsw% = 0% then goto L11340
                gosub print_history_report
                gosub print_end_of_report
                goto read_sa_report_file
L11340:     gosub column_computations
            gosub print_history_report
            goto report_read_loop

        end_of_job
        REM Delete all of this user's selected reports from SAREPORT
            plowkey$ = xor plowkey$
            str(plowkey$,1,3) = userid$
            call "DELETE" (#9, plowkey$, 3%)
            goto exit_program

        REM *************************************************************~
            * Retrieve data from input report format definition record. *~
            * Validate/set reasonable default values.                   *~
            *************************************************************

        get_report_def_data
            ranksw%, eorsw%, onetime%, err%, abort% = 0%
            mat colerr% = zer : mat lv1tot = zer : mat rpttot = zer
            init (" ") summ_desc$, group1$, group2$, err$(), code1desc$, ~
                code2desc$
            init (hex(00)) code1save$, code2save$

        REM Access the report definition record just read from SAREPORT
            get #9 using L60070, userid$, seq%, rept_name$, rept_desc$,   ~
                asof_date$, summ_file$, grcd1$(), grcd2$(), grp1_desc$,  ~
                grp2_desc$, rept_type$, summ_detl$, rank_colm$,          ~
                asnd_dsnd$, max_print$, page_brk$, yr1$(), ytd1$(),      ~
                fc1$(), oper$(), yr2$(), ytd2$(), fc2$(), ffmt$(),       ~
                colahdr$(), colbhdr$(), colchdr$(), coldhdr$(),          ~
                colehdr$(), colfhdr$(), colghdr$(), incl$

        REM Issue a status message to the operator
            convert seq% to seq$, pic (###,###)
            call "STRING" addr ("LJ", seq$, 7%)
            msg$ = "S/A Report Print now printing report '" & rept_name$&~
                "' (Seq. number " & seq$ & ")"
            if tt$ = "B" then goto L12300
                call "SHOSTAT" (msg$)
                goto summary_file_validate
L12300:     call "MESSAGE" addr ("XM", str(userid$)&hex(20), "rpt"&msg$, ~
                78%, u3%)

        summary_file_validate
            gosub select_summary_file
            if f2%(1) = 0% then goto L12410
                err% = err% + 1%
                err$(err%) = "*** ERROR: INVALID SUMMARY FILE SELECTED."&~
                     "  REPORT ABORTED."
                abort% = 1%

L12410: REM Establish defaults in the event of invalid data
            if f2%(1) = 0% and group2% = 0% then summ_detl$ = "S"
            rept_date$ = asof_date$
            call "DATEFMT" (rept_date$)
            grp1_desc% = 0%
            convert grp1_desc$ to grp1_desc%, data goto L12460
L12460:     grp2_desc% = 0%
            convert grp2_desc$ to grp2_desc%, data goto L12480
L12480:     if rept_type$<>"R" and rept_type$<>"B" then rept_type$ = "H"
            if rept_type$ = "R" then goto L12510
                if summ_detl$ <> "D" then summ_detl$ = "S"
L12510:     if rept_type$ = "H" then goto L12580
                if pos("ABCDEFG"=rank_colm$) = 0% then rank_colm$ = "A"
                if asnd_dsnd$ <> "D" then asnd_dsnd$ = "A"
                max_print% = 999999999
                if max_print$ = "ALL" then goto L12580
                     convert max_print$ to max_print%, data goto L12580

L12580: REM Set the codes that describe the report currently being printed
            rept$ = "H" : s_d$ = summ_detl$
            if rept_type$ = "R" then rept$ = "R"
            if page_brk$ <> "Y" then page_brk$ = "N"

        REM Validate for sufficient columns to print a report line
            gosub compute_posn_rem
            if posn_rem% >= 0% then goto L12710
                err% = err% + 1%
                err$(err%) = "*** ERROR: # OF PRINT COLUMNS DEFINED EXC"&~
                     "EEDS THE 132 COLUMNS AVAILABLE.  REPORT ABORTED."
                abort% = 1%

L12710: REM Validate the passed-in ranges of codes to print
            errormsg$ = " "
            call "TESTRNGE" (grcd1$(1), grcd1$(2), fr1$, to1$, errormsg$)
            if errormsg$ = " " then goto L12800
                err% = err% + 1%
                msg$ = group1$ & " " & errormsg$
                call "STRING" addr ("LJ", msg$, 79%)
                err$(err%) = "*** ERROR: " & msg$ & "  REPORT ABORTED."
                abort% = 1%
L12800:     errormsg$ = " "
            call "TESTRNGE" (grcd2$(1), grcd2$(2), fr2$, to2$, errormsg$)
            if errormsg$ = " " then goto L12890
                err% = err% + 1%
                msg$ = group2$ & " " & errormsg$
                call "STRING" addr ("LJ", msg$, 79%)
                err$(err%) = "*** ERROR: " & msg$ & "  REPORT ABORTED."
                abort% = 1%

L12890: REM Validate the passed Report Date. Save Year & Period pointers.
            for y% = 1% to 99%
                for p% = 1% to 13%
                     if years$(y%, p%) = " " or ~
                        years$(y%, p%) = blankdate$ then goto L12950
                     if years$(y%, p%) = asof_date$ then goto L13010
                next p%
L12950:     next y%
            err% = err% + 1%
            err$(err%) = "*** ERROR: 'AS-OF' DATE " & rept_date$ &       ~
                " IS INVALID.  REPORT ABORTED."
            abort% = 1%

L13010: REM Format READKEY1$ for initial use
            readkey1$ = str(fr1$,,25) & str(fr2$,,25)

            gosub build_workfil3_of_existing_summary_codes

        REM Issue any warning messages relating to the data columns
            for c% = 1% to 7%
                errormsg$ = " "
                gosub validate_a_column
                if errormsg$ = " " then goto L13110
                     err% = err% + 1%
                     err$(err%) = errormsg$
L13110:     next c%
            return

        build_workfil3_of_existing_summary_codes
            call "FILEBGON" (#12)  /* Kill it before we start */
            call "WORKOPEN" (#12, "IO   ", nbr_rec%, f2%(12))
            if f2%(12) <> 1% then L13280
                call "ASKUSER" (0%, "*** WORKFIL3 PROBLEM ***",          ~
                               "Couldn't open workfile #12", " ",        ~
                               "Press (RETURN) to exit program")
                goto exit_program

L13280:     break% = 6%   /* Start date of SA year */
            pass%  = 1%
            if incl$ <> "Y" then L13350
                break% = 0%  /* Read them all */
                pass%  = 0%
                plowkey2$ = all(hex(00))
                goto codes_plow_loop
L13350:     for ci% = 1% to 7%
                if pass% > 1% then L13400
                     if yr1$(ci%) = " " then next_ci
                          convert yr1$(ci%) to cyr%, data goto L13390
L13390:                   goto L13420
L13400:              if yr2$(ci%) = " " then next_ci
                          convert yr2$(ci%) to cyr%, data goto L13420
L13420:         if y% + cyr% < 1% or y% + cyr% > 99% then next_ci
                     if years$(y% + cyr%, 1) = " " or ~
                        years$(y% + cyr%, 1) = blankdate$ then next_ci
                plowkey2$ = all(hex(00))
                str(plowkey2$,,6) = years$(y% + cyr%, 1)
              codes_plow_loop
                call "PLOWNEXT" (#1, plowkey2$, break%, f1%(1))
                if f1%(1) = 0% then next_ci
                     get #1 using L13490, code1$, code2$
L13490:                   FMT POS(7), CH(25), CH(25)
                     write #12 using L13520, code1$, code2$,              ~
                                                 eod goto codes_plow_loop
L13520:                   FMT CH(25), CH(25)
                     goto codes_plow_loop
              next_ci
                if pass% = 0% then return
                next ci%
            pass% = pass% + 1%
            if pass% > 2% then return
            goto L13350

        REM *************************************************************~
            * Report column headings are formatted.                     *~
            *************************************************************

        form_column_headers
            init (" ") columns$()
            l% = 1%
            if s_d$ <> "D" then goto format_summary_headers
                columns$(3) = col_head$(group2%)
                w% = codelen%(group2%)
                str(columns$(4), l%, w%) = all("-")
                l% = l% + w% + 1%
                if grp2_desc% = 0% then goto format_7_headers
                     str(columns$(3),l%,grp2_desc%) = col_desc$(group2%)
                     str(columns$(4),l%,grp2_desc%) = all("-")
                     l% = l% + grp2_desc% + 1%
                     goto format_7_headers
        format_summary_headers
            if rept$ <> "R" then goto history_columns
                columns$(3) = " RANK"
                columns$(4) = "-----"
                l% = 7%
        history_columns
            w% = codelen%(group1%)
            str(columns$(3), l%, w%) = col_head$(group1%)
            str(columns$(4), l%, w%) = all("-")
            l% = l% + w% + 1%
            if grp1_desc% = 0% then goto format_7_headers
                str(columns$(3),l%,grp1_desc%) = col_desc$(group1%)
                str(columns$(4),l%,grp1_desc%) = all("-")
                l% = l% + grp1_desc% + 1%
        format_7_headers
            mat colxhdr$ = colahdr$ : n% = 1% : gosub column_justifier
            mat colxhdr$ = colbhdr$ : n% = 2% : gosub column_justifier
            mat colxhdr$ = colchdr$ : n% = 3% : gosub column_justifier
            mat colxhdr$ = coldhdr$ : n% = 4% : gosub column_justifier
            mat colxhdr$ = colehdr$ : n% = 5% : gosub column_justifier
            mat colxhdr$ = colfhdr$ : n% = 6% : gosub column_justifier
            mat colxhdr$ = colghdr$ : n% = 7% : gosub column_justifier
            return

        column_justifier
        REM Column headings are smashed to the bottom-right
            if colerr%(n%) <> 0% then return
            fdig%, fdec% = 0%
            convert str(ffmt$(n%),1,2) to fdig%, data goto L14470
L14470:     convert str(ffmt$(n%),3,1) to fdec%, data goto L14480
L14480:     w% = fdig% + 1%
            if fdec% <> 0% then w% = w% + fdec% + 1%
            w% = max(11%, w%)
            mat redim colyhdr$(3)w% : init (" ") colyhdr$()
            colyhdr$(1) = colxhdr$(1)
            colyhdr$(2) = colxhdr$(2)
            colyhdr$(3) = colxhdr$(3)
            if colyhdr$(3) <> " " then goto L14600
                if colyhdr$(2) = " " then goto L14590
                     colyhdr$(3) = colyhdr$(2) : colyhdr$(2) = " "
                     goto L14600
L14590:     colyhdr$(3) = colyhdr$(1) : colyhdr$(1) = " "
L14600:     if colyhdr$(2) <> " " then goto L14620
                colyhdr$(2) = colyhdr$(1) : colyhdr$(1) = " "
L14620:     call "STRING" addr ("RJ", colyhdr$(1), w%)
            call "STRING" addr ("RJ", colyhdr$(2), w%)
            call "STRING" addr ("RJ", colyhdr$(3), w%)
            str(columns$(1), l%, w%) = colyhdr$(1)
            str(columns$(2), l%, w%) = colyhdr$(2)
            str(columns$(3), l%, w%) = colyhdr$(3)
            str(columns$(4), l%+1%, w%-1%) = all("-")
            str(columns$(5), l%+1%, w%-1%) = all("-")
            l% = l% + w%
            return

        REM *************************************************************~
            * Report Header lines are formatted.                        *~
            * Page 0 (zero) is printed in the Report Definition format  *~
            * for operator verification.                                *~
            *************************************************************

        print_params_page_0
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)
            pline3$ = rept_name$ & ": " & rept_desc$ & ", " & summ_desc$
            call "STRING" addr ("CT", pline3$, 132%)
            pline4$ = " " : sub_hdr$ = "SALES ANALYSIS"
            if rept$ = "H" then goto L15250
                str(sub_hdr$, len(sub_hdr$) + 2%) = "RANKING"
                pline4$ = "RANKED"
                if asnd_dsnd$ = "A"                                      ~
                     then str(pline4$, len(pline4$) + 2%) = "ASCENDING"  ~
                     else str(pline4$, len(pline4$) + 2%) = "DESCENDING"
                str(pline4$, len(pline4$) + 2%) = "ON COLUMN " &         ~
                     rank_colm$ & "; " & "PRINTING " & max_print$ &      ~
                     " ITEMS"
                call "STRING" addr ("CT", pline4$, 132%)
                goto L15260
L15250:     str(sub_hdr$, len(sub_hdr$) + 2%) = "HISTORY"
L15260:     if s_d$ = "D"                                                ~
                then str(sub_hdr$, len(sub_hdr$) + 2%) = "DETAIL"        ~
                else str(sub_hdr$, len(sub_hdr$) + 2%) = "SUMMARY"
            call "FMTTITLE" (sub_hdr$, " ", 12%)
            page_nbr% = 0% : nbr_lines% = 999% : pagesw% = 1%
            gosub page_0_heading
            pagesw% = 0%
            print skip (5)
            print using L61100, str(spacer$,,len(str(spacer$))) & line1$
            print using L61100, str(spacer$,,len(str(spacer$))) & line2$
            print
            print using L61100, str(spacer$,,len(str(spacer$))) &         ~
                "  REPORT NAME: " & str(rept_name$,,10) & "   " &        ~
                str(rept_desc$,,30) & "   " & "SUMMARY FILE: " &         ~
                summ_file$
            print using L61100, str(spacer$,,len(str(spacer$))) &         ~
                str(group1$,,14) & " " & str(grcd1$(1),,25) & " - " &    ~
                str(grcd1$(2),,25) & " DESC: " & grp1_desc$
            print using L61100, str(spacer$,,len(str(spacer$))) &         ~
                str(group2$,,14) & " " & str(grcd2$(1),,25) & " - " &    ~
                str(grcd2$(2),,25) & " DESC: " & grp2_desc$
            print using L61100, str(spacer$,,len(str(spacer$))) &         ~
                "TYPE: " & rept_type$ & "  S/D: " & summ_detl$ &         ~
                "  RANK ON: " & rank_colm$ & "  A/D: " & asnd_dsnd$ &    ~
                "  # TO PRINT: " & str(max_print$,,3) & "  BRK? " &      ~
                page_brk$ & "  INC? " & incl$ & "  POS: " &              ~
                str(posn_rem$,,4)
            print
            msg$ = " "
            str(msg$, 1, 3) = col$ : str(msg$, 5, 3) = fc$
            str(msg$, 9, 2) = yr$  : str(msg$,12, 4) = ytd$
            str(msg$,17, 4) = op$  : str(msg$,22, 3) = fc$
            str(msg$,26, 2) = yr$  : str(msg$,29, 4) = ytd$
            str(msg$,34, 3) = fmt$ : str(msg$,38,18) = coldesc$
            print using L61100, str(spacer$,,len(str(spacer$))) & msg$
            for n% = 1% to 7%
                msg$ = " "
                str(msg$, 2, 1) = col$(n%) : str(msg$, 6, 1) = fc1$(n%)
                str(msg$, 9, 2) = yr1$(n%) : str(msg$,13, 1) = ytd1$(n%)
                str(msg$,18, 1) = oper$(n%): str(msg$,23, 1) = fc2$(n%)
                str(msg$,26, 2) = yr2$(n%) : str(msg$,30, 1) = ytd2$(n%)
                str(msg$,34, 3) = ffmt$(n%): str(msg$,38,41) = cdescr$(n%)
                print using L61100, str(spacer$,,len(str(spacer$))) & msg$
            next n%
            print
            print using L61100, str(spacer$,,len(str(spacer$))) & line16$
            print using L61100, str(spacer$,,len(str(spacer$))) & line17$
            print using L61100, str(spacer$,,len(str(spacer$))) & line18$
            print using L61100, str(spacer$,,len(str(spacer$))) & line19$
            print using L61100, str(spacer$,,len(str(spacer$))) & line20$
            print
            print using L61100, str(spacer$,,len(str(spacer$))) &         ~
                "   - A -      - B -      - C -      - D -      - E -" & ~
                "      - F -      - G -"
            for n% = 1% to 3%
            print using L61100, str(spacer$,,len(str(spacer$))) & "  " &  ~
                str(colahdr$(n%),,10)& " " & str(colbhdr$(n%),,10)& " " &~
                str(colchdr$(n%),,10)& " " & str(coldhdr$(n%),,10)& " " &~
                str(colehdr$(n%),,10)& " " & str(colfhdr$(n%),,10)& " " &~
                str(colghdr$(n%),,10)
            next n%
            print
            if err% < 1% then return
                for n% = 1% to err%
                     print using L61100, str(spacer$,,len(str(spacer$)))  ~
                          & err$(n%)
                next n%
            return

        REM *************************************************************~
            * Reads the Summary file records required for this report.  *~
            *************************************************************

        read_report_records
            call "PLOWNEXT" (#12, readkey1$, 0%, f1%(12))
            if f1%(12) = 0% then goto end_of_records
            if str(readkey1$, 1,25) < fr1$ then goto read_report_records
            if str(readkey1$, 1,25) > to1$ then goto end_of_records
            if group2% = 0% then goto L16140
            if str(readkey1$,26,25) < fr2$ then goto read_report_records
            if str(readkey1$,26,25) > to2$ then goto read_report_records

L16140: REM OK, we have the first/next record to be considered.  Now we
        REM want to use the record just read and it's related (yearly)
        REM records.
            get #12 using L16180, code1$, code2$
L16180:         FMT CH(25), CH(25)

        REM Clear out the values-accumulation area.
            mat values = zer
            for c% = 1% to 7%
                v% = ((c% - 1%) * 936%) + 1%
                put str(mega$(), v%, 936%) using L16270,                  ~
                     values(), values(), values(), values(), values(),   ~
                     values(), values(), values(), values()
L16270:              FMT  13*PD(14, 4), 13*PD(14, 4), 13*PD(14, 4),      ~
                          13*PD(14, 4), 13*PD(14, 4), 13*PD(14, 4),      ~
                          13*PD(14, 4), 13*PD(14, 4), 13*PD(14, 4)
            next c%

        REM Get associated records. Put current year in center of MEGA$()
            for n% = -3% to 3%
                if n% + y% < 1% or n% + y% > 99% then goto L16450
                v% = ((n% + 3%) * 936%) + 1%
                readkey2$ = str(years$(n%+y%,1),,6) & code1$
                str(readkey2$,32,25) = code2$  /* Detail- READ */
                call "READ100" (#1, readkey2$, f1%(1))
                if f1%(1) = 0% then goto L16450
                     get #1 using L16440, str(mega$(), v%, 936%)
L16440:                   FMT POS(57), CH(936)
L16450:     next n%
            return

        end_of_records
            eorsw% = 1%
            init (hex(ff)) code1$, code2$
            return

        REM *************************************************************~
            * End of a report.  Print final History totals (if required)*~
            * and sort, read, and print the Ranking report (if required)*~
            *************************************************************

        print_end_of_report
            if rept$ <> "H" then goto L17100
                l% = 1%
                gosub end_of_report
L17100:     close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if rept_type$ = "H" then return
            rept$ = "R" : s_d$ = "S"
            gosub form_column_headers
            gosub print_params_page_0
            if ranksw% = 0% then goto end_of_rank_report_2
            if abort% <> 0% then goto end_of_rank_report_2
            call "SLCTSORT" (#10, len(str(sortkey$)) + len(str(code1$)))
            rank%, count% = 0% : sortsav$ = all(hex(00))
        read_sorted_workfil1
            read #10 using L60400, sortkey$, code1save$, code1desc$,      ~
                lv1tot(), eod goto end_of_rank_report
            if nbr_lines% > max_lines% then gosub page_heading
            if sortkey$ = sortsav$ then goto L17270
                rank% = rank% + 1%
                sortsav$ = sortkey$
L17270:     convert rank% to printline$, pic (#####)
            l% = 7%
            gosub level_1_break
            if max_print$ = "ALL" then goto read_sorted_workfil1
            count% = count% + 1%
            if count% < max_print% then goto read_sorted_workfil1

        end_of_rank_report
            close #10 : f2%(10) = 1%
            call "SCRATCH" addr ("F", wfil$, wlib$, wvol$, "B", " ", u3%)
        end_of_rank_report_2
            if rept$ <> "R" then goto L17410
                l% = 7%
                gosub end_of_report
L17410:     close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            return

        REM *************************************************************~
            * This is the routine that actually computes the value that *~
            * will be printed in each column.                           *~
            *************************************************************

        column_computations
            mat coltot = zer
            for c% = 1% to 7%
                if colerr%(c%) <> 0% then goto L18230
                y$ = yr1$(c%) : yt$ = ytd1$(c%) : f$ = fc1$(c%) : r% = 1%
                gosub compute_coltot
                y$ = yr2$(c%) : yt$ = ytd2$(c%) : f$ = fc2$(c%) : r% = 2%
                gosub compute_coltot
                on pos("+-@% " = oper$(c%)) goto L18160, L18180, L18200,    ~
                     L18230, L18220
L18160:         coltot(c%, 3%) = coltot(c%, 1%) + coltot(c%, 2%)
                goto L18230
L18180:         coltot(c%, 3%) = coltot(c%, 1%) - coltot(c%, 2%)
                goto L18230
L18200:         if p% <> 0% then coltot(c%, 3%) = coltot(c%, 1%) / p%
                goto L18230
L18220:         coltot(c%, 3%) = coltot(c%, 1%)
L18230:     next c%
            return

        compute_coltot
            if f$ = " " then return
            if pos("123456789ABCDEF" = f$) < 10% then goto L18310
                coltot(c%, r%) = coltot(pos("ABCDEF" = f$), 3%)
                return
L18310:     convert y$ to yr%, data goto L18320
L18320:     convert f$ to f%, data goto L18330
L18330:     v% = ((yr% + 3%) * 936%) + ((f% - 1%) * 104%) + 1%
            get str(mega$(), v%, 104%) using L18350, values()
L18350:         FMT 13*PD(14, 4)
            z% = p%
            if yt$ = "Y" then z% = 1%
            for m% = z% to p%
                coltot(c%, r%) = coltot(c%, r%) + values(m%)
            next m%
            return

        REM *************************************************************~
            * The History report is formatted, printed and totaled.     *~
            *************************************************************

        print_history_report
            if onetime% = 0% then gosub get_code_descriptions
                if code1$ = code1save$ then goto L19240
                     if rept_type$ = "R" then goto L19230
                          l% = 1%
                          gosub level_1_break
                          gosub write_ranking_report
                          mat lv1tot = zer
                          gosub get_code_descriptions
                          if eorsw% = 1% or s_d$ = "S" then goto L19250
                               if page_brk$ = "N" then goto L19170
                                         nbr_lines%=999% : goto L19250
L19170:                        print
                          print using L61100, "   " & group1$ & " " &     ~
                               code1$ & "   " & code1desc$
                          print
                          nbr_lines% = nbr_lines% + 3%
                          goto L19250
L19230:              gosub write_ranking_report
                     mat lv1tot = zer
L19240:         gosub get_code_descriptions
L19250:     if eorsw% = 1% then return
            mat prtval = coltot
            if rept$ = "H" and s_d$ = "D" then gosub print_line_formatter
            mat lv1tot = lv1tot + coltot : mat coltot = zer
            return

        REM *************************************************************~
            * Level 1 (Code 1) break. Sub-totals & writing to Ranking   *~
            * work file may be necessary.                               *~
            *************************************************************

        level_1_break
            mat prtval = lv1tot
            if s_d$ = "S" then goto level_1_summary
                l% = l% + codelen%(group2%) + 1%
                if grp2_desc% <> 0% then l% = l% + grp2_desc% + 1%
                str(printline$, l% - 13%, 12%) = "* SUB TOTAL:"
                print using L61100, columns$(5)
                nbr_lines% = nbr_lines% + 1%
                goto level_1_format
        level_1_summary
            w% = codelen%(group1%)
            str(printline$, l%, w%) = code1save$
            l% = l% + w% + 1%
            if grp1_desc% = 0% then goto level_1_format
                str(printline$, l%, grp1_desc%) = code1desc$
                l% = l% + grp1_desc% + 1%
        level_1_format
            gosub format_7_columns
            mat rpttot = rpttot + lv1tot
            return

        REM *************************************************************~
            * The Ranking report values are written to disk for later   *~
            * sorting, formatting, printing, and totaling.              *~
            *************************************************************

        write_ranking_report
            if rept_type$ = "H" then return
            if ranksw% <> 0% then goto write_workfil1_record
                ranksw% = 1%
                call "WORKOPN2" (#10, "OUTPT", 200%, f2%(10))
                call "GETNAMES" addr (#10, wfil$, wlib$, wvol$)
        write_workfil1_record
            put sortkey$ using L19680, lv1tot(pos("ABCDEFG"=rank_colm$),3%)
L19680:         FMT PD(14,4)
            if asnd_dsnd$ = "D" then sortkey$ = xor all(hex(ff))
            write #10 using L60400, sortkey$, code1save$, code1desc$,     ~
                lv1tot()
            return

        REM *************************************************************~
            * End of the report. Print last sub-total & final total.    *~
            * Then proceed to see if the Ranking report needs sorting,  *~
            * printing, etc.                                            *~
            *************************************************************

        end_of_report
            if abort% <> 0% then goto L19950
            mat prtval = rpttot : mat rpttot = zer
            if s_d$ = "S" then goto end_report_summary
                l% = l% + codelen%(group2%) + 1%
                if grp2_desc% = 0% then goto end_report_format
                     l% = l% + grp2_desc% + 1%
                     goto end_report_format
        end_report_summary
            l% = l% + codelen%(group1%) + 1%
            if grp1_desc% = 0% then goto end_report_format
                l% = l% + grp1_desc% + 1%
        end_report_format
            str(printline$, l% - 14%, 13%) = "** RUN TOTAL:"
            print using L61100, columns$(5)
            nbr_lines% = nbr_lines% + 1%
            gosub format_7_columns
            time$ = " " : call "TIME" (time$)
L19950:     print skip(2) : print using L61150, time$  /* End Of Report */
            return

        REM *************************************************************~
            * A print line is formatted and printed.                    *~
            *************************************************************

        print_line_formatter
            l% = 1%
            if group2% = 0% then goto format_7_columns
                str(printline$, l%, codelen%(group2%)) = code2$
                l% = l% + codelen%(group2%) + 1%
            if grp2_desc% = 0% then goto format_7_columns
                str(printline$, l%, grp2_desc%) = code2desc$
                l% = l% + grp2_desc% + 1%
        format_7_columns
            for n% = 1% to 7%
                gosub column_value_formatter
            next n%
            if nbr_lines% > max_lines% then gosub page_heading
            print using L61100, printline$
            nbr_lines% = nbr_lines% + 1%
            printline$ = " "
            return

        column_value_formatter
            if colerr%(n%) <> 0% then return
            if oper$(n%) <> "%" then goto L21280
                if prtval(n%, 2%) = 0 then goto L21280
                prtval(n%, 3%) = (prtval(n%, 1%) / prtval(n%, 2%)) * 100
L21280:     fdig%, fdec% = 0%
            convert str(ffmt$(n%),1,2) to fdig%, data goto L21300
L21300:     convert str(ffmt$(n%),3,1) to fdec%, data goto L21310
L21310:     w% = fdig% + 1%
            if fdec% <> 0% then w% = w% + fdec% + 1%
            w% = max(11%, w%)
            mat redim edvalue$(1)w%
            edvalue$(1) = " "
            cformat = fdec% * 1.1
            call "CONVERT" (prtval(n%, 3%), cformat, edvalue$(1))
            str(printline$, l%, w%) = edvalue$(1)
            l% = l% + w%
            return

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        select_summary_file
*        Get which Summary File to use
            if f2%(1) = 0% then close #1
            f2%(1) = 1%
            sum% = 999%
            convert summ_file$ to sum%, data goto L25110
L25110:     if sum% = 999% then return
            sumry% = sum% + 1%
            call "PUTPRNAM" addr (#1, prnames$(sumry%))
            call "OPENCHCK" (#1,  fs%(1), f2%(1), 0%, rslt$(1))
                nbr_rec% = val(str(rslt$(1),17,4),4) /* What to create */
                nbr_rec% = int(nbr_rec% / 4)         /* workfile with. */
            if f2%(1) <> 0% then return
            summ_desc$ = "Summary " & summ_file$ & ": " & descrs$(sumry%)
            str(line2$,,60) = summ_desc$
            group1% = codes%(sumry%, 1%) : group1$ = codes$(group1%, 1)
            group2% = codes%(sumry%, 2%) : group2$ = " "
            if group2% <> 0% then group2$ = codes$(group2%, 1)
            return

        validate_a_column
            if fc1$(c%) <> " " then goto L25280
                yr1$(c%), ytd1$(c%), oper$(c%), fc2$(c%), yr2$(c%),      ~
                     ytd2$(c%), ffmt$(c%), cdescr$(c%) = " "
                goto error_exit_2
L25280:     if pos("123456789ABCDEF" = fc1$(c%)) = 0% then goto error_exit
            if pos("123456789ABCDEF" = fc1$(c%)) < 10% then goto v_year1
                if pos(col$() = fc1$(c%)) >= c% then goto error_exit
                yr1$(c%), ytd1$(c%) = " "
                goto v_oper
        v_year1
            convert yr1$(c%) to yr%, data goto error_exit
            if yr% < -3% then goto error_exit
            if yr% > 3% then goto error_exit
            if y% + yr% < 1% then goto error_exit
            if y% + yr% > 99% then goto error_exit
            if ytd1$(c%) = "Y" then goto v_oper
            if ytd1$(c%) <> "N" then goto error_exit
        v_oper
            on pos("+-%@ " = oper$(c%)) goto L25490, L25490, L25490, L25450, ~
                L25470
                goto error_exit
L25450:     if pos("123456789ABCDEF"=fc1$(c%)) >= 10% then goto error_exit
            ytd1$(c%) = "Y"
L25470:     yr2$(c%), ytd2$(c%), fc2$(c%) = " "
            goto get_column_description
L25490:     if fc2$(c%) = " " then goto error_exit
            if pos("123456789ABCDEF" = fc2$(c%)) = 0% then goto error_exit
            if pos("123456789ABCDEF" = fc2$(c%)) < 10% then goto v_year2
                if pos(col$() = fc2$(c%)) >= c% then goto error_exit
                yr2$(c%), ytd2$(c%) = " "
                goto get_column_description
        v_year2
            convert yr2$(c%) to yr%, data goto error_exit
            if yr% < -3% then goto error_exit
            if yr% > 3 then goto error_exit
            if y% + yr% < 1% then goto error_exit
            if y% + yr% > 99% then goto error_exit
            if ytd2$(c%) = "Y" then goto get_column_description
            if ytd2$(c%) <> "N" then goto error_exit
        get_column_description
            gosub compute_column_description
            ffmt$ = ffmt$(c%)
            call "STRING" addr ("RJ", ffmt$, 3%)
            if str(ffmt$,1,1) = " " then str(ffmt$,1,1) = "0"
            ffmt$(c%) = ffmt$
            convert str(ffmt$,1,2) to fdig%, data goto error_exit
            convert str(ffmt$,3,1) to fdec%, data goto error_exit
            convert fdig% to str(ffmt$,1,2), pic (00)
            convert fdec% to str(ffmt$,3,1), pic (0)
            ffmt$(c%) = ffmt$
            if fdig% + fdec% > 18% then error_exit
            if fdig% < 1% or fdig% > 14% then goto error_exit
            if not (fdec% < 0% or fdec% > 4%) then return
        error_exit
            errormsg$ = "* WARNING: COLUMN " & col$(c%) & " HAS INVALID"&~
                " DEFINITION.  DROPPED FROM REPORT."
        error_exit_2
            colerr%(c%) = 1%    /* THIS COLUMN WON'T BE PRINTED */
            return

        compute_column_description
            cdescr$(c%) = " "
            if fc1$(c%) = " " then return
            on pos("+-%@ " = oper$(c%)) goto L25900, L25900, L25900, L26090, ~
                L26100
            cdescr$(c%) = "*** UNKNOWN VALUES FOR FIELDS" : return
L25900:     cdescr$(c%) = sname$(pos("123456789ABCDEF" = fc1$(c%)))
            gosub compute_lh_years
            str(cdescr$(c%),len(cdescr$(c%))+2) = oper$(c%)
            str(cdescr$(c%),len(cdescr$(c%))+2) =                        ~
                sname$(pos("123456789ABCDEF" = fc2$(c%)))
            if yr2$(c%) = " " then goto L26060
            if yr2$(c%)<>" 0" and yr2$(c%)<>"0 " and yr2$(c%)<>"00"      ~
                then goto L26000
                     str(cdescr$(c%),len(cdescr$(c%))+1) = ",Curr"
                     goto L26060
L26000:     if str(yr2$(c%),1,1) <> "-" then goto L26040
                str(cdescr$(c%),len(cdescr$(c%))+1) = "," &              ~
                     str(yr2$(c%),2,1) & "Ago"
                goto L26060
L26040:     str(cdescr$(c%),len(cdescr$(c%))+1) = "," &                  ~
                str(yr2$(c%),2) & "Fut"
L26060:     if ytd2$(c%) <> "Y" then return
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",YTD"
            return
L26090:     cdescr$(c%) = "Average"
L26100:     str(cdescr$(c%),len(cdescr$(c%))+2) =                        ~
                lname$(pos("123456789ABCDEF" = fc1$(c%)))
            gosub compute_lh_years
            call "STRING" addr ("LJ", cdescr$(c%), 45%)
            return

        compute_lh_years
            if yr1$(c%) = " " then goto L26280
            if yr1$(c%)<>" 0" and yr1$(c%)<>"0 " and yr1$(c%)<>"00"      ~
                then goto L26220
                     str(cdescr$(c%),len(cdescr$(c%))+1) = ",Curr"
                     goto L26280
L26220:     if str(yr1$(c%),1,1) <> "-" then goto L26260
                str(cdescr$(c%),len(cdescr$(c%))+1) = "," &              ~
                     str(yr1$(c%),2,1) & "Ago"
                goto L26280
L26260:     str(cdescr$(c%),len(cdescr$(c%))+1) = "," &                  ~
                str(yr1$(c%),2) & "Fut"
L26280:     if ytd1$(c%) <> "Y" then return
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",YTD"
            return

        REM Compute number of positions remaining on a print line *******
        compute_posn_rem
            posn_rem% = 132% : grp1%, grp2% = 0%
            if group2% = 0% then goto L26380
                grp2% = codelen%(group2%) + 1%
                if grp2_desc% <> 0% then grp2% = grp2% + grp2_desc% + 1%
L26380:     if group1% = 0% then goto compute_posn_rem_lines
                grp1% = codelen%(group1%) + 1%
                if grp1_desc% <> 0% then grp1% = grp1% + grp1_desc% + 1%
                if group2% = 0% then goto compute_posn_rem_summary
                if rept_type$ = "R" then goto compute_posn_rem_summary
                if summ_detl$ = "S" then goto compute_posn_rem_summary
                     posn_rem% = posn_rem% - max(grp1%, grp2%)
                     goto compute_posn_rem_lines
        compute_posn_rem_summary
            posn_rem% = posn_rem% - grp1%
        compute_posn_rem_lines
            for n% = 1% to 7%
                if ffmt$(n%) = " " then goto L26570
                posn%, fdig%, fdec% = 0%
                convert str(ffmt$(n%),1,2) to fdig%, data goto L26530
L26530:         convert str(ffmt$(n%),3,1) to fdec%, data goto L26540
L26540:         posn% = posn% + fdig% + 1%
                if fdec% <> 0% then posn% = posn% + fdec% + 1%
                posn_rem% = posn_rem% - max(11%, posn%)
L26570:     next n%
            convert posn_rem% to posn_rem$, pic (-###)
            call "STRING" addr ("LJ", posn_rem$, 4%)
            return

        page_heading
            page_nbr% = page_nbr% + 1%
        page_0_heading
            print page
            print using L61040, date$, time$, coname$, "-" & rptid$
            print using L61070, rept_date$, sub_hdr$, page_nbr%
            print using L61100, pline3$
            if rept$ = "R" then print using L61100, pline4$
            print
            if pagesw% <> 0% then return
            if rept$ = "R" then nbr_lines% = 5% else nbr_lines% = 4%
            if s_d$ <> "D" then goto L26790
            if page_brk$ <> "Y" then goto L26790
                print using L61100, "   " & group1$ & " " & code1save$ &  ~
                     "   " & code1desc$
                print
                nbr_lines% = nbr_lines% + 2%
L26790:     for n% = 1% to 4%
                if columns$(n%) = " " then goto L26830
                     print using L61100, columns$(n%)
                     nbr_lines% = nbr_lines% + 1%
L26830:     next n%
            print
            nbr_lines% = nbr_lines% + 1%
            if s_d$ <> "D" then return
            if page_brk$ = "Y" then return
                print using L61100, "   " & group1$ & " " & code1save$ &  ~
                     "   " & code1desc$
                print
                nbr_lines% = nbr_lines% + 2%
            return

        get_code_descriptions
            if eorsw% <> 0% then goto L27050
            if code1$ = code1save$ then goto L26990
                codex$, code1save$ = code1$ : g% = group1%
                file% = codefile%(group1%)  : gosub describer
                code1desc$ = codexdesc$
L26990:     if s_d$ = "S" then goto L27050
            if code2$ = code2save$ then goto L27050
                if group2% = 0% then goto L27050
                     codex$, code2save$ = code2$ : g% = group2%
                     file% = codefile%(group2%)  : gosub describer
                     code2desc$ = codexdesc$
L27050:     onetime% = 1%
            return

        describer
            if file% <> 6% then goto L27130
                if g% = 5% then key$ = "CUS TYPES" & codex$
                if g% = 7% then key$ = "REGIONS  " & codex$
                goto L27140
L27130:     key$ = codex$
L27140:     call "DESCRIBE" (#file%, key$, codexdesc$, 0%, f1%(file%))
            return

        workfil2_outputter
            for n% = 1% to 13%
                if periods$(n%) = " " or periods$(n%) = blankdate$ then return
                     write #11 using L27220, periods$(1%), periods$(n%),  ~
                          eod goto L27230
L27220:                   FMT  CH(6), CH(6)
L27230:     next n%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            * Data field screen handling.                               *~
            *************************************************************

        deffn'101
L40080:     accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(79),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Print in Background (Y/N)?:",                ~
               at (06,30), fac(hex(81)), bg_yorn$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over"                      ,        ~
               at (22,64), "(13)Instructions"                   ,        ~
               at (23,64), "(15)Print Screen"                   ,        ~
               at (24,64), fac(pf16fac$), pf16$                 ,        ~
               keys(hex(010d0f1000)), key (keyhit%)

               if keyhit% <> 13 then L40270
                  call "MANUAL" ("SAPRINT ")
                  goto L40080

L40270:        if keyhit% <> 15 then L40310
                  call "PRNTSCRN"
                  goto L40080

L40310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            errormsg$ = " "
            if bg_yorn$ = "Y" then return
            if bg_yorn$ = "N" then return
            errormsg$ = "You must enter 'Y' for Yes or 'N' for No"
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *-----------------------------------------------------------*~
            * Record layouts.                                           *~
            *************************************************************

L60070:     FMT /* Report record layout in file SAREPORT               */~
                CH(3),                   /* User ID                    */~
                BI(4),                   /* Sequence number            */~
                CH(10),                  /* Report Format Def name     */~
                CH(30),                  /* Report Format Def descriptn*/~
                CH(8),                   /* Report Date                */~
                CH(1),                   /* S/A Summary file #         */~
                2*CH(25),                /* Group 1 range values       */~
                2*CH(25),                /* Group 2 range values       */~
                CH(2),                   /* Group 1 description length */~
                CH(2),                   /* Group 2 description length */~
                CH(1),                   /* Type of report (H, R, B)   */~
                CH(1),                   /* Summary or Detail (S, D)   */~
                CH(1),                   /* Rank on Column (A - G)     */~
                CH(1),                   /* Ascending or Descending    */~
                CH(3),                   /* Max # to print (ALL, n)    */~
                CH(1),                   /* Page break code            */~
                7*CH(2),                 /* 1st fld Year rel to current*/~
                7*CH(1),                 /* 1st fld YTD indicator      */~
                7*CH(1),                 /* 1st fld code/column entry  */~
                7*CH(1),                 /* Column operator            */~
                7*CH(2),                 /* 2nd fld Year rel to current*/~
                7*CH(1),                 /* 2nd fld YTD indicator      */~
                7*CH(1),                 /* 2nd fld code/column entry  */~
                7*CH(3),                 /* Column format- 'nnd'       */~
                3*CH(10),                /* Column A report headers    */~
                3*CH(10),                /* Column B report headers    */~
                3*CH(10),                /* Column C report headers    */~
                3*CH(10),                /* Column D report headers    */~
                3*CH(10),                /* Column E report headers    */~
                3*CH(10),                /* Column F report headers    */~
                3*CH(10),                /* Column G report headers    */~
                CH(1)                    /* Include codes outside yrs  */

L60400:     FMT /* WORKFIL1 record layout                              */~
                CH(8),                   /* SORTKEY                    */~
                CH(25),                  /* Code 1                     */~
                CH(32),                  /* Code 1 description         */~
                21*PD(14, 4)             /* The column values          */

        REM *************************************************************~
            * Print line formats.                                       *~
            *************************************************************

L61040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                       SAPRINT####~
        ~###
L61070: %   AS OF: ########                  ############################~
        ~################################                          PAGE: #~
        ~###
L61100: %################################################################~
        ~#################################################################~
        ~###
L61150: %                                                   ** END OF REP~
        ~ORT @ ######## **

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#11)   /* WORKFIL2 */
            call "FILEBGON" (#12)   /* WORKFIL3 */
            end
