        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   RRRR   PPPP   TTTTT   SSS   EEEEE  L       *~
            *  S      A   A  R   R  P   P    T    S      E      L       *~
            *   SSS   AAAAA  RRRR   PPPP     T     SSS   EEEE   L       *~
            *      S  A   A  R   R  P        T        S  E      L       *~
            *   SSS   A   A  R   R  P        T     SSS   EEEEE  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SARPTSEL - Maintain Report Groups.  Select S/A reports for*~
            *            print either singly or in groups.  Edit sel-   *~
            *            ected report formats for this run only.  Link  *~
            *            to report print programs.                      *~
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
            * 12/12/86 ! Original                                 ! JRH *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 10/28/87 ! Increased Work File size                 ! HES *~
            * 02/19/92 ! Added support for Including summary codes! JDH *~
            *          !   with no activity in specified years.   !     *~
            * 08/21/96 ! Century date conversion                  ! DER *~
            * 09/02/97 !  and add all(hex(00)) to plowkey         ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim    /*  Renumber the 1000 - 2000 series by 10  */             ~
            asnd_dsnd$(96)1,             /* Ascend or Descend          */~
            asof_date$8,                 /* Default As Of Date         */~
            blankdate$8,                 /* blank unfmt date           */~
            cdescr$(7)45,                /* Report Column description  */~
            code1$( 1)50,                /* 1st Grp Codes- Input/Edit  */~
            code2$( 1)50,                /* 2nd Grp Codes- Input/Edit  */~
            codefile%(8), codelen%(8),   /* Group Code Len, File#      */~
            codes%(10,2),                /* Summary File Group Codes   */~
            codes$(10,2)14,              /* Group Code Descriptors     */~
            codex$25,                    /* Group code work area       */~
            col$3,                       /* Column header              */~
            col$(7)1,                    /* Column designators A-G     */~
            colahdr$(96)30,              /* Column A report header     */~
            colbhdr$(96)30,              /* Column B report header     */~
            colchdr$(96)30,              /* Column C report header     */~
            coldhdr$(96)30,              /* Column D report header     */~
            colehdr$(96)30,              /* Column E report header     */~
            colfhdr$(96)30,              /* Column F report header     */~
            colghdr$(96)30,              /* Column G report header     */~
            coldesc$18,                  /* Column header              */~
            cursor%(2),                  /* Cursor location for edit   */~
            d$7, dfac$1,                 /* Edited record count & FAC  */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* Summary File Descriptions  */~
            dsc_map(4),                  /* description map plowcode   */~
            eoj1$79, eoj1fac$,           /* EOJ PF(1) message & FAC    */~
            eoj10$79, eoj10fac$,         /* EOJ PF(10) message & FAC   */~
            eoj11$79, eoj11fac$,         /* EOJ PF(11) message & FAC   */~
            eoj12$79, eoj12fac$,         /* EOJ PF(12) message & FAC   */~
            eoj16$79, eoj16fac$,         /* EOJ PF(16) message & FAC   */~
            eoj32$79, eoj32fac$,         /* EOJ PF(32) message & FAC   */~
            eojtab$8,                    /* PF keys at EOJ             */~
            errormsg$79,                 /* Error message              */~
            fc$3,                        /* Column header              */~
            fc1$(96)7,                   /* Field or Column 1          */~
            fc2$(96)7,                   /* Field or Column 2          */~
            ffmt$(96)21,                 /* Field Format               */~
            fmt$3,                       /* Column header              */~
            grcd1$(96)50,                /* 1st Grp Codes- SYSFILE2    */~
            grcd2$(96)50,                /* 2nd Grp Codes- SYSFILE2    */~
            group1$14, group2$14,        /* Group Code Descriptors     */~
            grp1_desc$(96)2,             /* Group Code 1 Desc length   */~
            grp2_desc$(96)2,             /* Group Code 2 Desc length   */~
            i$(24)80,                    /* Screen Image               */~
            incl$(96)1,                  /* Include codes outside yrs. */~
            incl_excl(1),                /* PLOWCODE place holder      */~
            incl_excl$(1)1,              /* PLOWCODE place holder      */~
            inpmessage$79,               /* Informational Message      */~
            junk$25,                     /* Miscellaneous              */~
            keytab$33,                   /* PF keys enabled at ACCEPT  */~
            l$7, lfac$1,                 /* Edited record count & FAC  */~
            l7fac$1,                     /* Line 7 FAC                 */~
            l8_date$8,                   /* Line 8 'Date'              */~
            l8_rept$10,                  /* Line 8 'Report'            */~
            l8_desc$30,                  /* Line 8 'Description'       */~
            l8_nr$1,                     /* Line 8 '#'                 */~
            l8_gp1$3,                    /* Line 8 'GP1'               */~
            l8_gp2$3,                    /* Line 8 'GP2'               */~
            l8_typ$3,                    /* Line 8 'TYP'               */~
            l8_sd$3,                     /* Line 8 'S/D'               */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Current Library            */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line6$79,                    /* Screen line 6              */~
            line16$79,                   /* Screen line 16             */~
            line17$79,                   /* Screen line 17             */~
            line18$79,                   /* Screen line 18             */~
            line19$79,                   /* Screen line 19             */~
            line20$79,                   /* Screen line 20             */~
            linenr$(96)3,                /* Edited line numbers        */~
            lname$(15)21,                /* Field/column names (long)  */~
            max_print$(96)3,             /* Max to print               */~
            msg$79,                      /* Message                    */~
            op$4,                        /* Column header              */~
            oper$(96)7,                  /* Operation                  */~
            page_brk$(96)1,              /* Page break codes           */~
            periods$(13)6,               /* Period Start Dates by Year */~
            pf1$17,                      /* PF 1 Screen Literal        */~
            pf3$16,                      /* PF 3 Screen Literal        */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$18,                      /* PF 5 Screen Literal        */~
            pf6$18,                      /* PF 6 Screen Literal        */~
            pf8$18,                      /* PF 8 Screen Literal        */~
            pf9$18,                      /* PF 9 Screen Literal        */~
            pf12$20,                     /* PF 12 Screen Literal       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf32$18,                     /* PF 32 Screen Literal       */~
            pfxfac$(32)1,                /* FACs for the PF keys       */~
            plowhdr$(3)79,               /* PLOWKEY Headers            */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            posn_rem$4,                  /* Edited positions remaining */~
            prnames$(10)8,               /* Summary File PR Names      */~
            rank_colm$(96)1,             /* Rank on Column             */~
            rdef_key$20,                 /* SYSFILE2 key               */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rept_ardsc$(12)30,           /* Report Def desc array      */~
            rept_array$(12)10,           /* Report Def name array      */~
            rept_date$(96)8,             /* Report Date                */~
            rept_def$10,                 /* Report Definition name     */~
            rept_desc$(96)30,            /* Report Description         */~
            rept_grp$10,                 /* Report Group name          */~
            rept_name$(96)10,            /* Report Name                */~
            rept_type$(96)1,             /* Report Type                */~
            ret$2,                       /* RET%, edited               */~
            rgrp_desc$30,                /* Report group description   */~
            rgrp_key$20,                 /* SYSFILE2 key               */~
            save1$(2)25,                 /* Group codes edit           */~
            save2$(2)25,                 /* Group codes edit           */~
            sname$(15)10,                /* Field/column names (short) */~
            summ_desc$60,                /* Summary file header descr  */~
            summ_detl$(96)1,             /* Summary or Detail          */~
            summ_file$(96)1,             /* Summary File #             */~
            test_date$12,                /* Date to be tested          */~
            tt$1,                        /* BG/FG indicator            */~
            u3$2,                        /* U3%, edited                */~
            userid$3,                    /* Current User Id            */~
            vol$6,                       /* Current Volume             */~
            work$(1)50,                  /* Array work area            */~
            workdesc$50,                 /* WORKFILE description field */~
            xfc1$(7)1,                   /* Field or Column 1          */~
            xfc2$(7)1,                   /* Field or Column 2          */~
            xffmt$(7)3,                  /* Field Format               */~
            xoper$(7)1,                  /* Operation                  */~
            xyr1$(7)2,                   /* Data Year 1                */~
            xyr2$(7)2,                   /* Data Year 2                */~
            xytd1$(7)1,                  /* Year to Date 1             */~
            xytd2$(7)1,                  /* Year to Date 2             */~
            ytd$4,                       /* Column header              */~
            yr$2,                        /* Column header              */~
            yr1$(96)14,                  /* Data Year 1                */~
            yr2$(96)14,                  /* Data Year 2                */~
            ytd1$(96)7,                  /* Year to Date 1             */~
            ytd2$(96)7                   /* Year to Date 2             */

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
        call "EXTRACT" addr ("ID", userid$, "CL", lib$, "CV", vol$,      ~
                                                               "TT", tt$)
        if tt$ <> "B" then L01952
            call "LINK" addr("SAPRINT ","P",lib$,vol$,0%," "," ",0%,"N", ~
                                                               u3%, ret%)
            if u3% = 0% then exit_program
                convert u3% to u3$, pic (##)
                convert ret% to ret$, pic (##)
                msg$ = "rptS/A Report Selection reports an unsuccessful"&~
                       " link to SAPRINT: " & u3$ & " " & ret$
                call "MESSAGE"addr("XM",str(userid$)&hex(20),msg$,78%,a%)
                goto exit_program

L01952: mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! CUSTOMER ! Customer Master File                     *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! CATEGORY ! Inventory Category Codes File            *~
            * #6  ! GENCODES ! System General Codes file.               *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! STORNAME ! Store Information File                   *~
            * #9  ! SAREPORT ! Sales Analysis Reports file              *~
            * #10 ! WORKFILE ! Valid report date work area              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

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

            select #9,  "SAREPORT",  varc, indexed, recsize = 463,       ~
                        keypos = 1, keylen = 7

            select #10, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  62,               ~
                        keypos =    1, keylen = 12,                      ~
                        alt key 1, keypos = 7, keylen = 6

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 101%, rslt$(9 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Initializing...")

*        Load S/A Summary File Descriptions
            plowkey$ = "SA.FILES.SASUMRY" & all(hex(00))
L09055:     call "PLOWNEXT" (#2, plowkey$, 16%, f1%(2))
            if f1%(2) = 1% then L09090
                if file% <> 0% then L09120
                call "ASKUSER" (0%, "*** NO S/A FILES ***",              ~
                                "There are no S/A Summary Files Defined",~
                                " ", "Press (RETURN) to exit program...")
                goto exit_program
L09090:     convert str(plowkey$,17,1) to file% : file% = file% + 1%
            get #2 using L09105, prnames$(file%), descrs$(file%),          ~
                               codes%(file%, 1), codes%(file%, 2)
L09105:         FMT XX(9), CH(8), XX(3), CH(30), XX(2), 2*BI(1)
            goto L09055

L09120
*        Build a WORKFILE of the valid report dates
            call "WORKOPEN" (#10, "IO   ", 2000%, f2%(10))
            if f2%(10) <> 0% then goto exit_program

            call "READ100" (#2, "SWITCHS.SA", f1%(2))
            if f1%(2) <> 0% then goto L09180
                call "ASKUSER" (0%, "*** NO SWITCHES RECORD ***",        ~
                     "The S/A Switches record does not exist", " ",      ~
                     "Press (RETURN) to exit program")
                goto exit_program

L09180:     plowhdr$(1) = "  Rpt Date  Period"
            plowhdr$(3) = hex(ac) & "Display shows possible As Of Date" &~
                "s within their respective S/A 'years'"
            open% = 1%
            get #2 using L09205, periods$() : gosub workfile_outputter
L09205:         FMT POS(74), 13*CH(6)
            get #2 using L09215, periods$() : gosub workfile_outputter
L09215:         FMT POS(152), 13*CH(6)
            get #2 using L09225, periods$() : gosub workfile_outputter
L09225:         FMT POS(230), 13*CH(6)

            open% = 0%
            plowkey$ = "SA.YEARS.USED." & all(hex(00))
L09245:     call "PLOWNEXT" (#2, plowkey$, 14%, f1%(2))
            if f1%(2) = 0% then goto L09275
                get #2 using L09260, periods$() : gosub workfile_outputter
L09260:              FMT POS(29), 13*CH(6)
                goto L09245

L09275: REM Set up Static Tables
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
            date$ = date : call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            col$ = "Col" : yr$ = "Yr" : ytd$ = "YTD?" : fc$ = "F/C"
            op$ = "Oper" : fmt$ = "Fmt" : coldesc$ = "Column Description"
            l7fac$ = hex(9c)
            line6$ = "The maximum of 96 reports has been input.  No" &   ~
                     " more entries permitted."
            l8_date$ = "  Date" : l8_rept$ = "Report"
            l8_desc$ = "Description" : l8_nr$ = "#" : l8_gp1$ = "GP1"
            l8_gp2$ = "GP2" : l8_typ$ = "TYP" : l8_sd$ = "S/D"
            line16$ = "Years          ! Operators ! Field Codes"
            line17$ = "-n:n years ago ! +:Total   ! 1:Actl Bkng Qty  4:Ac~
        ~tl Ship Val  7:Trgt Bkng Val"
            line18$ = " 0:Current     ! -:Diff    ! 2:Actl Bkng Val  5:Ac~
        ~tl Ship Cst  8:Trgt Ship Qty"
            line19$ = "+n:Future yrs  ! @:Average ! 3:Actl Ship Qty  6:Tr~
        ~gt Bkng Qty  9:Trgt Ship Val"
            line20$ = " (n = 1-3)     ! %:Percent !     or, Column Design~
        ~ators A-F"
            rdef_key$ = "SA.RPTDEF." : rgrp_key$ = "SA.RPTGRP."
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
            page%, pmax% = 1%
            for n% = 1% to 96%
                convert n% to linenr$(n%), pic (##)
                str(linenr$(n%),3) = "."
            next n%

*        Determine how many records already exist for this user ******
            d%, f% = 0%
            plowkey$ = str(userid$,,3) & hex(00000000)
L09650:     call "PLOWNEXT" (#9, plowkey$, 3%, f1%(9))
            if f1%(9) = 0% then goto L09680
                get #9 using L09665, f%
L09665:              FMT POS(4), BI(4)
                d% = d% + 1%
                goto L09650
L09680:     eoj1fac$, eoj16fac$ = hex(8c)
            str(eoj1$,8) = "(1)  Return to continue the selection process"
            str(eoj16$,7) = "(16)  Exit to Menu"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for selecting reports for print      *~
            *************************************************************

        erasemode
            datesw% = 0%
            gosub report_eraser

        inputmode
            errormsg$  = " "
            if datesw% = 0% then get_as_of_date
            sover% = 1%
            pf1$ = "(1)Start Over"
            pf3$ = "(3)Change Date"
            pf4$ = "(4)Prev Page"
            pf5$ = "(5)Next Page"
            if e% > 0% then L10200
                pf6$ = "(6)Select Groups" : e% = 2%
L10200:     pf8$ = "(8)Edit Report"
            pf9$ = "(9)Edit Groups"
            pf16$ = "(16)End Selectns"
            keytab$ = hex(01ff03040506ff0809ffffff0dff0f102000)

            init(" ") errormsg$, inpmessage$, rept_def$, rept_grp$
            if l% > 95% then l7fac$ = hex(84)

            init(hex(8c)) pfxfac$()
            if page% > 1% then L10310
                str(keytab$, 4,1) = hex(ff) : pfxfac$(4) = hex(9c)
L10310:     if page% < pmax% then L10330
                str(keytab$, 5,1) = hex(ff) : pfxfac$(5) = hex(9c)
L10330:     if l% < 96% then goto L10350
                str(keytab$, 6,1) = hex(ff) : pfxfac$(6) = hex(9c)
L10350:     if l% > 0% then goto L10380
                str(keytab$, 8,1) = hex(ff) : pfxfac$(8) = hex(9c)

L10380:     gosub'051(e%)           /* Check Enables, Set Defaults*/
                if enabled% = 0 then L10680
            gosub'101(e%)           /* Display & Accept Screen    */
                if keyhit%  =  1 then gosub startover
                if keyhit%  =  3 then       get_as_of_date
                if keyhit% <>  4 then goto L10460
                     if page% > 1% then page% = page% - 1%
                     goto inputmode
L10460:         if keyhit% <>  5 then L10490
                     if page% < pmax% then page% = page% + 1%
                     goto inputmode
L10490:         if keyhit% <> 6% then L10550
                     if e% <> 2% then L10530
                          e% = 3% : pf6$ = "(6)Select Reports"
                                                          goto inputmode
L10530:                   e% = 2% : pf6$ = "(6)Select Groups"
                                                          goto inputmode
L10550:         if keyhit% <> 8% then L10640
                     m% = cursor%(1) - 7%
                     if m% >= 1% and m% <= 12% then L10600
                          errormsg$ = "Position cursor to report line."
                          goto inputmode
L10600:              m% = ((page% - 1%) * 12%) + m%
                     if rept_name$(m%) = " " then inputmode
                     if m% < l% then dir% = 1% else dir% = 2%
                     goto edit_report_formats
L10640:         if keyhit%  =  9 then      input_groups
                if keyhit%  = 16 then      print_reports
                if keyhit%  = 32 then      exit_program
                if keyhit% <>  0 then goto L10380
L10680:     gosub'151(e%)           /* Edit Field for Valid Entry */
                if errormsg$ <> " " then goto L10380
                goto inputmode

        get_as_of_date
            init (hex(9c)) pfxfac$() : init (hex(ff)) keytab$
            pfxfac$(1), pfxfac$(9), pfxfac$(16) = hex(8c)
            pf1$  = "(1)Start Over"
            pf9$  = "(9)Edit Groups"
            pf16$ = "(16)Exit Program"
            keytab$ = hex(01090d0f1000)
            gosub'051(1%)
L10800:     gosub'101(1%)
                if keyhit% =  1% then gosub startover
                if keyhit% =  9% then       input_groups
                if keyhit% = 16% then       exit_program
                if keyhit% <> 0% then       L10800
            gosub'151(1%)
                if errormsg$ <> " " then goto L10800
            datesw%    = 1%
            goto inputmode


        REM *************************************************************~
            *       R E P O R T   G R O U P   I N P U T   M O D E       *~
            *-----------------------------------------------------------*~
            * Handles normal input for report group Input/Capture       *~
            *************************************************************

        input_groups
            sover% = 2%
            pf1$  = "(1)Start Over"
            pf3$  = "(3)Copy a Group"
            pf12$ = " "
            pf16$ = "(16)Selections"
            init(" ") errormsg$, inpmessage$, rept_grp$, rept_def$,      ~
                      rept_array$(), rept_ardsc$(), rgrp_desc$

            for fieldnr% = 1% to 14%
                keytab$ = hex(01ff03ffffffffffffffffff0dff0f1000)
                init (hex(8c)) pfxfac$()
                if fieldnr% = 3% then goto L12210
                     str(keytab$,3,1) = hex(ff) : pfxfac$(3) = hex(9c)
L12210:         if fieldnr% > 3% then pf16$ = "(16)Edit Mode " else      ~
                                      pf16$ = "(16)Selections"
                r% = fieldnr% - 2%

L12250:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L12350
L12270:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  3 then       L12320
                          gosub replicate_a_group
                          goto L12250
L12320:               if keyhit%  = 16 and fieldnr% > 3% then edit_groups
                      if keyhit%  = 16                   then inputmode
                      if keyhit% <>  0 then goto L12270
L12350:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L12270
                      if fieldnr% = 1% and f1%(2) = 1% then edit_groups
                next fieldnr%
                goto edit_groups

        replicate_a_group
            plowkey$ = str(rgrp_key$,,10) & all(hex(00))
            msg$ = hex(06) & "Select a report group to copy"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            if f1%(2) = 0% then return
                get #2 using L12470, rept_array$()
L12470:              FMT XX(50), 12*CH(10)
                for r% = 1% to 12%
                     gosub get_report_description
                next r%
                return clear

        edit_groups
            keytab$ = hex(01ffffffffffffffffffff0c0dff0f1000)
            pf3$  = " "
            pf12$ = "(12)Delete Group"
            pf16$ = "(16)Save Group"
            inpmessage$ = "To modify displayed values, position cursor"& ~
                          " to desired value and press RETURN."
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then gosub delete_group
                  if keyhit%  = 16 then       save_group
                  if keyhit% <>  0 then       edit_groups
            fieldnr% = max(2%, cursor%(1) - 5%)
            if fieldnr% > 14% then goto edit_groups
                r% = fieldnr% - 2%
                gosub'052(fieldnr%)
                     if enabled% = 0% then goto edit_groups
                keytab$ = hex(01ffffffffffffffffffffff0dff0fff00)
                pf12$, pf16$ = " "
L12720:         gosub'102(fieldnr%)
                     if keyhit%  =  1 then gosub startover
                     if keyhit% <>  0 then       edit_groups
                gosub'152(fieldnr%)
                     if errormsg$ <> " " then goto L12720
                goto edit_groups

        delete_group
            str(rgrp_key$,11) = rept_grp$
L12810:     ret% = 2%
            call "ASKUSER" (ret%, "*** CONFIRM DELETION ***",            ~
                "Press PF(12) to DELETE Report Group '" & rept_grp$ &"'",~
                "-- OR --", "Press (RETURN) to abort deletion")
            if ret%  =  0% then return
            if ret% <> 12% then L12810
                call "READ101" (#2, rgrp_key$, f1%(2))
                if f1%(2) <> 0% then delete #2
                return clear
                goto input_groups


        save_group
            str(rgrp_key$,11) = rept_grp$
            call "READ101" (#2, rgrp_key$, f1%(2))
                 if f1%(2) <> 0% then delete #2
            call "LINSMASH" (rept_array$())
            write #2 using L35360, rgrp_key$, rgrp_desc$, rept_array$(),  ~
                                                                 " ", " "
            goto input_groups



        REM *************************************************************~
            * This routine allows editing of selected report formats.   *~
            * Much of the code (screen, etc.) is borrowed from SARPTINP.*~
            * The operator is allowed to cruise through the formats,    *~
            * changing report parameters such as ranges to be printed,  *~
            * report type, etc., but NOT the report format itself.      *~
            *************************************************************

        edit_report_formats
            sover% = 1%
            pf1$  = "(1)Start Over"
            pf4$  = "(4)Prev Report"
            pf5$  = "(5)Next Report"
            pf12$ = "(12)Cancel Report"
            pf16$ = "(16)Return   "

        top_of_edit
            xfc1$() = fc1$(m%) : xfc2$() = fc2$(m%) : xffmt$() = ffmt$(m%)
            xyr1$() = yr1$(m%) : xyr2$() = yr2$(m%) : xytd1$() = ytd1$(m%)
            xytd2$() = ytd2$(m%) : xoper$() = oper$(m%)
            gosub select_summary_file
            gosub compute_column_description
            errormsg$ = " "

L14125:     init (hex(8c))pfxfac$()
            keytab$ = hex(01ffff04050c0d0f102000)
            if m% > 1% then goto L14145
                str(keytab$, 4, 1) = hex(ff) : pfxfac$(4)  = hex(9c)
L14145:     if m% < l% then goto L14160
                str(keytab$, 5, 1) = hex(ff) : pfxfac$(5)  = hex(9c)

L14160:     inpmessage$ = "Position Cursor to line and press RETURN."
            gosub'103(0%)         /* Display & Accept Screen    */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  4% then      L14185
                     dir% = 2% : goto find_next_format
L14185:         if keyhit% <>  5% then      L14195
                     dir% = 1% : goto find_next_format
L14195:         if keyhit%  = 12% then      de_select_report
                if keyhit%  = 16% then      inputmode
                if keyhit% <> 32% then      L14220
                     page% = int((m% - 1%) / 12%) + 1%
                     goto inputmode
L14220:         if keyhit% <>  0% then goto L14125
                     f1% = cursor%(1) - 3%
                     if f1% < 1 or f1% > 4% then L14125
                     init (hex(9c)) pfxfac$() : pfxfac$(1) = hex(8c)
                     on f1% goto L14245, L14250, L14255, L14260
L14245:                   f1% = 1%  :  f2% =  1%  :  goto L14270
L14250:                   f1% = 2%  :  f2% =  4%  :  goto L14270
L14255:                   f1% = 5%  :  f2% =  7%  :  goto L14270
L14260:                   f1% = 8%  :  f2% = 14%  :  goto L14270

L14270:     for fieldnr% = f1% to f2%
                gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then goto L14300
L14285:         gosub'103(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then       L14285
L14300:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L14285
            next fieldnr%
            goto L14125

        find_next_format
            msave% = m%
L14335:     if dir% = 1% then m% = min(l%, m% + 1%)                      ~
                         else m% = max(1%, m% - 1%)
            if rept_name$(m%) <> " " then goto top_of_edit
                if m% > 1% and m% < l% then goto L14335
                m% = msave%
                goto top_of_edit

        REM *************************************************************~
            * Allows the operator to remove a report from the stream of *~
            * selected reports. Does not delete anything from any file. *~
            *************************************************************
        de_select_report
L14395:     ret% = 2%
            call "ASKUSER" (ret%, "*** CONFIRM CANCELLATION ***",        ~
                "Press PF(12) to CANCEL '" & rept_name$(m%) &            ~
                     "' from print list",                                ~
                "-- OR --", "Press (RETURN) to abort cancellation")
            if ret%  =  0% then L14125
            if ret% <> 12% then L14395
            if l% = 1% then L14660
            for i% = m% to l%
                asnd_dsnd$(i%)  =  asnd_dsnd$(i%+1)
                fc1$      (i%)  =  fc1$      (i%+1)
                fc2$      (i%)  =  fc2$      (i%+1)
                ffmt$     (i%)  =  ffmt$     (i%+1)
                grcd1$    (i%)  =  grcd1$    (i%+1)
                grcd2$    (i%)  =  grcd2$    (i%+1)
                grp1_desc$(i%)  =  grp1_desc$(i%+1)
                grp2_desc$(i%)  =  grp2_desc$(i%+1)
                max_print$(i%)  =  max_print$(i%+1)
                page_brk$ (i%)  =  page_brk$ (i%+1)
                incl$     (i%)  =  incl$     (i%+1)
                oper$     (i%)  =  oper$     (i%+1)
                rank_colm$(i%)  =  rank_colm$(i%+1)
                rept_date$(i%)  =  rept_date$(i%+1)
                rept_desc$(i%)  =  rept_desc$(i%+1)
                rept_name$(i%)  =  rept_name$(i%+1)
                rept_type$(i%)  =  rept_type$(i%+1)
                summ_detl$(i%)  =  summ_detl$(i%+1)
                summ_file$(i%)  =  summ_file$(i%+1)
                yr1$      (i%)  =  yr1$      (i%+1)
                yr2$      (i%)  =  yr2$      (i%+1)
                ytd1$     (i%)  =  ytd1$     (i%+1)
                ytd2$     (i%)  =  ytd2$     (i%+1)
                colahdr$  (i%)  =  colahdr$  (i%+1)
                colbhdr$  (i%)  =  colbhdr$  (i%+1)
                colchdr$  (i%)  =  colchdr$  (i%+1)
                coldhdr$  (i%)  =  coldhdr$  (i%+1)
                colehdr$  (i%)  =  colehdr$  (i%+1)
                colfhdr$  (i%)  =  colfhdr$  (i%+1)
                colghdr$  (i%)  =  colghdr$  (i%+1)
            next i%
L14660:     init (" ") asnd_dsnd$(l%), fc1$(l%), fc2$(l%), ffmt$(l%),    ~
                grcd1$(l%), grcd2$(l%), grp1_desc$(l%), grp2_desc$(l%),  ~
                max_print$(l%), page_brk$(l%), oper$(l%), rank_colm$(l%),~
                rept_date$(l%), rept_desc$(l%), rept_name$(l%),          ~
                rept_type$(l%), summ_detl$(l%), summ_file$(l%), yr1$(l%),~
                yr2$(l%), ytd1$(l%), ytd2$(l%), colahdr$(l%),            ~
                colbhdr$(l%), colchdr$(l%), coldhdr$(l%), colehdr$(l%),  ~
                colfhdr$(l%), colghdr$(l%), incl$(l%)
            l% = l% - 1%
            goto inputmode



        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        select_summary_file
*        Get info about Summary File to display data for
            sum% = 0%
            convert summ_file$(m%) to sum%, data goto L15090
L15090:     sumry% = sum% + 1%
            summ_desc$ = "Summary " & summ_file$(m%) & ": " &            ~
                                                          descrs$(sumry%)
            group1% = codes%(sumry%, 1%)
            file1%  = codefile%(group1%)
            mat redim code1$(1)50
            code1$() = grcd1$(m%)
            mat redim code1$(2)25
            save1$(1) = code1$(1) : save1$(2) = code1$(2)
            mat redim code1$(2)max(5%, codelen%(group1%))
            code1$(1) = save1$(1) : code1$(2) = save1$(2)
            group1$ = codes$(group1%, 1)
            group2% = codes%(sumry%, 2%) : file2% = 0% : group2$ = " "
            grp1_desc% = 0%
            convert grp1_desc$(m%) to grp1_desc%, data goto L15240
L15240:     if group1% = 1% then grp1_max% = 32% else grp1_max% = 30%

            grp2_max%, grp2_desc% = 0%
            convert grp2_desc$(m%) to grp2_desc%, data goto L15280
L15280:     mat redim code2$(1)50
            code2$() = grcd2$(m%)
            mat redim code2$(2)25
            save2$(1) = code2$(1) : save2$(2) = code2$(2)
            if group2% = 0% then return
                if group2% = 1% then grp2_max% = 32% else grp2_max% = 30%
                file2%  = codefile%(group2%)
                group2$ = codes$(group2%, 1)
                mat redim code2$(2)max(5%, codelen%(group2%))
                code2$(1) = save2$(1) : code2$(2) = save2$(2)
            return

        REM COMPUTE NUMBER OF POSITIONS REMAINING ON A PRINT LINE *******
        compute_posn_rem
            posn_rem% = 132% : grp1%, grp2% = 0%
            if group2% = 0% then goto L15460
                grp2% = codelen%(group2%) + 1%
                if grp2_desc% <> 0% then grp2% = grp2% + grp2_desc% + 1%
L15460:     if group1% = 0% then goto compute_posn_rem_lines
                grp1% = codelen%(group1%) + 1%
                if grp1_desc% <> 0% then grp1% = grp1% + grp1_desc% + 1%
                if group2% = 0% then goto compute_posn_rem_summary
                if rept_type$(m%) = "R" then goto compute_posn_rem_summary
                if summ_detl$(m%) = "S" then goto compute_posn_rem_summary
                     posn_rem% = posn_rem% - max(grp1%, grp2%)
                     goto compute_posn_rem_lines
        compute_posn_rem_summary
            posn_rem% = posn_rem% - grp1%
        compute_posn_rem_lines
            mat redim work$(1)50
            work$(1) = ffmt$(m%)
            mat redim work$(7)3
            for n% = 1% to 7%
                if work$(n%) = " " then goto L15680
                posn%, fdig%, fdec% = 0%
                convert str(work$(n%),1,2) to fdig%, data goto L15640
L15640:         convert str(work$(n%),3,1) to fdec%, data goto L15650
L15650:         posn% = posn% + fdig% + 1%
                if fdec% <> 0% then posn% = posn% + fdec% + 1%
                posn_rem% = posn_rem% - max(11%, posn%)
L15680:     next n%
            convert posn_rem% to posn_rem$, pic (-###)
            call "STRING" addr ("LJ", posn_rem$, 4%)
            return

        capture_codes
*        Get Group Code                        CODEX$
            msg$ = hex(06) & "Select " & codes$(g%,2)
            if file% = 6% then L15840

*         Get code using GETCODE
            call "GETCODE" (#file%, codex$, msg$, 0%, 0, onfile%)
            if onfile% = 1% then return
L15810:         errormsg$ = codes$(g%,2) & " not found on file."
                return

L15840
*         Get code using PLOWCODE
            if g% = 5% then readkey$ = "CUS TYPES" & codex$
            if g% = 7% then readkey$ = "REGIONS  " & codex$
            call "PLOWCODE" (#file%, readkey$, msg$, 9%, 0.30, onfile%)

            if onfile% = 0% then L15810
                codex$ = str(readkey$,10)
                return

        compute_column_description
            init (" ") cdescr$()
            for c% = 1% to 7%
                if xfc1$(c%) = " " then goto L16250
                on pos("+-%@ " = xoper$(c%)) goto L16000, L16000, L16000,   ~
                     L16200, L16210
                cdescr$(c%) = "*** UNKNOWN VALUES FOR FIELDS" : return
L16000:         cdescr$(c%) = sname$(pos("123456789ABCDEF" = xfc1$(c%)))
                gosub compute_lh_years
                str(cdescr$(c%),len(cdescr$(c%))+2) = xoper$(c%)
                str(cdescr$(c%),len(cdescr$(c%))+2) =                    ~
                     sname$(pos("123456789ABCDEF" = xfc2$(c%)))
                if xyr2$(c%) = " " then goto L16170
                if xyr2$(c%) <> " 0" then goto L16110
                if xyr2$(c%)<>" 0" and xyr2$(c%)<>"0 " and               ~
                     xyr2$(c%)<>"00" then goto L16110
                          str(cdescr$(c%),len(cdescr$(c%))+1) = ",Curr"
                          goto L16170
L16110:         if str(xyr2$(c%),1,1) <> "-" then goto L16150
                     str(cdescr$(c%),len(cdescr$(c%))+1) = "," &         ~
                          str(xyr2$(c%),2,1) & "Ago"
                     goto L16170
L16150:         str(cdescr$(c%),len(cdescr$(c%))+1) = "," &              ~
                     str(xyr2$(c%),2) & "Fut"
L16170:         if xytd2$(c%) <> "Y" then goto L16250
                     str(cdescr$(c%),len(cdescr$(c%))+1) = ",YTD"
                goto L16250
L16200:         cdescr$(c%) = "Average"
L16210:         str(cdescr$(c%),len(cdescr$(c%))+2) =                    ~
                     lname$(pos("123456789ABCDEF" = xfc1$(c%)))
                gosub compute_lh_years
                call "STRING" addr ("LJ", cdescr$(c%), 45%)
L16250:     next c%
            return

        compute_lh_years
            if xyr1$(c%) = " " then goto L16400
            if xyr1$(c%)<>" 0" and xyr1$(c%)<>"0 " and xyr1$(c%)<>"00"   ~
                then goto L16340
                     str(cdescr$(c%),len(cdescr$(c%))+1) = ",Curr"
                     goto L16400
L16340:     if str(xyr1$(c%),1,1) <> "-" then goto L16380
                str(cdescr$(c%),len(cdescr$(c%))+1) = "," &              ~
                     str(xyr1$(c%),2,1) & "Ago"
                goto L16400
L16380:     str(cdescr$(c%),len(cdescr$(c%))+1) = "," &                  ~
                str(xyr1$(c%),2) & "Fut"
L16400:     if xytd1$(c%) <> "Y" then return
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",YTD"
            return

        report_eraser
            l% = 0% : page%, pmax% = 1% : l7fac$ = hex(9c)
            init (" ") rept_name$(), rept_desc$(), rept_date$(),         ~
                summ_file$(), grcd1$(), grcd2$(), grp1_desc$(),          ~
                grp2_desc$(), rept_type$(), summ_detl$(), rank_colm$(),  ~
                asnd_dsnd$(), max_print$(), page_brk$(), yr1$(), ytd1$(),~
                fc1$(), oper$(), yr2$(), ytd2$(), fc2$(), ffmt$(),       ~
                colahdr$(), colbhdr$(), colchdr$(), coldhdr$(),          ~
                colehdr$(), colfhdr$(), colghdr$(), incl$()
            return

        workfile_outputter:
            for n% = 1% to 13%
              if periods$(n%) = " " or ~
                 periods$(n%) = blankdate$ then return
                date$ = periods$(1%) : call "DATEFMT" (date$)
                     str(workdesc$,,8) = date$
                     if open% = 1% then str(workdesc$,10) = "(Open)"
                date$ = periods$(n%) : call "DATEFMT" (date$)
                     str(workdesc$,16) = date$ & "    ##"
                     convert n% to str(workdesc$,28,2), pic(##)
                write #10 using L16660, periods$(1%), periods$(n%),       ~
                                       workdesc$, eod goto L16670
L16660:                   FMT  CH(6), CH(6), CH(50)
L16670:     next n%
            return


        test_report_date
            if test_date$ = " " or ~
               test_date$ = blankdate$ then goto L16810
                call "DATEOK" (test_date$, u3%, errormsg$)
                if errormsg$ <> " " then return
                     call "DATUNFMT" (test_date$)
                     call "REDALT0" (#10, test_date$, 1%, f1%(10))
                     call "DATEFMT" (test_date$)
                     if f1%(10) = 0% then                                ~
                               errormsg$ = "Not a Period Beginning Date."
                     return
L16810:     msg$ = hex(06) & "Select the SA 'Year' to report on."

    if test_date$ = " " then test_date$ = blankdate$
       dsc_map(1) = 13.15
       dsc_map(2) = 1.00
       call "PLOWCODE" (#10, test_date$, msg$, 9000%, 0.15,       ~
            f1%(10), plowhdr$(), 6, 0, incl_excl(), incl_excl$(),    ~
            "D", " ", #10, dsc_map())

       if f1%(10) = 0% then L16940 else str(test_date$,7) = hex(00)

       msg$ = hex(06) & "Select the SA Period to report on."
       call "PLOWCODE" (#10, test_date$, msg$, 6006%, 0.32,         ~
            f1%(10), plowhdr$(), 0, 28, incl_excl(), incl_excl$(),   ~
            "D")
       if f1%(10) = 0% then L16940 else test_date$ = str(test_date$,7)
          call "DATEFMT" (test_date$)
        return
L16940: errormsg$ = "The report date must be one of the period dates "&~
                "for one of the S/A 'years'."
        call "DATEFMT" (test_date$)
        return


        REM *************************************************************~
            *        E X I T   T O   P R I N T   R E P O R T S          *~
            *-----------------------------------------------------------*~
            * Offers the operator several end-of-job options- Delete    *~
            * previously-selected reports, Add to previously-selected   *~
            * reports, return to selection, return to menu, exit to     *~
            * print selected reports, etc.                              *~
            *************************************************************
        print_reports
            inpmessage$ = " " : eojtab$ = hex(01ffffff10ff0d0f)
            convert d% to d$,pic(###,###):call "STRING" addr("LJ", d$, 7%)
            convert l% to l$,pic(###,###):call "STRING" addr("LJ", l$, 7%)
            if d% = 0% then goto L19110
                dfac$ = hex(84) : eoj10fac$, eoj32fac$ = hex(8c)
                eojtab$ = and hex(ff0affffff20ffff)
                str(eoj10$, 7) = "(10)  DELETE (from holding file) the" &~
                              dfac$ & d$ & eoj10fac$ & "previous reports"
                str(eoj32$, 7) = "(32)  Exit to PRINT the" & dfac$ & d$ &~
                                eoj32fac$ & "reports in the holding file"
                goto L19115
L19110:     dfac$, eoj10fac$, eoj32fac$ = hex(9c) : eoj10$, eoj32$ = " "
L19115:     if l% = 0% then goto L19155
                lfac$ = hex(84) : eoj11fac$, eoj12fac$ = hex(8c)
                eojtab$ = and hex(ffff0b0cffffffff)
                str(eoj11$, 7) = "(11)  CANCEL the" & lfac$ & l$ &       ~
                              eoj11fac$ & "reports selected this session"
                str(eoj12$, 7) = "(12)  SAVE (in holding file) the"    & ~
                          lfac$ & l$ & eoj12fac$ & "reports this session"
                goto print_reports_2
L19155:     lfac$, eoj11fac$, eoj12fac$ = hex(9c) : eoj11$, eoj12$ = " "

        print_reports_2
          gosub'104
            if keyhit% =   1% then inputmode
            if keyhit% <> 10% then L19245
L19185:         ret% = 0%
                call "ASKUSER" (ret%, "*** VERIFY DELETION ***",         ~
                     "Press PF(10) to DELETE (from holding file) the " & ~
                     "previous reports for user " & userid$, "-- OR --", ~
                     "Press (RETURN) to abort deletion")
                if ret%  =  0% then print_reports_2
                if ret% <> 10% then L19185
                     gosub stand_by
                     plowkey$ = str(userid$) & hex(00000000)
                     call "DELETE" (#9, plowkey$, 3%)
                     d%, f% = 0%
                     goto print_reports
L19245:     if keyhit% <> 11% then L19295
L19250:         ret% = 0%
                call "ASKUSER" (ret%, "*** VERIFY CANCELLATION ***",     ~
                     "Press PF(11) to CANCEL reports selected this sess"&~
                     "ion", "-- OR --",                                  ~
                     "Press (RETURN) to abort cancellation")
                if ret%  =  0% then print_reports_2
                if ret% <> 11% then L19250
                     gosub stand_by
                     goto  erasemode
L19295:     if keyhit% <> 12% then L19400
                gosub stand_by
                for m% = 1% to l%
                     call "DATUNFMT" (rept_date$(m%))
                     if rept_name$(m%) = " " then goto L19385
                     f% = f% + 1%
                     write #9 using L35410, userid$, f%, rept_name$(m%),  ~
                          rept_desc$(m%), rept_date$(m%), summ_file$(m%),~
                          grcd1$(m%), grcd2$(m%), grp1_desc$(m%),        ~
                          grp2_desc$(m%),                                ~
                          rept_type$(m%), summ_detl$(m%), rank_colm$(m%),~
                          asnd_dsnd$(m%), max_print$(m%), page_brk$(m%), ~
                          yr1$(m%),                                      ~
                          ytd1$(m%), fc1$(m%), oper$(m%), yr2$(m%),      ~
                          ytd2$(m%), fc2$(m%), ffmt$(m%), colahdr$(m%),  ~
                          colbhdr$(m%), colchdr$(m%), coldhdr$(m%),      ~
                          colehdr$(m%), colfhdr$(m%), colghdr$(m%),      ~
                          incl$(m%), data goto L19385
                     d% = d% + 1%
L19385:              call "DATEFMT" (rept_date$(m%))
                next m%
                gosub report_eraser
                goto  print_reports
L19400:     if keyhit% <> 16% then goto L19450
                if l% < 1% then goto exit_program
L19410:         ret% = 0%
                call "ASKUSER" (ret%, l$ & " " & "REPORTS SELECTED",     ~
                     "Press PF(16) to CANCEL the reports selected this "&~
                     "session and EXIT", "-- OR --",                     ~
                     "Press (RETURN) to continue with report selection")
                if ret% = 16% then exit_program
                if ret% <> 0% then L19410
                goto print_reports_2
L19450:     if keyhit% <> 32% then goto print_reports_2
                if l% < 1% then goto access_background_task
L19460:         ret% = 0%
                call "ASKUSER" (ret%, l$ & " " & "REPORTS SELECTED",     ~
                     "Press PF(32) to CANCEL the reports selected this "&~
                     "session and exit to PRINT", "-- OR --",            ~
                     "Press (RETURN) to continue with report selection")
                if ret% = 32% then access_background_task
                if ret% <> 0% then L19460
                goto print_reports_2

        access_background_task
            close ws : call "TASKUP" ("ME", 0%) : goto exit_program

        stand_by: call "SHOSTAT" ("Please stand by...") : return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                enabled% = 1%
                if l% > 95% then goto L20300
                on fieldnr% gosub   L20150,         /* As Of Date       */~
                                    L20200,         /* Report Format Def*/~
                                    L20250          /* Report Group Name*/
                return

L20150
*        Default As Of Date                    ASOF_DATE$
            inpmessage$ = "Enter the period beginning date.  Leave bla" &~
                          "nk to see dates on file."
            return

L20200: REM Report Format Definition              REPT_DEF$
            inpmessage$ = "Enter a Report Format Definition name or par"&~
                          "tial to see available codes."
            return

L20250: REM Report Group Name                     REPT_GRP$
            inpmessage$ = "Enter a Report Group name or partial to see "&~
                          "available codes."
            return

L20300: REM CAN'T ADD ANY MORE ENTRIES -- MAX OF 96 REACHED *************
            inpmessage$ = "You may edit the selected reports, edit repo"&~
                          "rt groups, or exit the program."
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   S C R E E N   2 *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21250,         /* Report Group name*/~
                                    L21300,         /* Report Group desc*/~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340,         /* Report Def name  */~
                                    L21340          /* Report Def name  */
                     return

L21250: REM Report Group Name                     REPT_GRP$
            inpmessage$ = "Enter a Report Group name or partial to see "&~
                          "available codes."
            return

L21300: REM Report group description
            inpmessage$ = "Enter a description of this group."
            return

L21340: REM Report Format Definition              REPT_DEF$
            inpmessage$ = "Enter a Report Format Definition name or par"&~
                          "tial to see available codes."
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L22240,         /* Report Date      */~
                                    L22290,         /* Group Code 1 from*/~
                                    L22340,         /* Group Code 1 to  */~
                                    L22390,         /* Group Code 1 desc*/~
                                    L22470,         /* Group Code 2 from*/~
                                    L22550,         /* Group Code 2 to  */~
                                    L22630,         /* Group Code 2 desc*/~
                                    L22740,         /* Report Type      */~
                                    L22790,         /* Summary or Detail*/~
                                    L22840,         /* Rank on Column   */~
                                    L22920,         /* Ascend or Descend*/~
                                    L23000,         /* Max to print     */~
                                    L23080,         /* Page break code  */~
                                    L23150          /* Incl outside yrs */
                     return

L22240: REM Report As-Of Date
            inpmessage$ = "Enter the report date.  Must be a valid " &   ~
                          "period date."
            return

L22290: REM Group Code 1 'FROM'
            inpmessage$ = "Enter 'from' range value for " &              ~
                          codes$(group1%, 2%) & ", 'FIRST', or 'ALL'."
            return

L22340: REM Group Code 1 'TO'
            inpmessage$ = "Enter 'to' range value for " &                ~
                          codes$(group1%, 2%) & ", or 'LAST'."
            return

L22390: REM Group Code 1 Desc                     GRP1_DESC$
            n% = min(grp1_max%, grp1_desc% + posn_rem%)
            convert n% to str(junk$,,2), pic (##)
            call "STRING" addr ("LJ", str(junk$,,2), 2%)
            inpmessage$ = "Enter the length of the description for " &   ~
                     codes$(group1%, 2%) & ".  Maximum: " & str(junk$,,2)
            return

L22470: REM Group Code 2 'FROM'
            if group2% <> 0 then goto L22510
                init (" ") grcd2$(), code2$(), grp2_desc$(m%)
                enabled% = 0% : return
L22510:     inpmessage$ = "Enter 'from' range value for " &              ~
                          codes$(group2%, 2%) & ", 'FIRST', or 'ALL'."
            return

L22550: REM Group Code 2 'TO'
            if group2% <> 0 then goto L22590
                init (" ") grcd2$(), code2$(), grp2_desc$(m%)
                enabled% = 0% : return
L22590:     inpmessage$ = "Enter 'to' range value for " &                ~
                          codes$(group2%, 2%) & ", or 'LAST'."
            return

L22630: REM Group Code 2 Desc                     GRP2_DESC$
            if group2% <> 0 then goto L22670
                init (" ") grcd2$(), code2$(), grp2_desc$(m%)
                enabled% = 0% : return
L22670:     n% = min(grp2_max%, grp2_desc% + posn_rem%)
            convert n% to str(junk$,,2), pic (##)
            call "STRING" addr ("LJ", str(junk$,,2), 2%)
            inpmessage$ = "Enter the length of the description for " &   ~
                     codes$(group2%, 2%) & ".  Maximum: " & str(junk$,,2)
            return

L22740: REM Report Type                           REPT_TYPE$
            inpmessage$ = "Enter the type of report: 'H' (History), 'R'"&~
                          " (Ranking) or 'B' (Both)."
            return

L22790: REM Summary or Detail                     SUMM_DETL$
            inpmessage$ = "Enter 'S' for Summary report; 'D' for a Deta"&~
                          "il report."
            return

L22840: REM Rank on Column                        RANK_COLM$
            if rept_type$(m%) <> "H" then goto L22870
                enabled% = 0% : return
L22870:     if rank_colm$(m%) = " " then rank_colm$(m%) = "A"
            inpmessage$ = "Enter the column to rank (sort) the report o"&~
                          "n (A thru G)."
            return

L22920: REM Ascend or Descend                     ASND_DSND$
            if rept_type$(m%) <> "H" then goto L22950
                enabled% = 0% : return
L22950:     if asnd_dsnd$(m%) = " " then asnd_dsnd$(m%) = "A"
            inpmessage$ = "Enter 'A' to rank in ascending sequence; 'D'"&~
                          " for descending."
            return

L23000: REM Max to print                          MAX_PRINT$
            if rept_type$(m%) <> "H" then goto L23030
                enabled% = 0% : return
L23030:     if max_print$(m%) = " " then max_print$(m%) = "ALL"
            inpmessage$ = "Enter the maximum number of items to print o"&~
                          "r 'ALL'."
            return

L23080: REM Page Break code                       PAGE_BRK$
            if summ_detl$(m%) <> "S" then goto L23110
                enabled% = 0% : return
L23110:     inpmessage$ = "Enter 'Y' to force page breaks at " &         ~
                      codes$(group1%, 2) & " totals; 'N' to omit breaks."
            return

L23150: REM Include groups outside specified years
            inpmessage$ = "Should Report Include Summary Groups that " & ~
                          "have no Activity in Years Specified?"
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
                on sover% goto erasemode, input_groups
                goto inputmode


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            rept_date$(l%) = asof_date$
            get #2 using L35050, rdef_key$, rept_desc$(l%),               ~
                summ_file$(l%), grcd1$(l%), grcd2$(l%), grp1_desc$(l%),  ~
                grp2_desc$(l%),                                          ~
                rept_type$(l%), summ_detl$(l%), rank_colm$(l%),          ~
                asnd_dsnd$(l%), max_print$(l%), page_brk$(l%),           ~
                yr1$(l%), ytd1$(l%),                                     ~
                fc1$(l%), oper$(l%), yr2$(l%), ytd2$(l%), fc2$(l%),      ~
                ffmt$(l%), colahdr$(l%), colbhdr$(l%), colchdr$(l%),     ~
                coldhdr$(l%), colehdr$(l%), colfhdr$(l%), colghdr$(l%),  ~
                incl$(l%)
            rept_name$(l%) = str(rdef_key$,11)
            if incl$(l%) = " " then incl$(l%) = "N"
            return


        REM *************************************************************~
            *               R E C O R D   L A Y O U T S                 *~
            *************************************************************

L35050:     FMT /* SA.RPTDEF.xxxxxxxxxx record layout in file SYSFILE2 */~
                CH(20),                  /* Report Format Def name     */~
                CH(30),                  /* Report Format Def descriptn*/~
                CH(1),                   /* S/A Summary file #         */~
                CH(50),                  /* Group 1 range values       */~
                CH(50),                  /* Group 2 range values       */~
                CH(2),                   /* Group 1 description length */~
                CH(2),                   /* Group 2 description length */~
                CH(1),                   /* Type of report (H, R, B)   */~
                CH(1),                   /* Summary or Detail (S, D)   */~
                CH(1),                   /* Rank on Column (A - G)     */~
                CH(1),                   /* Ascending or Descending    */~
                CH(3),                   /* Max # to print (ALL, n)    */~
                CH(1),                   /* Page break code            */~
                CH(14),                  /* 1st fld Year rel to current*/~
                CH(7),                   /* 1st fld YTD indicator      */~
                CH(7),                   /* 1st fld code/column entry  */~
                CH(7),                   /* Column operator            */~
                CH(14),                  /* 2nd fld Year rel to current*/~
                CH(7),                   /* 2nd fld YTD indicator      */~
                CH(7),                   /* 2nd fld code/column entry  */~
                CH(21),                  /* Column format- 'nnd'       */~
                CH(30),                  /* Column A report headers    */~
                CH(30),                  /* Column B report headers    */~
                CH(30),                  /* Column C report headers    */~
                CH(30),                  /* Column D report headers    */~
                CH(30),                  /* Column E report headers    */~
                CH(30),                  /* Column F report headers    */~
                CH(30),                  /* Column G report headers    */~
                CH(1) ,                  /* Include codes ouside yrs   */~
                XX(42)                   /* Filler                     */

L35360:     FMT /* SA.RPTGRP.xxxxxxxxxx record layout in file SYSFILE2 */~
                CH(20),                  /* Report Group name          */~
                CH(30),                  /* Report Group description   */~
                12*CH(10),               /* Possible 12 report names   */~
                CH(200),                 /* Filler                     */~
                CH(130)                  /* Filler                     */

L35410:     FMT /* Report record layout in file SAREPORT               */~
                CH(3),                   /* User ID                    */~
                BI(4),                   /* Sequence number            */~
                CH(10),                  /* Report Format Def name     */~
                CH(30),                  /* Report Format Def descriptn*/~
                CH(8),                   /* Report Date                */~
                CH(1),                   /* S/A Summary file #         */~
                CH(50),                  /* Group 1 range values       */~
                CH(50),                  /* Group 2 range values       */~
                CH(2),                   /* Group 1 description length */~
                CH(2),                   /* Group 2 description length */~
                CH(1),                   /* Type of report (H, R, B)   */~
                CH(1),                   /* Summary or Detail (S, D)   */~
                CH(1),                   /* Rank on Column (A - G)     */~
                CH(1),                   /* Ascending or Descending    */~
                CH(3),                   /* Max # to print (ALL, n)    */~
                CH(1),                   /* Page break code            */~
                CH(14),                  /* 1st fld Year rel to current*/~
                CH(7),                   /* 1st fld YTD indicator      */~
                CH(7),                   /* 1st fld code/column entry  */~
                CH(7),                   /* Column operator            */~
                CH(14),                  /* 2nd fld Year rel to current*/~
                CH(7),                   /* 2nd fld YTD indicator      */~
                CH(7),                   /* 2nd fld code/column entry  */~
                CH(21),                  /* Column format- 'nnd'       */~
                CH(30),                  /* Column A report headers    */~
                CH(30),                  /* Column B report headers    */~
                CH(30),                  /* Column C report headers    */~
                CH(30),                  /* Column D report headers    */~
                CH(30),                  /* Column E report headers    */~
                CH(30),                  /* Column F report headers    */~
                CH(30),                  /* Column G report headers    */~
                CH(1)                    /* Include codes outside yrs  */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
            convert page% to page$, pic (#)
            line2$ = "Page: " & page$
            str(line2$,62%) = "SARPTSEL: " & str(cms2v$,,8%)
            init(hex(8c)) lfac$() : lfac$(4) = hex(8e)
            on fieldnr%  gosub      L40190,         /* Default As Of Dat*/~
                                    L40190,         /* Report Format Def*/~
                                    L40190          /* Report Group Name*/
            goto fac_setting_done
                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

        fac_setting_done
            z% = ((page% - 1%) * 12%) + 1%
L40280:     accept                                                       ~
               at (01,02), "Sales Analysis: Select Reports for Print",   ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,07), "Default Report Date:",                       ~
               at (04,30), fac(lfac$( 1)), asof_date$           , ch(08),~
                                                                         ~
               at (05,02), "      Report Format:",                       ~
               at (05,30), fac(lfac$( 2)), rept_def$            , ch(10),~
               at (05,45), "-or- Group Name:",                           ~
               at (05,63), fac(lfac$( 3)), rept_grp$            , ch(10),~
                                                                         ~
               at (06,02), fac(l7fac$) , line6$                 , ch(79),~
                                                                         ~
               at (07,07), fac(hex(ac)), l8_date$               , ch(08),~
               at (07,17), fac(hex(ac)), l8_rept$               , ch(10),~
               at (07,29), fac(hex(ac)), l8_desc$               , ch(30),~
               at (07,61), fac(hex(ac)), l8_nr$                 , ch(01),~
               at (07,63), fac(hex(ac)), l8_gp1$                , ch(03),~
               at (07,67), fac(hex(ac)), l8_gp2$                , ch(03),~
               at (07,71), fac(hex(ac)), l8_typ$                , ch(03),~
               at (07,75), fac(hex(ac)), l8_sd$                 , ch(03),~
                                                                         ~
               at (08,02), fac(lfac$(4)), linenr$(z%)           , ch(03),~
               at (08,07), fac(hex(8c)), rept_date$(z%)         , ch(08),~
               at (08,17), fac(hex(8c)), rept_name$(z%)         , ch(10),~
               at (08,29), fac(hex(8c)), rept_desc$(z%)         , ch(30),~
               at (08,61), fac(hex(8c)), summ_file$(z%)         , ch(01),~
               at (08,63), fac(hex(8c)), grcd1$(z%)             , ch(03),~
               at (08,67), fac(hex(8c)), grcd2$(z%)             , ch(03),~
               at (08,72), fac(hex(8c)), rept_type$(z%)         , ch(01),~
               at (08,76), fac(hex(8c)), summ_detl$(z%)         , ch(01),~
                                                                         ~
               at (09,02), fac(lfac$(4)), linenr$(z%+1%)        , ch(03),~
               at (09,07), fac(hex(8c)), rept_date$(z%+1%)      , ch(08),~
               at (09,17), fac(hex(8c)), rept_name$(z%+1%)      , ch(10),~
               at (09,29), fac(hex(8c)), rept_desc$(z%+1%)      , ch(30),~
               at (09,61), fac(hex(8c)), summ_file$(z%+1%)      , ch(01),~
               at (09,63), fac(hex(8c)), grcd1$(z%+1%)          , ch(03),~
               at (09,67), fac(hex(8c)), grcd2$(z%+1%)          , ch(03),~
               at (09,72), fac(hex(8c)), rept_type$(z%+1%)      , ch(01),~
               at (09,76), fac(hex(8c)), summ_detl$(z%+1%)      , ch(01),~
                                                                         ~
               at (10,02), fac(lfac$(4)), linenr$(z%+2%)        , ch(03),~
               at (10,07), fac(hex(8c)), rept_date$(z%+2%)      , ch(08),~
               at (10,17), fac(hex(8c)), rept_name$(z%+2%)      , ch(10),~
               at (10,29), fac(hex(8c)), rept_desc$(z%+2%)      , ch(30),~
               at (10,61), fac(hex(8c)), summ_file$(z%+2%)      , ch(01),~
               at (10,63), fac(hex(8c)), grcd1$(z%+2%)          , ch(03),~
               at (10,67), fac(hex(8c)), grcd2$(z%+2%)          , ch(03),~
               at (10,72), fac(hex(8c)), rept_type$(z%+2%)      , ch(01),~
               at (10,76), fac(hex(8c)), summ_detl$(z%+2%)      , ch(01),~
                                                                         ~
               at (11,02), fac(lfac$(4)), linenr$(z%+3%)        , ch(03),~
               at (11,07), fac(hex(8c)), rept_date$(z%+3%)      , ch(08),~
               at (11,17), fac(hex(8c)), rept_name$(z%+3%)      , ch(10),~
               at (11,29), fac(hex(8c)), rept_desc$(z%+3%)      , ch(30),~
               at (11,61), fac(hex(8c)), summ_file$(z%+3%)      , ch(01),~
               at (11,63), fac(hex(8c)), grcd1$(z%+3%)          , ch(03),~
               at (11,67), fac(hex(8c)), grcd2$(z%+3%)          , ch(03),~
               at (11,72), fac(hex(8c)), rept_type$(z%+3%)      , ch(01),~
               at (11,76), fac(hex(8c)), summ_detl$(z%+3%)      , ch(01),~
                                                                         ~
               at (12,02), fac(lfac$(4)), linenr$(z%+4%)        , ch(03),~
               at (12,07), fac(hex(8c)), rept_date$(z%+4%)      , ch(08),~
               at (12,17), fac(hex(8c)), rept_name$(z%+4%)      , ch(10),~
               at (12,29), fac(hex(8c)), rept_desc$(z%+4%)      , ch(30),~
               at (12,61), fac(hex(8c)), summ_file$(z%+4%)      , ch(01),~
               at (12,63), fac(hex(8c)), grcd1$(z%+4%)          , ch(03),~
               at (12,67), fac(hex(8c)), grcd2$(z%+4%)          , ch(03),~
               at (12,72), fac(hex(8c)), rept_type$(z%+4%)      , ch(01),~
               at (12,76), fac(hex(8c)), summ_detl$(z%+4%)      , ch(01),~
                                                                         ~
               at (13,02), fac(lfac$(4)), linenr$(z%+5%)        , ch(03),~
               at (13,07), fac(hex(8c)), rept_date$(z%+5%)      , ch(08),~
               at (13,17), fac(hex(8c)), rept_name$(z%+5%)      , ch(10),~
               at (13,29), fac(hex(8c)), rept_desc$(z%+5%)      , ch(30),~
               at (13,61), fac(hex(8c)), summ_file$(z%+5%)      , ch(01),~
               at (13,63), fac(hex(8c)), grcd1$(z%+5%)          , ch(03),~
               at (13,67), fac(hex(8c)), grcd2$(z%+5%)          , ch(03),~
               at (13,72), fac(hex(8c)), rept_type$(z%+5%)      , ch(01),~
               at (13,76), fac(hex(8c)), summ_detl$(z%+5%)      , ch(01),~
                                                                         ~
               at (14,02), fac(lfac$(4)), linenr$(z%+6%)        , ch(03),~
               at (14,07), fac(hex(8c)), rept_date$(z%+6%)      , ch(08),~
               at (14,17), fac(hex(8c)), rept_name$(z%+6%)      , ch(10),~
               at (14,29), fac(hex(8c)), rept_desc$(z%+6%)      , ch(30),~
               at (14,61), fac(hex(8c)), summ_file$(z%+6%)      , ch(01),~
               at (14,63), fac(hex(8c)), grcd1$(z%+6%)          , ch(03),~
               at (14,67), fac(hex(8c)), grcd2$(z%+6%)          , ch(03),~
               at (14,72), fac(hex(8c)), rept_type$(z%+6%)      , ch(01),~
               at (14,76), fac(hex(8c)), summ_detl$(z%+6%)      , ch(01),~
                                                                         ~
               at (15,02), fac(lfac$(4)), linenr$(z%+7%)        , ch(03),~
               at (15,07), fac(hex(8c)), rept_date$(z%+7%)      , ch(08),~
               at (15,17), fac(hex(8c)), rept_name$(z%+7%)      , ch(10),~
               at (15,29), fac(hex(8c)), rept_desc$(z%+7%)      , ch(30),~
               at (15,61), fac(hex(8c)), summ_file$(z%+7%)      , ch(01),~
               at (15,63), fac(hex(8c)), grcd1$(z%+7%)          , ch(03),~
               at (15,67), fac(hex(8c)), grcd2$(z%+7%)          , ch(03),~
               at (15,72), fac(hex(8c)), rept_type$(z%+7%)      , ch(01),~
               at (15,76), fac(hex(8c)), summ_detl$(z%+7%)      , ch(01),~
                                                                         ~
               at (16,02), fac(lfac$(4)), linenr$(z%+8%)        , ch(03),~
               at (16,07), fac(hex(8c)), rept_date$(z%+8%)      , ch(08),~
               at (16,17), fac(hex(8c)), rept_name$(z%+8%)      , ch(10),~
               at (16,29), fac(hex(8c)), rept_desc$(z%+8%)      , ch(30),~
               at (16,61), fac(hex(8c)), summ_file$(z%+8%)      , ch(01),~
               at (16,63), fac(hex(8c)), grcd1$(z%+8%)          , ch(03),~
               at (16,67), fac(hex(8c)), grcd2$(z%+8%)          , ch(03),~
               at (16,72), fac(hex(8c)), rept_type$(z%+8%)      , ch(01),~
               at (16,76), fac(hex(8c)), summ_detl$(z%+8%)      , ch(01),~
                                                                         ~
               at (17,02), fac(lfac$(4)), linenr$(z%+9%)        , ch(03),~
               at (17,07), fac(hex(8c)), rept_date$(z%+9%)      , ch(08),~
               at (17,17), fac(hex(8c)), rept_name$(z%+9%)      , ch(10),~
               at (17,29), fac(hex(8c)), rept_desc$(z%+9%)      , ch(30),~
               at (17,61), fac(hex(8c)), summ_file$(z%+9%)      , ch(01),~
               at (17,63), fac(hex(8c)), grcd1$(z%+9%)          , ch(03),~
               at (17,67), fac(hex(8c)), grcd2$(z%+9%)          , ch(03),~
               at (17,72), fac(hex(8c)), rept_type$(z%+9%)      , ch(01),~
               at (17,76), fac(hex(8c)), summ_detl$(z%+9%)      , ch(01),~
                                                                         ~
               at (18,02), fac(lfac$(4)), linenr$(z%+10%)       , ch(03),~
               at (18,07), fac(hex(8c)), rept_date$(z%+10%)     , ch(08),~
               at (18,17), fac(hex(8c)), rept_name$(z%+10%)     , ch(10),~
               at (18,29), fac(hex(8c)), rept_desc$(z%+10%)     , ch(30),~
               at (18,61), fac(hex(8c)), summ_file$(z%+10%)     , ch(01),~
               at (18,63), fac(hex(8c)), grcd1$(z%+10%)         , ch(03),~
               at (18,67), fac(hex(8c)), grcd2$(z%+10%)         , ch(03),~
               at (18,72), fac(hex(8c)), rept_type$(z%+10%)     , ch(01),~
               at (18,76), fac(hex(8c)), summ_detl$(z%+10%)     , ch(01),~
                                                                         ~
               at (19,02), fac(lfac$(4)), linenr$(z%+11%)       , ch(03),~
               at (19,07), fac(hex(8c)), rept_date$(z%+11%)     , ch(08),~
               at (19,17), fac(hex(8c)), rept_name$(z%+11%)     , ch(10),~
               at (19,29), fac(hex(8c)), rept_desc$(z%+11%)     , ch(30),~
               at (19,61), fac(hex(8c)), summ_file$(z%+11%)     , ch(01),~
               at (19,63), fac(hex(8c)), grcd1$(z%+11%)         , ch(03),~
               at (19,67), fac(hex(8c)), grcd2$(z%+11%)         , ch(03),~
               at (19,72), fac(hex(8c)), rept_type$(z%+11%)     , ch(01),~
               at (19,76), fac(hex(8c)), summ_detl$(z%+11%)     , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(pfxfac$(1)), pf1$                ,        ~
               at (22,21), fac(pfxfac$(4)), pf4$                ,        ~
               at (22,42), fac(pfxfac$(8)), pf8$                ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,21), fac(pfxfac$(5)), pf5$                ,        ~
               at (23,42), fac(pfxfac$(9)), pf9$                ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
               at (24,02), fac(pfxfac$(3)), pf3$                ,        ~
               at (24,21), fac(pfxfac$(6)), pf6$                ,        ~
               at (24,42), fac(pfxfac$(16)), pf32$              ,        ~
               at (24,65), fac(pfxfac$(16)), pf16$              ,        ~
                     keys(keytab$), key (keyhit%)

               if keyhit% <> 13 then L41920
                  call "MANUAL" ("SARPTSEL")
                  goto L40280

L41920:        if keyhit% <> 15 then L41960
                  call "PRNTSCRN"
                  goto L40280

L41960:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
            line2$ = " ":str(line2$,62%) = "SARPTSEL: " & str(cms2v$,,8%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr%  gosub      L42300,         /* Report group name*/~
                                    L42270,         /* Report group desc*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300,         /* Report Format Def*/~
                                    L42300          /* Report Group Name*/
            goto L42370
L42270:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L42300:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L42370:     accept                                                       ~
               at (01,02), "Sales Analysis: Input & Edit Report Groups", ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Report Group Name:",                         ~
               at (05,23), fac(lfac$( 1)), rept_grp$            , ch(10),~
               at (05,35), fac(lfac$( 2)), rgrp_desc$           , ch(30),~
                                                                         ~
               at (07,10), fac(hex(ac)), l8_rept$               , ch(10),~
               at (07,23), fac(hex(ac)), l8_desc$               , ch(30),~
                                                                         ~
               at (08,02), fac(hex(8c)),   linenr$(1%)          , ch(03),~
               at (08,10), fac(lfac$( 3)), rept_array$(1%)      , ch(10),~
               at (08,23), fac(hex(8c)),   rept_ardsc$(1%)      , ch(30),~
                                                                         ~
               at (09,02), fac(hex(8c)),   linenr$(2%)          , ch(03),~
               at (09,10), fac(lfac$( 4)), rept_array$(2%)      , ch(10),~
               at (09,23), fac(hex(8c)),   rept_ardsc$(2%)      , ch(30),~
                                                                         ~
               at (10,02), fac(hex(8c)),   linenr$(3%)          , ch(03),~
               at (10,10), fac(lfac$( 5)), rept_array$(3%)      , ch(10),~
               at (10,23), fac(hex(8c)),   rept_ardsc$(3%)      , ch(30),~
                                                                         ~
               at (11,02), fac(hex(8c)),   linenr$(4%)          , ch(03),~
               at (11,10), fac(lfac$( 6)), rept_array$(4%)      , ch(10),~
               at (11,23), fac(hex(8c)),   rept_ardsc$(4%)      , ch(30),~
                                                                         ~
               at (12,02), fac(hex(8c)),   linenr$(5%)          , ch(03),~
               at (12,10), fac(lfac$( 7)), rept_array$(5%)      , ch(10),~
               at (12,23), fac(hex(8c)),   rept_ardsc$(5%)      , ch(30),~
                                                                         ~
               at (13,02), fac(hex(8c)),   linenr$(6%)          , ch(03),~
               at (13,10), fac(lfac$( 8)), rept_array$(6%)      , ch(10),~
               at (13,23), fac(hex(8c)),   rept_ardsc$(6%)      , ch(30),~
                                                                         ~
               at (14,02), fac(hex(8c)),   linenr$(7%)          , ch(03),~
               at (14,10), fac(lfac$( 9)), rept_array$(7%)      , ch(10),~
               at (14,23), fac(hex(8c)),   rept_ardsc$(7%)      , ch(30),~
                                                                         ~
               at (15,02), fac(hex(8c)),   linenr$(8%)          , ch(03),~
               at (15,10), fac(lfac$(10)), rept_array$(8%)      , ch(10),~
               at (15,23), fac(hex(8c)),   rept_ardsc$(8%)      , ch(30),~
                                                                         ~
               at (16,02), fac(hex(8c)),   linenr$(9%)          , ch(03),~
               at (16,10), fac(lfac$(11)), rept_array$(9%)      , ch(10),~
               at (16,23), fac(hex(8c)),   rept_ardsc$(9%)      , ch(30),~
                                                                         ~
               at (17,02), fac(hex(8c)),   linenr$(10%)         , ch(03),~
               at (17,10), fac(lfac$(12)), rept_array$(10%)     , ch(10),~
               at (17,23), fac(hex(8c)),   rept_ardsc$(10%)     , ch(30),~
                                                                         ~
               at (18,02), fac(hex(8c)),   linenr$(11%)         , ch(03),~
               at (18,10), fac(lfac$(13)), rept_array$(11%)     , ch(10),~
               at (18,23), fac(hex(8c)),   rept_ardsc$(11%)     , ch(30),~
                                                                         ~
               at (19,02), fac(hex(8c)),   linenr$(12%)         , ch(03),~
               at (19,10), fac(lfac$(14)), rept_array$(12%)     , ch(10),~
               at (19,23), fac(hex(8c)),   rept_ardsc$(12%)     , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(pfxfac$(1)), pf1$                ,        ~
               at (22,23), fac(pfxfac$(4)), pf4$                ,        ~
               at (22,42), fac(pfxfac$(12)), pf12$              ,        ~
               at (22,63), "(13)Instructions"                   ,        ~
               at (23,63), "(15)Print Screen"                   ,        ~
               at (24,02), fac(pfxfac$(3)), pf3$                ,        ~
               at (24,63), fac(pfxfac$(16)), pf16$              ,        ~
                     keys(keytab$), key (keyhit%)

               if keyhit% <> 13 then L43180
                  call "MANUAL" ("SARPTSEL")
                  goto L42370

L43180:        if keyhit% <> 15 then L43230
                  call "PRNTSCRN"
                  goto L42370

L43230:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'103(fieldnr%)
                line2$ = summ_desc$
                  str(line2$,62%) = "SARPTSEL: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L45310,         /* Report Date      */~
                                    L45310,         /* Group Code 1 from*/~
                                    L45310,         /* Group Code 1 to  */~
                                    L45340,         /* Group Code 1 desc*/~
                                    L45310,         /* Group Code 2 from*/~
                                    L45310,         /* Group Code 2 to  */~
                                    L45340,         /* Group Code 2 desc*/~
                                    L45310,         /* Report Type      */~
                                    L45310,         /* Summary or Detail*/~
                                    L45310,         /* Rank on Column   */~
                                    L45310,         /* Ascend or Descend*/~
                                    L45310,         /* Max to print     */~
                                    L45310,         /* Page break code  */~
                                    L45310          /* Incl outside yrs */

                  goto fac_setting_done_3

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L45310:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L45340:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

        fac_setting_done_3
            gosub compute_posn_rem
L45400:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis: Customize Selected Reports",          ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,04), "Customizing:",                               ~
               at (04,17), fac(hex(8c))  , linenr$(m%)          , ch( 3),~
               at (04,21), fac(lfac$( 1)), rept_date$(m%)       , ch( 8),~
               at (04,32), fac(hex(8c))  , rept_name$(m%)       , ch(10),~
               at (04,45), fac(hex(8c))  , rept_desc$(m%)       , ch(30),~
                                                                         ~
               at (05,02), fac(hex(8c))  , group1$              , ch(14),~
               at (05,17), fac(lfac$( 2)), code1$(1)            ,        ~
               at (05,43),                 "-"                  ,        ~
               at (05,45), fac(lfac$( 3)), code1$(2)            ,        ~
               at (05,71),                 "Desc:"              ,        ~
               at (05,77), fac(lfac$( 4)), grp1_desc$(m%)       , ch(02),~
                                                                         ~
               at (06,02), fac(hex(8c))  , group2$              , ch(14),~
               at (06,17), fac(lfac$( 5)), code2$(1)            ,        ~
               at (06,43),                 "-"                  ,        ~
               at (06,45), fac(lfac$( 6)), code2$(2)            ,        ~
               at (06,71),                 "Desc:"              ,        ~
               at (06,77), fac(lfac$( 7)), grp2_desc$(m%)       , ch(02),~
                                                                         ~
               at (07,02), "Type:",                                      ~
               at (07,08), fac(lfac$( 8)), rept_type$(m%)       , ch(01),~
               at (07,11), "S/D:",                                       ~
               at (07,16), fac(lfac$( 9)), summ_detl$(m%)       , ch(01),~
               at (07,19), "Rank on:",                                   ~
               at (07,28), fac(lfac$(10)), rank_colm$(m%)       , ch(01),~
               at (07,31), "A/D:",                                       ~
               at (07,36), fac(lfac$(11)), asnd_dsnd$(m%)       , ch(01),~
               at (07,39), "# to print:",                                ~
               at (07,51), fac(lfac$(12)), max_print$(m%)       , ch(03),~
               at (07,56), "Brk?",                                       ~
               at (07,61), fac(lfac$(13)), page_brk$(m%)        , ch(01),~
               at (07,64), "Inc?",                                       ~
               at (07,69), fac(lfac$(14)), incl$(m%)            , ch(01),~
               at (07,72), "Pos:",                                       ~
               at (07,77), fac(hex(8c)),   posn_rem$            , ch(04),~
                                                                         ~
               at (08,02), fac(hex(ac)), col$                   , ch(03),~
               at (08,06), fac(hex(ac)), fc$                    , ch(03),~
               at (08,10), fac(hex(ac)), yr$                    , ch(02),~
               at (08,13), fac(hex(ac)), ytd$                   , ch(04),~
               at (08,18), fac(hex(ac)), op$                    , ch(04),~
               at (08,23), fac(hex(ac)), fc$                    , ch(03),~
               at (08,27), fac(hex(ac)), yr$                    , ch(02),~
               at (08,30), fac(hex(ac)), ytd$                   , ch(04),~
               at (08,35), fac(hex(ac)), fmt$                   , ch(03),~
               at (08,39), fac(hex(ac)), coldesc$               , ch(42),~
                                                                         ~
               at (09,03), "A"                                  ,        ~
               at (09,07), fac(hex(8c))  ,xfc1$(1)              , ch(01),~
               at (09,10), fac(hex(8c))  ,xyr1$(1)              , ch(02),~
               at (09,14), fac(hex(8c))  ,xytd1$(1)             , ch(01),~
               at (09,19), fac(hex(8c))  ,xoper$(1)             , ch(01),~
               at (09,24), fac(hex(8c))  ,xfc2$(1)              , ch(01),~
               at (09,27), fac(hex(8c))  ,xyr2$(1)              , ch(02),~
               at (09,31), fac(hex(8c))  ,xytd2$(1)             , ch(01),~
               at (09,35), fac(hex(8c))  ,xffmt$(1)             , ch(03),~
               at (09,39), fac(hex(8c))  ,cdescr$(1)            , ch(42),~
                                                                         ~
               at (10,03), "B"                                  ,        ~
               at (10,07), fac(hex(8c))  ,xfc1$(2)              , ch(01),~
               at (10,10), fac(hex(8c))  ,xyr1$(2)              , ch(02),~
               at (10,14), fac(hex(8c))  ,xytd1$(2)             , ch(01),~
               at (10,19), fac(hex(8c))  ,xoper$(2)             , ch(01),~
               at (10,24), fac(hex(8c))  ,xfc2$(2)              , ch(01),~
               at (10,27), fac(hex(8c))  ,xyr2$(2)              , ch(02),~
               at (10,31), fac(hex(8c))  ,xytd2$(2)             , ch(01),~
               at (10,35), fac(hex(8c))  ,xffmt$(2)             , ch(03),~
               at (10,39), fac(hex(8c))  ,cdescr$(2)            , ch(42),~
                                                                         ~
               at (11,03), "C"                                  ,        ~
               at (11,07), fac(hex(8c))  ,xfc1$(3)              , ch(01),~
               at (11,10), fac(hex(8c))  ,xyr1$(3)              , ch(02),~
               at (11,14), fac(hex(8c))  ,xytd1$(3)             , ch(01),~
               at (11,19), fac(hex(8c))  ,xoper$(3)             , ch(01),~
               at (11,24), fac(hex(8c))  ,xfc2$(3)              , ch(01),~
               at (11,27), fac(hex(8c))  ,xyr2$(3)              , ch(02),~
               at (11,31), fac(hex(8c))  ,xytd2$(3)             , ch(01),~
               at (11,35), fac(hex(8c))  ,xffmt$(3)             , ch(03),~
               at (11,39), fac(hex(8c))  ,cdescr$(3)            , ch(42),~
                                                                         ~
               at (12,03), "D"                                  ,        ~
               at (12,07), fac(hex(8c))  ,xfc1$(4)              , ch(01),~
               at (12,10), fac(hex(8c))  ,xyr1$(4)              , ch(02),~
               at (12,14), fac(hex(8c))  ,xytd1$(4)             , ch(01),~
               at (12,19), fac(hex(8c))  ,xoper$(4)             , ch(01),~
               at (12,24), fac(hex(8c))  ,xfc2$(4)              , ch(01),~
               at (12,27), fac(hex(8c))  ,xyr2$(4)              , ch(02),~
               at (12,31), fac(hex(8c))  ,xytd2$(4)             , ch(01),~
               at (12,35), fac(hex(8c))  ,xffmt$(4)             , ch(03),~
               at (12,39), fac(hex(8c))  ,cdescr$(4)            , ch(42),~
                                                                         ~
               at (13,03), "E"                                  ,        ~
               at (13,07), fac(hex(8c))  ,xfc1$(5)              , ch(01),~
               at (13,10), fac(hex(8c))  ,xyr1$(5)              , ch(02),~
               at (13,14), fac(hex(8c))  ,xytd1$(5)             , ch(01),~
               at (13,19), fac(hex(8c))  ,xoper$(5)             , ch(01),~
               at (13,24), fac(hex(8c))  ,xfc2$(5)              , ch(01),~
               at (13,27), fac(hex(8c))  ,xyr2$(5)              , ch(02),~
               at (13,31), fac(hex(8c))  ,xytd2$(5)             , ch(01),~
               at (13,35), fac(hex(8c))  ,xffmt$(5)             , ch(03),~
               at (13,39), fac(hex(8c))  ,cdescr$(5)            , ch(42),~
                                                                         ~
               at (14,03), "F"                                  ,        ~
               at (14,07), fac(hex(8c))  ,xfc1$(6)              , ch(01),~
               at (14,10), fac(hex(8c))  ,xyr1$(6)              , ch(02),~
               at (14,14), fac(hex(8c))  ,xytd1$(6)             , ch(01),~
               at (14,19), fac(hex(8c))  ,xoper$(6)             , ch(01),~
               at (14,24), fac(hex(8c))  ,xfc2$(6)              , ch(01),~
               at (14,27), fac(hex(8c))  ,xyr2$(6)              , ch(02),~
               at (14,31), fac(hex(8c))  ,xytd2$(6)             , ch(01),~
               at (14,35), fac(hex(8c))  ,xffmt$(6)             , ch(03),~
               at (14,39), fac(hex(8c))  ,cdescr$(6)            , ch(42),~
                                                                         ~
               at (15,03), "G"                                  ,        ~
               at (15,07), fac(hex(8c))  ,xfc1$(7)              , ch(01),~
               at (15,10), fac(hex(8c))  ,xyr1$(7)              , ch(02),~
               at (15,14), fac(hex(8c))  ,xytd1$(7)             , ch(01),~
               at (15,19), fac(hex(8c))  ,xoper$(7)             , ch(01),~
               at (15,24), fac(hex(8c))  ,xfc2$(7)              , ch(01),~
               at (15,27), fac(hex(8c))  ,xyr2$(7)              , ch(02),~
               at (15,31), fac(hex(8c))  ,xytd2$(7)             , ch(01),~
               at (15,35), fac(hex(8c))  ,xffmt$(7)             , ch(03),~
               at (15,39), fac(hex(8c))  ,cdescr$(7)            , ch(42),~
                                                                         ~
               at (16,02), fac(hex(ac)), line16$                , ch(79),~
               at (17,02), fac(hex(8c)), line17$                , ch(79),~
               at (18,02), fac(hex(8c)), line18$                , ch(79),~
               at (19,02), fac(hex(8c)), line19$                , ch(79),~
               at (20,02), fac(hex(8c)), line20$                , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), fac(pfxfac$(1)), pf1$                ,        ~
               at (22,21), fac(pfxfac$(4)), pf4$                ,        ~
               at (22,42), fac(pfxfac$(12)), pf12$              ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,21), fac(pfxfac$(5)), pf5$                ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
               at (24,65), fac(pfxfac$(16)), pf16$              ,        ~
                     keys(keytab$), key (keyhit%)

               if keyhit% <> 13 then L46960
                  call "MANUAL" ("SARPTINP")
                  goto L45400

L46960:        if keyhit% <> 15 then L47000
                  call "PRNTSCRN"
                  goto L45400

L47000:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * End of Job Options Screen.                                *~
            *************************************************************

            deffn'104
            inpmessage$ = "Select desired option by pressing PF key."
            line2$ = " "
            str(line2$,62%) = "SARPTSEL: " & str(cms2v$,,8%)

L48120:     accept                                                       ~
                at (01,02), "Sales Analysis Report Selection End of Job",~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                                                                         ~
                at (05,28), "Report Status for User"                    ,~
                at (05,51), fac(hex(8c)), userid$               , ch(03),~
                                                                         ~
                at (08,02), fac(eoj1fac$),  eoj1$                       ,~
                at (10,02), fac(eoj10fac$), eoj10$                      ,~
                at (12,02), fac(eoj11fac$), eoj11$                      ,~
                at (14,02), fac(eoj12fac$), eoj12$                      ,~
                at (16,02), fac(eoj16fac$), eoj16$                      ,~
                                                                         ~
                at (18,02), fac(eoj32fac$), eoj32$                      ,~
                                                                         ~
                at (21,02), fac(hex(ac)),   inpmessage$         , ch(79),~
                at (22,63), "(13)Instructions"                  ,        ~
                at (23,63), "(15)Print Screen"                  ,        ~
                     keys(eojtab$), key(keyhit%)
                     inpmessage$ = " "

               if keyhit% <> 13 then L48390
                  call "MANUAL" ("SARPTSEL")
                  goto L48120

L48390:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L48120

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* As Of Date       */~
                                    L50400,         /* Report Format Def*/~
                                    L50530          /* Report Group Name*/
                  return

L50140
*        As Of Date                                 ASOF_DATE$
            test_date$ = asof_date$
            gosub test_report_date
            asof_date$ = test_date$
            return

L50400: REM Report Format Definition              REPT_DEF$
            if l% > 95% then return
            plowkey$ = str(rdef_key$,,10) & rept_def$
            msg$ = hex(06) & "Select a report definition: (" &           ~
                rept_def$ & ")"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            if f1%(2) = 0% then return
            l% = l% + 1%
            gosub dataload
            str(rdef_key$,11) = rept_name$(l%)
            pmax%, page% = int((l% - 1%) / 12%) + 1%
            return

L50530: REM Report Group Name                     REPT_GRP$
            plowkey$ = str(rgrp_key$,,10) & rept_grp$
            msg$ = hex(06) & "Select a report group name: (" &           ~
                rept_grp$ & ")"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L35360, rgrp_key$, rgrp_desc$, rept_array$()
            rept_grp$ = str(rgrp_key$,11)
            for a% = 1% to 12%
                if rept_array$(a%) = " " then return
                rept_def$ = rept_array$(a%)
                gosub L50400
            next a%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52250,         /* Report Group name*/~
                                    L52410,         /* Report Group desc*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440,         /* Report Format Def*/~
                                    L52440          /* Report Format Def*/
                  return

L52250: REM Report Group Name                     REPT_GRP$
            plowkey$ = str(rgrp_key$,,10) & rept_grp$
            msg$ = hex(06) & "Select a report group name: (" &           ~
                rept_grp$ & ")"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            if f1%(2) = 1% then L52340
                if rept_grp$ = " " then errormsg$ = "The Report Group " &~
                                                 "name may not be blank."
                return
L52340:     get #2 using L35360, rgrp_key$, rgrp_desc$, rept_array$()
            rept_grp$ = str(rgrp_key$,11)
            for r% = 1% to 12%
                gosub get_report_description
            next r%
            return

L52410: REM Report Group Description
            return

L52440: REM Report Format Definition
            plowkey$ = str(rdef_key$,,10) & rept_array$(r%)
            msg$ = hex(06) & "Select a report definition: (" &           ~
                                                    rept_array$(r%) & ")"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            rept_array$(r%) = str(plowkey$,11)
            if f1%(2) = 1% or rept_array$(r%) = " " then L52540
                errormsg$ = "Invalid Report Format Definition: " &       ~
                                                    rept_array$(r%) & "."
                return
L52540:     gosub get_report_description
            return

        get_report_description
            if rept_array$(r%) <> " " then L52610
                rept_ardsc$(r%) = " "
                return
L52610:     readkey$ = str(rdef_key$,,10) & rept_array$(r%)
            call "DESCRIBE" (#2, readkey$, rept_ardsc$(r%), 0%, u3%)
            return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54240,         /* Report Date      */~
                                    L54300,         /* Group Code 1 from*/~
                                    L54470,         /* Group Code 1 to  */~
                                    L54620,         /* Group Code 1 desc*/~
                                    L54720,         /* Group Code 2 from*/~
                                    L54850,         /* Group Code 2 to  */~
                                    L55000,         /* Group Code 2 desc*/~
                                    L55110,         /* Report Type      */~
                                    L55290,         /* Summary or Detail*/~
                                    L55400,         /* Rank on Column   */~
                                    L55470,         /* Ascend or Descend*/~
                                    L55540,         /* Max to print     */~
                                    L55680,         /* Page break code  */~
                                    L55750          /* Incl outside yrs */
                  return

L54240: REM Report Date
            test_date$ = rept_date$(m%)
            gosub test_report_date
            rept_date$(m%) = test_date$
            return

L54300: REM Group Code 1 FROM
            if code1$(1) <> "ALL" then goto L54350
                code1$(2) = " "
                fieldnr% = fieldnr% + 1%
                goto L54550
L54350:     if code1$(1) = "FIRST" then return
            g% = group1%
            if len(code1$(1)) > codelen%(group1%) then goto L54420
            codex$ = code1$(1) : file% = file1%
            gosub capture_codes
            code1$(1) = codex$
            return
L54420:     convert codelen%(g%) to junk$, pic (##)
            errormsg$ = "Length of entry is too long. " & str(junk$,,2) &~
                " is the maximum length."
            return

L54470: REM Group Code 1 TO
            if code1$(1) = "ALL" then goto L54550
            if code1$(2) = "LAST" then goto L54550
            g% = group1%
            if len(code1$(2)) > codelen%(group1%) then goto L54420
            codex$ = code1$(2) : file% = file1%
            gosub capture_codes
            code1$(2) = codex$
L54550:     call "TESTRNGE" (code1$(1), code1$(2), junk$, junk$,         ~
                                                               errormsg$)
            if errormsg$ <> " " then return
                str(grcd1$(m%), 1,25) = code1$(1)
                str(grcd1$(m%),26,25) = code1$(2)
                return

L54620: REM Group Code 1 Desc                     GRP1_DESC$
            n% = grp1_desc%
            convert grp1_desc$(m%) to grp1_desc%, data goto L54680
            if grp1_desc% <= min(grp1_max%, n% + posn_rem%) then return
            errormsg$ = "Description length exceeds allowable maximum"
            return
L54680:     errormsg$ = "Description length format error: " &            ~
                                                           grp1_desc$(m%)
            return

L54720: REM Group Code 2 FROM
            if code2$(1) <> "ALL" then goto L54770
                code2$(2) = " "
                fieldnr% = fieldnr% + 1%
                goto L54930
L54770:     if code2$(1) = "FIRST" then return
            g% = group2%
            if len(code2$(1)) > codelen%(group2%) then goto L54420
            codex$ = code2$(1) : file% = file2%
            gosub capture_codes
            code2$(1) = codex$
            return

L54850: REM Group Code 2 TO
            if code2$(1) = "ALL" then goto L54930
            if code2$(2) = "LAST" then goto L54930
            g% = group2%
            if len(code2$(2)) > codelen%(group2%) then goto L54420
            codex$ = code2$(2) : file% = file2%
            gosub capture_codes
            code2$(2) = codex$
L54930:     call "TESTRNGE" (code2$(1), code2$(2), junk$, junk$,         ~
                                                               errormsg$)
            if errormsg$ <> " " then return
                str(grcd2$(m%), 1,25) = code2$(1)
                str(grcd2$(m%),26,25) = code2$(2)
                return

L55000: REM Group Code 2 Desc                     GRP2_DESC$
            if group2% = 0% then return
            n% = grp2_desc%
            convert grp2_desc$(m%) to grp2_desc%, data goto L55070
            if grp2_desc% <= min(grp2_max%, n% + posn_rem%) then return
            errormsg$ = "Description length exceeds allowable maximum"
            return
L55070:     errormsg$ = "Description length format error: " &            ~
                                                           grp2_desc$(m%)
            return

L55110: REM Report Type                           REPT_TYPE$
            if rept_type$(m%) <> "H" then goto L55150
                rank_colm$(m%), asnd_dsnd$(m%), max_print$(m%) = " "
                goto L55200
L55150:     if rank_colm$(m%) = " " then rank_colm$(m%) = "A"
            if asnd_dsnd$(m%) = " " then asnd_dsnd$(m%) = "A"
            if max_print$(m%) = " " then max_print$(m%) = "ALL"
            if rept_type$(m%) = "R" then goto L55210
            if rept_type$(m%) <> "B" then goto L55250
L55200:     if group2% <> 0% then return
L55210:         summ_detl$(m%) = "S" : page_brk$(m%) = " "
                fieldnr% = fieldnr% + 1%
                return
            if rept_type$(m%) = "B" then return
L55250:     errormsg$ = "Type must be 'H' (History), 'R' (Ranking), or "&~
                "'B' (Both)"
            return

L55290: REM Summary or Detail                     SUMM_DETL$
            if summ_detl$(m%) <> "S" then goto L55320
                page_brk$(m%) = " " : goto L55330
L55320:     if summ_detl$(m%) <> "D" then goto L55360
L55330:     if rept_type$(m%) <> "H" then return
                fieldnr% = fieldnr% + 3%
                return
L55360:     errormsg$ = "You must enter 'S' for a Summary report; 'D'"&  ~
                " for a Detail report"
            return

L55400: REM Rank on Column                        RANK_COLM$
            if rank_colm$(m%) < "A" then goto L55430
            if rank_colm$(m%) < "H" then return
L55430:     errormsg$ = "Ranking must be on one of the report columns ("&~
                "A thru G), below"
            return

L55470: REM Ascend or Descend                     ASND_DSND$
            if asnd_dsnd$(m%) = "A" then return
            if asnd_dsnd$(m%) = "D" then return
            errormsg$ = "You must enter 'A' for Ascending sequence; 'D'"&~
                " for Descending sequence"
            return

L55540: REM Max to print                          MAX_PRINT$
            if max_print$(m%) = "ALL" then return
            convert max_print$(m%) to max_print%, data goto L55640
            if max_print% < 1% then goto L55640
            if max_print% > 0% then goto L55610
            errormsg$ = "Must be greater than zero"
            return
L55610:     convert max_print% to max_print$(m%), pic (###)
            call "STRING" addr ("LJ", max_print$(m%), 3%)
            return
L55640:     errormsg$ = "Number to Print has an invalid format: " &      ~
                max_print$(m%)
            return

L55680: REM Page break code                       PAGE_BRK$
            if page_brk$(m%) = "Y" then return
            if page_brk$(m%) = "N" then return
            errormsg$ = "You must enter 'Y' for page breaks; 'N' to " &  ~
                "omit them"
            return

L55750: REM Include groups outside years          INCL$
            if incl$(m%) = "Y" or incl$(m%) = "N" then return
            errormsg$ = "You must enter 'Y' or 'N'."
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
            call "FILEBGON" (#10)
            end
