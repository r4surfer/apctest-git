        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   IIIII  N   N   QQQ   RRRR   Y   Y          *~
            *  S      A   A    I    NN  N  Q   Q  R   R  Y   Y          *~
            *   SSS   AAAAA    I    N N N  Q   Q  RRRR    YYY           *~
            *      S  A   A    I    N  NN  Q Q Q  R   R    Y            *~
            *   SSS   A   A  IIIII  N   N   QQQ   R   R    Y            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAINQRY  - Allows on-line inquiry into Sales Analysis     *~
            *            Summary files.                                 *~
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
            * 09/29/86 ! Original                                 ! ERN *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 08/24/88 ! Fixed display of previous years data.    ! RJM *~
            * 06/04/91 ! PRR 11438.  Allowed blank codes to be    ! JDH *~
            *          !   selected to allow visibilty into       !     *~
            *          !   records with blank fields (eg. Customer!     *~
            *          !   Type).  Now use '?' for selection scrn.!     *~
            *          ! PRR 11720.  Allowed unit qty display when!     *~
            *          !   part is either of the summary fields.  !     *~
            * 06/17/91 ! Added ALLFREE.                           ! JDH *~
            * 04/08/93 ! PRRs 10886, 11079 Non-Stock Parts OK if  ! JIM *~
            *          !   SYSFILE2 SWITCHS.SA record says so.    !     *~
            * 04/08/93 ! New Next Code logic- PF 5/21 & 6/22- 5&6 ! JIM *~
            *          !   act as before ('next' comes from master!     *~
            *          !   file); 21&22 get 'next' from Summary.  !     *~
            * 04/08/93 ! Selected implied integer conversions.    ! JIM *~
            * 04/08/93 ! PF(13)Instrs now available everywhere.   ! JIM *~
            * 04/08/93 ! Blanks and ?s no longer valid entries.   ! JIM *~
            * 09/10/93 ! PF 21 & 22 logic tightened to display    ! JIM *~
            *          !   only SASUMRY# key combos that actually ! JIM *~
            *          !   exist for the year(s) in question.     ! JIM *~
            * 02/25/94 ! Blanks ARE ok; reverses last 4/8/93 mod. ! GOD *~
            * 11/13/95 ! HHHHeeeeeeeeeeee's Baaaccccckkkkkk!!!!!  ! ERN *~
            *          ! Added 'GUI Stuff' (call to SAGRAPH)      !     *~
            * 09/05/96 ! Millie date conversion                   ! DER *~
            * 09/02/97 !  and add all(hex(00)) to plowkey         ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* blank unfmt date           */~
            cdescr$(9)30, chdrs$(9)10,   /* Column Descriptors and Hdrs*/~
            code1$(1)25, code1descr$32,  /* 1st Group Code             */~
            code2$(1)25, code2descr$32,  /* 2nd Group Code             */~
            codex$25   , codexdescr$32,  /* Work variables             */~
            codefile%(8), codelen%(8),   /* Group Code Len, File#      */~
            codes%(10,2),                /* Summary File Group Codes   */~
            codes$(10,2)14,              /* Group Code Descriptors     */~
            cok%(6),                     /* Column Ok flags            */~
            command$256,                 /* GUI API Command Line       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* Summary File Descriptions  */~
            display$9, displaydescr$30,  /* Display Parameters         */~
            dsply(6,13),                 /* Display Work Area          */~
            errormsg$79,                 /* Error message              */~
            gid$4, gnbr$12,              /* Graph Msg ID, Number       */~
            group1$14, group2$14,        /* Group Code Descriptors     */~
            hdr1$(6)10, hdr2$(6)10,      /* Column Headers             */~
            hex7f$1,                     /* Hex 7F                     */~
            history$(13)79,              /* History Display            */~
            hstry$(30)200,               /* History in a String        */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            msg$79,                      /* Misc Use Message           */~
            nonstock$1,                  /* Non-Stock parts OK?        */~
            nextcodekey$99,              /* For 'next code' logic      */~
            nrs(13),                     /* Numbers                    */~
            oprs$(6)6, oprs%(5),         /* Display Operands           */~
            oprsdescr$(6)40,             /* Operands Description       */~
            period$(6,13)8,              /* Period Start Dates by Year */~
            pf$(3)79, pfkey$22,          /* PF Key Variables           */~
            pfac$(6,8)1,                 /* Col Parameter FACs         */~
            phdrs$(8)40,                 /* Parameter Screen Hdrs      */~
            prnames$(10)8,               /* Summary File PR Names      */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            years$(6)8, yearsu$(6)6,     /* Years available list       */~
            yrs$(6)1, yrs%(6),           /* Display Years (b=0, 1-6)   */~
            ytd$(6)1, ytd%(6),           /* Y-T-D? (Y/ /N; 0=N, 1=Y)   */~
            ywork$(30)6                  /* Available Years Work Array */

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
            * #3  ! CUSTOMER ! Customer Master File                     *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! CATEGORY ! Inventory Category Codes File            *~
            * #6  ! GENCODES ! General Codes File                       *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! STORNAME ! Store Master                             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SASUMRY#",                                      ~
                        varc,     indexed,  recsize =  1048,             ~
                        keypos =      1, keylen =  56,                   ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #5,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select #6,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #7,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4                      ~

            select #8,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "SHOSTAT" ("Initializing....")

*        Get the Non-stocked part OK? flag from SYSFILE2.
            nonstock$ = "N"
            call "READ100" (#2, "SWITCHS.SA", f1%(2%))
            if f1%(2%) <> 0% then get #2 using L09043, nonstock$
L09043:         FMT POS(24), CH(1)

*        Load SA Summary File Descriptions
            plowkey$ = "SA.FILES.SASUMRY" & hex(00)
L09055:     call "PLOWNEXT" (#2, plowkey$, 16%, f1%(2))
            if f1%(2) = 1% then L09090
                if file% <> 0% then L09120
                call "ASKUSER" (0%, "NO SA FILES",                       ~
                                "There are no SA Summary Files Defined.",~
                                " ", "Press RETURN to exit program...")
                goto exit_program
L09090:     convert str(plowkey$,17,1) to file% : file% = file% + 1%
            get #2 using L09105, prnames$(file%), descrs$(file%),          ~
                               codes%(file%, 1), codes%(file%, 2)
L09105:         FMT XX(9), CH(8), XX(3), CH(30), XX(2), 2*BI(1)
            goto L09055

L09120
*        Load the last six years of history on file, and their periods
            years$(1), years$(2), years$(3), years$(4), years$(5),       ~
            years$(6) = " -N/A- "
            plowkey$ = "SA.YEARS.USED." & all(hex(00))
            y% = 0%
L09145:     call "PLOWNEXT" (#2, plowkey$, 14%, f1%(2))
            if f1%(2) = 0% then L09170
                y% = y% + 1%
                ywork$(y%) = str(plowkey$,15,6)
                goto L09145
L09170:     if y% <> 0% then L09205
                call "ASKUSER" (keyhit%, "NO HISTORY",                   ~
                                "There is no Sales History is on file",  ~
                                "Press PF-1 to continue, -or-",          ~
                                "Press RETURN to exit program." )
                if keyhit% <> 1% then exit_program
                goto L09305
L09205:     for year% = 1% to 6%
                years$(year%), yearsu$(year%) = ywork$(y%)
                readkey$ = "SA.YEARS.USED." & years$(year%)
                call "READ100" (#2, readkey$, f1%(2))
                get #2 using L09260, period$(year%, 1), period$(year%, 2), ~
                                   period$(year%, 3), period$(year%, 4), ~
                                   period$(year%, 5), period$(year%, 6), ~
                                   period$(year%, 7), period$(year%, 8), ~
                                   period$(year%, 9), period$(year%,10), ~
                                   period$(year%,11), period$(year%,12), ~
                                   period$(year%,13)
L09260:              FMT POS(29), 13*CH(6)
                call "DATEFMT" (years$(year%))
                for p% = 1% to 13%
                     call "DATEFMT" (period$(year%, p%))
                next p%
                y% = y% - 1%
                if y% = 0% then L09305
            next year%

L09305
*        Set up Static Tables
            cdescr$(1) = "Booking Quantity              "
            chdrs$ (1) = "Bookng Qty"
            cdescr$(2) = "Booking Value                 "
            chdrs$ (2) = "Bkng Value"
            cdescr$(3) = "Shipment Quantity             "
            chdrs$ (3) = " Ship Qty "
            cdescr$(4) = "Shipment Value                "
            chdrs$ (4) = "Ship Value"
            cdescr$(5) = "Shipment Costs                "
            chdrs$ (5) = "Ship Costs"
            cdescr$(6) = "Booking Quantity Target       "
            chdrs$ (6) = "Bkng Qty T"
            cdescr$(7) = "Booking Value Target          "
            chdrs$ (7) = "Bkng Val T"
            cdescr$(8) = "Shipment Quantity Target      "
            chdrs$ (8) = "Ship Qty T"
            cdescr$(9) = "Shipment Value Target         "
            chdrs$ (9) = "Ship Val T"

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

            phdrs$( 1) = "Column"
            phdrs$( 2) = "Year"
            phdrs$( 3) = "Y-T-D?"
            phdrs$( 4) = "Contents (Op, Col)"
            phdrs$( 5) = "Column Description"
            phdrs$( 6) = "Years (Start Dates)"
            phdrs$( 7) = "Operands"
            phdrs$( 8) = "Content Codes"

*        See if we're in GUI mode.
            hex7f$ = hex(7f)
            call "CHECKGUI" addr(gui%)    /* 0 = No          */


        REM *************************************************************~
            *             D I S P L A Y   H I S T O R Y                 *~
            *-----------------------------------------------------------*~
            * Select Summary file and Display History.                  *~
            *************************************************************

            gosub initialize


        display_history
            errormsg$ = " "
            if f2%(1) = 0% then L10370    /* Summary file already open  */

        select_summary_file
*        Get which Summary File to display data for
            if f2%(1) = 0% then close #1
            f2%(1) = 1%
L10160:     plowkey$ = "SA.FILES.SASUMRY"
            call "SASUMINP" (" SAINQRY",                                 ~
                             "Sales Analysis Inquiry: Select File",      ~
                             "NY", descrs$(), ret%)
            if ret% = 116% then exit_program
                sumry% = ret% + 1%
                call "PUTPRNAM" addr (#1, prnames$(sumry%))
                call "OPENCHCK" (#1,  fs%(1), f2%(1), 0%, rslt$(1))
                if f2%(1) <> 0% then L10160
                     group1% = codes%(sumry%, 1%)
                     file1%  = codefile%(group1%)
                     mat redim code1$(1)codelen%(group1%)
                     group1$ = codes$(group1%, 1)

                     group2% = codes%(sumry%, 2%)  : file2% = 0%
                     group2$ = " "
                     if group2% = 0% then history_screen
                          file2%  = codefile%(group2%)
                          mat redim code2$(1)codelen%(group2%)
                          group2$ = codes$(group2%, 1)

L10370: history_screen
*        First get which Codes to Display
            init (" ") code1$(), code1descr$, code2$(), code2descr$,     ~
                       history$()
L10410:     for fieldnr% = 1% to 2%
                gosub'052(fieldnr%)
                     if enabled% = 0% then L10520
L10440:         gosub'102(fieldnr%)
                     if keyhit%  =  1% then       select_summary_file
                     if keyhit%  =  8% then       change_display
                     if keyhit%  = 10% then gosub show_periods
                     if keyhit%  = 16% then       exit_program
                     if keyhit% <>  0% then       L10440
                gosub'152(fieldnr%)
                     if errormsg$ <> " " then L10440
L10520:     next fieldnr%
            dont_care% = 1%        /* DON'T care if SASUMRY# not found */
            gosub load_history          /* Load and format History     */

L10560:     gosub'102(0%)               /* Display History             */
                  errormsg$ = " "
                  if keyhit%  =  1% then       select_summary_file
                  if keyhit%  =  4% then       L10410
                  if keyhit%  =  5% then gosub next_code1
                  if keyhit%  = 21% then gosub next_code1
                  if keyhit%  =  6% then gosub next_code2
                  if keyhit%  = 22% then gosub next_code2
                  if keyhit%  =  8% then       change_display
                  if keyhit%  =  9% and gui% <> 0% then                  ~
                                         gosub call_graph_rtn
                  if keyhit%  = 10% then gosub show_periods
                  if keyhit%  = 16% then       exit_program
                  if keyhit% <>  0 then L10560
                  goto L10410

        next_code1
            if keyhit% = 5% then goto L10679 /* 'Next' from which file? */
*        KEYHIT% must be 21% -- get 'next' code 1 from SASUMRY# via AK2.
L10664:         readkey$ = str(nextcodekey$,7%,25%)
                call "REDALT2" (#1, readkey$, 2%, f1%(1%))
                if f1%(1%) <> 0% then goto L10670
                     errormsg$ = "At End of " & codes$(group1%,2%) &     ~
                          " codes/this Summary file."
                     return
L10670:         get #1 using L10672, codex$
L10672:              FMT POS(7), CH(25)
                dont_care% = 0%       /* DO care if SASUMRY# not found */
                goto L10690
L10679
*        KEYHIT% is 5% -- get 'next' code 1 from the 'Master' file.
            file% = file1% : g% = group1% : codex$ = code1$(1%)
            gosub next_code
            dont_care% = 1%        /* DON'T care if SASUMRY# not found */
L10690:     code1$(1%) = codex$
            gosub'152(1%)          /* To describe Code           */
            gosub load_history
            if not_found% = 1% and dont_care% = 0% then goto L10664
            return

        next_code2
            if keyhit% = 6% then goto L10749 /* 'Next' from which file? */
*        KEYHIT% must be 22% -- get 'next' code 2 from SASUMRY# via AK1.
L10737:         readkey$ = str(nextcodekey$,32%,25%) & hex(ff)
                dont_care% = 0%       /* DO care if SASUMRY# not found */
                call "REDALT2" (#1, readkey$, 1%, f1%(1%))
                if f1%(1%) <> 0% then goto L10746
                     errormsg$ = "At End of " & codes$(group2%,2%) &     ~
                          " codes/this Summary file."
                     return
L10746:         get #1 using L10747, codex$
L10747:              FMT POS(32), CH(25)
                goto L10760
L10749
*        KEYHIT% is 6% -- get 'next' code 2 from the 'Master' file.
            file% = file2% : g% = group2% : codex$ = code2$(1%)
            gosub next_code
            dont_care% = 1%        /* DON'T care if SASUMRY# not found */
L10760:     code2$(1%) = codex$
            gosub'152(2%)          /* To describe Code           */
            gosub load_history
            if not_found% = 1% and dont_care% = 0% then goto L10737
            return

        next_code
            readkey$ = codex$
            if file% <> 6% then goto L10820
                if g% = 5% then readkey$ = "CUS TYPES" & codex$
                if g% = 7% then readkey$ = "REGIONS  " & codex$
L10820:     call "READ102" (#file%, readkey$, onfile%)/* 'Master' file */
            if onfile% = 1% then L10860
L10840:         errormsg$ = "At End of Master file of " &                ~
                     codes$(g%,2%) & " codes."
                return clear  :  return
L10860:     if file% <> 6% then L10930
                get #6 using L10880, readkey$, codex$   /* GENCODES     */
L10880:              FMT CH(9), CH(15)
                if g% = 5% and readkey$ <> "CUS TYPES" then L10840
                if g% = 7% and readkey$ <> "REGIONS  " then L10840
                return

L10930:         get #file%, str(codex$,,codelen%(g%))  /* All Others   */
                return



        REM *************************************************************~
            *       D E F I N E  /  C H A N G E   D I S P L A Y         *~
            *-----------------------------------------------------------*~
            * Allows Definition and changing of Display Parameters.     *~
            *************************************************************

        change_display

*        Get option to perform from the parameters screen
L11340:     gosub'051(1%)               /* Set Input Message           */
L11350:     gosub'101(1%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       display_history
                  if keyhit%  = 10 then gosub show_periods
                  if keyhit%  = 12 then gosub delete_display
                  if keyhit%  = 14 then gosub load_display
                  if keyhit%  = 16 then gosub datasave
                  if keyhit%  = 32 then       exit_program
                  if keyhit% <>  0 then       L11350
                  errormsg$ = " "

*        Set INPMESSAGE$ and get changes to display
            gosub'051(2%)               /* Check Enables, Set Defaults */
L11460:     gosub'101(2%)               /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11460
            gosub'151                   /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11460
            goto L11340


        delete_display
            if display$ = " " then return
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE DISPLAY",                       ~
                            "Enter PF-16 to Delete this Display", "-OR-",~
                            "Press RETURN to abort Delete.")
            if u3% <> 16% then return
                readkey$ = "SA.DISPLAY." & display$
                call "DELETE" (#2, readkey$, 20%)
                gosub initialize
                return clear
                goto change_display

        load_display      /* Bring Exiting Display in for editing      */
*        Display Parameters Name                  DISPLAY$
                plowkey$ = "SA.DISPLAY." & all(hex(00))
                msg$ = hex(06) & "Select Display"
                call "PLOWCODE" (#2, plowkey$, msg$, 11%, 0.30, f1%(2))
                if f1%(2) = 0% then return
                     display$ = str(plowkey$,12)
                     gosub load_file_parameters
                     return

        REM *************************************************************~
            *            S H O W   P E R I O D S                        *~
            *-----------------------------------------------------------*~
            * Display the Start Dates for the periods of the Years.     *~
            *************************************************************

        show_periods
            gosub'103(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       exit_program
                  if keyhit% <>  0 then       show_periods
            keyhit% = 99%
            return


*       ****************************************************************
        call_graph_rtn
*       ****************************************************************

            if gui% = 0% then return
            call "SHOSTAT" ("Passing data to graphics display routine...")
            command$ = hex7f$ & "UWVBXSAGRAPH,9999,QUIT" & hex7f$
            call "SENDCMD" (command$)
            call "PAUSE" addr(200%)

            call "GUIVBRUN" ("SAGRAPH.EXE", 0%, rc%)
            if rc% < 0 then return

            command$ = hex7f$ & "UWVBXsagraph,1" & hex7f$
            call "SENDCMD" (command$)

*        100x and 200x -- Descriptors and Codes
            command$ = hex7f$ & "UWVBXsagraph,1001," & group1$ & hex7f$
            call "SENDCMD" (command$)
            command$ = hex7f$ & "UWVBXSAGRAPH,1002," & group2$ & hex7f$
            call "SENDCMD" (command$)
            command$ = hex7f$ & "UWVBXSAGRAPH,2001," & code1$(1) & hex7f$
            call "SENDCMD" (command$)
            command$ = hex7f$ & "UWVBXSAGRAPH,2002," & code2$(1) & hex7f$
            call "SENDCMD" (command$)
            command$ = hex7f$ & "UWVBXSAGRAPH,2003," & code1descr$ &     ~
                                                                   hex7f$
            call "SENDCMD" (command$)
            command$ = hex7f$ & "UWVBXSAGRAPH,2004," & code2descr$ &     ~
                                                                   hex7f$
            call "SENDCMD" (command$)

*        3spp -- Period Dates.
            for gset% =  1% to 6%
                for gperiod% = 1% to 13%
                     gid% = 3000% + (gset% * 100) + gperiod%
                     convert gid% to gid$, pic(0000)
                     command$ = hex7f$ & "UWVBXSAGRAPH," &               ~
                                gid$ & "," &                             ~
                                period$(gset%,gperiod%) & hex7f$
                     call "SENDCMD" (command$)
                next gperiod%
            next gset%

*        4spp -- S A Number.
            for gset% =  1% to 6%
                for gperiod% = 1% to 13%
                     gid% = 4000% + (gset% * 100) + gperiod%
                     convert gid% to gid$, pic(0000)
*                   GNBR  = 0
                     gnbr$ = str(history$(gperiod%),6+(gset%*12-11),10)
                     if gnbr$ <= " " then L18530
*                        GNBR = NUM(GNBR$)
*                        CONVERT GNBR TO GNBR$, PIC(-000000000.00)
                          command$ = hex7f$ & "UWVBXSAGRAPH," &          ~
                                     gid$ & "," & gnbr$ & hex7f$
                          call "SENDCMD" (command$)
L18530:         next gperiod%
            next gset%

*        500s -- Column Headings.
            for  gset% = 1% to 6%
                gid% = 5000% + gset%
                     convert gid% to gid$, pic(0000)
                     command$ = hex7f$ & "UWVBXSAGRAPH," &               ~
                                gid$ & "," &                             ~
                                hdr1$(gset%) & " " & hdr2$(gset%) &      ~
                                hex7f$
                     call "SENDCMD" (command$)
            next gset%

*        Now signal end of data...
            command$ = hex7f$ & "UWVBXSAGRAPH,8888,THAT'S ALL" & hex7f$
            call "SENDCMD" (command$)
            return


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if display$ <> " " then L19100
                errormsg$ = "Please Supply Display Name"
                return
L19100:     gosub dataput
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(edit%)
            on edit%     gosub     L20120,          /* Select Option    */~
                                   L20170           /* Edit Display     */
            return


L20120
*        Edit Mode- Select Option
            inpmessage$ = "Press PF-Key for Option Desired.  Press" &    ~
                          " RETURN to edit Display."
            return

L20170
*        Edit Mode- Change Display
            inpmessage$ = "Make changes and then press RETURN."
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21110, L21160
            return

L21110
*        Group Code 1                                CODE1$
            inpmessage$ = "Enter " & codes$(group1%,2) & ".  Enter" &    ~
                          " '?' or a partial code for search."
            return

L21160
*        Group Code 2                                CODE2$
            if group2% <> 0% then L21200
                enabled% = 0%
                return
L21200:     inpmessage$ = "Enter " & codes$(group2%,2) & ".  Enter" &    ~
                          " '?' or a partial code for search."
            return

        REM *************************************************************~
            *                 I N I T I A L I Z E                       *~
            *-----------------------------------------------------------*~
            * Intialize Variables.                                      *~
            *************************************************************

        initialize
            init (" ") display$, displaydescr$, errormsg$
            gosub load_dflt_parameters
            call "ALLFREE"
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
            return clear all
            gosub initialize
            goto  change_display

        REM *************************************************************~
            *        L O A D   D I S P L A Y   P A R A M E T E R S      *~
            *-----------------------------------------------------------*~
            * Loads display parameters saved on file.                   *~
            *************************************************************



        load_dflt_parameters   /*  Set up with Defaults */
            mat yrs% = con
            init ("1") yrs$()
            mat ytd% = zer
            init(" ") oprs$()
            ytd$(1) = "N" : str(oprs$(1)) = "2     "  /* Bkngs $s - TY */
            ytd$(2) = "Y" : str(oprs$(2)) = "2     "  /* Bkngs $s YTD  */
            ytd$(3) = "N" : str(oprs$(3)) = "4     "  /* Sales $s - TY */
            ytd$(4) = "Y" : str(oprs$(4)) = "4     "  /* Sales $s YTD  */
            yrs$(5) = "2"                             /* Last Year     */
            ytd$(5) = "Y" : str(oprs$(5)) = "4     "  /* Sales $s LY   */
            yrs$(6) = " "                             /* No Year Req'd */
            ytd$(6) = " " : str(oprs$(6)) = "-45   "  /* Diff This/Last*/
            gosub describe_parameters
            return

        load_file_parameters   /* Load from File      */
            get #2 using L30260, displaydescr$, yrs$(), ytd$(), oprs$()
L30260:         FMT XX(20), CH(30), 6*CH(1), 6*CH(1), 6*CH(6)
            gosub describe_parameters
            return


        describe_parameters    /* Describe parameters, define headings */
            init (" ") oprsdescr$(), hdr1$(), hdr2$()
            mat ytd%  = zer
            mat yrs%  = zer

            for c% = 1% to 6%            /* Step thru By Column        */
              if oprs$(c%) = " " then L30460        /* Column Not Used  */
                convert yrs$(c%) to yrs%(c%), data goto L30390
L30390:
                if ytd$(c%) = "Y" then ytd%(c%) = 1%

                p%  = pos("123456789+-%" = str(oprs$(c%),1%,1%))
                p1% = p% - 8%  :  if p1% < 2% then p1% = 1%
                on p1% gosub descrx, descr_plus, descr_minus,            ~
                             descr_percent
L30460:     next c%
            return

            descrx   /* Generalized Description Routine                */
                oprsdescr$(c%) = cdescr$(p%) & ", " & years$(yrs%(c%))
                if ytd$(c%) = "Y" then                                   ~
                               oprsdescr$(c%) = oprsdescr$(c%) & " Y-T-D"
                hdr1$(c%) = str(years$(yrs%(c%))) & "-P"
                if ytd$(c%) = "Y" then str(hdr1$(c%),10,1) = "Y"
                hdr2$(c%) = chdrs$(p%)
                return

            descr_plus    /* Summation Operand                         */
                hdr1$(c%) = "  Columns "
                oprsdescr$(c%) = "Summation of Columns"
                hdr2$(c%) = str(oprs$(c%),2%,1%) & "+"
                for o% = 3% to 6%
                     if str(oprs$(c%),o%,1%) <> " " then                 ~
                       hdr2$(c%) = hdr2$(c%) & str(oprs$(c%),o%,1%) & "+"
                next o%
                str(hdr2$(c%), len(hdr2$(c%)), 1) = " "
                call "STRING" addr("CT", str(hdr2$(c%)), 10%)
                return

            descr_minus   /* Column Subtraction                        */
                oprsdescr$(c%) = "Column # less Column #"
                hdr1$(c%) = "  Column #"
                hdr2$(c%) = "- Column #"
                str(oprsdescr$(c%), 8,1), str(hdr1$(c%),10,1) =          ~
                                                    str(oprs$(c%),2%,1%)
                str(oprsdescr$(c%),22,1), str(hdr2$(c%),10,1) =          ~
                                                    str(oprs$(c%),3%,1%)
                return

            descr_percent /* Column Percent                            */
                oprsdescr$(c%) = "Percent: Column # / Column #"
                hdr1$(c%) = "% Column #"
                hdr2$(c%) = "/ Column #"
                str(oprsdescr$(c%),17,1), str(hdr1$(c%),10,1) =          ~
                                                    str(oprs$(c%),2%,1%)
                str(oprsdescr$(c%),28,1), str(hdr2$(c%),10,1) =          ~
                                                    str(oprs$(c%),3%,1%)
                return

        REM *************************************************************~
            *          L O A D  /   F O R M A T   H I S T O R Y         *~
            *-----------------------------------------------------------*~
            * Load and format the History Data for the Codes supplied.  *~
            *************************************************************
        load_history
            not_found% = 1%           /* Indicate 'SASUMRY# not found' */
            init (" ")     history$()
            init (hex(ff)) hstry$()
            mat dsply = zer
            for p% = 1% to 13%
                str(history$(p%),1,2) = hex(0b8c)
                convert p% to str(history$(p%),3,2), pic(#0)
            next p%

*        Load History and Targets for years required for display
            nextcodekey$ = " "
            for c% = 1% to 6%                     /* Loop by Column    */
                y% = yrs%(c%)                     /* Year for Col C%   */
                if y% = 0% then L31145             /* No Year Specified */
                if c% = 1% then L31105
                if pos(str(yrs$(),1%,c%-1%) = yrs$(c%)) > 0% then L31145
                if yearsu$(y%) = " " or yearsu$(y%) = blankdate$ ~
                   then L31145   /* Year not Used     */
L31105:              put readkey$ using L31115, yearsu$(y%),              ~
                                               code1$(1), code2$(1)
L31115:                   FMT CH(6), CH(25), CH(25)
                     if nextcodekey$ = " " then nextcodekey$ = readkey$
                     call "READ100" (#1, readkey$, f1%(1))
                     if f1%(1) = 0% then L31145
                          not_found% = 0% /* Indicate 'SASUMRY# found' */
                          y% = y% * 936% - 935%
                          get #1 using L31140, str(hstry$(), y%, 936%)
L31140:                        FMT XX(56), CH(936)
L31145:     next c%
            if not_found% = 1% and dont_care% = 0% then return

*        Now Extract the details need into columns
            mat cok% = con     /* 1= Column Not Ok, 0= Column Ok-Fine  */
            for c% = 1% to 6%
                convert str(oprs$(c%),,1) to b%, data goto L31260
                if b% < 1% or b% > 9% then L31260
                if b% = 2% or b% = 4% or b% = 5% or b% = 7% or b% = 9%   ~
                                                               then L31210
                     if group1% = 1% or group2% = 1% then L31210
                          goto L31260     /* Qty Column w/o Part Data   */
L31210:         y% = yrs%(c%) * 936% - 935%
                if str(hstry$(), y%, 1%) = hex(ff) then L31260

                     cok%(c%) = 0%
                     b% = (b% * 104% - 103%) + y% - 1%
                     get str(hstry$(), b%, 104%), using L31240, nrs()
L31240:                   FMT 13*PD(14,4)
                     for p% = 1% to 13%
                          dsply(c%, p%) = nrs(p%)
                     next p%
L31260:     next c%

*        Calc Up YTDs
            for c% = 1% to 6%
                if cok%(c%) = 1% or ytd%(c%) <> 1% then L31300
                     for p% = 2% to 13%
                          dsply(c%,p%) = dsply(c%,p%) + dsply(c%, p%-1%)
                     next p%
L31300:     next c%


*        Throw what we've got so far into display array
            for c% = 1% to 6%
              if cok%(c%) = 1% then L31360
                c1% = 6% + (c% * 12% - 11%)  /* Column in display      */
                for p% = 1% to 13%
                     if period$(yrs%(c%), p%) = " " or ~
                        period$(yrs%(c%), p%) = blankdate$ then L31355
                          call "CONVERT" (dsply(c%,p%), 2.2,             ~
                                               str(history$(p%),c1%,10%))
L31355:         next p%
L31360:     next c%


*        Do any Calculated Columns
            for c% = 1% to 6%
                if pos("+-%" = str(oprs$(c%),,1)) = 0% then L31580

                mat oprs% = zer
                for o% = 2% to 5%
                     convert str(oprs$(c%),o%,1%) to oprs%(o%-1%),       ~
                                                         data goto L31415
L31415:         next o%

                for p% = 1% to 13%
                     gosub test_if_active  : if a% = 0% then L31575

                     on pos("+-%" = str(oprs$(c%),,1)) goto L31460,       ~
                                                            L31505,       ~
                                                            L31530

L31460:            /* Summation Calculation */
                     for o% = 1% to 5%
                          o% = oprs%(o%)
                          if o% = 0% then L31555
                          dsply(c%,p%) = dsply(c%,p%) + dsply(o%,p%)
                      next o%
                      goto L31555

L31505:            /* Subtraction of two columns      */
                     o1% = oprs%(1%)  :  o2% = oprs%(2%)
                     dsply(c%,p%) = dsply(o1%,p%) - dsply(o2%,p%)
                     goto L31555

L31530:            /* Percentage of two Columns       */
                     o1% = oprs%(1%)  :  o2% = oprs%(2%)
                     if dsply(o2%,p%) = 0 then L31575
                     dsply(c%,p%) = (dsply(o1%,p%) / dsply(o2%,p%)) * 100

L31555:         /* Format results into display     */
                     c1% = 6% + (c% * 12% - 11%)  /* Display Pos  */
                     call "CONVERT" (dsply(c%,p%), 2.2,                  ~
                                               str(history$(p%),c1%,10%))
L31575:         next p%
L31580:     next c%

            return

        test_if_active  /* If the line we are on has no active periods */
                        /* then don't perform calcualation             */
            a% = 1%
            for x% = 1% to 6%
                if yrs%(x%) = 0% then L31630
                     if period$(yrs%(x%),p%) <> " " and ~
                        period$(yrs%(x%),p%) <> blankdate$ then return
L31630:     next x%
            a% = 0%
            return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Saving Display " & display$)
            readkey$ = "SA.DISPLAY." & display$
            call "READ101" (#2, readkey$, f1%(2))
            put #2 using L32100, readkey$, displaydescr$,                 ~
                                yrs$(), ytd$(), oprs$(), " ", " "
L32100:         FMT CH(20), CH(30), 6*CH(1), 6*CH(1), 6*CH(6), CH(202),  ~
                    CH(200)
            if f1%(2) = 0% then write #2 else rewrite #2
            call "ALLFREE"
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Set Display Parameters Screen.                            *~
            *************************************************************

        deffn'101(edit%)
            line2$ = " " : str(line2$,63) = "SAINQRY: " & str(cms2v$,,8)
            gosub setpf1
            if edit% <> 1% then L40065
                init(hex(84)) lfac$()    /* Edit- Select Option */
                init(hex(84)) pfac$()
                goto L40095
L40065:     if errormsg$ <> " " then L40095
                init(hex(81)) lfac$()    /* Edit- Fields Enabled */
                init(hex(81)) pfac$()
                goto L40095


L40095: accept                                                           ~
            at (01,02), "Sales Analysis Inquiry: Define Display",        ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (04,02), "Display Name:",                                 ~
            at (04,16), fac(lfac$( 1)), display$                , ch(09),~
            at (04,28), "Description:",                                  ~
            at (04,41), fac(lfac$(  1)), displaydescr$          , ch(30),~
                                                                         ~
            at (06,02), fac(hex(ac)),    phdrs$(1)              , ch(06),~
            at (07,05), "1",   at(08,05), "2",  at(09,05), "3",          ~
            at (10,05), "4",   at(11,05), "5",  at(12,05), "6",          ~
                                                                         ~
            at (06,09), fac(hex(ac)),    phdrs$(2)              , ch(04),~
            at (07,11), fac(pfac$(1,1)), yrs$(1)                , ch(01),~
            at (08,11), fac(pfac$(2,1)), yrs$(2)                , ch(01),~
            at (09,11), fac(pfac$(3,1)), yrs$(3)                , ch(01),~
            at (10,11), fac(pfac$(4,1)), yrs$(4)                , ch(01),~
            at (11,11), fac(pfac$(5,1)), yrs$(5)                , ch(01),~
            at (12,11), fac(pfac$(6,1)), yrs$(6)                , ch(01),~
                                                                         ~
            at (06,14), fac(hex(ac)),    phdrs$(3)              , ch(06),~
            at (07,16), fac(pfac$(1,2)), ytd$(1)                , ch(01),~
            at (08,16), fac(pfac$(2,2)), ytd$(2)                , ch(01),~
            at (09,16), fac(pfac$(3,2)), ytd$(3)                , ch(01),~
            at (10,16), fac(pfac$(4,2)), ytd$(4)                , ch(01),~
            at (11,16), fac(pfac$(5,2)), ytd$(5)                , ch(01),~
            at (12,16), fac(pfac$(6,2)), ytd$(6)                , ch(01),~
                                                                         ~
            at (06,21), fac(hex(ac)),    phdrs$(4)              , ch(18),~
            at (07,22), fac(pfac$(1,3)), str(oprs$(1),1,1)      , ch(01),~
            at (07,25), fac(pfac$(1,4)), str(oprs$(1),2,1)      , ch(01),~
            at (07,28), fac(pfac$(1,5)), str(oprs$(1),3,1)      , ch(01),~
            at (07,31), fac(pfac$(1,6)), str(oprs$(1),4,1)      , ch(01),~
            at (07,34), fac(pfac$(1,7)), str(oprs$(1),5,1)      , ch(01),~
            at (07,37), fac(pfac$(1,8)), str(oprs$(1),6,1)      , ch(01),~
                                                                         ~
            at (08,22), fac(pfac$(2,3)), str(oprs$(2),1,1)      , ch(01),~
            at (08,25), fac(pfac$(2,4)), str(oprs$(2),2,1)      , ch(01),~
            at (08,28), fac(pfac$(2,5)), str(oprs$(2),3,1)      , ch(01),~
            at (08,31), fac(pfac$(2,6)), str(oprs$(2),4,1)      , ch(01),~
            at (08,34), fac(pfac$(2,7)), str(oprs$(2),5,1)      , ch(01),~
            at (08,37), fac(pfac$(2,8)), str(oprs$(2),6,1)      , ch(01),~
                                                                         ~
            at (09,22), fac(pfac$(3,3)), str(oprs$(3),1,1)      , ch(01),~
            at (09,25), fac(pfac$(3,4)), str(oprs$(3),2,1)      , ch(01),~
            at (09,28), fac(pfac$(3,5)), str(oprs$(3),3,1)      , ch(01),~
            at (09,31), fac(pfac$(3,6)), str(oprs$(3),4,1)      , ch(01),~
            at (09,34), fac(pfac$(3,7)), str(oprs$(3),5,1)      , ch(01),~
            at (09,37), fac(pfac$(3,8)), str(oprs$(3),6,1)      , ch(01),~
                                                                         ~
            at (10,22), fac(pfac$(4,3)), str(oprs$(4),1,1)      , ch(01),~
            at (10,25), fac(pfac$(4,4)), str(oprs$(4),2,1)      , ch(01),~
            at (10,28), fac(pfac$(4,5)), str(oprs$(4),3,1)      , ch(01),~
            at (10,31), fac(pfac$(4,6)), str(oprs$(4),4,1)      , ch(01),~
            at (10,34), fac(pfac$(4,7)), str(oprs$(4),5,1)      , ch(01),~
            at (10,37), fac(pfac$(4,8)), str(oprs$(4),6,1)      , ch(01),~
                                                                         ~
            at (11,22), fac(pfac$(5,3)), str(oprs$(5),1,1)      , ch(01),~
            at (11,25), fac(pfac$(5,4)), str(oprs$(5),2,1)      , ch(01),~
            at (11,28), fac(pfac$(5,5)), str(oprs$(5),3,1)      , ch(01),~
            at (11,31), fac(pfac$(5,6)), str(oprs$(5),4,1)      , ch(01),~
            at (11,34), fac(pfac$(5,7)), str(oprs$(5),5,1)      , ch(01),~
            at (11,37), fac(pfac$(5,8)), str(oprs$(5),6,1)      , ch(01),~
                                                                         ~
            at (12,22), fac(pfac$(6,3)), str(oprs$(6),1,1)      , ch(01),~
            at (12,25), fac(pfac$(6,4)), str(oprs$(6),2,1)      , ch(01),~
            at (12,28), fac(pfac$(6,5)), str(oprs$(6),3,1)      , ch(01),~
            at (12,31), fac(pfac$(6,6)), str(oprs$(6),4,1)      , ch(01),~
            at (12,34), fac(pfac$(6,7)), str(oprs$(6),5,1)      , ch(01),~
            at (12,37), fac(pfac$(6,8)), str(oprs$(6),6,1)      , ch(01),~
                                                                         ~
            at (06,41), fac(hex(ac)),    phdrs$(5)              , ch(40),~
            at (07,41), fac(hex(8c)),    oprsdescr$(1)          , ch(40),~
            at (08,41), fac(hex(8c)),    oprsdescr$(2)          , ch(40),~
            at (09,41), fac(hex(8c)),    oprsdescr$(3)          , ch(40),~
            at (10,41), fac(hex(8c)),    oprsdescr$(4)          , ch(40),~
            at (11,41), fac(hex(8c)),    oprsdescr$(5)          , ch(40),~
            at (12,41), fac(hex(8c)),    oprsdescr$(6)          , ch(40),~
                                                                         ~
            at (14,02), fac(hex(ac)),    phdrs$(6)              , ch(24),~
            at (15,02), "1:",  at(16,02), "2:",  at(17,02), "3:",        ~
            at (15,15), "4:",  at(16,15), "5:",  at(17,15), "6:",        ~
            at (15,05), fac(hex(8c)),    years$(1)              , ch(08),~
            at (16,05), fac(hex(8c)),    years$(2)              , ch(08),~
            at (17,05), fac(hex(8c)),    years$(3)              , ch(08),~
            at (15,18), fac(hex(8c)),    years$(4)              , ch(08),~
            at (16,18), fac(hex(8c)),    years$(5)              , ch(08),~
            at (17,18), fac(hex(8c)),    years$(6)              , ch(08),~
                                                                         ~
            at (14,28), fac(hex(ac)),    phdrs$(7)              , ch(11),~
            at (15,28), "#: Content",  at(16,32), "Code",                ~
            at (17,28), "+: Sum    ",  at(18,28), "-: Minus",            ~
            at (19,28), "%: Percent",                                    ~
                                                                         ~
            at (14,40), fac(hex(ac)),    phdrs$(8)              , ch(40),~
            at (15,40), "1: Booking Units   6: Booking Trgt Units ",     ~
            at (16,40), "2: Booking Value   7: Booking Trgt Value ",     ~
            at (17,40), "3: Shipment Units  8: Shipment Trgt Units",     ~
            at (18,40), "4: Shipment Value  9: Shipment Trgt Value",     ~
            at (19,40), "5: Shipment Cost                         ",     ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L40665
                  call "MANUAL" ("SAINQRY ") : goto L40095

L40665:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN" : goto L40095

L40680:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf1
         if edit% <> 1% then L40760       /* Edit Mode- Select Field    */
           pf$(1) = "(1)Reset to Defaults  (10)Show Periods   (12)Delet"&~
                    "e Display    (13)Instructions"
           pf$(2) = "                                         (14)Load "&~
                    "New Display  (15)Print Screen"
           pf$(3) = "(9)Display History                       (16)Save "&~
                    "This Display (32)Exit Program"
           pfkey$ = hex(01ffffffffffff08090aff0c0d0e0f10ffff2000)
           return

                                         /* Edit Mode- Field Enabled   */
L40760:    pf$(1) = "(1)Reset to Defaults                              "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
            line2$ = descrs$(sumry%) & " History"
            if display$ <> " " then line2$ = line2$ &                    ~
                                    "  (Using Display: " & display$ & ")"
            str(line2$,63%) = "SAINQRY: " & str(cms2v$,,8%)
            init(hex(8c)) lfac$()
            if fieldnr% = 0% then L41105

*        Set up for Code Entry
            init(hex(8c)) lfac$()
            lfac$(fieldnr%) = hex(81)
            gosub setpf_entry
            if errormsg$ <> " " then inpmessage$ = errormsg$
            goto L41130

L41105
*        Set up for Display
            init (hex(84)) lfac$()
            inpmessage$ = errormsg$
            gosub setpf_display

L41130: accept                                                           ~
            at (01,02), "Sales Analysis Inquiry: Display History",       ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
                                                                         ~
            at (03,02), fac(hex(8c))  , group1$                 , ch(14),~
            at (03,16), fac(lfac$( 1)), code1$(1),                       ~
            at (03,49), fac(hex(8c))  , code1descr$             , ch(32),~
                                                                         ~
            at (04,02), fac(hex(8c))  , group2$                 , ch(14),~
            at (04,16), fac(lfac$( 2)), code2$(1),                       ~
            at (04,49), fac(hex(8c))  , code2descr$             , ch(32),~
                                                                         ~
            at (05,08), "-Column 1-",                                    ~
            at (05,20), "-Column 2-",                                    ~
            at (05,32), "-Column 3-",                                    ~
            at (05,44), "-Column 4-",                                    ~
            at (05,56), "-Column 5-",                                    ~
            at (05,68), "-Column 6-",                                    ~
                                                                         ~
            at (06,08), fac(hex(8c)),   hdr1$(1)                , ch(10),~
            at (06,20), fac(hex(8c)),   hdr1$(2)                , ch(10),~
            at (06,32), fac(hex(8c)),   hdr1$(3)                , ch(10),~
            at (06,44), fac(hex(8c)),   hdr1$(4)                , ch(10),~
            at (06,56), fac(hex(8c)),   hdr1$(5)                , ch(10),~
            at (06,68), fac(hex(8c)),   hdr1$(6)                , ch(10),~
                                                                         ~
            at (07,08), fac(hex(ac)),   hdr2$(1)                , ch(10),~
            at (07,20), fac(hex(ac)),   hdr2$(2)                , ch(10),~
            at (07,32), fac(hex(ac)),   hdr2$(3)                , ch(10),~
            at (07,44), fac(hex(ac)),   hdr2$(4)                , ch(10),~
            at (07,56), fac(hex(ac)),   hdr2$(5)                , ch(10),~
            at (07,68), fac(hex(ac)),   hdr2$(6)                , ch(10),~
                                                                         ~
            at (08,02), fac(hex(81)),   history$( 1)            , ch(79),~
            at (08,02), fac(hex(8e)),   history$( 1)            , ch(79),~
            at (09,02), fac(hex(8e)),   history$( 2)            , ch(79),~
            at (10,02), fac(hex(8e)),   history$( 3)            , ch(79),~
            at (11,02), fac(hex(8e)),   history$( 4)            , ch(79),~
            at (12,02), fac(hex(8e)),   history$( 5)            , ch(79),~
            at (13,02), fac(hex(8e)),   history$( 6)            , ch(79),~
            at (14,02), fac(hex(8e)),   history$( 7)            , ch(79),~
            at (15,02), fac(hex(8e)),   history$( 8)            , ch(79),~
            at (16,02), fac(hex(8e)),   history$( 9)            , ch(79),~
            at (17,02), fac(hex(8e)),   history$(10)            , ch(79),~
            at (18,02), fac(hex(8e)),   history$(11)            , ch(79),~
            at (19,02), fac(hex(8e)),   history$(12)            , ch(79),~
            at (20,02), fac(hex(8e)),   history$(13)            , ch(79),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L41425
                  call "MANUAL" ("SAINQRY ") : goto L41130

L41425:        if keyhit% <> 15 then L41440
                  call "PRNTSCRN" : goto L41130

L41440:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf_entry
           pf$(1) = "(1)Select New                               (8)Cha"&~
                    "nge Display  (13)Instructions"
           pf$(2) = "Summary File                                      "&~
                    "             (15)Print Screen"
           pf$(3) = "                                           (10)Sho"&~
                    "w Periods    (16)Exit Program"
           pfkey$ = hex(01ffffffffffff08ff0affff0dff0f10ffffff00)
           return

        setpf_display
           pf$(1) = "(1)Select New     (4)Re-enter Codes         (8)Cha"&~
                    "nge Display  (13)Instructions"
           pf$(2) = "Summary File   (5/21)Next XXXXXXXXXXXXXX    (9)Dis"&~
                    "play Graph   (15)Print Screen"
           pf$(3) = "               (6/22)Next XXXXXXXXXXXXXX   (10)Sho"&~
                    "w Periods    (16)Exit Program"
           pfkey$ = hex(01ffff040506ff08090affff0dff0f10ffffff001516)
           str(pf$(2%),27%,14%) = codes$(group1%, 2%)
           if group2% = 0% then str(pf$(3%),16%,27%) = " "      else     ~
                str(pf$(3%),27%,14%) = codes$(group2%, 2%)
           if group2% = 0% then str(pfkey$,6%,1%), str(pfkey$,22%,1%)    ~
                = hex(ff)
           if gui% = 0% then str(pf$(2%),40,22) = " "
           if gui% = 0% then str(pfkey$,9%,1%)  = hex(ff)
           return


        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Display of Period Start Dates for all possible years.     *~
            *************************************************************

        deffn'103(fieldnr%)
            str(line2$,63%) = "SAINQRY: " & str(cms2v$,,8%)
            inpmessage$ = "Press RETURN to return from display."


L42055:     accept                                                       ~
               at (01,02), "Sales Analysis Inquiry: Display SA Periods", ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (06,26), "S A   Y E A R   S T A R T   D A T E S",      ~
               at (07,09), fac(hex(ac)), years$(1)              , ch(08),~
               at (07,21), fac(hex(ac)), years$(2)              , ch(08),~
               at (07,33), fac(hex(ac)), years$(3)              , ch(08),~
               at (07,45), fac(hex(ac)), years$(4)              , ch(08),~
               at (07,59), fac(hex(ac)), years$(5)              , ch(08),~
               at (07,71), fac(hex(ac)), years$(6)              , ch(08),~
                                                                         ~
               at (08,04), " 1",  at(09,04), " 2",  at(10,04), " 3",     ~
               at (11,04), " 4",  at(12,04), " 5",  at(13,04), " 6",     ~
               at (14,04), " 7",  at(15,04), " 8",  at(16,04), " 9",     ~
               at (17,04), "10",  at(18,04), "11",  at(19,04), "12",     ~
               at (20,04), "13",                                         ~
                                                                         ~
               at (08,09), fac(hex(84)),  period$(1, 1),                 ~
               at (09,09), fac(hex(84)),  period$(1, 2),                 ~
               at (10,09), fac(hex(84)),  period$(1, 3),                 ~
               at (11,09), fac(hex(84)),  period$(1, 4),                 ~
               at (12,09), fac(hex(84)),  period$(1, 5),                 ~
               at (13,09), fac(hex(84)),  period$(1, 6),                 ~
               at (14,09), fac(hex(84)),  period$(1, 7),                 ~
               at (15,09), fac(hex(84)),  period$(1, 8),                 ~
               at (16,09), fac(hex(84)),  period$(1, 9),                 ~
               at (17,09), fac(hex(84)),  period$(1,10),                 ~
               at (18,09), fac(hex(84)),  period$(1,11),                 ~
               at (19,09), fac(hex(84)),  period$(1,12),                 ~
               at (20,09), fac(hex(84)),  period$(1,13),                 ~
                                                                         ~
               at (08,21), fac(hex(84)),  period$(2, 1),                 ~
               at (09,21), fac(hex(84)),  period$(2, 2),                 ~
               at (10,21), fac(hex(84)),  period$(2, 3),                 ~
               at (11,21), fac(hex(84)),  period$(2, 4),                 ~
               at (12,21), fac(hex(84)),  period$(2, 5),                 ~
               at (13,21), fac(hex(84)),  period$(2, 6),                 ~
               at (14,21), fac(hex(84)),  period$(2, 7),                 ~
               at (15,21), fac(hex(84)),  period$(2, 8),                 ~
               at (16,21), fac(hex(84)),  period$(2, 9),                 ~
               at (17,21), fac(hex(84)),  period$(2,10),                 ~
               at (18,21), fac(hex(84)),  period$(2,11),                 ~
               at (19,21), fac(hex(84)),  period$(2,12),                 ~
               at (20,21), fac(hex(84)),  period$(2,13),                 ~
                                                                         ~
               at (08,33), fac(hex(84)),  period$(3, 1),                 ~
               at (09,33), fac(hex(84)),  period$(3, 2),                 ~
               at (10,33), fac(hex(84)),  period$(3, 3),                 ~
               at (11,33), fac(hex(84)),  period$(3, 4),                 ~
               at (12,33), fac(hex(84)),  period$(3, 5),                 ~
               at (13,33), fac(hex(84)),  period$(3, 6),                 ~
               at (14,33), fac(hex(84)),  period$(3, 7),                 ~
               at (15,33), fac(hex(84)),  period$(3, 8),                 ~
               at (16,33), fac(hex(84)),  period$(3, 9),                 ~
               at (17,33), fac(hex(84)),  period$(3,10),                 ~
               at (18,33), fac(hex(84)),  period$(3,11),                 ~
               at (19,33), fac(hex(84)),  period$(3,12),                 ~
               at (20,33), fac(hex(84)),  period$(3,13),                 ~
                                                                         ~
               at (08,45), fac(hex(84)),  period$(4, 1),                 ~
               at (09,45), fac(hex(84)),  period$(4, 2),                 ~
               at (10,45), fac(hex(84)),  period$(4, 3),                 ~
               at (11,45), fac(hex(84)),  period$(4, 4),                 ~
               at (12,45), fac(hex(84)),  period$(4, 5),                 ~
               at (13,45), fac(hex(84)),  period$(4, 6),                 ~
               at (14,45), fac(hex(84)),  period$(4, 7),                 ~
               at (15,45), fac(hex(84)),  period$(4, 8),                 ~
               at (16,45), fac(hex(84)),  period$(4, 9),                 ~
               at (17,45), fac(hex(84)),  period$(4,10),                 ~
               at (18,45), fac(hex(84)),  period$(4,11),                 ~
               at (19,45), fac(hex(84)),  period$(4,12),                 ~
               at (20,45), fac(hex(84)),  period$(4,13),                 ~
                                                                         ~
               at (08,59), fac(hex(84)),  period$(5, 1),                 ~
               at (09,59), fac(hex(84)),  period$(5, 2),                 ~
               at (10,59), fac(hex(84)),  period$(5, 3),                 ~
               at (11,59), fac(hex(84)),  period$(5, 4),                 ~
               at (12,59), fac(hex(84)),  period$(5, 5),                 ~
               at (13,59), fac(hex(84)),  period$(5, 6),                 ~
               at (14,59), fac(hex(84)),  period$(5, 7),                 ~
               at (15,59), fac(hex(84)),  period$(5, 8),                 ~
               at (16,59), fac(hex(84)),  period$(5, 9),                 ~
               at (17,59), fac(hex(84)),  period$(5,10),                 ~
               at (18,59), fac(hex(84)),  period$(5,11),                 ~
               at (19,59), fac(hex(84)),  period$(5,12),                 ~
               at (20,59), fac(hex(84)),  period$(5,13),                 ~
                                                                         ~
               at (08,71), fac(hex(84)),  period$(6, 1),                 ~
               at (09,71), fac(hex(84)),  period$(6, 2),                 ~
               at (10,71), fac(hex(84)),  period$(6, 3),                 ~
               at (11,71), fac(hex(84)),  period$(6, 4),                 ~
               at (12,71), fac(hex(84)),  period$(6, 5),                 ~
               at (13,71), fac(hex(84)),  period$(6, 6),                 ~
               at (14,71), fac(hex(84)),  period$(6, 7),                 ~
               at (15,71), fac(hex(84)),  period$(6, 8),                 ~
               at (16,71), fac(hex(84)),  period$(6, 9),                 ~
               at (17,71), fac(hex(84)),  period$(6,10),                 ~
               at (18,71), fac(hex(84)),  period$(6,11),                 ~
               at (19,71), fac(hex(84)),  period$(6,12),                 ~
               at (20,71), fac(hex(84)),  period$(6,13),                 ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
                   keys(hex(00010d0f)),  key (keyhit%)


               if keyhit% <> 13 then L42630
                  call "MANUAL" ("SAINQRY ")
                  goto L42055

L42630:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42055

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151
            errormsg$ = " "

*        Edit Display Parameters
        init (hex(89)) pfac$()

        for c% = 1% to 6%
            if str(oprs$(c%),,1) <> " " then L50095
        next c%
            errormsg$ = "You must specify at least one operand"
            init (hex(81)) pfac$()
            return

L50095: for c% = 1% to 6%            /* Major Loop on Column       */
*        Test for inactive column
            if str(oprs$(c%),,1) <> " " then L50135
                yrs$(c%) = " "  :  yrs%(c%) = 0%
                ytd$(c%) = " "  :  ytd%(c%) = 0%
                str(oprs$(c%)) = " "
                goto L50465

L50135
*        First Test Year
            if pos("+-%" = str(oprs$(c%),,1)) <> 0% then yrs$(c%) = " "
            if pos("+-%" = str(oprs$(c%),,1)) <> 0% then yrs%(c%) = 0%
            if pos("+-%" = str(oprs$(c%),,1)) <> 0% then L50195

            if yrs$(c%) <> " " then L50180
L50165:         errormsg$ = "Year Code must be 1-6."
                pfac$(c%, 1%) = hex(81)
                return
L50180:     convert yrs$(c%) to yrs%(c%), data goto L50165
            if yrs%(c%) < 0% or yrs%(c%) > 6%  then L50165

L50195
*        Next Test Y-T-D
            if yrs$(c%) <> " " then L50215
                ytd$(c%) = " "
                goto L50235
L50215:     if ytd$(c%) = "Y" or ytd$(c%) = "N" then L50235
                errormsg$ = "Enter 'Y' or 'N' for Y-T-D Option."
                pfac$(c%, 2%) = hex(81)
                return
L50235:     if ytd$(c%) = "Y" then ytd%(c%) = 1% else ytd%(c%) = 0%


*        Now test Column Contents Operands
            if pos("+-%" = str(oprs$(c%),,1)) <> 0% then L50305

*        Contents Operand Indicated or Unknown entry
            if pos("123456789" = str(oprs$(c%),,1)) <> 0% then L50290
                errormsg$ = "Enter 1 - 9, '+', '-', or '%' for Operand."
                pfac$(c%,3) = hex(81)
                return
L50290:     str(oprs$(c%),2) = " "
            goto L50465

L50305
*        Arithmetic Operand Indicated
            call "SPCSMASH" (str(oprs$(c%),2,5))  /* Pack 'em up       */
            if str(oprs$(c%),2,2) <> " " then L50345
                errormsg$ = "Please specify which columns to use."
                pfac$(c%,4), pfac$(c%,5), pfac$(c%,6), pfac$(c%,7),      ~
                pfac$(c%,8)  = hex(81)
                return

L50345:     if pos("-%" = str(oprs$(c%),1,1)) > 0% then                  ~
                                                 str(oprs$(c%),4) = " "

            for p% = 2% to 6%
                if str(oprs$(c%),p%,1) = " " then L50460

                convert str(oprs$(c%),p%,1) to temp%, data goto L50390
                if temp% < 1% or temp% > 6 then L50390
                goto L50405
L50390:              errormsg$ = "Column number must be 1 - 6"
L50395:              pfac$(c%,2%+p%) = hex(81)
                     return
L50405:         if str(oprs$(temp%),1,1) <> " " then L50420
                     errormsg$ = "Cannot reference an unused Column"
                     goto L50395
L50420:         if temp% <> c% then L50435
                     errormsg$ = "Cannot use the Column you are on"
                     goto L50395
L50435:         if pos("+-%" = str(oprs$(temp%),1,1)) > 0% and           ~
                   temp% > c% then                                       ~
                     errormsg$ = "Cannot reference a Calculated field" & ~
                                 " greater than the column you are on."
                if errormsg$ <> " " then L50395
L50460:     next p%
L50465: next c%

        gosub describe_parameters

        return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test the Group Codes entered.                             *~
            *************************************************************

        deffn'152(c%)
            errormsg$ = " "
            on c% goto L51100, L51160

L51100
*        Set Up for Group Code 1
            code1descr$ = " "
            if code1$(1%) = "?" then code1$(1%) = " "
            codex$ = code1$(1%)  : g% = group1% : file% = file1%
            gosub L51230
            code1$(1%) = codex$  :  code1descr$ = codexdescr$
            return

L51160
*        Set Up for Group Code 2
            code2descr$ = " "
            if code2$(1%) = "?" then code2$(1%) = " "
            codex$ = code2$(1%)  : g% = group2% : file% = file2%
            gosub L51230
            code2$(1%) = codex$  :  code2descr$ = codexdescr$
            return


L51230
*        Get Group Code                        CODEX$
            codexdescr$ = hex(06) & "Select " & codes$(g%,2%)
            if file% = 6% then L51330

*         Get code using GETCODE
            if keyhit% <> 21% and keyhit% <> 22% then goto L51280
                readkey$ = codex$
                gosub describe_it
                goto L51290
L51280:     call "GETCODE" (#file%, codex$, codexdescr$, 0%, 0, onfile%)
L51290:     if onfile% = 1% then return
                if codex$ = " " then goto blank_error
                if nonstock$ <> "Y" or file% <> 4% then goto L51300
                     codexdescr$ = "Non-Stocked Part"
                     return
L51300:         codexdescr$ = codes$(g%,2%) & " not found on file."
                return

L51330
*         Get code using PLOWCODE
            if g% = 5% then readkey$ = "CUS TYPES" & codex$
            if g% = 7% then readkey$ = "REGIONS  " & codex$
            if keyhit% <> 21% and keyhit% <> 22% then goto L51360
                gosub describe_it
                goto L51380
L51360:     call "PLOWCODE" (#file%, readkey$, codexdescr$, 9%, 0.30,    ~
                                                                 onfile%)
L51380:     if onfile% <> 0% then L51390
                if codex$ = " " then goto blank_error else goto L51300
L51390:     codex$ = str(readkey$,10%)
            return

        blank_error
*          ERRORMSG$ = "You must enter or select a code value."
            return

        describe_it
            call "DESCRIBE" (#file%, readkey$, codexdescr$, 0%, onfile%)
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
            command$ = hex7f$ & "UWVBXSAGRAPH,9999,QUIT" & hex7f$
            if gui% <> 0% then call "SENDCMD" (command$)
            end
