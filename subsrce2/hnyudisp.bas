        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  U   U  DDDD   IIIII   SSS   PPPP    *~
            *  H   H  NN  N  Y   Y  U   U  D   D    I    S      P   P   *~
            *  HHHHH  N N N   YYY   U   U  D   D    I     SSS   PPPP    *~
            *  H   H  N  NN    Y    U   U  D   D    I        S  P       *~
            *  H   H  N   N    Y     UUU   DDDD   IIIII   SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYUDISP - Display HNYQUAN Info for Part Specified.       *~
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
            * 10/29/86 ! Original                                 ! ERN *~
            * 04/08/86 ! Standard Cost Project Modifications      ! ERN *~
            * 06/28/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYUDISP"   (part$,         /* Part Number to Display     */~
                          #1,            /* HNYMASTR Channel           */~
                          #2,            /* HNYHSTRY Channel           */~
                          #3,            /* STORNAME Channel           */~
                          #4 )           /* HNYDETAL Channel           */

*        Part Number must be on file or the subroutine whips right
*        back to the caller.  Also the Caller must open the files.


        dim                                                              ~
            addnsu(12), addnsc(12),      /* Additions- Units/ Cost     */~
            cursor%(2),                  /* Cursor Location            */~
            dash$79,                     /* A Dash of Dashes           */~
            dat1$8, dat2$8,              /* Date Range for HNYDDISP    */~
            date$8,                      /* Date for screen display    */~
            days%(12),                   /* Number of Days per Month   */~
            dfac$(15)1,                  /* Display Facs               */~
            dsply$(13)65,                /* Display Strings            */~
            descr$32,                    /* Part Description           */~
            errormsg$79,                 /* Error message              */~
            hdr1$79, hdr2$79,            /* Screen Column Headings     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            netu(12), netc(12),          /* Net Units / Cost           */~
            oldyear$4,                   /* Holds a year string CCYY   */~
            part$25,                     /* Part Number                */~
            pf$(3)79, pfkeys$20,         /* PF Keys                    */~
            plowkey$99,                  /* Misc Use Plow keys         */~
            readkey$50,                  /* A Read Key                 */~
            str$3, str1$3,               /* Store Info                 */~
            ttls(6),                     /* Report Totals              */~
            wdsu(12), wdsc(12),          /* Withdrawls- Units/ Cost    */~
            year$4, yy$4                 /* Year (19xx), Test Year     */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! HNYHSTRY ! Inventory Usage History File             *~
            *************************************************************~


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "HNYUDISP: " & str(cms2v$,,8)

            hdr1$ = "             --A D D S / A D J S--"  &              ~
                                " ----- U S A G E -----"  &              ~
                                " -N E T   M O V M N T-"

            hdr2$ = "   Month    " & hex(ac) & " Quantity " & hex(ac) &  ~
                    "Inv. Value"   & hex(ac) & " Quantity " & hex(ac) &  ~
                    "Inv. Value"   & hex(ac) & " Quantity " & hex(ac) &  ~
                    "Inv. Value"   & hex(8c)

            dash$ = "             ---------- ----------"  &              ~
                                " ---------- ----------"  &              ~
                                " ---------- ----------"

            days%( 1) = 31%    :      days%( 7) = 31%
            days%( 2) = 28%    :      days%( 8) = 31%
            days%( 3) = 31%    :      days%( 9) = 30%
            days%( 4) = 30%    :      days%(10) = 31%
            days%( 5) = 31%    :      days%(11) = 30%
            days%( 6) = 30%    :      days%(12) = 31%

            call "READ100" (#1, part$, f1%(1))
            if f1%(1) = 0% then exit_program
                get #1 using L09330, part$, descr$
L09330:              FMT CH(25), CH(32)
                str(line2$,,61) = "Part: " & part$ & " (" & descr$ & ")"

            plowkey$ = part$
            call "PLOWNEXT" (#2, plowkey$, 25%, f1%(2))
            if f1%(2) = 1% then L09450
                u3% = 2%
                call "ASKUSER" (u3%, "HNYHSTRY DISPLAY",                 ~
                          "There is no Usage History for this Part",     ~
                          "Press RETURN to continue...", " ")
                goto exit_program

L09450
*        Get Last Year on file for this Store
L09460:     str$, str1$ = str(plowkey$,26%,3%)
            convert (mod( val( str(plowkey$,29%,2%), 2%), 100%)) to yy$, pic(00)
            convert     ( val( str(plowkey$,29%,2%), 2%)) to oldyear$, pic(0000)
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2%))
            if f1%(2%) = 1% then L09460
                year$ = oldyear$
                init(" ") errormsg$, inpmessage$
                gosub load_screen


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************


*        Main Screen: Get Display Options
        main_screen
            gosub set_pf_main_screen
            gosub'101               /* Display & Accept Screen    */
                errormsg$   = " "
                if keyhit%  =  2% then first_year
                if keyhit%  =  4% then prev_year
                if keyhit%  =  5% then next_year
                if keyhit%  = 10% then change_store
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then main_screen

*        RETURN indicates to show detail for month where cursor is at
            mm% = cursor%(1) - 5%
            if mm% < 1% or mm% > 12% then main_screen
                dat1$ = str(year$,3,2) & "MM01"
                convert mm% to str(dat1$,3,2), pic(00)
                dat2$ = dat1$
                convert str(year$,3,2) to yy%, data goto main_screen
                days%(2) = 28
                if yy% = 4%*(yy%/4%) then days%(2) = 29%
                convert days%(mm%) to str(dat2$,5,2), pic(00)

                call "DATEFMT" (dat1$): call "DATUNFMT" (dat1$)
                call "DATEFMT" (dat2$): call "DATUNFMT" (dat2$)

                call "HNYDDISP" (part$, "D", dat1$, dat2$, str$, str$,   ~
                                 "ALL", "   ", " ", #1, #4)
                goto main_screen

        first_year
            plowkey$ = str(part$,,25) & str(str$) & hex(00)
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 0% then main_screen
                get #2 using L10380, yyyy%
L10380:              FMT XX(28), BI(2)
                convert yyyy% to year$, pic (0000)
                gosub load_screen
                goto  main_screen

        prev_year
            plowkey$ = str(part$,,25) & str(str$) & hex(00)
            first%   = 1%
L10460:     call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 0% then L10530
                get #2 using L10380, yyyy%
                convert yyyy% to temp$, pic(0000)
                if temp$ = year$ then L10530
                     yy$ = temp$
                     first% = 0%
                     goto L10460
L10530:     if first% = 0% then L10560
                errormsg$ = "Already displaying first year on file."
                goto main_screen
L10560:     year$ = yy$
            gosub load_screen
            goto  main_screen

        next_year
            convert year$ to year%
            plowkey$ = str(part$,,25) & str(str$) & bin( year%, 2% )
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 1% then L10660
                errormsg$ = "Already displaying last year on file."
                goto main_screen
L10660:     get #2 using L10380, year%
            convert year% to year$, pic(0000)
            gosub load_screen
            goto  main_screen


        change_store  /* Get Selection Criteria and Return   */
            inpmessage$ = "Enter Store to Display."
            gosub set_pf_criteria
L10750:     gosub'101
                errormsg$   = " "
                if keyhit%  =  1% then str1$ = str$
                if keyhit%  =  1% then main_screen
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L10750
            gosub'151            /* Test Store and load it's data */
                if errormsg$ <> " " then L10750
            goto main_screen


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads the Next Screen's Worth of Data.                    *~
            *************************************************************
        load_screen
            init (" ") dsply$()

            convert year$ to year%
            readkey$ = str(part$,,25) & str(str$) & bin( year%, 2% )
            call "READ100" (#2, readkey$, onfile%)
            if onfile% = 1% then L30140
                errormsg$ = "No Data on file for Store " & str$ &        ~
                            ", Year " & year$
                return
L30140:     get #2 using L30150, addnsu(), wdsu(), addnsc(), wdsc()
L30150:         FMT XX(30), 48*PD(14,4)
            mat netu = addnsu - wdsu
            mat netc = addnsc - wdsc
            mat ttls = zer
            for m% = 1% to 12%
                ttls(1) = ttls(1) + addnsu(m%)
                ttls(2) = ttls(2) + addnsc(m%)
                ttls(3) = ttls(3) + wdsu  (m%)
                ttls(4) = ttls(4) + wdsc  (m%)
                ttls(5) = ttls(5) + netu  (m%)
                ttls(6) = ttls(6) + netc  (m%)
                if addnsu(m%) <> 0 then                                  ~
                  call "CONVERT" (addnsu(m%), 2.2, str(dsply$(m%), 1,10))
                if addnsc(m%) <> 0 then                                  ~
                  call "CONVERT" (addnsc(m%), 2.2, str(dsply$(m%),12,10))
                if wdsu(m%)   <> 0 then                                  ~
                  call "CONVERT" (wdsu  (m%), 2.2, str(dsply$(m%),23,10))
                if wdsc(m%)   <> 0 then                                  ~
                  call "CONVERT" (wdsc  (m%), 2.2, str(dsply$(m%),34,10))
                if dsply$(m%) = " " then L30380
                  call "CONVERT" (netu  (m%), 2.2, str(dsply$(m%),45,10))
                  call "CONVERT" (netc  (m%), 2.2, str(dsply$(m%),56,10))
L30380:     next m%

            call "CONVERT" (ttls(1), 2.2, str(dsply$(13), 1,10))
            call "CONVERT" (ttls(2), 2.2, str(dsply$(13),12,10))
            call "CONVERT" (ttls(3), 2.2, str(dsply$(13),23,10))
            call "CONVERT" (ttls(4), 2.2, str(dsply$(13),34,10))
            call "CONVERT" (ttls(5), 2.2, str(dsply$(13),45,10))
            call "CONVERT" (ttls(6), 2.2, str(dsply$(13),56,10))

            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101                                  /* Display Screen   */

L40080:     accept                                                       ~
               at (01,02), "Display Part Usage History",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), hdr1$                  , ch(79),~
               at (05,02), fac(hex(ac)), hdr2$                  , ch(79),~
               at (04,05), fac(hex(84)), year$                  , ch(04),~
                                                                         ~
               at (06,02), "January",                                    ~
               at (07,02), "February",                                   ~
               at (08,02), "March  ",                                    ~
               at (09,02), "April  ",                                    ~
               at (10,02), "May    ",                                    ~
               at (11,02), "June   ",                                    ~
               at (12,02), "July   ",                                    ~
               at (13,02), "August",                                     ~
               at (14,02), "September",                                  ~
               at (15,02), "October",                                    ~
               at (16,02), "November",                                   ~
               at (17,02), "December",                                   ~
               at (19,02), "*TOTALS*",                                   ~
                                                                         ~
               at (06,15), fac(dfac$( 1)), dsply$( 1)           , ch(65),~
               at (07,15), fac(dfac$( 2)), dsply$( 2)           , ch(65),~
               at (08,15), fac(dfac$( 3)), dsply$( 3)           , ch(65),~
               at (09,15), fac(dfac$( 4)), dsply$( 4)           , ch(65),~
               at (10,15), fac(dfac$( 5)), dsply$( 5)           , ch(65),~
               at (11,15), fac(dfac$( 6)), dsply$( 6)           , ch(65),~
               at (12,15), fac(dfac$( 7)), dsply$( 7)           , ch(65),~
               at (13,15), fac(dfac$( 8)), dsply$( 8)           , ch(65),~
               at (14,15), fac(dfac$( 9)), dsply$( 9)           , ch(65),~
               at (15,15), fac(dfac$(10)), dsply$(10)           , ch(65),~
               at (16,15), fac(dfac$(11)), dsply$(11)           , ch(65),~
               at (17,15), fac(dfac$(12)), dsply$(12)           , ch(65),~
               at (18,02), fac(hex(8c))  , dash$                , ch(79),~
               at (19,15), fac(dfac$(13)), dsply$(13)           , ch(65),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               at (23,46), fac(lfac$    ), str1$                , ch(03),~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13 then L40600
                  call "MANUAL" ("HNYUDISP")
                  goto L40080

L40600:        if keyhit% <> 15 then L40640
                  call "PRNTSCRN"
                  goto L40080

L40640:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return


        set_pf_main_screen
           inpmessage$ = "Select Option  -OR-  Position Cursor and"  &   ~
                         " Press Return to see Movement Details."
           init (hex(84)) dfac$(), lfac$
           pf$(1) = "(2)First Year On File                             "&~
                    "             (13)Instructions"
           pf$(2) = "(4)Previous Year         (10)Display Store: XXX   "&~
                    "             (15)Print Screen"
           pf$(3) = "(5)Next Year                                      "&~
                    "             (16)Exit Display"
           pfkeys$ = hex(ff02ff0405ffffffff0affff0d0e0f10ffffff00)
           return

        set_pf_criteria
           init (hex(8c)) dfac$()
           init (hex(81)) lfac$
           pf$(1) = "(1)Exit Change                                    "&~
                    "             (13)Instructions"
           pf$(2) = "                             Display Store:       "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Display"
           pfkeys$ = hex(01ffffffffffffffffffffff0dfe0f10ffffff00)
           str(pf$(2),29,1) = hex(84)
*         STR(PF$(2),37,1) = HEX(8C)
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Store Number Entered.                                *~
            *************************************************************

        deffn'151
            errormsg$ = " "

*        First Test STORE
            call "GETCODE" (#3, str1$, " ", 0%, .30, f1%(3))
            if f1%(3) = 1% then L50150
                errormsg$ = "Store not on File."
                return

L50150
*        Now see if there is anything on file for this store.
            convert year$ to year%
            plowkey$ = str(part$,,25) & str(str1$) & bin( year%, 2%)
            call "READ100" (#2, plowkey$, f1%(2))
            if f1%(2) = 1% then L50240
                plowkey$ = str(part$,,25) & str(str1$) & hex(00)
                call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
                if f1%(2) = 1% then L50240
                     errormsg$ = "No data on file for this store."
                     return
L50240:     get #2 using L50250, year%
L50250:         FMT XX(28), BI(2)
            convert year% to year$, pic(0000)
            str$ = str1$
            gosub load_screen
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

            end
