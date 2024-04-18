        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  U   U  DDDD    SSS   PPPP   FFFFF   *~
            *  H   H  NN  N  Y   Y  U   U  D   D  S      P   P  F       *~
            *  HHHHH  N N N   YYY   U   U  D   D   SSS   PPPP   FFFF    *~
            *  H   H  N  NN    Y    U   U  D   D      S  P      F       *~
            *  H   H  N   N    Y     UUU   DDDD    SSS   P      F       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYUDSPF - Display HNYQUAN Info for Part Specified.       *~
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
            * 06/28/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYUDSPF"   (part$,         /* Part Number to Display     */~
                          #1,            /* HNYMASTR Channel           */~
                          #2,            /* HNYHSTRF Channel           */~
                          #3,            /* STORNAME Channel           */~
                          #4 )           /* HNYDETAL Channel           */

*        Part Number must be on file or the subroutine whips right
*        back to the caller.  Also the Caller must open the files.


        dim                                                              ~
            addnsu(13), addnsc(13),      /* Additions- Units/ Cost     */~
            blankdate$8,                 /* Blank date for comparison  */~
            cursor%(2),                  /* Cursor Location            */~
            dash$66,                     /* A Dash of Dashes           */~
            date$8,                      /* Date for screen display    */~
            dates$(17)8,                 /* Fiscal Calendar            */~
            dates2$(13)8,                /* Fiscal Calendar            */~
            dates3$(13)8,                /* Fiscal Calendar            */~
            dfac$(15)1,                  /* Display Facs               */~
            disp$(2)8,                   /* Display Strings            */~
            dsply$(15)66,                /* Display Strings            */~
            descr$32,                    /* Part Description           */~
            errormsg$79,                 /* Error message              */~
            hdr1$79, hdr2$79,            /* Screen Column Headings     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            netu(13), netc(13),          /* Net Units / Cost           */~
            part$25,                     /* Part Number                */~
            period$(13)2,                /* GL periods for display     */~
            pf$(3)79, pfkeys$20,         /* PF Keys                    */~
            plowkey$99,                  /* Misc Use Plow keys         */~
            readkey$50,                  /* A Read Key                 */~
            str$3, str1$3,               /* Store Info                 */~
            ttls(6),                     /* Report Totals              */~
            wdsu(13), wdsc(13),          /* Withdrawls- Units/ Cost    */~
            year$6, yy$6, yy1$           /* Year (19xx), Test Year     */

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
            * # 5 ! SYSFILE2 ! Fiscal dates                             *~
            *************************************************************~

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            f2% = 0
            if fs% > 0% then L09000
            call "OPENCHCK" (#5, fs%, f2%, 0%, " ")
               if fs% < 0% then L65000 /* Couldn't Open or Create */

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            u3% = 0
            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "HNYUDSPF: " & str(cms2v$,,8)

            hdr1$ = "             --A D D S / A D J S--"  &              ~
                                " ----- U S A G E -----"  &              ~
                                " -N E T   M O V M N T-"

            hdr2$ = "Fiscal Period" & hex(ac) & " Quantity" & hex(ac) &  ~
                    "Inv. Value"   & hex(ac) & " Quantity " & hex(ac) &  ~
                    "Inv. Value"   & hex(ac) & " Quantity " & hex(ac) &  ~
                    "Inv. Value"   & hex(8c)

            dash$ = "----------- ----------" & " ---------- ----------" &~
                    " ---------- ----------"

            call "READ100" (#1, part$, f1%(1))
            if f1%(1) = 0% then exit_program
                get #1 using L09390, part$, descr$
L09390:              FMT CH(25), CH(32)
                str(line2$,,61) = "Part: " & part$ & " (" & descr$ & ")"

            plowkey$ = part$
            call "PLOWNEXT" (#2, plowkey$, 25%, f1%(2))
            if f1%(2) = 1% then L09510
                u3% = 2%
                call "ASKUSER" (u3%, "HNYHSTRF DISPLAY",                 ~
                          "There is no Usage History for this Part",     ~
                          "Press RETURN to continue...", " ")
                goto exit_program

L09510
*        Get Last Year on file for this Store
L09520:     str$, str1$ = str(plowkey$,26,3)
            convert val( str(plowkey$,29,4), 4%) to year$, pic(000000)
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 1% then L09520
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
            if mm% < 1% then main_screen
               if mm% > periods% then main_screen
                if dates$(mm%) = " " or dates$(mm%) = blankdate$ ~
                  then main_screen
                call "HNYDDISP" (part$, "D", dates$(mm%), dates2$(mm%),  ~
                                 str$, str$, "ALL", "   ", " ", #1, #4)
                goto main_screen

        first_year
            plowkey$ = str(part$,,25) & str(str$) & all(hex(00))
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 0% then main_screen
                convert val( str(plowkey$,29,4), 4%) to year$, pic(000000)
                gosub load_screen
                goto  main_screen

        prev_year
            plowkey$ = str(part$,,25) & str(str$) & all(hex(00))
            first%   = 1%
L10390:     call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 0% then L10460
                yyyy% = val( str( plowkey$, 29%, 4%), 4%)
                convert yyyy% to yy$, pic(000000)
                if yy$ = year$ then L10460
                     first% = 0%
                     yy1$ = yy$
                     goto L10390
L10460:     if first% = 0% then L10490
                errormsg$ = "Already displaying first year on file."
                goto main_screen
L10490:     year$ = yy1$
            gosub load_screen
            goto  main_screen

        next_year
            convert year$ to yyyy%
            plowkey$ = str(part$,,25) & str(str$) & bin(yyyy%,4%)
            call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
            if f1%(2) = 1% then L10600
                errormsg$ = "Already displaying last year on file."
                goto main_screen
L10600:     convert val( str(plowkey$,29%,4%), 4%) to year$, pic(000000)
*           year$ = str( year$, 1%, 4% )
            gosub load_screen
            goto  main_screen


        change_store  /* Get Selection Criteria and Return   */
            inpmessage$ = "Enter Store to Display."
            gosub set_pf_criteria
L10680:     gosub'101
                errormsg$   = " "
                if keyhit%  =  1% then str1$ = str$
                if keyhit%  =  1% then main_screen
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L10680
            gosub'151            /* Test Store and load it's data */
                if errormsg$ <> " " then L10680
            goto main_screen

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads the Next Screen's Worth of Data.                    *~
            *************************************************************
        load_screen
            init (" ") dsply$(), dates$()

            convert year$ to yyyy%
            readkey$ = str(part$,,25) & str(str$) & bin(yyyy%,4%)
            call "READ100" (#2, readkey$, onfile%)
            if onfile% = 1% then L30150
                errormsg$ = "No Data on file for Store " & str$ &        ~
                            ", Year " & year$
                return
L30150:     get #2 using L30160, addnsu(), wdsu(), addnsc(), wdsc()
L30160:         FMT XX(32), 52*PD(14,4)
            readkey$ = "FISCAL HISTORY" & str(year$) & " "
            call "READ100" (#5, readkey$, f1%(5))
            if f1%(5) = 1 then L30210
               errormsg$ = "This year has no fiscal dates on file"
               periods% = 13%
               goto L30240
L30210:     get #5 using L30220, periods%, dates$()
L30220:     FMT POS(21), BI(2), 17*CH(8)

            for i% = 1 to periods%
                if i% = 1 then L30236
                j% = i% - 1%
                call "DATE" addr("G+", dates$(i%), -1%, dates2$(j%), ret%)
L30236:     next i%
            call "DATE" addr("G+", dates$(14), -1%, dates2$(periods%),   ~
                             ret%)

L30240:     mat netu = addnsu - wdsu
            mat netc = addnsc - wdsc
            mat ttls = zer
            for m% = 1% to periods%
                ttls(1) = ttls(1) + addnsu(m%)
                ttls(2) = ttls(2) + addnsc(m%)
                ttls(3) = ttls(3) + wdsu  (m%)
                ttls(4) = ttls(4) + wdsc  (m%)
                ttls(5) = ttls(5) + netu  (m%)
                ttls(6) = ttls(6) + netc  (m%)
                convert m% to period$(m%), pic (##)
                dates3$(m%) = dates$(m%)
                call "DATEFMT" (dates3$(m%))
                if addnsu(m%) <> 0 then                                  ~
                  call "CONVERT" (addnsu(m%), 2.2, str(dsply$(m%), 1,10))
                if addnsc(m%) <> 0 then                                  ~
                  call "CONVERT" (addnsc(m%), 2.2, str(dsply$(m%),12,10))
                if wdsu(m%)   <> 0 then                                  ~
                  call "CONVERT" (wdsu  (m%), 2.2, str(dsply$(m%),23,10))
                if wdsc(m%)   <> 0 then                                  ~
                  call "CONVERT" (wdsc  (m%), 2.2, str(dsply$(m%),34,10))
                if dsply$(m%) = " " then L30410
                  call "CONVERT" (netu  (m%), 2.2, str(dsply$(m%),45,10))
                  call "CONVERT" (netc  (m%), 2.2, str(dsply$(m%),57,10))
L30410:     next m%

            dfac$(15) = hex(84)
            call "CONVERT" (ttls(1), 2.2, str(dsply$(15), 1,10))
            call "CONVERT" (ttls(2), 2.2, str(dsply$(15),12,10))
            call "CONVERT" (ttls(3), 2.2, str(dsply$(15),23,10))
            call "CONVERT" (ttls(4), 2.2, str(dsply$(15),34,10))
            call "CONVERT" (ttls(5), 2.2, str(dsply$(15),45,10))
            call "CONVERT" (ttls(6), 2.2, str(dsply$(15),57,10))

            if periods% = 12% then L30560
               dfac$(14) = hex(8c)
               dsply$(14) = dash$
               disp$(1) = " "
               disp$(2) = "*TOTALS*"
               return

L30560:     dsply$(13) = dash$
            dfac$(13) = hex(8c)
            dsply$(14) = dsply$(15)
            dfac$(14) = hex(84)
            dfac$(15) = hex(9c)
            dates3$(13) = " "
            disp$(1) = "*TOTALS*"
            disp$(2) = " "
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
               at (04,05), fac(hex(84)), year$                  , ch(06),~
                                                                         ~
               at (06,05), fac(hex(8c)), dates3$(1)             , ch(08),~
               at (07,05), fac(hex(8c)), dates3$(2)             , ch(08),~
               at (08,05), fac(hex(8c)), dates3$(3)             , ch(08),~
               at (09,05), fac(hex(8c)), dates3$(4)             , ch(08),~
               at (10,05), fac(hex(8c)), dates3$(5)             , ch(08),~
               at (11,05), fac(hex(8c)), dates3$(6)             , ch(08),~
               at (12,05), fac(hex(8c)), dates3$(7)             , ch(08),~
               at (13,05), fac(hex(8c)), dates3$(8)             , ch(08),~
               at (14,05), fac(hex(8c)), dates3$(9)             , ch(08),~
               at (15,05), fac(hex(8c)), dates3$(10)            , ch(08),~
               at (16,05), fac(hex(8c)), dates3$(11)            , ch(08),~
               at (17,05), fac(hex(8c)), dates3$(12)            , ch(08),~
               at (18,05), fac(hex(8c)), dates3$(13)            , ch(08),~
                                                                         ~
               at (06,15), fac(dfac$( 1)), dsply$( 1)           , ch(66),~
               at (07,15), fac(dfac$( 2)), dsply$( 2)           , ch(66),~
               at (08,15), fac(dfac$( 3)), dsply$( 3)           , ch(66),~
               at (09,15), fac(dfac$( 4)), dsply$( 4)           , ch(66),~
               at (10,15), fac(dfac$( 5)), dsply$( 5)           , ch(66),~
               at (11,15), fac(dfac$( 6)), dsply$( 6)           , ch(66),~
               at (12,15), fac(dfac$( 7)), dsply$( 7)           , ch(66),~
               at (13,15), fac(dfac$( 8)), dsply$( 8)           , ch(66),~
               at (14,15), fac(dfac$( 9)), dsply$( 9)           , ch(66),~
               at (15,15), fac(dfac$(10)), dsply$(10)           , ch(66),~
               at (16,15), fac(dfac$(11)), dsply$(11)           , ch(66),~
               at (17,15), fac(dfac$(12)), dsply$(12)           , ch(66),~
               at (18,15), fac(dfac$(13)), dsply$(13)           , ch(66),~
               at (19,05), fac(hex(8c))  , disp$(1)             , ch(08),~
               at (19,15), fac(dfac$(14)), dsply$(14)           , ch(66),~
               at (20,05), fac(hex(8c))  , disp$(2)             , ch(08),~
               at (20,15), fac(dfac$(15)), dsply$(15)           , ch(66),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               at (23,46), fac(lfac$    ), str1$                , ch(03),~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13 then L40620
                  call "MANUAL" ("HNYUDSPF")
                  goto L40080

L40620:        if keyhit% <> 15 then L40660
                  call "PRNTSCRN"
                  goto L40080

L40660:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return


        set_pf_main_screen
           inpmessage$ = "Select Option  -OR-  Position Cursor and"  &   ~
                         " Press Return to see Movement Details."
           pfkeys$ = hex(ff02ff0405ffffffff0affff0d0e0f10ffffff00)
           init (hex(84)) dfac$(), lfac$
           if periods% = 12 then dfac$(15) = hex(9c)
           pf$(1) = "(2)First Year On File                             "&~
                    "             (13)Instructions"
           pf$(2) = "(4)Previous Year         (10)Display Store: XXX   "&~
                    "             (15)Print Screen"
           pf$(3) = "(5)Next Year                                      "&~
                    "             (16)Exit Display"
           return

        set_pf_criteria
           init (hex(8c)) dfac$()
           if periods% = 12 then dfac$(15) = hex(9c)
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

*        First Test Store
            call "GETCODE" (#3, str1$, " ", 0%, .30, f1%(3))
            if f1%(3) = 1% then L50150
                errormsg$ = "Store not on File."
                return

L50150
*        Now see if there is anything on file for this store.
            convert year$ to yyyy%
            plowkey$ = str(part$,,25) & str(str1$) & bin(yyyy%,4%)
            call "READ100" (#2, plowkey$, f1%(2))
            if f1%(2) = 1% then L50260
                plowkey$ = str(part$,,25) & str(str1$) & all(hex(00))
                call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2))
                if f1%(2) = 1% then L50260
                     errormsg$ = "No data on file for this store."
                     return
L50260:     convert val( str(plowkey$,29%,4%), 4%) to year$, pic(000000)
            str$ = str1$
            gosub load_screen
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

            end
