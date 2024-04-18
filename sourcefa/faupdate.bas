        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   U   U  PPPP   DDDD    AAA   TTTTT  EEEEE   *~
            *  F      A   A  U   U  P   P  D   D  A   A    T    E       *~
            *  FFFF   AAAAA  U   U  PPPP   D   D  AAAAA    T    EEEE    *~
            *  F      A   A  U   U  P      D   D  A   A    T    E       *~
            *  F      A   A   UUU   P      DDDD   A   A    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAUPDATE - THIS PROGRAM ADDS THE CURRENT DEPRECIATION TO  *~
            *            THE ACCUMULATED DEPR, CALCULATES NEW DEPR,     *~
            *            OPTIONALLY DELETES RETIRED ASSETS AND UPDATES  *~
            *            THE FISCAL YEAR BEGINNING AND END DATES.       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/21/84 ! ORIGINAL                                 ! NLH *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 09/09/88 ! Updated w/ new file structures           ! RJM *~
            * 09/30/88 ! Modified test for bad fiscal dates.      ! RJM *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            begin_date$8,                /* FIXED ASSET FISCAL YR BEGIN*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            del$3,                       /* DO YOU WISH TO DELETE ASSET*/~
            end_date$8,                  /* FIXED ASSET FISCAL YR END  */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fiscal_begin$10,             /* CURRENT FISCAL BEGIN DATE  */~
            fiscal_end$10,               /* CURRENT FISCAL END DATE    */~
            line19$62,                   /* PROMPT AT LINE 19          */~
            line2$79,                                                    ~
            next_begin$10,               /* NEXT FISCAL BEGIN DATE     */~
            next_end$10,                 /* NEXT FISCAL END DATE       */~
            proceed$3,                   /* PROCEED FLAG               */~
            tdate$10,                    /* Temporary Date Variable    */~
            yy$4                         /* YEAR                       */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! FATABLE  ! FIXED ASSETS DEPRECIATION PERCENTAGES    *~
            * #03 ! SYSFILE2 ! CAELUS MANAGEMENT SYSTEM INFORMATION     *~
            * #07 ! FATEXT   ! FIXED ASSETS DESCRIPTIVE TEXT            *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "FAMASTER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 120,  keylen = 10,                      ~
                        alt key  1, keypos = 58, keylen =   1, dup

            select #02, "FATABLE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1,    keylen = 18


            select #03, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1,    keylen = 20                       ~

            select #07, "FATEXT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 125,                                   ~
                        keypos = 1,    keylen = 14                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01,  "IO   ", f2%(1 ), rslt$(1 ), axd$(1 ))
            if f2%(1) = 0 then L02270
                accept                                                   ~
                at (02,02), "THE FIXED ASSETS MASTER FILE",              ~
                at (03,02), "HAS NOT BEEN ESTABLISHED.",                 ~
                at (05,02), "PLEASE PRESS (ENTER) TO END."
                goto L65000
L02270:     call "OPENFILE" (#02,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#03,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            if f2%(2) = 0 then L02370
                accept                                                   ~
                at (02,02), "THE ACRS DEPRECIATION TABLES",              ~
                at (03,02), "HAVE NOT BEEN ESTABLISHED.",                ~
                at (05,02), "PLEASE PRESS (ENTER) TO END."
                goto L65000

L02370:     call "OPENFILE" (#07,  "IO   ", f2%(7 ), rslt$(7 ), axd$(7 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            errormsg$ = " "

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("FA ", proceed$, del$)
            if proceed$ = "Y" or del$ = "Y" then L09200
         errormsg$ = "You must be a Data Base or FA Module Administrator"~
                     & " to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", errormsg$, " ")
            goto L65000

L09200: REM GET FISCAL BEGINNING AND ENDING DATES FROM "SYSFILE2"
            call "READ100" (#03, "SWITCHS.FA", f1%(3))
            if f1%(3) = 0% then L09700

        REM READ THE FIXED ASSETS DATES
            get #03, using L09330, begin_date$, end_date$
L09330:         FMT XX(20), CH(8), CH(8)
            fiscal_begin$ = begin_date$
            fiscal_end$   = end_date$
            next_begin$ = fiscal_begin$
            next_end$ = fiscal_end$

            tdate$ = next_begin$
            call "DATEFMT" (tdate$, 0%, next_begin$)
            convert str(next_begin$,1%,4%) to yy%, data goto L09700
            yy% = yy% + 1%
            convert yy% to yy$, pic(####)
            next_begin$ = yy$ & str(next_begin$,5%,4%)
            call "DATECONV" (next_begin$)

            tdate$ = next_end$
            call "DATEFMT" (tdate$, 0%, next_end$)
            convert str(next_end$,1%,4%) to yy%, data goto L09700
            yy% = yy% + 1%
            convert yy% to yy$, pic(####)
            next_end$ = yy$ & str(next_end$,5%,4%)
            call "DATECONV" (next_end$)

            call "DATFMTC" (fiscal_begin$)
            call "DATFMTC" (fiscal_end$)
            call "DATFMTC" (next_begin$)
            call "DATFMTC" (next_end$)

            line19$ = "DO YOU WISH TO DELETE THOSE ASSETS RETIRED BEFORE ~
        ~" & next_begin$ & "?"
            update% = 1%

            put str(line2$,,79) using L09600, fiscal_end$, next_end$,      ~
                                            str(cms2v$,,8)
L09600: %Fiscal Year End: ##########  Next Fiscal Yr End: ##########  FAU~
        ~PDATE: ########

            call "DATEOKC" (fiscal_begin$, u3%, errormsg$)
                if errormsg$ <> " " then L09700
            call "DATEOKC" (fiscal_end$, u3%, errormsg$)
                if errormsg$ <> " " then L09700
            goto L10000

L09700:     call "ASKUSER" (0%, "ERROR IN FIXED-ASSETS FISCAL DATES",    ~
                          errormsg$,                                     ~
                          "CORRECT THIS BEFORE CONTINUING WITH YEAR END",~
                          "PRESS RETURN TO EXIT PROGRAM")
            goto L65000

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, del$, proceed$

L10120:         gosub L40000
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then L65000
                      if keyhit% <>  0% then L10120
                gosub L50000
                      if errormsg$ <> " " then L10120

L10230:         gosub L41000
                      if keyhit%  =  1% then inputmode
                      if keyhit% <>  0% then L10230
                gosub L51000
                      if errormsg$ <> " " then L10230


            call "SHOSTAT" ("Fixed Assets Year End Update In Progress.")

            call "DATUFMTC" (fiscal_begin$)
            call "DATUFMTC" (fiscal_end$)
            call "DATUFMTC" (next_begin$)
            call "DATUFMTC" (next_end$)

            call "FAUPDSUB" (#01,        /* FAMASTER                   */~
                             #02,        /* FATABLE                    */~
                             #03,        /* SYSFILE2                   */~
                             #07,        /* FATEXT                     */~
                             update%,    /* 1% = UPDATE, 0%=REPORT ONLY*/~
                             del$,       /* YES=DELETE DISPOSED ASSETS */~
                             fiscal_begin$,        /* MM/DD/YY         */~
                             fiscal_end$,          /* MM/DD/YY         */~
                             next_begin$,          /* MM/DD/YY         */~
                             next_end$)            /* MM/DD/YY         */

            call "DATFMTC" (fiscal_begin$)
            call "DATFMTC" (fiscal_end$)
            call "DATFMTC" (next_begin$)
            call "DATFMTC" (next_end$)



            goto L65000

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            return clear all
            goto inputmode

L40000: REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************


L40070:     accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS YEAR-END UPDATE",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,16),                                               ~
                  "        ** PLEASE READ THE FOLLOWING **",             ~
               at (06,16),                                               ~
                "This program will add the current year's depreciation", ~
               at (07,16),                                               ~
                "to the accumulated depreciation and calculate current", ~
               at (08,16),                                               ~
                "depreciation for the next fiscal year.",                ~
               at (10,16),                                               ~
                "The program will optionally delete those assets",       ~
               at (11,16),                                               ~
                "retired on or before the end of the current",           ~
               at (12,16),                                               ~
                "fiscal year.",                                          ~
               at (14,16),                                               ~
                "Before proceeding with the year-end update,",           ~
               at (15,16),                                               ~
                "(1) make sure that you have printed final copies",      ~
               at (16,16),                                               ~
                "    of all fixed assets reports, and",                  ~
               at (17,16),                                               ~
                "(2) you have a backup of the fixed assets files.",      ~
               at (19,08), fac(hex(8c)), line19$                , ch(62),~
               at (19,70), fac(hex(81)), del$                   , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   errormsg$            , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40580
                  call "MANUAL" ("FAUPDATE")
                  goto L40070

L40580:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40070

L41000: REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS YEAR-END UPDATE",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (06,19),                                               ~
                "********************************************",          ~
               at (07,19),                                               ~
                "*       FIXED ASSETS YEAR END UPDATE       *",          ~
               at (08,19),                                               ~
                "*       DO YOU WISH TO PROCEED?            *",          ~
               at (09,19),                                               ~
                "*                                          *",          ~
               at (10,19),                                               ~
                "*                                          *",          ~
               at (11,19),                                               ~
                "* ENTER 'YES' To Begin the YEAR END UPDATE *",          ~
               at (12,19),                                               ~
                "* Any Other Response Will Return You to    *",          ~
               at (13,19),                                               ~
                "* the Previous Screen.                     *",          ~
               at (14,19),                                               ~
                "********************************************",          ~
               at (08,52), fac(hex(81)), proceed$               , ch(03),~
               at (21,02), fac(hex(a4)),   errormsg$            , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41510
                  call "MANUAL" ("FAUPDATE")
                  goto L41000

L41510:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41000

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            errormsg$ = " "
            REM TEST DATA FOR DO YOU WISH TO DELETE ASSET
                if del$ <> "YES" and del$ <> "NO" then                   ~
                  errormsg$ = "PLEASE ENTER 'YES' OR 'NO'"
                return

L51000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            errormsg$ = " "
            REM TEST DATA FOR DO YOU WISH TO PROCEED
                if proceed$ = "YES" then return
                return clear
                goto inputmode
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please. . .")

            end
