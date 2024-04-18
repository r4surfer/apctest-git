        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   Y   Y  RRRR   EEEEE  RRRRR  PPPP   TTTTT   *~
            *  F      A   A  Y   Y  R   R  E      R   R  P   P    T     *~
            *  FFFF   AAAAA   YYY   RRRR   EEEE   RRR    PPP      T     *~
            *  F      A   A    Y    R   R  E      R  R   P        T     *~
            *  F      A   A    Y    R   R  EEEEE  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAYRERPT - THIS PROGRAM PRINTS THE PROPOSED YEAR END      *~
            *            UPDATE REPORT.  THE ACTUAL UPDATE IS DONE BY   *~
            *            RUNNING FAUPDATE.  (BOTH PROGRAMS USE THE SAME *~
            *            EXTERNAL SUBROUTINE - FAUPDSUB.)               *~
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
            * 09/12/88 ! F/A File Changes                         ! RJM *~
            * 09/21/88 ! Now Opens Files in Share Mode, not IO.   ! RJM *~
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
            inpmessage$79,               /* INPUT MESSAGE              */~
            line2$79,                    /* Screen Header              */~
            line13$50,                   /* PROMPT AT LINE 13          */~
            line14$50,                   /* PROMPT AT LINE 14          */~
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

            call "OPENFILE" (#01,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            if f2%(1) = 0 then L02490
                accept                                                   ~
                at (02,02), "THE FIXED ASSETS MASTER FILE",              ~
                at (03,02), "HAS NOT BEEN ESTABLISHED.",                 ~
                at (05,02), "PLEASE PRESS (ENTER) TO END."
                goto L65000
L02490:     call "OPENFILE" (#02,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#03,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            if f2%(2) = 0 then L02580
                accept                                                   ~
                at (02,02), "THE ACRS DEPRECIATION TABLES",              ~
                at (03,02), "HAVE NOT BEEN ESTABLISHED.",                ~
                at (05,02), "PLEASE PRESS (ENTER) TO END."
                goto L65000

L02580:     call "OPENFILE" (#07,  "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            inpmessage$ = "Enter 'YES' or 'NO' And Press RETURN To Print ~
        ~Report."


        REM GET FISCAL BEGINNING AND ENDING DATES FROM "SYSFILE2"
            call "READ100" (#03, "SWITCHS.FA       ", f1%(3))
            if f1%(3) = 0% then L09880

        REM READ THE FIXED ASSETS DATES
            get #03, using L09250, begin_date$, end_date$
L09250:         FMT XX(20), CH(8), CH(8)
            fiscal_begin$ = begin_date$
            fiscal_end$   = end_date$
            next_begin$ = fiscal_begin$
            next_end$ = fiscal_end$

            tdate$ = next_begin$
            call "DATEFMT" (tdate$, 0%, next_begin$)
            convert str(next_begin$,1%,4%) to yy%, data goto L09880
            yy% = yy% + 1%
            convert yy% to yy$, pic(####)
            next_begin$ = yy$ & str(next_begin$,5%,4%)
            call "DATECONV" (next_begin$)

            tdate$ = next_end$
            call "DATEFMT" (tdate$, 0%, next_end$)
            convert str(next_end$,1%,4%) to yy%, data goto L09880
            yy% = yy% + 1%
            convert yy% to yy$, pic(####)
            next_end$ = yy$ & str(next_end$,5%,4%)
            call "DATECONV" (next_end$)

            call "DATFMTC" (fiscal_begin$)
            call "DATFMTC" (fiscal_end$)
            call "DATFMTC" (next_begin$)
            call "DATFMTC" (next_end$)

            put str(line2$,,79) using L09520, fiscal_end$, next_end$,      ~
                                            str(cms2v$,,8)
L09520: %Fiscal Year End: ##########  Next Fiscal Yr End: ##########  FAY~
        ~YERPT: ########

            line13$="Do You Wish to Show Those Assets Retired as of the"
            line14$="End of the Fiscal Year as Being Deleted?"
            update% = 0%

            call "DATEOKC" (fiscal_begin$, u3%, errormsg$)
                if errormsg$ <> " " then L09880
            call "DATEOKC" (fiscal_end$, u3%, errormsg$)
                if errormsg$ <> " " then L09880
            goto L10000

L09880:     call "ASKUSER" (0%, "ERROR IN FIXED-ASSETS FISCAL DATES",    ~
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

            call "SHOSTAT"                                               ~
               ("Fixed Assets Pro Forma Year End Report In Progress")

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
                             fiscal_begin$,        /* MM/DD/YYYY       */~
                             fiscal_end$,          /* MM/DD/YYYY       */~
                             next_begin$,          /* MM/DD/YYYY       */~
                             next_end$)            /* MM/DD/YYYY       */

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


L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS PRO FORMA YEAR-END REPORT",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,16),                                               ~
                  "This program prints a pro forma report showing the",  ~
               at (08,16),                                               ~
                "changes that would occur during a fixed assets year",   ~
               at (09,16),                                               ~
                "end update. This is a report only. No updating or",     ~
               at (10,16),                                               ~
                "deleting will occur.",                                  ~
               at (13,16),                                               ~
                fac(hex(8c)), line13$                           , ch(50),~
               at (14,16),                                               ~
                fac(hex(8c)), line14$                           , ch(50),~
               at (14,57), fac(hex(81)), del$                   , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40810
                  call "MANUAL" ("FAYRERPT")
                  goto L40210

L40810:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            errormsg$ = " "
            REM TEST DATA FOR DO YOU WISH TO DELETE ASSET
                if del$ <> "YES" and del$ <> "NO" then                   ~
                  errormsg$ = "Please Enter 'YES' or 'NO'"
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

            call "SHOSTAT" ("One Moment Please")

            end
