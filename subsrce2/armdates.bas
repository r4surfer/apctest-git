        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  DDDD    AAA   TTTTT  EEEEE   SSS    *~
            *  A   A  R   R  MM MM  D   D  A   A    T    E      S       *~
            *  AAAAA  RRRR   M M M  D   D  AAAAA    T    EEEE    SSS    *~
            *  A   A  R   R  M   M  D   D  A   A    T    E          S   *~
            *  A   A  R   R  M   M  DDDD   A   A    T    EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMDATES - Subroutine to allow entry of aging dates for   *~
            *            reporting and inquiry programs.                *~
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
            * 12/22/86 ! Original                                 ! ERN *~
            * 08/27/93 ! Allow for forward aging. FUTURES% = 2%   ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "ARMDATES"   (asof$,         /* Report As of date- unfmtd  */~
                          line1$,        /* Screen Header              */~
                          days%(),       /* Aging in Days              */~
                          futures%,      /* Allow Futures? 0=N,1=Y,2=R */~
                          eps%,          /* Change Points? 0=N, 1=Y    */~
                          descr$(),      /* Aging in Dates             */~
                          ret%)          /* Return Status Code         */

*        ASOF$ specifies that date that the aging take place from.
*
*        LINE1$ is the Screen Header that appears on the 1st line.
*
*        DAYS%() is a BI(9) array.
*          IN : Buckets (1-7) Default aging parameters ('to' points);
*                       (8  ) Minimum aging parameter;
*                       (9  ) Maximum aging parameter.
*                       (10 ) Number of Buckets allowed (2-7).
*
*        FUTURES% is 1% if future dates are allowed, 0% if not allowed;
*                    2% if forward aging is required (we count forward
*                    rather that backward as with 1% and 0%)
*
*        EPS% is 1% if the aging end points may be modified, else 0%.
*
*        DESCR$() is a CH(7)20 array which is returned with the aging
*          dates formatted (MM/DD/YY to MM/DD/YY).
*
*        RET%. 0% = Continue with processing; 1% = Start Over taken.
*              (If passed in = 99% then mode is 'Describe only').

        dim                                                              ~
            asof$6,                      /* Report as of Date          */~
            buckets$(7)3,                /* Bucket Descriptors         */~
            date$8,                      /* Date for screen display    */~
            dates$(7,2)8,                /* Aging in Dates             */~
            days%(10), days$(7,2)5,      /* Aging in Days              */~
            descr$(7)20,                 /* Aging Bucket Descriptions  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdrs$(3)22,                  /* Screen Column Headings     */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(7,4)1,                 /* Field Attribute Characters */~
            line1$60, line2$79,          /* 1st and 2nd Screen Lines   */~
            pf16$16, pf8$24, pf9$24      /* PF Key Literals            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "To change Days press PF-8; to change Dates "&~
                           "press PF-9.                        "
            errormsg$ = " "

            test$  = asof$  :  call "DATEFMT" (test$)
            line2$ = "Define Aging Parameters.  As of Date = " & test$
            str(line2$,62%) = "ARMDATES: " & str(cms2v$,,8%)

            hdrs$(1) = "Aging Bucket"
            hdrs$(2) = " Number of Days"
            hdrs$(3) = "  Bucket Date Ranges"

            buckets% = days%(10)
            init (" ") buckets$()
            for b% = 1% to buckets%
                buckets$(b%) = "#to"
                convert b% to str(buckets$(b%),,1), pic(0)
            next b%

            if eps% = 0% then days%(8) = -99999%
            if eps% = 0% then days%(9) =  99999%

            gosub describe_agings

            if ret% = 99% then exit_program


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf8$  = "(8)Change Aging by Days"
            pf9$  = "(9)Change Aging by Dates"
            pf16$ = "(16)Accept Aging"
            inpmessage$ = edtmessage$
            gosub'101(0%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  8% then mode% = 1%
                if keyhit%  =  9% then mode% = 2%
                if keyhit%  = 16% then datasave
                if mode%    =  0% then editpg1
            gosub'051(mode%)
                pf8$, pf9$, pf16$ = " "
L10190:     gosub'101(mode%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L10190
            gosub'151(mode%)
                if errormsg$ <> " " then L10190 else editpg1


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            ret% = 0%
            goto exit_program


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(mode%)
            inpmessage$ = " "
            on mode% gosub L20110, L20150
            return

L20110
*        Set by DAYS                           DAYS$
            inpmessage$ = "Enter Aging Days."
            return

L20150
*        Set by DAYS                           DAYS$
            inpmessage$ = "Enter Aging Dates."
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
                ret% = 1%
                goto exit_program

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(mode%)
            init(hex(8c)) lfac$()
            if mode% = 0% then L40160
                for b% = 1% to buckets% - 1% + eps%
                     lfac$(b%, mode%*2%) = hex(82)
                next b%
                if eps% = 0% then L40160
                     lfac$(1, mode%*2%-1%)     = hex(81)
                     lfac$(buckets%, mode%*2%) = hex(81)

L40160:     accept                                                       ~
               at (01,02), fac(hex(8c)), line1$,                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,12), fac(hex(ac)), hdrs$(1)               , ch(12),~
               at (06,27), fac(hex(ac)), hdrs$(2)               , ch(16),~
               at (06,47), fac(hex(ac)), hdrs$(3)               , ch(22),~
                                                                         ~
               at (07,16), fac(hex(8c))  , buckets$(1)          , ch(01),~
               at (07,34), fac(hex(8c))  , str(buckets$(1),2)   , ch(02),~
               at (07,57), fac(hex(8c))  , str(buckets$(1),2)   , ch(02),~
               at (07,27), fac(lfac$(1,1)), days$ (1,1)         , ch(05),~
               at (07,38), fac(lfac$(1,2)), days$ (1,2)         , ch(05),~
               at (07,47), fac(lfac$(1,3)), dates$(1,1)         , ch(08),~
               at (07,61), fac(lfac$(1,4)), dates$(1,2)         , ch(08),~
                                                                         ~
               at (08,16), fac(hex(8c))  , buckets$(2)          , ch(01),~
               at (08,34), fac(hex(8c))  , str(buckets$(2),2)   , ch(02),~
               at (08,57), fac(hex(8c))  , str(buckets$(2),2)   , ch(02),~
               at (08,27), fac(hex(8c))   , days$ (2,1)         , ch(05),~
               at (08,38), fac(lfac$(2,2)), days$ (2,2)         , ch(05),~
               at (08,47), fac(hex(8c))   , dates$(2,1)         , ch(08),~
               at (08,61), fac(lfac$(2,4)), dates$(2,2)         , ch(08),~
                                                                         ~
               at (09,16), fac(hex(8c))  , buckets$(3)          , ch(01),~
               at (09,34), fac(hex(8c))  , str(buckets$(3),2)   , ch(02),~
               at (09,57), fac(hex(8c))  , str(buckets$(3),2)   , ch(02),~
               at (09,27), fac(hex(8c))   , days$ (3,1)         , ch(05),~
               at (09,38), fac(lfac$(3,2)), days$ (3,2)         , ch(05),~
               at (09,47), fac(hex(8c))   , dates$(3,1)         , ch(08),~
               at (09,61), fac(lfac$(3,4)), dates$(3,2)         , ch(08),~
                                                                         ~
               at (10,16), fac(hex(8c))  , buckets$(4)          , ch(01),~
               at (10,34), fac(hex(8c))  , str(buckets$(4),2)   , ch(02),~
               at (10,57), fac(hex(8c))  , str(buckets$(4),2)   , ch(02),~
               at (10,27), fac(hex(8c))   , days$ (4,1)         , ch(05),~
               at (10,38), fac(lfac$(4,2)), days$ (4,2)         , ch(05),~
               at (10,47), fac(hex(8c))   , dates$(4,1)         , ch(08),~
               at (10,61), fac(lfac$(4,4)), dates$(4,2)         , ch(08),~
                                                                         ~
                                                                         ~
               at (11,16), fac(hex(8c))  , buckets$(5)          , ch(01),~
               at (11,34), fac(hex(8c))  , str(buckets$(5),2)   , ch(02),~
               at (11,57), fac(hex(8c))  , str(buckets$(5),2)   , ch(02),~
               at (11,27), fac(hex(8c))   , days$ (5,1)         , ch(05),~
               at (11,38), fac(lfac$(5,2)), days$ (5,2)         , ch(05),~
               at (11,47), fac(hex(8c))   , dates$(5,1)         , ch(08),~
               at (11,61), fac(lfac$(5,4)), dates$(5,2)         , ch(08),~
                                                                         ~
               at (12,16), fac(hex(8c))  , buckets$(6)          , ch(01),~
               at (12,34), fac(hex(8c))  , str(buckets$(6),2)   , ch(02),~
               at (12,57), fac(hex(8c))  , str(buckets$(6),2)   , ch(02),~
               at (12,27), fac(hex(8c))   , days$ (6,1)         , ch(05),~
               at (12,38), fac(lfac$(6,2)), days$ (6,2)         , ch(05),~
               at (12,47), fac(hex(8c))   , dates$(6,1)         , ch(08),~
               at (12,61), fac(lfac$(6,4)), dates$(6,2)         , ch(08),~
                                                                         ~
               at (13,16), fac(hex(8c))  , buckets$(7)          , ch(01),~
               at (13,34), fac(hex(8c))  , str(buckets$(7),2)   , ch(02),~
               at (13,57), fac(hex(8c))  , str(buckets$(7),2)   , ch(02),~
               at (13,27), fac(hex(8c))   , days$ (7,1)         , ch(05),~
               at (13,38), fac(lfac$(7,2)), days$ (7,2)         , ch(05),~
               at (13,47), fac(hex(8c))   , dates$(7,1)         , ch(08),~
               at (13,61), fac(lfac$(7,4)), dates$(7,2)         , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (22,20), fac(hex(8c)), pf8$,                           ~
               at (23,20), fac(hex(8c)), pf9$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000108090d0f10)),  key (keyhit%)

               if keyhit% <> 13% then L40970
                  call "MANUAL" ("ARMDATES")
                  goto L40160

L40970:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40160

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(mode%)
            errormsg$ = " "

            if eps% = 1% then L50130
                days%(8) = -99999%
                days%(9) =  99999%
                goto L50160
L50130:     if mode% = 1% then test$ = days$ (1,1) else                  ~
                               test$ = dates$(1,1)
            f%, b% = 1%  :  gosub test_days  :  days%(8) = days%
L50160:     for b% = 1% to buckets% - 1% + eps%
                if mode% = 1% then test$ = days$ (b%,2) else             ~
                                   test$ = dates$(b%,2)
                f% = futures%  :  gosub test_days  :  days%(b%) = days%
            next b%
            days%(9) = days%(buckets%)

            for b% = 1% to buckets% - 1% + eps%
                if b% = 1% then prev% = days%(8)      else               ~
                                prev% = days%(b%-1%)
                if days%(b%) > prev% then L50290
                     errormsg$ = "Entries must be in ascending order."
                     gosub error_exit
L50290:     next b%

        describe_agings
            init(" ") dates$(), days$()
            if days%(8) <> -99999% then L50350
                days$(1,1), dates$(1,1) = "FIRST"  :  goto L50370
L50350:     convert days%(8%) to days$(1,1), pic(-###0)
            if futures% = 2% then                                        ~
            call "DATE" addr("G+", asof$,  days%(8%), dates$(1,1), u3%)  ~
                else                                                     ~
            call "DATE" addr("G+", asof$, -days%(8%), dates$(1,1), u3%)
L50370:     for b% = 1% to buckets%
                convert days%(b%) to days$(b%,2), pic(-###0)
                if futures% = 2% then                                    ~
                call "DATE" addr("G+", asof$,  days%(b%), dates$(b%,2),  ~
                                                                     u3%)~
                     else                                                ~
                call "DATE" addr("G+", asof$, -days%(b%), dates$(b%,2),  ~
                                                                     u3%)
                if b% = buckets% then L50450
                     convert days%(b%)+1% to days$(b%+1%,1), pic(-###0)
                     if futures% = 2% then                               ~
                     call "DATE" addr("G+", asof$,  days%(b%)+1%,        ~
                                                    dates$(b%+1%,1), u3%)~
                          else                                           ~
                     call "DATE" addr("G+", asof$, -days%(b%)-1%,        ~
                                                    dates$(b%+1%,1), u3%)
L50450:     next b%
            if days%(9) <> 99999% then L50480
                days$(buckets%,2), dates$(buckets%,2) = "LAST"
L50480:     init(" ") descr$()
            for b% = 1% to buckets%
                if len(dates$(b%,1)) = 6% then                           ~
                                             call "DATEFMT"(dates$(b%,1))
                if len(dates$(b%,2)) = 6% then                           ~
                                             call "DATEFMT"(dates$(b%,2))
                str(descr$(b%), 1,8) = dates$(b%,1)
                str(descr$(b%),10,2) = "to"
                str(descr$(b%),13,8) = dates$(b%,2)
            next b%
            return

        test_days
            if test$ <> "FIRST" and test$ <> "LAST" then L50650
                if test$ = "FIRST" then days% = -99999%
                if test$ = "LAST"  then days% =  99999%
                return
L50650:     if mode% = 2% then L50780
                if test$ = " " then test$ = "0"
                convert test$ to days%, data goto L50680  :  goto L50700
L50680:              errormsg$ = "Invalid Days entry: " & test$
                     goto error_exit
L50700:         if f% = 0% then L50740
                     if days% >= -9999% and days% <= 9999% then return
                          errormsg$ = "Days must be -9999 to 9999."
                          goto error_exit
L50740:              if days% >= 0%     and days% <= 9999% then return
                          errormsg$ = "Days must be 0 to 9999."
                          goto error_exit

L50780:     call "DATEOK" (test$, u3%, errormsg$)
            if errormsg$ <> " " then error_exit
                call "DATUNFMT" (test$)
                if futures% < 2% then L50810
                     call "DATE" addr("G-", asof$, test$, days%, u3%)
                     return
L50810:         call "DATE" addr("G-", test$, asof$, days%, u3%)
                if f% = 1% or days% >= 0% then return
                     errormsg$ = "Future Dates Not Allowed"
                     goto error_exit

            error_exit
                errormsg$ = "Bucket #: " & errormsg$
                convert b% to str(errormsg$,8,1), pic(0)
                return clear
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
