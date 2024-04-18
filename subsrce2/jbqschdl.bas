        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ    SSS    CCC   H   H  DDDD   L       *~
            *    J    B   B  Q   Q  S      C   C  H   H  D   D  L       *~
            *    J    BBBB   Q   Q   SSS   C      HHHHH  D   D  L       *~
            *  J J    B   B  Q Q Q      S  C   C  H   H  D   D  L       *~
            *   J     BBBB    QQQ    SSS    CCC   H   H  DDDD   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBQSCHDL - Displays planned and actual routing for a Job. *~
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
            * 07/08/87 ! Original                                 ! RN2 *~
            * 06/30/88 ! Changed heading literial on screen       ! RJM *~
            * 07/26/94 ! Changed RTE$ DIM to match rest of system.! JDH *~
            * 11/27/95 ! PRR 13460. Chg PF Paging Prompts.        ! JDH *~
            *          ! PRR 13239. Added Part Info.              !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBQSCHDL"   (job$,          /* Job to Display For         */~
                          #2 , #3 ,      /* SYSFILE2, HNYMASTR         */~
                          #4 , #5 ,      /* BOMMASTR, ENGMASTR         */~
                          #6 , #7 ,      /* RTEMASTR, WCMASTR          */~
                          #8 , #10,      /* WCOUT   , JBMASTR2         */~
                          #11, #12,      /* JBSTATUS, JBCROSS2         */~
                          #13, #14 )     /* JBMATER2, JBVALUE2         */

        dim                                                              ~
            actl$(300)36,                /* Actual Routing Display     */~
            date$8,                      /* Date for screen display    */~
            hdr$(5)8,                    /* Column Headings            */~
            inpmessage$79,               /* Informational Message      */~
            job$8, job_descr$32,         /* Job Info                   */~
            line2$79,                    /* 2nd Line of Screen Header  */~
            line3$79,                    /* 3rd Line of Screen Header  */~
            part$25, part_descr$34,      /* Part Info                  */~
            plan$(300)36,                /* Planned Routing Display    */~
            plowkey$99,                  /* Plow Key Variable          */~
            rte$(255)200,                /* Calling arg filler (n/u)   */~
            travel$(256)67               /* Formatted WCOUT Data       */

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
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
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
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! HNYMASTR ! Inventory Master File                    *~
            * # 4 ! BOMMASTR ! BOM relationship file                    *~
            * # 5 ! ENGMASTR ! Engineering Master Filer                 *~
            * # 6 ! RTEMASTR ! Production routing master file           *~
            * # 7 ! WCMASTR  ! Workcenter Master File                   *~
            * # 8 ! WCOUT    ! Planned work center use detail           *~
            * #10 ! JBMASTR2 ! Production job master file               *~
            * #11 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #12 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #13 ! JBMATER2 ! Job Material Ledger                      *~
            * #14 ! JBVALUE2 ! Job Value Ledger                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

*       * Get Job Description & Part Info
            call "GETCODE" (#10, job$, job_descr$, 0%, .30, f1%(10))
                if f1%(10%) = 0% then exit_program
            call "PUTPAREN" (job_descr$)
            line2$ = "Job: " & job$ & "  " & job_descr$
            str(line2$,62) = "JBQSCHDL: " & str(cms2v$,,8)
            get #10 using L09110, part$
L09110:         FMT POS(58), CH(25)
            call "GETCODE" (#3, part$, part_descr$, 0%, .32, f1%(3%))
            call "PUTPAREN" (part_descr$)
            line3$ = "For Part: " & part$ & "  " & part_descr$

*       * Other Stuff
            hdr$(1) = "Cntr"
            hdr$(2) = "  From  "
            hdr$(3) = "   To   "
            hdr$(4) = " Hours"
            hdr$(5) = "Quantity"

*       * Load Data for Display
            gosub load_data

        REM *************************************************************~
            *             D I S P L A Y   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Control Displaying of Information.                        *~
            *************************************************************

        display_screen
            gosub'101
                if keyhit%  =  2% then plan_off% = 0%
                if keyhit%  =  3% then plan_off% = 9999%
                if keyhit%  =  4% then plan_off% = plan_off% - 12%
                if keyhit%  =  5% then plan_off% = plan_off% + 12%
                if keyhit%  =  6% then plan_off% = plan_off% -  1%
                if keyhit%  =  7% then plan_off% = plan_off% +  1%
                                       plan_off% = min(plan_off%,        ~
                                                       plan_max% - 12%)
                                       plan_off% = max(0%, plan_off%)
                if keyhit%  =  8% then prod_times
                if keyhit%  = 16% then exit_program

                if keyhit%  = 18% then actl_off% = 0%
                if keyhit%  = 19% then actl_off% = 9999%
                if keyhit%  = 20% then actl_off% = actl_off% - 12%
                if keyhit%  = 21% then actl_off% = actl_off% + 12%
                if keyhit%  = 22% then actl_off% = actl_off% -  1%
                if keyhit%  = 23% then actl_off% = actl_off% +  1%
                                       actl_off% = min(actl_off%,        ~
                                                       actl_max% - 12%)
                                       actl_off% = max(0%, actl_off%)
                goto display_screen


        prod_times
            call "JBSEEACT" (job$, #11, #7, #3, #10, #6, #12)
            goto display_screen


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
            init (" ") plan$(), actl$()
            plan_max%, plan_off%, actl_max%, actl_off% = 0%

*        Load Planned Routing
            call "PLNTRVLR" ("JOB ORDER: " & str(job$),                  ~
                             #6 , #8 ,   /* RTEMASTR, WCOUT            */~
                             #12, #4 ,   /* JBCROSS2, BOMMASTR         */~
                             #5 , #2 ,   /* ENGMASTR SYSFILE2          */~
                             #10,        /* JBMASTR2                   */~
                             " ",        /* Route For Tag (Returned)   */~
                             " ",        /* Bill  For Tag (Returned)   */~
                             rte$(),     /* Phantomized Routing        */~
                             0%,         /* Number of Elements in Array*/~
                             travel$(),  /* Traveler Array             */~
                             plan_max%)  /* Number Of Elements in Array*/

            if plan_max% = 0% then L30390

            for i% = 1% to plan_max%
                get travel$(i%) using L30300,                             ~
                     str(plan$(i%),  , 4),         /* Work Center      */~
                     su, run,                      /* Setup & Run Hrs  */~
                     str(plan$(i%), 6, 6),         /* From Date        */~
                     str(plan$(i%),15, 6)          /* To   Date        */
L30300:                 FMT XX(9), CH(4), XX(14), 2*BI(2), XX(6), 2*CH(6)
                call "DATEFMT" (str(plan$(i%), 6,8))
                call "DATEFMT" (str(plan$(i%),15,8))
                call "WCUN2HRS" (#7, str(plan$(i%),,4), 0, su , " ")
                call "WCUN2HRS" (#7, str(plan$(i%),,4), 0, run, " ")
                call "CONVERT" (su , 2.2, str(plan$(i%),24,6))
                call "CONVERT" (run, 2.2, str(plan$(i%),31,6))
            next i%

L30390
*        Load Actual Routing
            plowkey$ = job$
            str(plowkey$,9) = all(hex(00))

L30430:     call "PLOWNEXT" (#11, plowkey$, 8%, f1%(11))
            if f1%(11) = 0% then return
                i%, actl_max% = actl_max% + 1%
                get #11 using L30500,                                     ~
                     str(actl$(i%),  , 4),         /* Work Center      */~
                     str(actl$(i%), 6, 8),         /* Transaction Date */~
                     su, run, qty
L30500:                   FMT XX(28), CH(4), CH(6), XX(52), 2*BI(4),     ~
                              PD(14,4)

                if str(actl$(i%),,4) <> " " then L30580
                     actl$(i%) = " "
                     i%, actl_max% = actl_max% - 1%
                     goto L30430

L30580:         call "WCUN2HRS" (#7, str(actl$(i%),,4), 0, su , " ")
                call "WCUN2HRS" (#7, str(actl$(i%),,4), 0, run, " ")

                call "DATEFMT" (str(actl$(i%),6,8))
                call "CONVERT" (qty, 0.2, str(actl$(i%),15,8))
                call "CONVERT" (su , 2.2, str(actl$(i%),24,6))
                call "CONVERT" (run, 2.2, str(actl$(i%),31,6))
                goto L30430

            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101

L40080:     accept                                                       ~
               at (01,02), "Job Routing Display",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), line3$                 , ch(79),~
                                                                         ~
               at (05,02), "-- REMAINING PROD ROUTE AS PLANNED  --",     ~
               at (06,02), "Work                   Set-up   Run ",       ~
               at (07,02), fac(hex(ac)), hdr$(1)                , ch(04),~
               at (07,07), fac(hex(ac)), hdr$(2)                , ch(08),~
               at (07,16), fac(hex(ac)), hdr$(3)                , ch(08),~
               at (07,25), fac(hex(ac)), hdr$(4)                , ch(06),~
               at (07,32), fac(hex(ac)), hdr$(4)                , ch(06),~
                                                                         ~
               at (08,02), fac(hex(84)), plan$( 1% + plan_off%) , ch(36),~
               at (09,02), fac(hex(84)), plan$( 2% + plan_off%) , ch(36),~
               at (10,02), fac(hex(84)), plan$( 3% + plan_off%) , ch(36),~
               at (11,02), fac(hex(84)), plan$( 4% + plan_off%) , ch(36),~
               at (12,02), fac(hex(84)), plan$( 5% + plan_off%) , ch(36),~
               at (13,02), fac(hex(84)), plan$( 6% + plan_off%) , ch(36),~
               at (14,02), fac(hex(84)), plan$( 7% + plan_off%) , ch(36),~
               at (15,02), fac(hex(84)), plan$( 8% + plan_off%) , ch(36),~
               at (16,02), fac(hex(84)), plan$( 9% + plan_off%) , ch(36),~
               at (17,02), fac(hex(84)), plan$(10% + plan_off%) , ch(36),~
               at (18,02), fac(hex(84)), plan$(11% + plan_off%) , ch(36),~
               at (19,02), fac(hex(84)), plan$(12% + plan_off%) , ch(36),~
               at (20,02), fac(hex(84)), plan$(13% + plan_off%) , ch(36),~
                                                                         ~
               at (05,44), "--- PRODUCTION ROUTE AS REPORTED ---",       ~
               at (06,44), "Work                   Set-up   Run ",       ~
               at (07,44), fac(hex(ac)), hdr$(1)                , ch(04),~
               at (07,49), fac(hex(ac)), hdr$(2)                , ch(08),~
               at (07,58), fac(hex(ac)), hdr$(5)                , ch(08),~
               at (07,67), fac(hex(ac)), hdr$(4)                , ch(06),~
               at (07,74), fac(hex(ac)), hdr$(4)                , ch(06),~
                                                                         ~
               at (08,44), fac(hex(84)), actl$( 1% + actl_off%) , ch(36),~
               at (09,44), fac(hex(84)), actl$( 2% + actl_off%) , ch(36),~
               at (10,44), fac(hex(84)), actl$( 3% + actl_off%) , ch(36),~
               at (11,44), fac(hex(84)), actl$( 4% + actl_off%) , ch(36),~
               at (12,44), fac(hex(84)), actl$( 5% + actl_off%) , ch(36),~
               at (13,44), fac(hex(84)), actl$( 6% + actl_off%) , ch(36),~
               at (14,44), fac(hex(84)), actl$( 7% + actl_off%) , ch(36),~
               at (15,44), fac(hex(84)), actl$( 8% + actl_off%) , ch(36),~
               at (16,44), fac(hex(84)), actl$( 9% + actl_off%) , ch(36),~
               at (17,44), fac(hex(84)), actl$(10% + actl_off%) , ch(36),~
               at (18,44), fac(hex(84)), actl$(11% + actl_off%) , ch(36),~
               at (19,44), fac(hex(84)), actl$(12% + actl_off%) , ch(36),~
               at (20,44), fac(hex(84)), actl$(13% + actl_off%) , ch(36),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "Screen Control: Plan 2-7, Actual 18-23",     ~
               at (23,02), " (2/18)First  (4/20)Prev  (6/22)Down",       ~
               at (24,02), " (3/19)Last   (5/21)Next  (7/23)Up  ",       ~
               at (23,40), "(8)See Production Times",                    ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Display",                           ~
                                                                         ~
               keys(hex(ff02030405060708ffffffff0d0f10ff121314151617ff)),~
               key (keyhit%)

               if keyhit% <> 13 then L40740
                  call "MANUAL" ("JBQSCHDL")
                  goto L40080

L40740:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40080


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution.                                     *~
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
            end
