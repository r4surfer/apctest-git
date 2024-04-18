        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    AAA   L       CCCC  H   H  N   N   GGG   3333    *~
            *  C   C  A   A  L      C      H   H  NN  N  G          3   *~
            *  C      AAAAA  L      C      HHHHH  N N N  G GGG  3333    *~
            *  C   C  A   A  L      C      H   H  N  NN  G   G      3   *~
            *   CCC   A   A  LLLLL   CCCC  H   H  N   N   GGG   3333    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CALCHNG3 - POST A NEW PLANNING CALANDER THROUGHOUT THE    *~
            *            PLANNING SYSTEM.                               *~
            *            THIS IS A CONSIDERABLE TASK, AND IS THEREFORE  *~
            *            A STAND ALONE OPERATION.  THE NEW CALANDER     *~
            *            MUST HAVE BEEN ENTERED THROUGH CALINPUT.       *~
            *            THIS PROGRAM IS RAN BY CALPROC (PROCEDURE).    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/15/83 ! ORIGINAL                                 ! HES *~
            * 12/16/85 ! WCMASTR file format change               ! HES *~
            * 10/20/86 ! WCOUT file format change                 ! HES *~
            * 10/11/93 ! Purchased Job 'BWs' treated same as 'WOs'! MLJ *~
            * 11/11/93 ! Purchased Job- 'ROs'& 'RWs' same as 'WOs'! JBK *~
            * 09/17/96 ! Millie 2000 - Blank date compare         ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            day1$(490)2,                                                 ~
            day2$(490)2,                                                 ~
            newday1$(490)2,                                              ~
            newday2$(490)2,                                              ~
            hol$(490)1,                                                  ~
            newhol$(490)1,                                               ~
            yymmdd$(490)6,                                               ~
            dow$(490)3,                                                  ~
            eng$(490)3,                                                  ~
            yy%(490),                                                    ~
            mm%(490),                                                    ~
            dd%(490),                                                    ~
            mwoy%(490),                                                  ~
            fwoy%(490),                                                  ~
            cqoy%(490),                                                  ~
            fqoy%(490),                                                  ~
            nhw%(7),                                                     ~
            wds$23,                                                      ~
            a36$36,                                                      ~
            a5$5,                                                        ~
            a20$20,                                                      ~
            a27$27,                                                      ~
            a39$39,                                                      ~
            a9$9,                                                        ~
            a235$101,                                                    ~
            a245$245,                                                    ~
            a29$29

        dim                                                              ~
            blankdate$8,                 /* blank unfmt date           */~
            errormsg$79,                                                 ~
            old$6,                       /* OLD PLAN PERIOD START DATE */~
            new$6                        /* NEW PLAN PERIOD START DATE */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! SYSTEM INFORMATION FILE FOR FISCAL YEAR  *~
            * #07 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * #08 ! XWCOUT   ! ORIGINAL Planned work center use detail  *~
            * #17 ! WCOUT    ! NEW Planned work center use detail       *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            * #13 ! ENGMASTR ! EFFECTIVITY RECORDS                      *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos =   1, keylen =  20

            select #07, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #08, "XWCOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27

            select #17, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27

           select #12, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

           select #13, "ENGMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            call "SHOSTAT" ("Opening Files, One Moment Please")

            select pool #7, #8, blocks = 50, pool #17, blocks = 50

            errormsg$ = "Error Encountered Opening Files"

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
                     if f2%( 1) <> 0 then goto abort
            close # 1:f2%(1)=1%
            call "OPENFILE" (#01, "IO   ", f2%(01), rslt$(01), axd$(01))

            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
                     if f2%( 7) <> 0 then L02670
            close # 7:f2%( 7)=1%
            call "OPENFILE" (#07, "IO   ", f2%(07), rslt$(07), axd$(07))

L02670:     call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
                     if f2%( 8) <> 0 then L02720
            close # 8:f2%( 8)=1%
            call "OPENFILE" (#08, "IO   ", f2%(08), rslt$(08), axd$(08))

L02720:     call "OPENFILE" (#17, "SHARE", f2%(17), rslt$(17), axd$(17))
                     if f2%(17) <> 0 then L02760
            close #17:f2%(17)=1%
            call "OPENFILE" (#17, "IO   ", f2%(17), rslt$(17), axd$(17))
L02760:
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
                     if f2%(13) <> 0 then L02820
            close #13:f2%(13)=1%
            call "OPENFILE" (#13, "IO   ", f2%(13), rslt$(13), axd$(13))

L02820:     call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
                     if f2%(12) <> 0 then goto abort

            if f2%(17) = 0 then L02890
            if f2%( 8) = 0 then abort
            goto L10000

L02890:     if f2%( 8) <> 0 then abort

L10000: REM *************************************************************~
            *       M A I N   P R O C E S S I N G   S E C T I O N       *~
            *                                                           *~
            * RE-DO ALL THE FILES THAT REFERENCE CALMASTR.              *~
            *************************************************************

            gosub L30000                  /* LOAD CALENDERS */

            if old$ <> " " and old$ <> blankdate$ then L10090
            howfar% = 1
            goto L10120     /*????*/

L10090:     call "DATE" addr("G-", old$, new$, howfar%, r%)
            if r% <> 0 or howfar% < 1 or howfar% > 489 then abort

L10120:     call "SHOSTAT" ("Rolling Planned Inventory Dates")

            gosub'98 ("'WCMASTR'")    /* PROCESS WCMASTR */

            wds$ = "**MONTUEWEDTHUFRISATSUN"
L10170:     init (hex(00)) newday2$()
            call "READNXT1" (#7, f1%(7)) : if f1%(7) = 0 then L10330
            get #7 using L26040, a36$, nhw%(), a9$, day1$(), day2$(), a5$
            str(newday1$(),,(490-howfar%)*2) = str(day1$(), howfar%*2+1)
            str(newday2$(),,(490-howfar%)*2) = str(day2$(), howfar%*2+1)

            for i% = 491 - howfar% to 490
               search wds$ = str(dow$(i%),,3) to yy%()
               newday1$(i%) = bin(nhw%(yy%(1)/3),2)
            next i%

            rewrite #7, using L26040, a36$, nhw%(), a9$, newday1$(),      ~
                                                          newday2$(), a5$
            gosub L10860
            goto L10170

L10330:     gosub'98 ("'WCOUT'")   /* PROCESS WCOUT */

            errormsg$ = "Error encountered processing WC details"

L10370:     call "READNXT1" (#8, f1%(8)) : if f1%(8) = 0 then L10510
            get #8 using L26120, a27$, day%, a39$
            day% = max(day% -  howfar%, 0)
            on day% + 1 goto abort
            put str(a27$,5,2), using L10402, day%
L10402:     FMT BI(2)
            if str(a27$,9%,2%)  = "BO" then L10430
            if str(a27$,9%,2%)  = "BW" then L10430
            if str(a27$,9%,2%)  = "RO" then L10430
            if str(a27$,9%,2%)  = "RW" then L10430
            if str(a27$,9%,2%) <> "WO" then L10470
L10430:            convert str(a27$,11%,3%) to day1%, data goto L10470
                   day1% = max(day1% -  howfar%, 0)
                   on day1% + 1% goto abort
                   convert day1% to str(a27$,11%,3%),pic(###)
L10470:     write #17, using L26120, a27$, day%, a39$
            gosub L10860
            goto L10370

L10510:     gosub'98 ("'SYSFILE2'")    /* PROCESS HOLMASTR */

            call "READ101" (#1, "HOLIDAY SCHEDULE", f1%(1))
                if f1%(1) = 0 then L10640
            get #1, using L10570, a20$, a245$, a235$
            hexunpack a245$ to str(hol$())
L10570:     FMT CH(20), CH(245), CH(235)
            newhol$() = all("0")
            str(newhol$(),,491-(howfar%+1)) = str(hol$(), howfar% + 1)
            hexpack a245$ from str(newhol$())
            rewrite #1, using L10570, a20$, a245$, a235$
            gosub L10860

L10640:     gosub'98 ("'ENGMASTR'")   /* PROCESS ENGMASTR */

L10660:     call "READNXT1" (#13, f1%(13)) : if f1%(13) = 0 then L19000
            get #13 using L10680, a29$, dow$()
L10680:     FMT CH(29), 490*CH(3), CH(516)
            if str(a29$,26,4) <> "1001" then L10660
            eng$() = str(dow$(), howfar% * 3 + 1)
            for i% = 491 - howfar% to 490
            eng$(i%) = dow$(490)
            next i%
            rewrite #13, using L10680, a29$, eng$(), " "
            gosub L10860
            goto L10660

        REM COMMON FILE DISPLAY AND COUNT ROUTINES

            deffn'98 (current_file$)
            c% = 0
            print at(22,1,); "Current file is "; hex(84); current_file$; ~
                  at(23,1,); "Current count is "
            return

L10860:     c% = c% + 1
            if mod(c%,100) <> 0 then return
            print at (23,18); hex(84); c%
            return

L19000: REM *************************************************************~
            * CLEAN UP & GET THE HECK OUT OF DODGE.                     *~
            *************************************************************

            call "SHOSTAT" ("Saving New Calendar")

            gosub replace_old_cal
            errormsg$ = "Error On Finding Month Open Record In SYSFILE2"
            call "READ101" (#1, "MONTHS OPEN", f1%(1))
                if f1%(1) = 0 then abort
            put #1, using L19110, yymmdd$(1)
L19110:     FMT POS(33), CH(6)
            rewrite #1
            goto L65000

        REM *************************************************************~
            *          F O R M A T     S T A T E M E N T S              *~
            *************************************************************

L26040: FMT                      /* FILE: WCMASTR                      */~
            CH(36),                                                      ~
            7*BI(2),                                                     ~
            CH(9),                                                       ~
            490*CH(2),           /* amount of wc capacity available    */~
            490*CH(2),           /* amount of wc capacity used         */~
            CH(5)                /* filler for rest of record or inter */~

L26120: FMT                      /* FILE: WCOUT                        */~
            CH(27),                                                      ~
            BI(2),               /* Date out of PIP in date subscript  */~
            CH(39)               /* Part code                          */~

L30000: rem**************************************************************~
            *               l o a d   c a l a n d e r s                 *~
            *************************************************************

            errormsg$ = "Error encountered loading calendar"

            call "READ100" (#12, "A0", f1%(12))
                if f1%(12) = 0 then goto abort
            get #12, using L30070, new$
L30070:         FMT XX(2), CH(6)

            call "READ100" (#12, "10", f1%(12))
                if f1%(12) = 0 then return
            get #12, using L30120, old$
L30120:         FMT XX(2), CH(6)

            call "READ101" (#12, "E0", f1%(12))
                if f1%(12) = 0 then abort
            get #12, using L30136,  dow$()
            return
L30136:         FMT XX(2), 490*CH(3)

             rem*********************************************************~
             *              a b a n d o n   s h i p !                   *~
             *                this boat is sinking!                     *~
             ************************************************************~

        abort

        accept                                                           ~
            at(04,02), "ERROR HIT  WARNING  WARNING  WARNING  WARNING  WA~
        ~RNING  WARNING  WARNING       ",                                 ~
            at(06,02), fac(hex(94)), errormsg$, ch(79),                  ~
            at(08,20), "***********************************************",~
            at(10,20), "*                                             *",~
            at(11,20), "*  Either you should not be running this at   *",~
            at(12,20), "*  all at this point, or something            *",~
            at(13,20), "*  strange is going on.  At any rate, your    *",~
            at(14,20), "*  planning data base is severly damaged.     *",~
            at(15,20), "*  it is critical that you                    *",~
            at(16,20), "*                                             *",~
            at(17,20), "*  RUN RESTORE IMEDIATELY!!!!!!!!!!!!!!!!     *",~
            at(18,20), "*                                             *",~
            at(19,20), "***********************************************",~
            at(21,02), "   Press RETURN to exit from this program",      ~
                keys(hex(0010))

            call "PRNTSCRN"
            return_code% = 999
            goto L65000

             rem*********************************************************~
             *      l a s t   s t e p   i n   t h i s  m e s s y        *~
             *                     p r o c e s s                        *~
             ************************************************************~

        replace_old_cal
            call "READ101" (#12, "A0", f1%(12))
                if f1%(12) = 0 then L52100
                get #12, using L52170, key$, str(yymmdd$(),1,1470)
                delete #12
            call "READ101" (#12, "10", f1%(12))
                put #12, using L52170, "10", str(yymmdd$(),1,1470)
                if f1%(12) = 1 then rewrite #12 else write #12

L52100:     call "READ101" (#12, "A1", f1%(12))
                if f1%(12) = 0 then L52190
            get #12, using L52170,  key$,   str(yymmdd$(),1471)
            delete #12
            call "READ101" (#12, "11", f1%(12))
            put #12, using L52170,  "11",   str(yymmdd$(),1471)
                if f1%(12) = 1 then rewrite #12 else write #12
L52170:     FMT      CH(2), CH(1470)

L52190:     call "READ101" (#12, "B0", f1%(12))
                if f1%(12) = 0 then L52260
            get #12, using L52400,  key$,     yy%()
            delete #12
            call "READ101" (#12, "20", f1%(12))
            put #12, using L52400,  "20",   yy%()
                if f1%(12) = 1 then rewrite #12 else write #12

L52260:     call "READ101" (#12, "C0", f1%(12))
                if f1%(12) = 0 then L52340
            get #12, using L52400,  key$,  mm%()
            delete #12
            call "READ101" (#12, "30", f1%(12))
            put #12, using L52400,  "30",   mm%()
                if f1%(12) = 1 then rewrite #12 else write #12

L52340:     call "READ101" (#12, "D0", f1%(12))
                if f1%(12) = 0 then L52420
            get #12, using L52400,  key$,        dd%()
            delete #12
            call "READ101" (#12, "40", f1%(12))
            put #12, using L52400,  "40",        dd%()
            if f1%(12) = 1 then rewrite #12 else write #12
L52400:         FMT CH(02),   490*BI(4)

L52420:     call "READ101" (#12, "E0", f1%(12))
                if f1%(12) = 0 then L52510
            get #12, using L52480,  key$,        dow$()
            delete #12
            call "READ101" (#12, "50", f1%(12))
            put #12, using L52480,  "50",        dow$()
                if f1%(12) = 1 then rewrite #12 else write #12
L52480:         FMT CH(2), 490*CH(3)

L52510:     call "READ101" (#12, "F0", f1%(12))
                if f1%(12) = 0 then L52600
            get #12, using L52570,  key$,        mwoy%()
            delete #12
            call "READ101" (#12, "60", f1%(12))
            put #12, using L52570,  "60",        mwoy%()
L52570:         FMT CH(2), 490*BI(4)
                if f1%(12) = 1 then rewrite #12 else write #12

L52600:     call "READ101" (#12, "F1", f1%(12))
                if f1%(12) = 0 then L52680
            get #12, using L52570,  key$,     fwoy%()
            delete #12
            call "READ101" (#12, "61", f1%(12))
            put #12, using L52570,  "61",     fwoy%()
                if f1%(12) = 1 then rewrite #12 else write #12

L52680:     call "READ101" (#12, "G0", f1%(12))
                if f1%(12) = 0 then L52760
            get #12, using L52570,  key$,       cqoy%()
            delete #12
            call "READ101" (#12, "70", f1%(12))
            put #12, using L52570,  "70",       cqoy%()
                if f1%(12) = 1 then rewrite #12 else write #12

L52760:     call "READ101" (#12, "G1", f1%(12))
                if f1%(12) = 0 then L52940
            get #12, using L52570,  key$,        fqoy%()
            delete #12
            call "READ101" (#12, "71", f1%(12))
            put #12, using L52570,  "71",        fqoy%()
                if f1%(12) = 1 then rewrite #12 else write #12

L52940:     return

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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end return_code%
