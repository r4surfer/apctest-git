        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    AAA   L       CCCC  H   H  N   N   GGG     11    *~
            *  C   C  A   A  L      C      H   H  NN  N  G       111    *~
            *  C      AAAAA  L      C      HHHHH  N N N  G GGG    11    *~
            *  C   C  A   A  L      C      H   H  N  NN  G   G    11    *~
            *   CCC   A   A  LLLLL   CCCC  H   H  N   N   GGG   11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CALCHNG1 - POST A NEW PLANNING CALANDER THROUGHOUT THE    *~
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
            * 10/11/93 ! Purchased Job 'BWs' treated same as 'WOs'! MLJ *~
            * 11/09/93 ! Purchased Job, added 'RW' & 'RO' Tags    ! JBK *~
            *          !  Tags 'BO', 'BW', 'RO', 'RW' and 'WO'    !     *~
            *          !  treated as unreleased.                  !     *~
            *          ! Miscellaneous - Chged SHOWMST to SHOSTAT !     *~
            * 09/17/96 ! Millie 2000 - Blank date compare         ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            days$(490)4,                                                 ~
            newdays$(490)4,                                              ~
            a26$26,                                                      ~
            a38$38,                                                      ~
            a44$44,                                                      ~
            a19$19,                                                      ~
            a63$63,                                                      ~
            a64$64,                                                      ~
            a16$16,                                                      ~
            a75$75,                                                      ~
            a150$150

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
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE FOR FISCAL YEAR  *~
            * #04 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #06 ! XPIPOUT  ! Original planned inventory use detail fil*~
            * #07 ! JBPIPXRF !                                          *~
            * #08 ! PIPCROSS !                                          *~
            * #17 ! PIPOUT   ! New planned inventory use detail file    *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos =   1, keylen =  20

            select #04, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #06, "XPIPOUT",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

           select #07, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

           select #08, "PIPCROSS",varc, indexed, recsize=150,            ~
                        keypos=1, keylen=71,                             ~
                        alt key 1, keypos=20, keylen=52,                 ~
                            key 2, keypos=39, keylen=33

            select #17, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

           select #12, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

            select pool #4, #6, blocks = 50, pool #17, blocks = 50

            errormsg$="ERROR ENCOUNTERED OPENING FILES"

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
                     if f2%( 1) <> 0 then goto abort
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
                     if f2%( 4) <> 0 then L01160
            close # 4:f2%( 4)=1%
            call "OPENFILE" (#04, "IO",    f2%(04), rslt$(04), axd$(04))
L01160:     call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
                     if f2%( 6) <> 0 then L01200
            close # 6:f2%( 6)=1%
            call "OPENFILE" (#06, "IO",    f2%(06), rslt$(06), axd$(06))
L01200:     call "OPENFILE" (#17, "SHARE", f2%(17), rslt$(17), axd$(17))
                     if f2%(17) <> 0 then L01240
            close #17:f2%(17)=1%
            call "OPENFILE" (#17, "IO",    f2%(17), rslt$(17), axd$(17))
L01240:     call "OPENFILE" (#07, "SHARE", f2%(07), rslt$(07), axd$(07))
                     if f2%(07) <> 0 then L01275
            close #07:f2%(07)=1%
            call "OPENFILE" (#07, "IO",    f2%(07), rslt$(07), axd$(07))
L01275:     call "OPENFILE" (#08, "SHARE", f2%(08), rslt$(08), axd$(08))
                     if f2%(08) <> 0 then L01310
            close #08:f2%(08)=1%
            call "OPENFILE" (#08, "IO",    f2%(08), rslt$(08), axd$(08))
L01310:     call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
                     if f2%(12) <> 0 then goto abort

            if f2%(17) = 0 then L01380
            if f2%( 6) = 0 then abort
                goto L10000

L01380:     if f2%( 6) <> 0 then abort

L10000: REM *************************************************************~
            *       M A I N   P R O C E S S I N G   S E C T I O N       *~
            *                                                           *~
            * RE-DO ALL THE FILES THAT REFERENCE CALMASTR.              *~
            *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)

            gosub L30000                  /* LOAD CALENDERS */
            if old$ <> " " and old$ <> blankdate$ then L10080
            howfar% = 1
            goto L10110        /*????*/

L10080:     call "DATE" addr("G-", old$, new$, howfar%, r%)
            if r% <> 0 or howfar% < 1 or howfar% > 489 then abort

L10110:     call "SHOSTAT" ("Rolling Planned Inventory Dates")


            gosub'98 ("'PIPMASTR'")  /* PROCESS PIPMASTR */

L10220:     call "READNXT1" (#4, f1%(4)) : if f1%(4) = 0 then L10500
            get #4 using L26010, a26$, days$(), a38$
            newdays$() = str(days$(), howfar% * 4 + 1)
            for i% = 491 - howfar% to 490
            newdays$(i%) = days$(490)
            next i%
            rewrite #4, using L26010, a26$, newdays$(), a38$
            gosub L12080
            goto L10220

L10500:     gosub'98 ("'PIPOUT'")     /* PROCESS PIPOUT, START OF LOOP */
            errormsg$="ERROR ENCOUNTERED ROLLING DETAILS"

L10530:     call "READNXT1" (#6, f1%(6)) : if f1%(6) = 0 then L65000
            get #6 using L26170, a44$, day%, a16$
            a19$=str(a44$,1,19)
            day% = max(day% -  howfar%, 0)
            on day% + 1 goto abort
            if str(a44$,1%,2%)  = "BO" then L10600
            if str(a44$,1%,2%)  = "BW" then L10600
            if str(a44$,1%,2%)  = "RO" then L10600
            if str(a44$,1%,2%)  = "RW" then L10600
            if str(a44$,1%,2%) <> "WO" then L10640
L10600:            convert str(a44$,3%,3%) to day1%, data goto L10640
                   day1% = max(day1% - howfar%,0)
                   on day1% + 1% goto abort
                   convert day1% to str(a44$,3%,3%), pic(###)
L10640:     write #17, using L26170, a44$, day%, a16$

            if str(a19$,1%,2%)  = "BO" then L10660
            if str(a19$,1%,2%)  = "BW" then L10660
            if str(a19$,1%,2%)  = "RO" then L10660
            if str(a19$,1%,2%)  = "RW" then L10660
            if str(a19$,1%,2%) <> "WO" then L11020
L10660:     a64$=str(a19$,1,19) & hex(00)
            init (hex(00)) a75$:str(a75$,1,19)=str(a19$,1,19)
            a19$=str(a44$,1,19)

        REM PROCESS PIPCROSS

L10720:     call "PLOWAL1" (#8,a75$,2%,19%,f1%(8))
            if f1%(8)=0 then goto L10910
            get #8, using L10750, a150$
L10750:         FMT CH(150)
            delete #8
            str(a150$,41,3) = str(a19$,3,3)
            write #8, using L10750, a150$
            goto L10720

        REM PROCESS JBPIPXRF

L10910:     call "PLOWNXT1"(#7,a64$,19%,f1%(7))
            if f1%(7)=0 then L11020
            get #7, using L10980, a63$
            str(a63$,1,19)=a19$
            delete #7
            write #7,using L10980, a63$
            goto L10910
L10980:         FMT CH(63)

        REM END OF PROCESS LOOP AND RETURN TO PLOW

L11020:     gosub L12080
            goto L10530

        REM COMMON FILE DISPLAY AND COUNT ROUTINES

            deffn'98 (current_file$)
            c% = 0
            print at(22,1,); "Current file is "; hex(84); current_file$; ~
                  at(23,1,); "Current count is "
            return

L12080:     c% = c% + 1
            if mod(c%,100) <> 0 then return
            print at (23,18); hex(84); c%
            return

        REM *************************************************************~
            *          F O R M A T     S T A T E M E N T S              *~
            *************************************************************

L26010: FMT                      /* FILE: PIPMASTR                     */~
            CH(26),                                                      ~
            490*CH(4),           /* Planned inventory position         */~
            CH(38)               /* 3 PD(14,4)'S + FILLER              */

L26170: FMT                      /* FILE: PIPOUT                       */~
            CH(44),              /* Tag number + PART CODE             */~
            BI(4),               /* Date out of PIP in date subscript  */~
            CH(16)               /* Time from the system clock + PD(14,*/

L30000: rem**************************************************************~
            *               l o a d   c a l a n d e r s                 *~
            *************************************************************

            errormsg$ = "ERROR ENCOUNTERED LOADING CALENDARS"

            call "READ100" (#12, "A0", f1%(12))
                if f1%(12) = 0 then goto abort
            get #12, using L30110, new$
L30110:         FMT XX(2), CH(6)

            call "READ100" (#12, "10", f1%(12))
                if f1%(12) = 0 then return
            get #12, using L30187, old$
L30187:         FMT XX(2), CH(6)
            return

        abort

        accept                                                           ~
            at(04,02), "ERROR HIT  WARNING  WARNING  WARNING  WARNING  WA~
        ~RNING  WARNING  WARNING       ",                                 ~
            at(06,02), fac(hex(94)), errormsg$, ch(79),                  ~
            at(08,20), "***********************************************",~
            at(10,20), "*                                             *",~
            at(11,20), "*  EITHER YOU SHOULD NOT BE RUNING THIS AT    *",~
            at(12,20), "*  ALL AT THIS POINT, OR SOMETHING            *",~
            at(13,20), "*  STRANGE IS GOING ON.  AT ANY RATE, YOUR    *",~
            at(14,20), "*  PLANNING DATA BASE IS SEVERLY DAMAGED.     *",~
            at(15,20), "*  IT IS CRITICAL THAT YOU                    *",~
            at(16,20), "*                                             *",~
            at(17,20), "*  RUN RESTORE IMEDIATELY!!!!!!!!!!!!!!!!     *",~
            at(18,20), "*                                             *",~
            at(19,20), "***********************************************",~
            at(21,02),"HIT (ENTER) TO EXIT FROM THIS PROGRAM",           ~
                keys(hex(0010))

            call "PRNTSCRN"
            return_code% = 999
            goto L65000

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

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end return_code%
