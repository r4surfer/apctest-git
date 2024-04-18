        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    AAA   L       CCCC  H   H  N   N   GGG   22222   *~
            *  C   C  A   A  L      C      H   H  NN  N  G          2   *~
            *  C      AAAAA  L      C      HHHHH  N N N  G GGG  22222   *~
            *  C   C  A   A  L      C      H   H  N  NN  G   G  2       *~
            *   CCC   A   A  LLLLL   CCCC  H   H  N   N   GGG   22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CALCHNG2 - POST A NEW PLANNING CALANDER THROUGHOUT THE    *~
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
            * 12/02/87 ! Don't let PIPIN start dates stop roll if ! HES *~
            *          ! PIPIN is 'released'                      ! HES *~
            * 08/23/88 ! Now Updates TAG in PORLSE.               ! RJM *~
            * 10/11/93 ! Purchased Job 'BWs' treated same as 'WOs'! MLJ *~
            * 11/11/93 ! Purchased Job - Tread 'RWs' same as 'ROs'! JBK *~
            *          !  Modified Tag change for PORLSE (now the !     *~
            *          !  header records changed.  Added Roll for !     *~
            *          !  POPIPXRF file and PORLSE detail records !     *~
            *          ! Miscellaneous-Changed SHOWMSG to SHOSTAT !     *~
            * 09/17/96 ! Millie 2000 - Blank date compare         ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            days$(490)4,                                                 ~
            newdays$(490)4,                                              ~
            a25$25,                                                      ~
            a27$27,                                                      ~
            a19$19,                                                      ~
            a94$94,                                                      ~
            rokey$19,                    /* KEY SPECIALLY FOR PORLSE   */~
            a150$150,                                                    ~
            a241$241,                    /* PORLSE Data                */~
            b19$19,                      /* PORLSE Alternate Key Area  */~
            b232$232,                    /* PORLSE Data Area           */~
            b46$46,                      /* POPIPXRF Data Area         */~
            b30$30,                      /* POPIPXRF Data Area         */~
            newtag$19                    /* New Tag for POPIPXRF       */

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
            * # 2 ! PIPCROSS ! PIP CROSS REFERENCE                      *~
            * # 4 ! JBCROSS2 ! JOB CROSS REFERNCE FILE                  *~
            * # 6 ! JBPIPXRF !                                          *~
            * # 8 ! PORLSE   ! PURCHASE ORDER REQUISITIONS FILE         *~
            * #17 ! PIPIN    ! NEW Planned inventory use detail         *~
            * #05 ! XPIPIN   ! ORIGINAL Planned inventory use detail    *~
            * #09 ! SFMASTR2 ! Sales forecast master file               *~
            * #10 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            * #13 ! XPOPIPXR ! ORIGINAL Cross Refer of Honored Advices  *~
            * #14 ! POPIPXRF ! NEW Cross Refer of Honored Advices       *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos =   1, keylen =  20

            select #2,  "PIPCROSS", varc, indexed,                       ~
                     recsize = 150,  keypos =  1  , keylen =  71,        ~
                     alt key 1, keypos = 20, keylen = 52,                ~
                         key 2, keypos = 39, keylen = 33

            select #4,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 94,                                    ~
                        keypos=29, keylen = 19,                          ~
                        alt key 1, keypos = 1 , keylen = 47,             ~
                            key 2, keypos = 48, keylen = 47

            select #05, "XPIPIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

           select  #6, "JBPIPXRF",varc, indexed, recsize=63,             ~
                        keypos=1, keylen=63,                             ~
                        alt key 1, keypos=45, keylen=19

           select  #8, "PORLSE",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 492,                                   ~
                        keypos = 1, keylen = 66,                         ~
                        alt key 1, keypos =  48, keylen = 19, dup,       ~
                            key 2, keypos =   5, keylen = 62, dup,       ~
                            key 3, keypos =  14, keylen = 53, dup,       ~
                            key 4, keypos =  39, keylen = 28, dup,       ~
                            key 5, keypos = 242, keylen = 19, dup

            select #17, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #09, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #10, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

           select #12, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

            select #13, "XPOPIPXR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 58

            select #14, "POPIPXRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 58

        select pool #5, #8, #9, #10, #17, blocks = 50


            errormsg$ = "ERROR ENCOUNTERED OPENING FILES"

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
                     if f2%( 1) <> 0 then goto abort
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
                     if f2%( 5) <> 0 then L07590
            close # 5:f2%( 5)=1%
            call "OPENFILE" (#05, "IO   ", f2%(05), rslt$(05), axd$(05))
L07590:     call "OPENFILE" (#17, "SHARE", f2%(17), rslt$(17), axd$(17))
                     if f2%(17) <> 0 then L07630
            close #17:f2%(17)=1%
            call "OPENFILE" (#17, "IO   ", f2%(17), rslt$(17), axd$(17))
L07630:     call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
                     if f2%( 9) <> 0 then L07670
            close # 9:f2%( 9)=1%
            call "OPENFILE" (#09, "IO   ", f2%(09), rslt$(09), axd$(09))
L07670:     call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
                     if f2%(10) <> 0 then L07710
            close #10:f2%(10)=1%
            call "OPENFILE" (#10, "IO   ", f2%(10), rslt$(10), axd$(10))
L07710:     call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
                     if f2%( 4) <> 0 then L07760
            close # 4:f2%( 4)=1%
            call "OPENFILE" (# 4, "IO   ", f2%( 4), rslt$( 4), axd$( 4))

L07760:     call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
                     if f2%( 2) <> 0 then L07810
            close # 2:f2%( 2)=1%
            call "OPENFILE" (# 2, "IO   ", f2%( 2), rslt$( 2), axd$( 2))

L07810:     call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
                     if f2%( 6) <> 0 then L07860
            close # 6:f2%( 6)=1%
            call "OPENFILE" (# 6, "IO   ", f2%( 6), rslt$( 6), axd$( 6))

L07860:     call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
                     if f2%( 8) <> 0 then L07910
            close # 8:f2%( 8)=1%
            call "OPENFILE" (# 8, "IO   ", f2%( 8), rslt$( 8), axd$( 8))

L07910:     call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
                     if f2%(12) <> 0 then goto abort

            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
                     if f2%(13%) <> 0% then L07980
            close #13  :  f2%(13%) = 1%
            call "OPENFILE" (#13, "IO   ", f2%(13), rslt$(13), axd$(13))
L07980:     call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
                     if f2%(14%) <> 0% then L08650
            close #14  :  f2%(14%) = 1%
            call "OPENFILE" (#14, "IO   ", f2%(14), rslt$(14), axd$(14))

L08650:     if f2%(17) = 0 then L08690
            if f2%( 5) = 0 then abort
               goto L10000

L08690:     if f2%( 5) <> 0 then abort

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
            goto L10110      /*????*/

L10080:     call "DATE" addr("G-", old$, new$, howfar%, r%)
            if r% <> 0 or howfar% < 1 or howfar% > 489 then abort

L10110:     call "SHOSTAT" ("Rolling Planned Inventory Dates")


            gosub'98 ("'PIPIN'")     /* PROCESS PIPIN AND DETAILS */
            errormsg$ = "ERROR ENCOUNTERED PROCESSING DETAILS"

L10230:     call "READNXT1" (#5, f1%(5)) : if f1%(5) = 0 then L11000
            get #5 using L26015, a25$, day%, a27$, day1%
            day% = max(day% -  howfar%, 0)
            on day% + 1 goto abort
            day1% = day1% - howfar%
            a19$=str(a27$,1,19)
            if str(a27$,1%,2%)  = "BO" then L10330
            if str(a27$,1%,2%)  = "BW" then L10330
            if str(a27$,1%,2%)  = "RO" then L10330
            if str(a27$,1%,2%)  = "RW" then L10330
            if str(a27$,1%,2%) <> "WO" then L10370
L10330:            convert str(a27$,3%,3%) to day3%, data goto L10370
                   day3% = max(day3% -  howfar%, 0)
                   on day3% + 1% goto abort
                   convert day3% to str(a27$,3%,3%), pic(###)
L10370:     write #17, using L26015, a25$, day%, a27$, day1%
            rokey$ = " "
            if str(a19$,1%,2%)  = "RO" then rokey$ = a19$
            if str(a19$,1%,2%)  = "RW" then rokey$ = a19$

            if str(a19$,1%,2%)  = "BO" then L10520
            if str(a19$,1%,2%)  = "BW" then L10520
            if str(a19$,1%,2%)  = "RO" then L10520
            if str(a19$,1%,2%)  = "RW" then L10520
            if str(a19$,1%,2%) <> "WO" then L10910

        REM PROCESS JBCROSS2

L10520:     call "READ101" (#4, a19$,f1%(4))
            if f1%(4)=0 then goto L10620 else delete #4
            get #4, using L10550, a94$
L10550:         FMT CH(94)
            str(a94$,31,3)= str(a27$,3,3)
            str(a94$,78,3)= str(a27$,3,3)
            write #4, using L10550, a94$

        REM PROCESS PIPCROSS

L10620:     init (hex(00)) a94$  :  str(a94$,1%,19%) = str(a19$,1%,19%)
L10630:     call "PLOWAL1" (#2, a94$, 1%, 19%, f1%(2%))
            if f1%(2%) = 0% then goto L10720
            get #2, using L10660, a150$
L10660:         FMT CH(150)
            delete #2
            str(a150$,22,3) = str(a27$,3,3)
            write #2, using L10660, a150$
            goto L10630

        REM PROCESS PORLSE   /* Processes the PORLSE Header Records */

L10720:     if rokey$ = " " then L10820
            call "REDALT1" (#8, rokey$, 5%, f1%(8%))
                 if f1%(8%) = 0% then goto L10820
            get #8, using L10740, a241$, b19$, b232$
L10740:         FMT CH(241), CH(19), CH(232)
            delete #8
            str(b19$,3%,3%) = str(a27$,3,3)
            write #8, using L10740, a241$, b19$, b232$

        REM PROCESS JBPIPXRF

L10820:     call "REDALT1" (#6,a19$,1%,f1%(6))
            if f1%(6)=0 then goto L10910 else delete #6
            get #6, using L10850, a94$
L10850:         FMT CH(63)
            str(a94$,47,3) = str(a27$,3,3)
            write #6, using L10850, a94$

        REM END OF LOOP AND RETURN TO PLOW
L10910:     gosub L12080
            goto L10230

L11000:     gosub'98 ("'SFMASTR2'")   /* PROCESS SFMASTR2 FILE */

            init (hex(00)) newdays$()
L11030:     call "READNXT1" (#9, f1%(9)) : if f1%(9) = 0 then L11500
            get #9 using L26345, a25$, days$()
            str(newdays$(),,(490-howfar%)*4) = str(days$(), howfar%*4+1)
            rewrite #9, using L26345, a25$, newdays$()
            gosub L12080
            goto L11030

L11500:     gosub'98 ("'SFCUM2'")    /* PROCESS SFCUM2 FILE */

            init(hex(00)) newdays$()
L11530:     call "READNXT1" (#10, f1%(10)) : if f1%(10) = 0 then L11630
            get #10 using L26415, a25$, days$()
            str(newdays$(),,(490-howfar%)*4) = str(days$(), howfar%*4+1)
            for i% = 491 - howfar%  to 490
            newdays$(i%) = days$(490)
            next i%
            rewrite #10, using L26415, a25$, newdays$()
            gosub L12080
            goto L11530

L11630:     gosub'98 ("'POPIPXRF'")    /* Process POPIPXRF File */

L11650:     call "READNXT1" (#13, f1%(13%))
                if f1%(13%) = 0% then L65000
            get #13 using L11680, b46$, day_in%, b16$, day_st%, b30$
L11680:         FMT CH(46), BI(4), CH(16), BI(4), CH(30)
            day_in% = max(day_in% - howfar%, 0%)
            if str(b46$,20%,2%) = "PO" then L11890
                day_st% = day_st% - howfar%
                a19$ = str(b46$,1%,19%)
                convert str(b46$,3%,3%) to tagday%, data goto L11755
                tagday% = max(tagday% - howfar%, 0%)
                convert tagday% to str(b46$,3%,3%), pic(###)
L11755:         newtag$ = str(b46$,1%,19%)
                write #14 using L11680, b46$, day_in%, b16$, day_st%, b30$

            /* Now check for PORLSE detail line records */
                call "REDALT1" (#8, a19$, 5%, f1%(8%))
                     if f1%(8%) = 0% then L11650
                get #8 using L11820, a241$, b19$, b232$
L11820:              FMT CH(241), CH(19), CH(232)
                delete #8
                str(a241$,50%,17%) = str(newtag$,3%,17%)
                b19$ = newtag$
                write #8 using L11820, a241$, b19$, b232$
                goto L11650

L11890
*        Process 'PO' (OUTS) type records
            if str(b46$,1%,19%) <> a19$ then L11650
            if newtag$ = " " then L11650
                str(b46$,1%,19%) = newtag$
                write #14 using L11680, b46$, day_in%, b16$, day_st%, b30$
                goto L11650

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
L26015: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(27),              /* Tag number &   PD(14,4)            */~
            BI(4)                /* Date to start as a date subscript  */~

L26345: FMT                      /* FILE: SFMASTR2                     */~
            CH(25),              /* Part code                          */~
            490*CH(4)            /* integer array of sales forecasts   */

L26415: FMT                      /* FILE: SFCUM2                       */~
            CH(25),              /* Part code                          */~
            490*CH(4)            /* cumulative sales forecast integer  */~

L30000: rem**************************************************************~
            *               l o a d   c a l a n d e r s                 *~
            *************************************************************

            errormsg$ = "ERROR ENCOUNTERED LOADING CALENDAR"

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
            at(11,20), "*  EITHER YOU SHOULD NOT BE RUNNING THIS      *",~
            at(12,20), "*  AT ALL AT THIS POINT, OR SOMETHING         *",~
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
