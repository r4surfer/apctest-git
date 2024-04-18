        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   RRRR   EEEEE   SSS   TTTTT  RRRR    *~
            *  P   P    I    P   P  R   R  E      S        T    R   R   *~
            *  PPPP     I    PPPP   RRRR   EEE     SSS     T    RRRR    *~
            *  P        I    P      R   R  E          S    T    R   R   *~
            *  P      IIIII  P      R   R  EEEEE   SSS     T    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPRESTR - BALANCE PLANNED INVENTORY POSITION, CREATING OR*~
            *            DELETING NECESSARY RECORDS BASED ON CURRENT    *~
            *            INVENTORY PART TYPE.                           *~
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
            * 09/19/84 ! ORIGINAL                                 ! KEN *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blank$79,                                                    ~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            part$25,                                                     ~
            pipinpart$25,                                                ~
            pipoutpart$25,                                               ~
            hnypart$50,                                                  ~
            readkey$100,                                                 ~
            yymmdd$6,                                                    ~
            moq$10,                                                      ~
            type$10,                                                     ~
            pip(490),                                                    ~
            pip%(490)                                                    ~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
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
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * # 4 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 5 ! PIPIN    ! Planned inventory additions detail       *~
            * # 6 ! PIPOUT   ! Planned inventory use detail rec         *~
            * # 7 ! SFCUM2   ! Cumulative sales forecast file           *~
            * # 8 ! SFMASTR2 ! Sales forecast master file               *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select # 2, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 3, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select # 4, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 5, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select # 6, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select # 7, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select # 8, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~


            call "SHOWMSG" ("Opening Files, One Moment Please")

                rslt$(1) = "REQUIRED"
            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
                if f2%( 1) <> 0 then L65000

                rslt$(2) = "REQUIRED"
            call "OPENFILE" (# 2, "IO   ", f2%( 2), rslt$( 2), axd$( 2))
                if f2%( 2) <> 0 then L65000

            call "OPENFILE" (# 3, "IO   ", f2%( 3), rslt$( 3), axd$( 3))

            call "OPENFILE" (# 4, "IO   ", f2%( 4), rslt$( 4), axd$( 4))
                if f2%( 4) = 0 then L03160
            call "OPENFILE" (# 4, "OUTPT", f2%( 4), rslt$( 4), axd$( 4))
                close #4 : f2%(4) = 1
            call "OPENFILE" (# 4, "IO   ", f2%( 4), rslt$( 4), axd$( 4))

L03160:     call "OPENFILE" (# 5, "IO   ", f2%( 5), rslt$( 5), axd$( 5))

            call "OPENFILE" (# 6, "IO   ", f2%( 6), rslt$( 6), axd$( 6))

            call "OPENFILE" (# 7, "IO   ", f2%( 7), rslt$( 7), axd$( 7))

            call "OPENFILE" (# 8, "IO   ", f2%( 8), rslt$( 8), axd$( 8))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date

            call "READ100" (#1, "MONTHS OPEN", f1%(1))
               if f1%(1) <> 1% then goto L65000
            get #1, using    L09120, yymmdd$
L09120:         FMT XX(32), CH(6)

            call "DATE" addr("G-", yymmdd$, date$, d%, ret%)
              if ret% <> 0% then goto L65000
              if d% = 0% then goto L65000

            call "DATEFMT" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

L10120:         gosub'101
                      if keyhit%  =  1 then L65000
                      if keyhit%  = 16 then datasave
                      if keyhit% <>  0 then       L10120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            init (hex(00)) part$,pipinpart$,pipoutpart$,hnypart$
                f1%(5), f1%(6), f1%(3) = -1% : hit% = 0

            call "PLOWNEXT" (#2, part$, 0%, f1%(2))
                  goto L19110
L19100:     call "READNEXT" (#2, f1%(2))
L19110:         if f1%(2) = 0 then L19500
            get #2, using L19130, part$,atch%,lead$,type$,moq$,q2,q4,pr$
L19130:         FMT CH(25), XX(102), BI(2), XX(40), 3*CH(10), XX(118),   ~
                                                       2*PD(14,4), CH(1)
            if q2 > 99999999 then q2 = 0
            if q4 > 99999999 then q4 = 0
            atch% = atch% + (1000% * (max(0%, val(pr$, 1) - 64%)))
            q3=0:convert moq$ to q3, data goto L19150
L19150:     convert type$ to type%, data goto L19210
            lead% = 0:convert lead$ to lead%, data goto L19160
L19160:     if type% > 0% and type% < 200% then goto L19210

            gosub bal_pips
            goto L19100

L19210:     gosub del_pips
            goto L19100

L19500: REM FINAL CLEANUP ON PIPS

            print at (18,02), "*SCANNING FOR UNNEEDED PIP RECORDS*"
            init (hex(00)) part$

            call "PLOWNEXT" (#4, part$, 0%, f1%(4))
                goto L19560
L19550:     call "READNEXT" (#4, f1%(4))
L19560:         if f1%(4) = 0 then L65000
            get #4, using L19562, part$
L19562:         FMT XX(1), CH(25)
            call "READ100" (#2, part$, f1%(2))
            if f1%(2) = 0 then gosub del_pips
            goto L19550

        del_pips
            print at (18,02), "*DELETING:" & str(part$,1,25)
            call "DELETE" (#4, part$, 25%)  /* PIPMASTR */
            call "DELETE" (#7, part$, 25%)  /* SFCUM2   */
            call "DELETE" (#8, part$, 25%)  /* SFMASTR2 */

            init (hex(00)) readkey$
L20070:     str(readkey$,1,25) = str(part$,1,25)
            call "PLOWAL1" (#5, readkey$, 1%, 25%, f1%(5))
                if f1%(5) = 0 then L20130
            delete #5                       /* PIPIN    */
            goto L20070

L20130:     init (hex(00)) readkey$
L20140:     str(readkey$,1,25) = str(part$,1,25)
            call "PLOWAL1" (#6, readkey$, 1%, 25%, f1%(6))
                if f1%(6) = 0 then L20182
            delete #6                       /* PIPOUT   */
            goto L20140

L20182:     f1%(5), f1%(6) = -1%
            return


        bal_pips
            if mod(hit%,100) = 0% then                                   ~
                    print at (18,02), "BALANCING:" & str(part$,1,25)
            hit% = hit% + 1%
            mat pip = zer
            q1=0
            if f1%(3) < 0% then L20240
            if str(hnypart$,1,1) = hex(ff) then L20340
            goto L20271
L20240:     init (hex(00)) readkey$
            str(readkey$,1,26) = str(part$,1,25) & hex(2f)
            call "PLOWNEXT" (#3, readkey$, 0%, f1%(3))
                if f1%(3) = 0 then L20321
L20271:     get #3, using L20272, hnypart$
L20272:         FMT POS(17), CH(26)
                if str(hnypart$,1,25) < str(part$,1,25) then L20311
                if str(hnypart$,1,25) > str(part$,1,25) then L20340
                if str(hnypart$,26,1) < hex(30) then L20311
                if str(hnypart$,26,1) > hex(39) then L20340
            get #3, using L20300, q
L20300:         FMT POS(69), PD(14,4)
            q1=round(q1+q,2)
L20311:     read #3, eod goto L20321
            goto L20271
L20321:     init (hex(ff)) hnypart$

L20340:     if f1%(5) < 0% then L20400
            if str(pipinpart$,1,1) = hex(ff) then L20592
            goto L20460
L20400:     init (hex(00)) readkey$
            str(readkey$,1,25) = str(part$,1,25)
            call "PLOWALTS" (#5, readkey$, 1%, 0%, f1%(5))
                if f1%(5) = 0 then L20510
L20460:     get #5, using L20470, pipinpart$, p%, q
L20470:         FMT CH(25), BI(4), XX(19), PD(14,4)
            if pipinpart$ > part$ then L20592
            if pipinpart$ < part$ then L20491
            p% = min(490%, max(1%,p%))
            pip(p%) = round(pip(p%) + q,2)
L20491:     read #5, eod goto L20510
            goto L20460
L20510:     init (hex(ff)) pipinpart$

L20592:     if f1%(6) < 0% then L20600
            if str(pipoutpart$,1,1) = hex(ff) then L20700
            goto L20640
L20600:     init (hex(00)) readkey$
            str(readkey$,1,25) = str(part$,1,25)
            call "PLOWALTS" (#6, readkey$, 1%, 0%, f1%(6))
                if f1%(6) = 0 then L20681
L20640:     get #6, using L20650, pipoutpart$, p%, q
L20650:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            if pipoutpart$ > part$ then L20700
            if pipoutpart$ < part$ then L20671
            p% = min(490%, max(1%,p%))
            pip(p%) = round(pip(p%) - q,2)
L20671:     read #6, eod goto L20681
            goto L20640
L20681:     init (hex(ff)) pipoutpart$

L20700:     pip(d%) = round(pip(d%) + q1,2)

            for i% = 489% to 1% step -1%
            if abs(pip(i%)) < .0001 then L20770
               for j% = i%+1% to 490%
                 pip(j%) = round(pip(j%) + pip(i%),2)
               next j%
L20770:     next i%

            mat pip% = pip

            call "READ101" (#4, part$, f1%(4))
                if f1%(4) <> 0 then delete #4
            write #4 using L20830, " ", part$, pip%(), q1, q2, q3, q4,    ~
                     type%, lead%, atch%
L20830:     FMT CH(1), CH(25), 490*BI(4), 4*PD(14,4), 3*BI(2)
            call "PIPFLAGS" (part$, d%, d%, 0, #4, #7)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "BALANCE PLANNED INVENTORY POSITIONS",                 ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (06,02),                                               ~
                  "This is a maintenance routine to insure that Planned I~
        ~nventory Positions shown ",                                      ~
               at (07,02),                                               ~
                  "in the PIPMASTR file agree with current quantity on ha~
        ~nd (via HNYQUAN) and the ",                                      ~
               at (08,02),                                               ~
                  "Planned Inventory Position details reflected in the PI~
        ~PIN and PIPOUT files.    ",                                      ~
               at (10,02),                                               ~
                  "Also, by responding to the current part type in HNYMAS~
        ~TR, it will create any   ",                                      ~
               at (11,02),                                               ~
                  "missing PIPMASTR records & delete PIPMASTR, SFCUM2, SF~
        ~MASTR2, PIPIN, or PIPOUT",                                       ~
               at (12,02),                                               ~
                  "records which are no longer appropriate.              ~
        ~                         ",                                      ~
               at (19,02),                                               ~
                  "PRESS PF1 TO EXIT IMMEDIATELY.                     PRE~
        ~SS PF16 TO PROCESS FILES.",                                      ~
                                                                         ~
               at (21,02), fac(hex(a4)),   blank$               , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)EXIT PROGRAM",                                     ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)BALANCE PIPS",                                    ~
                                                                         ~
               keys(hex(010d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("PIPRESTR")
                  goto L40210

L40690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

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

            call "SHOWMSG" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
