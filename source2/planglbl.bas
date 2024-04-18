        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L       AAA   N   N   GGG   L      BBBB   L       *~
            *  P   P  L      A   A  NN  N  G      L      B   B  L       *~
            *  PPPP   L      AAAAA  N N N  G GGG  L      BBBB   L       *~
            *  P      L      A   A  N  NN  G   G  L      B   B  L       *~
            *  P      LLLLL  A   A  N   N   GGG   LLLLL  BBBB   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLANGLBL - Completely regenerates the planning sytem by   *~
            *            A) Killing all unreleased advices b) Resetteing*~
            *            all demands to unplanned c)Resetting work      *~
            *            center utilization records d)Rebalancing PIP   *~
            *            and then planning forward, level by level      *~
            *----------------------------------------------------------Q*~
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
            * 10/30/84 ! ORIGINAL                                 ! KEN *~
            * 02/13/87 ! FMT ERROR AT 9440, 62110 (PLANFLAGS$())  ! KEN *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 06/03/88 ! Consistent Use of Flags                  ! kab *~
            * 10/15/93 ! Purchase Job Project - Treat 'BW' like   ! JBK *~
            *          !   'BO' and 'WO'.                         !     *~
            *          ! Misc. corrected branches at 33195 & 34215!     *~
            * 12/22/93 ! PRR 12759.  # recs for print more logical! JDH *~
            * 03/08/94 ! Changed record length for BOMSPEC        ! WPH *~
            * 08/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        com planflags$(25)20,                                            ~
            yymmdd$(490)6,                                               ~
            eff$(490)3,                  /* EFF, PLT WORK ARRAY        */~
            oldcompplowkey$(100)31,      /* FOR PHANTOM LOGIC          */~
            pip%(490),                                                   ~
            cumf%(490),                                                  ~
            awca%(490),                  /* CONCURRENT WC AVAILABILITY */~
            awcu1%(490),                 /* 1ST CONCURRENT USED        */~
            awcu2%(490),                 /* 2ND CONCURRENT USED        */~
            awcu3%(490),                 /* 3RD CONCURRENT USED        */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            phfact(101),                                                 ~
                               /* THE ABOVE ELEMENTS CANNOT BE CHANGED */~
            part$    (1000)25,                                           ~
            intagnr$ (1000)19,                                           ~
            outtagnr$(1000)19,                                           ~
            rte$     (1000)3,            /* WHICH ROUTE TO USE         */~
            bom$     (1000)3,            /* WHICH BOM TO USE           */~
            ed%      (1000),                                             ~
            sd%      (1000),                                             ~
            parline% (1000),             /* PARENT LINE NUMBER         */~
            action%  (1000),             /* ACTION TO TAKE             */~
            lt%      (1000),             /* LEADTIME ARRAY             */~
            moq%     (1000),             /* MOQ                        */~
            type%    (1000),             /* PART TYPE                  */~
            qtyu     (1000),             /* QTY USED (NEEDED)          */~
            qtyp     (1000),             /* QTY TO PROCURE             */~
                               /* THE ABOVE ELEMENTS ARE THE MATERIALS */~
            wc$(2000)4,                  /* WORK CENTER                */~
            wl$(2000)1,                  /* JUST FOR SUMMARY REPORT    */~
            wa$(2000)1,                  /* IN CASE OF ARRAY OVERFLOW  */~
            ws$(2000)5,                  /* WC STEP #                  */~
            wl%(2000),                   /* LINE STACK                 */~
            du%(2000),                   /* DATE USED ARRAY STACK      */~
            au%(2000),                   /* AMOUNT USED ARRAY STACK    */~
            su%(2000),                   /* SET UP TIME TODAY          */~
            wa%(2000),                   /* WC ALTERNATE SEQ NO.       */~
                               /* THESE ARE THE WORK CENTER ARRAYS     */~
            rtestep$(255)200,            /* THE STEP AS A STRING       */~
            step$   (255)5,              /* STEP                       */~
            yld     (255),               /* STEP YIELD                 */~
            pd%     (255),               /* WC STEP START DATE(SPL PCK)*/~
            mmx%    (255),               /* START OF RTE STEP          */~
            xsd%    (255)                /* START OF RTE STEP          */~
                               /* THESE ARE FOR ROUTE STEPS            */~

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            control$(7)1,                                                ~
            pcontrol$(4)1,                                               ~
            startdate$(6)8,                                              ~
            starttime$(6)8,                                              ~
            demseq$8,                                                    ~
            date$8,                                                      ~
            demand$150,                                                  ~
            errormsg$79,                                                 ~
            effplt$(490)3,                                               ~
            file$8,                                                      ~
            inpmessage$79,                                               ~
            inputmsg$79,                                                 ~
            lib$8,                                                       ~
            line2$79,                                                    ~
            part$25,                                                     ~
            pipinpart$25,                                                ~
            pipoutpart$25,                                               ~
            planflagshold$(25)20,                                        ~
            pldate$6,                                                    ~
            prtdate$8,                                                   ~
            readkey$100,                                                 ~
            readkey1$100,                                                ~
            rec1$60,                                                     ~
            rec2$6,                                                      ~
            startok$3,                                                   ~
            tagdate$6,                   /* Tag No date, YYMMDD used   */~
            tagdtfull$8,                 /* Tag No date, CCYYMMDD      */~
            tagdttmp$8,                  /* temp/scratch date          */~
            vol$6,                                                       ~
            lta%(26),                                                    ~
            workpip%(490),                                               ~
            pip(490),                                                    ~
            cursor%(2),                                                  ~
            lfac$(20)1,                                                  ~
            i$(24)80,                                                    ~
            pf4$18,                                                      ~
            pf16$16                                                      ~

        dim                                                              ~
            bom$3,                                                       ~
            date$(2)8,                                                   ~
            demcode$16,                                                  ~
            demline$3,                                                   ~
            demquant$10,                                                 ~
            duedate$6,                                                   ~
            status$1,                                                    ~
            type$1,                                                      ~
            partdescr$32,                                                ~
            priority$1,                                                  ~
            printline$132,                                               ~
            dts%(8),                                                     ~
            pprior$(8)1,                                                 ~
            testdemd$6,                                                  ~
            testbyte$6                                                   ~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
         /* F1%(64), IN COMMON BLOCK     /* = 1 IF READ WAS SUCCESSFUL */~
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

            call "SHOSTAT" ("Linking to Planning System Data Base")


        REM *************************************************************~
            *  SELECT FILES                                             *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! DEMMASTR ! Demand Master File                       *~
            * #2  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #6  ! RTEALTRS ! Rte alternates                           *~
            * #7  ! RTEMASTR ! Production routing master file           *~
            * #8  ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #12 ! CALMASTR ! Planning Production Calendar File        *~
            * #14 ! BOMSPEC  ! options selected file                    *~
            * #15 ! BOMMASTR ! BOM relationship file                    *~
            * #16 ! HNYALTRS ! Inventory Alternate Part File            *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            * #24 ! ENGMASTR ! Engineering Master Filer                 *~
            * #33 ! PIPIN    ! Planned inventory additions detail       *~
            * #34 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #35 ! PIPCROSS ! hard peg cross reference                 *~
            * #36 ! JBPIPXRF ! option part harder peg                   *~
            * #40 ! SFMASTR2 ! Sales forecast master file               *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #50 ! PRNTWORK ! PRINTFILE                                *~
            * #60 ! SYSFILE2 ! Caelus Management System Information     *~
            * #62 ! WORK1    ! PLANNING WORKFILE - MATERIALS            *~
            * #63 ! WORK2    ! PLANNING WORKFILE - WORK CENTER ALLOC    *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #2,  "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #6,  "RTEALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  34                      ~

            select #7,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #8,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   94,                                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #11, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #12, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #14, "BOMSPEC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23          ~

            select #15, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #16, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33                      ~

            select #23, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27

            select #24, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #33, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #34, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #35, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #36, "JBPIPXRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   63,                                  ~
                        keypos =    1, keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19          ~

            select #40, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #41, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #50, "PRNTWORK",                                      ~
                        consec ,                                         ~
                        recsize = 132

            select #60, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #62, "WORK1",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 31, keylen = 8,                         ~
                        alt key 1, keypos = 1, keylen = 29,              ~
                            key 2, keypos = 35, keylen= 4,               ~
                            key 3, keypos = 30, keylen= 9

            select #63, "WORK2",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 66,                                    ~
                        keypos = 5, keylen = 10,                         ~
                        alt key 1, keypos = 1, keylen = 4, dup


        REM  CALL "WORKOPEN" (#50, "OUTPT", 5000%, F2%(50))
        REM  CALL "OPENPRT" (#50, 5000%, STR(RSLT$,1,8),                 ~
                            STR(RSLT$,9,8), STR(RSLT$(50),17,6), F2%(50))

           call "OPENFILE" (# 1, "IO   ", f2%( 1), rslt$( 1), axd$( 1))

           call "OPENFILE" (# 2, "IO   ", f2%( 2), rslt$( 2), axd$( 2))

           call "OPENFILE" (# 4, "IO   ", f2%( 4), rslt$( 4), axd$( 4))

           call "OPENFILE" (# 6, "IO   ", f2%( 6), rslt$( 6), axd$( 6))

           call "OPENFILE" (# 7, "IO   ", f2%( 7), rslt$( 7), axd$( 7))

           call "OPENFILE" (# 8, "IO   ", f2%( 8), rslt$( 8), axd$( 8))
            if f2%(8) = 0 then L05110
           call "OPENFILE" (# 8, "OUTPT", f2%( 8), rslt$( 8), axd$( 8))
                close #8:f2%(8)=1
           call "OPENFILE" (# 8, "IO   ", f2%( 8), rslt$( 8), axd$( 8))

L05110:    call "OPENFILE" (#11, "IO   ", f2%(11), rslt$(11), axd$(11))

           call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))

           call "OPENFILE" (#14, "IO   ", f2%(14), rslt$(14), axd$(14))

           call "OPENFILE" (#15, "IO   ", f2%(15), rslt$(15), axd$(15))

           call "OPENFILE" (#16, "IO   ", f2%(16), rslt$(16), axd$(16))

           call "OPENFILE" (#23, "IO   ", f2%(23), rslt$(23), axd$(23))
            if f2%(23) = 0 then L05200
           call "OPENFILE" (#23, "OUTPT", f2%(23), rslt$(23), axd$(23))
                close #23:f2%(23)=1
           call "OPENFILE" (#23, "IO   ", f2%(23), rslt$(23), axd$(23))

L05200:    call "OPENFILE" (#24, "IO   ", f2%(24), rslt$(24), axd$(24))
            if f2%(24) = 0 then L05210
           call "OPENFILE" (#24, "OUTPT", f2%(24), rslt$(24), axd$(24))
                close #24:f2%(24)=1
           call "OPENFILE" (#24, "IO   ", f2%(24), rslt$(24), axd$(24))

L05210:    call "OPENFILE" (#33, "IO   ", f2%(33), rslt$(33), axd$(33))
            if f2%(33) = 0 then L05220
           call "OPENFILE" (#33, "OUTPT", f2%(33), rslt$(33), axd$(33))
                close #33:f2%(33)=1
           call "OPENFILE" (#33, "IO   ", f2%(33), rslt$(33), axd$(33))

L05220:    call "OPENFILE" (#34, "IO   ", f2%(34), rslt$(34), axd$(34))
            if f2%(34) = 0 then L05230
           call "OPENFILE" (#34, "OUTPT", f2%(34), rslt$(34), axd$(34))
                close #34:f2%(34)=1
           call "OPENFILE" (#34, "IO   ", f2%(34), rslt$(34), axd$(34))

L05230:    call "OPENFILE" (#35, "IO   ", f2%(35), rslt$(35), axd$(35))
            if f2%(35) = 0 then L05240
           call "OPENFILE" (#35, "OUTPT", f2%(35), rslt$(35), axd$(35))
                close #35:f2%(35)=1
           call "OPENFILE" (#35, "IO   ", f2%(35), rslt$(35), axd$(35))

L05240:    call "OPENFILE" (#36, "IO   ", f2%(36), rslt$(36), axd$(36))
            if f2%(36) = 0 then L05250
           call "OPENFILE" (#36, "OUTPT", f2%(36), rslt$(36), axd$(36))
                close #36:f2%(36)=1
           call "OPENFILE" (#36, "IO   ", f2%(36), rslt$(36), axd$(36))

L05250:    call "OPENFILE" (#40, "SHARE", f2%(40), rslt$(40), axd$(40))
           call "OPENFILE" (#41, "SHARE", f2%(41), rslt$(41), axd$(41))

           call "OPENFILE" (#60, "SHARE", f2%(60), rslt$(60), axd$(60))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )

            call "READ100" (#60, "MONTHS OPEN", f1%(60))
                 if f1%(60)=0 then L65000
            get #60, using L09120, pldate$
L09120:          FMT XX(32), CH(6)
            call "DATE" addr("G-", pldate$, date, today%, err%)
                 if err%<>0 then L65000
            today%=today%+1%
                 if today% < 1% or today% > 490% then L65000

            call "READ100" (#12,"10", f1%(12))
                if f1%(12) = 0 then L65000
            get #12, using L09290, str(yymmdd$(),1,1470)

            call "READ100" (#12,"11", f1%(12))
                if f1%(12) = 0 then L65000
            get #12, using L09290, str(yymmdd$(),1471,1470)

L09290:         FMT XX(2), CH(1470)

            call "READ100" (#60, "PLANNING SYSTEM FLAG", f1%(60))
            if f1%(60)=0 then L65000
            get #60, using L09340, str(planflags$(),1,480)
L09340:         FMT XX(20), CH(480)
                if str(planflags$(),1,1) = " " then L65000
            str(planflagshold$(),1,480) = str(planflags$(),1,480)

            get str(planflags$(),281) using L09440, lta%(),pprior$(),dts%()
L09440:         FMT 26*BI(4), 8*CH(1), 8*BI(4)

            supplyday% = 1%

           search str(yymmdd$(),4%) = hex(0301) to ed%() step 6%
           if ed%(1%) = 0% then L65000
           pltbase%=(ed%(1%)+5%)/6%

           str(line2$,61) = "PLANGLBL: " & str(cms2v$,,8)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, inputmsg$
            init(" ") control$(), pcontrol$(), demseq$, startok$

            inpmessage$ = "Press PF16 to EXIT IMMEDIATELY.  Press RETURN ~
        ~To Continue."
L10120:         gosub L40000
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10120

        REM *************************************************************~
            *                                                           *~
            * PAGE 2, REAL INTERACTIVE SCREEN                           *~
            *                                                           *~
            *************************************************************

            pf16$ = "(16)Exit Program"
L11060:     for fieldnr% = 1 to 13
                gosub'051 (fieldnr%)
                   if enabled% = 0% then L11210
                   if fieldnr% <> 1 then pf4$ = "(4)Previous Field"      ~
                                         else pf4$ = " "
L11090:         gosub'101 (fieldnr%)
                   if keyhit% = 16 then L65000
                   if keyhit% =  1 then gosub startover
                   if keyhit% <> 4 then L11180
L11130:               fieldnr% = max(1, fieldnr% - 1)
                      gosub'051 (fieldnr%)
                        if fieldnr% =  1% then L11060
                        if enabled% <> 0% then L11090
                           goto L11130
L11180:            if keyhit% =  0 then L11210
                   print at(02,02), bell
                   goto L11090
L11210:         gosub'151(fieldnr%)
                   if errormsg$ <> " " then L11090
                next fieldnr%

        edtpg1

            pf16$ = "(16)Begin Regen."
            pf4$  = " "
            gosub'051 (0%)
            gosub'101 (0%)
                   if keyhit% =  1 then gosub startover
                   if keyhit% = 16 then       datasave
                   if keyhit% = 0% then L11370
                print at (02,02), bell
                goto edtpg1

L11370:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1 or fieldnr% > 13 then edtpg1

            gosub'051 (fieldnr%)
              if enabled% = 0% then edtpg1
            pf16$ = " "
L11430:     gosub'101 (fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit%  =  0 then L11490
            print at(02,02), bell
            goto L11430

L11490:     gosub'151 (fieldnr%)
                if errormsg$ <> " " then L11430
                if fieldnr% <> cursor%(1) - 5% then L11370
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            call "WORKOPEN" (#50, "OUTPT", 5000%, f2%(50))
            init (" ") printline$
            startdate$(1)=date:starttime$(1)=time
            call "DATEFMT" (startdate$(1))
            put printline$, using L19100, startdate$(1), starttime$(1)
L19100: %RUN STARTED, ######## ########
            write #50, using L19120, printline$
L19120:         FMT CH(132)
            init (" ") printline$
            write #50, using L19120, printline$

            if control$ (1) = "N" then L19180
            gosub plnrstr

L19180:     control$ (1) = "D"
            init (" ") printline$
            startdate$(2)=date:starttime$(2)=time
            call "DATEFMT" (startdate$(2))
            put printline$, using L19200, startdate$(2), starttime$(2)
L19200: %DEMAND PLANNING STARTED, ######## ########
            write #50, using L19220, printline$
L19220:         FMT CH(132)
            init (" ") printline$
            write #50, using L19220, printline$

                str(planflags$(),46,6)=hex(ffffffffffff)
            if control$ (2) = "N" then L19290
            gosub plandemands

L19290:     control$ (2) = "D"
            init (" ") printline$
            startdate$(3)=date:starttime$(3)=time
            call "DATEFMT" (startdate$(3))
            put printline$, using L19310, startdate$(3), starttime$(3)
L19310: %MANUFACTURING SHORTAGE PLANS STARTED, ######## ########
            write #50, using L19330, printline$
L19330:         FMT CH(132)
            init (" ") printline$
            write #50, using L19330, printline$

            str(planflags$(),4,1) = "P"
            if control$ (3) = "N" then L19400
            gosub mfgshortage

L19400:     control$ (3) = "D"
            init (" ") printline$
            startdate$(4)=date:starttime$(4)=time
            call "DATEFMT" (startdate$(4))
            put printline$, using L19420, startdate$(4), starttime$(4)
L19420: %PURCHASING SHORTAGE CLEARING STARTED, ######## ########
            write #50, using L19440, printline$
L19440:         FMT CH(132)
            init (" ") printline$
            write #50, using L19440, printline$

            if control$ (4) = "N" then L19500
            gosub purshortage

L19500:     control$ (4) = "D"
            init (" ") printline$
            startdate$(5)=date:starttime$(5)=time
            call "DATEFMT" (startdate$(5))
            put printline$, using L19520, startdate$(5), starttime$(5)
L19520: %FINAL CLEAN UP STARTED, ######## ########
            write #50, using L19540, printline$
L19540:         FMT CH(132)
            init (" ") printline$
            write #50, using L19540, printline$

            if control$ (5) = "N" then L19600
            gosub cleanup

L19600:     control$ (5) = "D"
            init (" ") printline$
            startdate$(6)=date:starttime$(6)=time
            call "DATEFMT" (startdate$(6))
            put printline$, using L19620, startdate$(6), starttime$(6)
L19620: %RUN FINISHED, ######## ########
            write #50, using L19640, printline$
L19640:         FMT CH(132)
            init (" ") printline$
            write #50, using L19640, printline$

            gosub time_summary
            if control$ (6) = "N" then L19690
            gosub final_summary
L19690:     control$ (6) = "D"
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************
        deffn'051 (fieldnr%)
            inputmsg$  = "To Modify Displayed Values, Position Cursor" & ~
                         " to Desired Value & Press (RETURN)."
            enabled%   = 1%

            on fieldnr% gosub L20200,                                     ~
                              L20300,                                     ~
                              L20400,                                     ~
                              L20500,                                     ~
                              L20600,                                     ~
                              L20700,                                     ~
                              L20800,                                     ~
                              L20900,                                     ~
                              L21000,                                     ~
                              L21100,                                     ~
                              L21200,                                     ~
                              L21300,                                     ~
                              L21400

            return

L20200: REM PURGE OPTION
            if control$(1) = " " then control$(1) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for PURGE FUNCTIONS."
            return

L20300: REM DETAIL CLEAR
            if control$(1) = "Y" then L20350
               pcontrol$(1) = "N"
               enabled% = 0%
               return
L20350:     if pcontrol$(1) = " " then pcontrol$(1) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for PURGE DETAILS."
            return

L20400: REM RESET DEMANDS
            if control$(1) = "Y" then L20450
               pcontrol$(2) = "N"
               enabled% = 0%
               return
L20450:     if pcontrol$(2) = " " then pcontrol$(2) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for RESET DEMANDS."
            return

L20500: REM RESET WC CAPACITY
            if control$(1) = "Y" then L20550
               pcontrol$(3) = "N"
               enabled% = 0%
               return
L20550:     if pcontrol$(3) = " " then pcontrol$(3) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for RESET CAPACITY."
            return

L20600: REM RESET PIPMASTR
            if control$(1) = "Y" then L20650
               pcontrol$(4) = "N"
               enabled% = 0%
               return
L20650:     if pcontrol$(4) = " " then pcontrol$(4) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for RESET PIP."
            return

L20700: REM PLAN DEMANDS
            if control$(2) = " " then control$(2) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for PLANNING DEMANDS."
            return

L20800: REM MFG SHORTAGE
            if control$(3) = " " then control$(3) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for CLEAR MANUFACTURING SHOR~
        ~TAGES."
            return

L20900: REM PUR SHORTAGE
            if control$(4) = " " then control$(4) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for CLEAR PURCHASED SHORTAGE~
        ~S."
            return

L21000: REM FINAL CLEAN-UP
            if control$(5) = " " then control$(5) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for RESETTING THE STATUS of ~
        ~EXPEDITE Parts."
            return

L21100: REM FINAL REPORT
            if control$(6) = " " then control$(6) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for PRINTING OF THE LOG."
            return

L21200: REM INCLUDE EXPEDITE WARNINGS
            if control$(6) = "Y" then L21250
               control$(7) = "N"
               enabled% = 0%
               return
L21250:     if control$(7) = " " then control$(7) = "Y"
            inputmsg$ = "Indicate 'Y' or 'N' for INCLUDING EXPEDITE MESSA~
        ~GES in Log."
            return

L21300: REM OPTIMIZE?
            if startok$ = " " then startok$ = "NO"
            inputmsg$ = "Should the System Try to OPTIMIZE PIP While clea~
        ~ring Shortages?"
            return

L21400: REM DEMAND TYPE ORDER
            if demseq$ = " " then demseq$ = "7845312"
            inputmsg$ = "Indicate the ORDER in which DEMAND TYPES Will Be~
        ~ Planned."
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto L10000

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

        plnrstr
            if pcontrol$(1) = "N" then L30350
            call "SHOSTAT" ("Clearing PIP Detail files.")

            readkey$ = all(hex(00))
            str(readkey$,,2) = "WO"
            call "DELETE" (#33, readkey$,2%)

            readkey$ = all(hex(00))
            str(readkey$,,2) = "BO"
            call "DELETE" (#33, readkey$,2%)

            readkey$ = all(hex(00))
            str(readkey$,,2) = "WO"
            call "DELETE" (#36, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2) = "WO"
            call "DELETE" (#34, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2) = "WO"
            call "DELETE" (#8, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2) = "WO"
            call "DELETE" (#23, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#33, readkey$,2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#36, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#34, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#8, readkey$, 2%)

            readkey$ = all(hex(00))
            str(readkey$,,2%) = "BW"
            call "DELETE" (#23, readkey$, 2%)

L30350:     pcontrol$(1) = "D"
            if pcontrol$(2) = "N" then L30700
            call "SHOSTAT" ("Resetting the status of demands.")
            gosub purge_sf_files
            readkey$ = all(hex(00)):hits%=0%
L30380:     call "PLOWNXT1"  (#1, readkey$, 0%, f1%(1))
                if f1%(1) = 0 then L30700
            get #1, using L30410, demand$
L30410:         FMT CH(123)
            str(demand$,77,12) = " "  /* INITIALIZE LAST PLANNED & */
                                      /* PLANNED COMPLETION DATES  */

            if str(demand$,1,1)=" " then L30490
            if str(demand$,1,1)="1" then L30490
            if pos("68" = str(demand$,1,1)) = 0  then                    ~
                str(demand$,1,1) = "1"  else str(demand$,1,1) = " "
            delete #1
            write #1, using L30410, demand$
L30490:     readkey1$=str(demand$,10,19)&hex(00000000)
            dd%=0%:qty=0
            call "DATE" addr("G-", pldate$, str(demand$,4,6), dd%, err%)
            dd%=dd%+1%:dd%=min(490%,max(1%,dd%))
L30530:     call "PLOWNXT1" (#34, readkey1$, 19%, f1%(34))
                     if f1%(34) = 0 then L30600
            get #34, using L30560, q
L30560:         FMT XX(19), XX(25), XX(4), XX(8), PD(14,4)
            delete #34
            qty=qty+q
            goto L30530
L30600:     call "READ101" (#33, str(demand$,10,19), f1%(33))
               if f1%(33)<>0 then delete #33
            if abs(qty)<.00001 then L30670
            write #33 using L30640, str(demand$,29,25), dd%,              ~
                                             str(demand$,10,19), qty, dd%
L30640:         FMT CH(25),BI(4),CH(19),PD(14,4),BI(4)
            write #34 using L30660, str(demand$,10,19),str(demand$,29,25),~
                                                         dd%, time, qty
L30660:         FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
L30670:     call "DELETE" (#35, str(demand$,10,19),19%)
            if mod(hits%,100%) = 0 then                                  ~
                print at(20,02), str(demand$,10,19), hits%, time
            hits% = hits% + 1%
            goto L30380

L30700:     pcontrol$(2) = "D"
            if pcontrol$(3) = "N" then L31100
            REM **** ADJUST FOR DEAD WCOUT RECORDS *****
            call "WCBUILD" ("ALL^", #11, #23)

L31100:     pcontrol$(3) = "D"
            if pcontrol$(4) = "N" then return

            call "SHOSTAT" ("Rebuilding the PIPMASTR file. (Last step)")

            init (hex(00)) part$, pipinpart$, pipoutpart$
            f1%(33), f1%(34) = -1%:active%,hits%=0%
            call "READ103" (#2, part$, f1%(2))
                     if f1%(2) = 0 then return
L31170:     get #2,using L31180,rec1$,part$,pip%(),oh,ss,mq,pz,tp%,rec2$
L31180:            FMT CH(1), CH(25), 490*BI(4), 4*PD(14,4), BI(2), CH(4)

            mat pip = zer
            pip(today%) = oh

            if f1%(33) < 0% then L31230
            if str(pipinpart$,1,1) = hex(ff) then L31302
            goto L31271
L31230:     readkey$ = all(hex(00))
            str(readkey$,,25) = part$
            call "PLOWALTS" (#33, readkey$, 1%, 0%, f1%(33))
                     if f1%(33) = 0 then L31291
L31270:     get #33, using L31710, pipinpart$, di%, qi
L31271:     if pipinpart$ < part$ then L31286
            if pipinpart$ > part$ then L31302
            pip(di%) = pip(di%) + qi
            active% = active% + 1%
L31286:     read #33, eod goto L31291
            goto L31270
L31291:     init (hex(ff)) pipinpart$

L31302:     if f1%(34) < 0% then L31310
            if str(pipoutpart$,1,1) = hex(ff) then L31390
            goto L31352
L31310:     readkey$ = all(hex(00))
            str(readkey$,,25) = part$
            call "PLOWALTS" (#34, readkey$, 1%, 0%, f1%(34))
                     if f1%(34) = 0 then L31375
L31350:     get #34, using L31780, pipoutpart$, do%, qo
L31352:     if pipoutpart$ < part$ then L31365
            if pipoutpart$ > part$ then L31390
            pip(do%) = pip(do%) - qo
            active% = active% + 1%
L31365:     read #34, eod goto L31375
            goto L31350
L31375:     init (hex(ff)) pipoutpart$

L31390: rem OH=ROUND(OH,2):ss=round(ss,2):mq=round(mq,2):pz=round(pz,2)
            if active% > 0% then L31440
               mat pip% = zer
                  if abs (oh) < .0001 then L31432
               oh% = sgn(oh)*int(abs(oh))
               for i% = today% to 490%
               pip%(i%) = oh%
               next i%
L31432:      atc% = pip%(today%)
             goto L31550

L31440:     for i% = 489% to 1% step -1%
            if abs(pip(i%)) < .0001 then L31490
               for j% = i%+1% to 490%
                 pip(j%) = pip(j%) + pip(i%)
               next j%
L31490:     next i%
            mat pip% = pip
            atc% = pip%(today%)
            for i% = today% to 490%
                if pip%(i%) < atc% then atc% = pip%(i%)
            next i%
L31550:     rec1$=" "
            if tp% = 0% or tp%>499% then L31592
            if atc% < ss then rec1$="5"
            if atc% < 0% then rec1$="9"
            if atc% > ss + mq then rec1$="3"
            goto L31600
L31592:     if atc% < ss then rec1$="4"
            if atc% < 0% then rec1$="8"
            if atc% > ss + mq then rec1$="2"

L31600:     put #2, using L31180, rec1$,part$,pip%(),oh,ss,mq,pz,tp%,rec2$
            rewrite #2

            active% = 0%
            if mod(hits%,100%) = 0% then                                 ~
                                  print at (20,02), part$, hits%, time
            hits% = hits% + 1%
            read #2, hold, eod goto L31690
            goto L31170
L31690:     return

L31710: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            XX(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity of something in packed de */~
            XX(4)                /* Date to start as a date subscript  */~

L31780: FMT                      /* FILE: PIPOUT                       */~
            XX(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            XX(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity of something in packed de */~


        purge_sf_files
            if f2%(40) <> 0 then L31925
               close #40
               get rslt$(40) using L31910, r%
L31910:            FMT XX(16), BI(4)
               put rslt$(40) using L31920, "OUTPUT", r%, 100%, 100%
L31920:            FMT CH(6), PIC(00000000),PIC(000),PIC(000)
L31925:     if f2%(41) <> 0 then L31955
               close #41
               get rslt$(41) using L31940, r%
L31940:            FMT XX(16), BI(4)
               put rslt$(41) using L31950, "OUTPUT", r%, 100%, 100%
L31950:            FMT CH(6), PIC(00000000),PIC(000),PIC(000)
L31955:     call "GETNAMES" addr (#40, file$, lib$, vol$)
            call "SCRATCH"  addr ("F", file$, lib$, vol$, "B", " ", u3%)
            call "GETNAMES" addr (#41, file$, lib$, vol$)
            call "SCRATCH"  addr ("F", file$, lib$, vol$, "B", " ", u3%)
        REM THAT TOOK CARE OF ALL THE SF DATA !!
            f2%(40), f2%(41) = 1%
            call "OPENFILE" (#40, "OUTPT", f2%(40), rslt$(40), axd$(40))
            call "OPENFILE" (#41, "OUTPT", f2%(41), rslt$(41), axd$(41))
                close #40
                close #41
            call "OPENFILE" (#40, "IO   ", f2%(40), rslt$(40), axd$(40))
            call "OPENFILE" (#41, "IO   ", f2%(41), rslt$(41), axd$(41))

            return

        REM *************************************************************~
            *                                                           *~
            *      PROCESS DEMAND MASTER FILE IN CORRECT ORDER          *~
            *                                                           *~
            *************************************************************
        plandemands

            call "SHOSTAT" ("Planning Demands")

            hits% = 0%

            for i% = 1% to 8%

            init (hex(00)) readkey$
            str(readkey$,1,1) = str(demseq$,i%,1)
            if str(readkey$,1,1) = hex(20) then L32300

L32080:     call "PLOWNEXT" (#1, readkey$, 1%, f1%(1))
                 if f1%(1) = 0 then L32300
            get #1, using L32095, demand$
L32095:         FMT CH(123)

            if mod(hits%,100%) = 0 then                                  ~
                print at(20,02), str(demand$,10,19), hits%, time
            hits% = hits% + 1%

            get str(demand$,1,123) using L32125, status$,type$,priority$, ~
                duedate$, demcode$, demline$, part$, demquant$, bom$
L32125:         FMT 3*CH(1),CH(6),CH(16),CH(3),CH(25),CH(10),XX(4),CH(3)

            if status$ > "1" then L32080
            convert demquant$ to demquant, data goto L32080
            if demquant < .0001 then L32080
            call "DATE" addr("G-", pldate$, duedate$, cd%, err%)
                if err% <> 0% then L32080
            cd% = max(1%, min(490%, cd%+1%))

            call "READ100" (#4, part$, f1%(4))
                if f1%(4) = 0 then L32080
            get #4, using L32168, ptype$
L32168:         FMT XX(179), CH(10)
            convert ptype$ to type%, data goto L32080
            if type% = 0% then L32080

            ltss%=val(priority$,1)-64%
            if ltss% < 1% or ltss% > 26% then L32195
            if cd%-lta%(ltss%) > today% then L32080

L32195:     gosub L39000

            if final% < 1% or final% > 490% then L32280
            str(demand$,77,6) = date
            str(demand$,83,6) = yymmdd$(final%)
            if status$ =" " then L32240
            if str(demand$,83,6) > str(demand$,4,6) then                 ~
               str(demand$,1,1) = "7" else str(demand$,1,1) = "9"
            goto L32250
L32240:     if str(demand$,83,6) > str(demand$,4,6) then                 ~
               str(demand$,1,1) = "6" else str(demand$,1,1) = "8"
L32250:     call "REDALT1" (#1, str(demand$,10,19), 1%, f1%(1))
            if f1%(1) <> 0 then delete #1
            write #1, using L32265, demand$
L32265:           FMT CH(123)
            date$(2) = str(demand$,83,6)
            goto L32285
L32280:     date$(2) = " "
L32285:     date$(1) = str(demand$,4,6)
            gosub print_it
            goto L32080
L32300:     next i%
            return

        REM *************************************************************~
            *                                                           *~
            *      CLEAR MFG SHORTAGES                                  *~
            *                                                           *~
            *************************************************************
        mfgshortage
            call "SHOSTAT" ("Clearing Manufactured Part Shortages")
            get str(planflags$()) using L33032, compoff%
L33032:         FMT POS(277), BI(4)
            todayc% = min(490%, max(1%, today% + compoff%))
            hits% = 0%
L33040:     init (hex(00)) readkey$
            str(readkey$,1,1) = "8"

L33055:     call "PLOWAL1" (#2, readkey$, 1%, 1%, f1%(2))
                if f1%(2) = 0 then return

            get #2, using L33075, part$, type%, priority%
L33075:         FMT XX(1),CH(25),XX(1960),XX(32),BI(2),XX(2),BI(2)
            if type% = 0% then L33525
            if type% < 500% then L33570
            if type% > 789% and type% < 800% then L33525
            ltss% = (type%/100%) - 1%
            priority% = 64% + int(priority%/1000%)
            if priority% < 65% then priority% = 32%
            priority$ = bin(priority%, 1)
            if priority$ = " " then priority$=pprior$(ltss%)
            if priority$=" " then priority$="A"
            supplyday%=max(1%,min(dts%(ltss%),490%))

            if mod(hits%,100%) = 0 then                                  ~
                print at(20,02), part$, hits%, time
            hits% = hits% + 1%

            if startok$ <> "YES" then L33305
            purgemsg% = 0%
            init (hex(00)) readkey1$
            put str(readkey1$,1,29), using L33170, part$, today%
L33170:         FMT CH(25), BI(4)
L33175:     call "PLOWALTS" (#33, readkey1$, 1%, 25%, f1%(33))
                if f1%(33) = 0 then L33295
            get #33, using L33190, due%, intagnr$(1), qty
L33190:         FMT XX(25), BI(4), CH(19), PD(14,4)
            if str(intagnr$(1%),1%,2%)  = "BO" then L33202
            if str(intagnr$(1%),1%,2%)  = "BW" then L33202
            if str(intagnr$(1%),1%,2%) <> "WO" then L33175
L33202:     if purgemsg% = 0% then gosub print_purge
            purgemsg% = 1%
        call "CLRPIPIN" (                                                ~
                     part$,                                              ~
                     today%,                                             ~
                     due%,               /* DATE OF PIPIN TO CLEAR     */~
                     intagnr$(1),        /* TAG OF PIPIN TO CLEAR      */~
                     #1,                 /* DEMMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

            goto L33175

L33295:    call "READ100" (#2, part$, f1%(2))
            if f1%(2) = 0 then L33055
L33305:    get #2, using L33310, workpip%()
L33310:         FMT XX(1), XX(25), 490*BI(4)

        REM START LOOP
            startday% = todayc%:make = 0
L33330:     if startday% = 1% then L33355
                if workpip%(startday%-1%) >= 0% then L33355
                   for i%=startday% to 490%
                     workpip%(i%) = workpip%(i%) - workpip%(startday%-1%)
                   next i%
L33355:     if workpip%(startday%) < 0% then L33375
L33360:     startday% = startday% + 1% : make = 0
            if startday% < 490% then L33330 else L33500

L33375:     for i% = startday% to min(490%, startday% + supplyday%)
                make = min(make, workpip%(i%))
            next i%
            if abs(make) < .0001 then L33360

            demcode$ = "OP000" & tagdate$ & time:demline$="001"
            cd% = startday% :demquant = -make
            type$="8"

            gosub L39000
            if final% < 1% or final% > 490% then L33470
                call "READ100" (#2, part$, f1%(2))
                if f1%(2) = 0 then L33040
                get #2, using L33445, workpip%()
L33445:              FMT XX(26), 490*BI(4)

            demcode$="*CLEAR SHORTAGE*":demline$=" "
            errormsg$ = "WORK ORDER ADVICE PLACED IN SYSTEM"
                goto L33480
L33470:     if control$(7) <> "Y" then L33360
            demcode$="* * EXPEDITE * *":demline$=" "
            demquant=workpip%(cd%)
L33480:         date$(1)=" ":date$(2)=yymmdd$(cd%)
                gosub print_it
                goto L33360

L33500:     call "READ101" (#2, part$, f1%(2))
                if f1%(2) = 0 then L33040
            get #2, using L33515, status$
L33515:         FMT CH(1)
                if status$ <>"8" then L33040
L33525:     put #2, using L33515, "6"
            rewrite #2
            goto L33040

            call "READ101" (#2, part$, f1%(2))
                if f1%(2) = 0 then L33040
            get #2, using L33560, status$
L33560:         FMT CH(1)
                if status$ <>"9" then L33040
L33570:     put #2, using L33560, "9"
            rewrite #2
            goto L33040

        REM *************************************************************~
            *                                                           *~
            *      CLEAR PURCHASE PARTS                                 *~
            *                                                           *~
            *************************************************************

        purshortage

            call "SHOSTAT" ("Clearing Purchased Part Shortages")
            get str(planflags$()) using L34042, compoff%
L34042:         FMT POS(277), BI(4)
            todayc% = min(490%, max(1%, today% + compoff%))
            hits% = 0%
            init (hex(00)) readkey$
            str(readkey$,1,1) = "9"

L34065:     call "PLOWAL1" (#2, readkey$, 1%, 1%, f1%(2))
                if f1%(2) = 0 then return

            get #2, using L34085, part$,type%,plt%,priority%
L34085:         FMT XX(1),CH(25),XX(1960),XX(32),3*BI(2)
            if type% = 0% then L34640
            if type% > 499% then L34640
            if type% > 489% and type% < 500% then L34065
            ltss% = (type%/100%) - 1%
            priority% = 64% + int(priority%/1000%)
            if priority% < 65% then priority% = 32%
            priority$ = bin(priority%, 1)
            if pprior$(ltss%)<>" " then priority$=pprior$(ltss%)
            if priority$=" " then priority$="A"
            supplyday%=max(1%,min(dts%(ltss%),490%))

            if mod(hits%,100%) = 0 then                                  ~
                print at(20,02), part$, hits%, time
            hits% = hits% + 1%

            if startok$ <> "YES" then L34325
            purgemsg% = 0%
            init (hex(00)) readkey1$
            put str(readkey1$,1,29), using L34190, part$, today%
L34190:         FMT CH(25), BI(4)
L34195:     call "PLOWALTS" (#33, readkey1$, 1%, 25%, f1%(33))
                if f1%(33) = 0 then L34315
            get #33, using L34210, due%, intagnr$(1), qty
L34210:         FMT XX(25), BI(4), CH(19), PD(14,4)
            if str(intagnr$(1%),1%,2%)  = "BO" then L34222
            if str(intagnr$(1%),1%,2%)  = "BW" then L34222
            if str(intagnr$(1%),1%,2%) <> "WO" then L34195
L34222:     if purgemsg% = 0% then gosub print_purge
            purgemsg% = 1%
        call "CLRPIPIN" (                                                ~
                     part$,                                              ~
                     today%,                                             ~
                     due%,               /* DATE OF PIPIN TO CLEAR     */~
                     intagnr$(1),        /* TAG OF PIPIN TO CLEAR      */~
                     #1,                 /* DEMMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #2,                 /* PIPMASTR                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT                      */~
                     #33,                /* PIPIN                      */~
                     #34,                /* PIPOUT                     */~
                     #41,                /* SFCUM2                     */~
                     #35,                /* PIPCROSS                   */~
                     #36)                /* JBPIPXRF                   */

            goto L34195

L34315:    call "READ100" (#2, part$, f1%(2))
            if f1%(2) = 0 then L34065
L34325:    get #2, using L34330, workpip%(), moq, pz
L34330:         FMT XX(1), XX(25), 490*BI(4), XX(16), 2*PD(14,4)
           str(testdemd$,1,6)= bin(2^(int(type%/100)-2%))                ~
                             & hex(80)                                   ~
                             & bin(2^(val(priority$,1)-64%),4)

          str(testbyte$,1,6)=str(testdemd$,1,6) and str(planflags$(),52,6)
          if str(testbyte$,1,1)=hex(00) then L34367
             moq,pz=0

L34367:         readkey1$ = str(part$) & "2001"
                call "READ100" (#24, readkey1$, effplt%)
                    if effplt% = 0% then L34375
                get #24 using L34371, effplt$()
L34371:              FMT XX(29), 490*CH(3)

L34375: REM START LOOP
            startday% = todayc%:make = 0
L34385:     if startday% = 1% then L34410
                if workpip%(startday%-1%) >= 0% then L34410
                   for i%=startday% to 490%
                     workpip%(i%) = workpip%(i%) - workpip%(startday%-1%)
                   next i%
L34410:     if workpip%(startday%) < 0% then L34430
L34415:     startday% = startday% + 1% : make = 0
            if startday% < 490% then L34385 else L34065

L34430:         lt% = plt%
                if effplt% = 0% then L34475
                convert effplt$(mod(startday% - pltbase%, 365%) + 1%) to ~
                                        lt%, data goto L34475

L34475:     for i% = startday% to min(490%, startday% + supplyday%)
                make = min(make, workpip%(i%))
            next i%
            make= abs(make)
            if abs(make) < .0001 then L34415

           make = max(make, moq)
           if pz < .0001 then L34530
           if abs(mod(make-moq,pz)) < .00001 then L34530
           make=round(make+pz-mod(make-moq,pz),2)

L34530:     intagnr$(1) = "BO000" & tagdate$ & time
            cd% = startday% :demquant = make:st%=cd%-lt%
            convert st% to str(intagnr$(1),3,3), pic(000)
            if st% < todayc% then L34585

            write #33, using L34560, part$, cd%, intagnr$(1), demquant, st%
L34560:           FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            call "PIPFLAGS" (part$, 1%, cd%, demquant, #2, #41)
            for i%=startday% to 490%
                workpip%(i%) = workpip%(i%) + demquant
            next i%

            demcode$="*CLEAR SHORTAGE*":demline$=" "
            errormsg$="PURCHASE ORDER ADVICE PLACED IN SYSTEM"
                goto L34595
L34585:     if control$(7) <> "Y" then L34415
            demcode$="* * EXPEDITE * *":demquant=workpip%(cd%)
            errormsg$ = "INSUFFICIENT PURCHASING LEAD TIME":demline$=" "
L34595:     type$="8"
            date$(1)=" ":date$(2)=yymmdd$(cd%)
            gosub print_it
            goto L34415

L34640:     call "PIPFLAGS" (part$, 1%, today%, 0, #2, #41)
            goto L34065

            put #2, using L34665, "7"
L34665:         FMT CH(1)
            rewrite #2
            goto L34065

        REM *************************************************************~
            *                                                           *~
            *      FINAL CLEANUP                                        *~
            *                                                           *~
            *************************************************************
        cleanup
            call "SHOSTAT" ("Final Clean Up In Process")

            init (hex(00)) readkey$
            str(readkey$,1,1) = "6"
L35100:     call "PLOWAL1" (#2, readkey$, 1%, 1%, f1%(2))
                if f1%(2) = 0 then return
            put #2, using L35114, "8"
            rewrite #2
            goto L35100
L35114:         FMT CH(1)

            get #2, using L35130, part$
L35130:         FMT XX(1), CH(25)
            call "PIPFLAGS" (part$, 1%, today%, 0, #2, #41)
            goto L35100

            init (hex(00)) readkey$
            str(readkey$,1,1) = "7"
L35220:     call "PLOWALTS" (#2, readkey$, 1%, 1%, f1%(2))
                if f1%(2) = 0 then return
            get #2, using L35250, part$
L35250:         FMT XX(1), CH(25)
            call "PIPFLAGS" (part$, 1%, today%, 0, #2, #41)
            goto L35220

L39000: REM *************************************************************~
            *                                                           *~
            *      CALL TO PLANSUB                                      *~
            *                                                           *~
            *************************************************************
        planflag% = 5%
        call"PLANSUB" (demcode$,         /* DEMAND CODE                */~
                     demline$,           /* DEMAND LINE                */~
                     type$,              /* DEMAND TYPE                */~
                     priority$,          /* DEMAND PRIORITY            */~
                     part$,              /* PART NEEDED                */~
                     demquant,           /* QUANTITY NEEDED            */~
                     cd%,                /* REQ'D COMPL DATE           */~
                     "001",              /* DELIVER TO WAREHOUSE       */~
                     "   ",              /* THE WC ROUTE TO USE        */~
                     bom$,               /* WHICH BOM TO USE           */~
                     errormsg$,          /* THE RETURN MESSAGE         */~
                     first%,             /* FIRST DATE PASSED BACK     */~
                     final%,             /* DATE PLANNED FOR           */~
                     today%,             /* SUBSCRIPT FOR TODAY'S DATE */~
                     planflag%,          /* >0= PLAN, 0 = CHECK ONLY   */~
                     #16,                /* HNYALTRS                   */~
                     #15,                /* BOMMASTR                   */~
                     #2,                 /* PIPMASTR                   */~
                     #7,                 /* RTEMASTR                   */~
                     #8,                 /* JBCROSS2                   */~
                     #11,                /* WCMASTR                    */~
                     #23,                /* WCOUT   WC-CROSSREFERENCE  */~
                     #33,                /* PIPIN (QUANTS ADDED/DAY)   */~
                     #34,                /* PIPOUT (QUANTS SUBTR/DAY)  */~
                     #40,                /* SFMASTR2  SALES FORECASTS  */~
                     #41,                /* SFCUM2  CUMULATIVE FCSTS   */~
                     #35,                /* PIPCROSS                   */~
                     #24,                /* ENGMASTR                   */~
                     #14,                /* BOMSPEC                    */~
                     #36,                /* JBPIPXRF                   */~
                     #6,                 /* RTEALTRS                   */~
                     #62,                                                ~
                     #63)

            return

            first% = first%

            FMT CH( 1),                       /* RECORD STATUS         */~
                CH( 1),                       /* DEMAND TYPE           */~
                CH( 1),                       /* PRIORITY              */~
                CH( 6),                       /* REQ COMPLETION DATE   */~
                CH(16),                       /* DEMAND CODE           */~
                CH( 3),                       /* DEMAND LINE           */~
                CH(25),                       /* PART NEEDED           */~
                CH(10),                       /* QUANTITY              */~
                CH( 4),                       /* WC (FOR PM ONLY)      */~
                CH( 3),                       /* WHICH BOM REQD?       */~
                CH( 3),                       /* WHICH WCROUTE REQD?   */~
                CH( 3),                       /* DEL TO/SHIP FROM WHSE */~
                CH( 6),                       /* DATE LAST PLANNED     */~
                CH( 6),                       /* PLANNED COMPL DATE    */~
                CH(09),                       /* CUSTOMER IF TYPE 1    */~
                CH(01),                       /* APPROVAL STATUS       */~
                CH(15),                       /* APPROVAL BY           */~
                CH(10)                        /* APPROVAL ON MMDD (BIN)*/

L40000: REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Complete Planning Regeneration",                      ~
               at (01,66),                                               ~
                  "Date:",                                               ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "This program is designed to regenerate the en~
        ~tire planning system by:         ",                              ~
               at (07,02), "   1)  Deleting all unreleased advices from t~
        ~he planning system files.        ",                              ~
               at (08,02), "   2)  Resetting all demands to an unplanned ~
        ~status.                          ",                              ~
               at (09,02), "   3)  Rebalance the planned inventory positi~
        ~on and work center utilization.  ",                              ~
               at (10,02), "   4)  Replan all demands for one level only,~
        ~ planning from today forward.    ",                              ~
               at (11,02), "   5)  Clear resulting shortages for manufact~
        ~ured components.                 ",                              ~
               at (12,02), "   6)  Place purchase advices for all purchas~
        ~ed parts.                        ",                              ~
               at (13,02), "                                             ~
        ~                                 ",                              ~
               at (14,02), "After processing, a journal of planning activ~
        ~ities and unresolved shortages   ",                              ~
               at (15,02), "will be printed for review and possible furth~
        ~er action.                       ",                              ~
               at (16,02), "                                             ~
        ~                                 ",                              ~
               at (17,02), "                                             ~
        ~                                 ",                              ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(3)Planning Switches",                                ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00030d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <>  3 then L41030
                  gosub L62000
                  goto L40210

L41030:        if keyhit% <> 13 then L41070
                  call "MANUAL" ("PLANGLBL")
                  goto L40210

L41070:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

L42000: REM *************************************************************~
            *      C O N T R 0 L   M O D E   S C R E E N   P A G E   1  *~
            *                                                           *~
            * MAINTAIN CONTROL SETTINGS                                 *~
            *************************************************************

        deffn'101 (fieldnr%)

            if fieldnr% = 0% then init(hex(86)) lfac$() else             ~
                                  init(hex(8c)) lfac$()

            if fieldnr% <> 0% then lfac$(fieldnr%) = hex(81)
            if errormsg$ > " " and fieldnr% > 0% then                    ~
                                     lfac$(fieldnr%) = or hex(10)

            accept                                                       ~
               at (01,02),                                               ~
                  "Complete Planning Regeneration, Control Selections",  ~
               at (01,66),                                               ~
                  "Date:",                                               ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Execute Plan Restore",                       ~
               at (06,40), fac(lfac$( 1)), control$( 1)       ,   ch( 1),~
               at (07,05), "Purge Detail Files",                         ~
               at (07,40), fac(lfac$( 2)), pcontrol$( 1)      ,   ch( 1),~
               at (08,05), "Reset Demand File",                          ~
               at (08,40), fac(lfac$( 3)), pcontrol$( 2)      ,   ch( 1),~
               at (09,05), "Reset Work Center Capacity",                 ~
               at (09,40), fac(lfac$( 4)), pcontrol$( 3)      ,   ch( 1),~
               at (10,05), "Reset PIP Master File",                      ~
               at (10,40), fac(lfac$( 5)), pcontrol$( 4)      ,   ch( 1),~
               at (11,02), "Execute Plan of Demands",                    ~
               at (11,40), fac(lfac$( 6)), control$( 2)       ,   ch( 1),~
               at (12,02), "Execute MFG Shortage Clearing",              ~
               at (12,40), fac(lfac$( 7)), control$( 3)       ,   ch( 1),~
               at (13,02), "Execute PUR Shortage Clearing",              ~
               at (13,40), fac(lfac$( 8)), control$( 4)       ,   ch( 1),~
               at (14,02), "Execute Final Cleanup",                      ~
               at (14,40), fac(lfac$( 9)), control$( 5)       ,   ch( 1),~
               at (15,02), "Execute Print Summary",                      ~
               at (15,40), fac(lfac$(10)), control$( 6)       ,   ch( 1),~
               at (16,02), "Print Expedite Messages",                    ~
               at (16,40), fac(lfac$(11)), control$( 7)         , ch( 1),~
               at (17,02), "Optimize While Clearing Shortages",          ~
               at (17,40), fac(lfac$(12)),   startok$           , ch(03),~
               at (18,02), "Demand Type Planning Sequence",              ~
               at (18,40), fac(lfac$(13)), demseq$              , ch( 8),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inputmsg$            , ch(79),~
                                                                         ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,24), fac(hex(8c)), pf4$,                           ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(3)Planning Switches",                                ~
               at (24,65), fac(hex(84)), pf16$,                          ~
                                                                         ~
               keys(hex(000103040d0f10)),                                ~
               key (keyhit%)

               if keyhit% <>  3 then L42620
                  gosub L62000
                  goto  L42000

L42620:        if keyhit% <> 13 then L42660
                  call "MANUAL" ("PLANGLBL")
                  goto L42000

L42660:        if keyhit% <> 15 then L42700
                  call "PRNTSCRN"
                  goto L42000

L42700:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

        deffn'151 (fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L50290,                                     ~
                              L50340,                                     ~
                              L50430,                                     ~
                              L50520,                                     ~
                              L50610,                                     ~
                              L50700,                                     ~
                              L50750,                                     ~
                              L50810,                                     ~
                              L50870,                                     ~
                              L50930,                                     ~
                              L50980,                                     ~
                              L51080,                                     ~
                              L51140

            return

L50290: REM PURGE OPTION
            if pos("YN" = control$(1)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50340: REM DETAIL CLEAR
            if pos("YN" = pcontrol$(1)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50430: REM RESET DEMANDS
            if pos("YN" = pcontrol$(2)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50520: REM RESET WC CAPACITY
            if pos("YN" = pcontrol$(3)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50610: REM RESET PIPMASTR
            if pos("YN" = pcontrol$(4)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50700: REM PLAN DEMANDS
            if pos("YN" = control$(2)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50750: REM MFG SHORTAGE
            if pos("YN" = control$(3)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50810: REM PUR SHORTAGE
            if pos("YN" = control$(4)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50870: REM FINAL CLEAN-UP
            if pos("YN" = control$(5)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50930: REM FINAL REPORT
            if pos("YN" = control$(6)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L50980: REM INCLUDE EXPEDITE WARNINGS
            if pos("YN" = control$(7)) <> 0% then return
            errormsg$ = "Indicate 'Y' or 'N'."
            return

L51080: REM OPTIMIZE?
            if startok$ = "YES" then return
            if startok$ = "NO"  then return
            errormsg$ = "Indicate 'YES' or 'NO'."
            return

L51140: REM DEMAND TYPE ORDER

             temp1$=demseq$
             init (hex(20)) str(temp1$, len(demseq$) + 1)
             init (" ") demseq$

             for i% = 1% to 8%
                if str(temp1$,i%,1%) = hex(20) then L51230
                if str(temp1$,i%,1%) < hex(31) or                        ~
                     str(temp1$,i%,1%) > hex(38) then L51250
                if pos(demseq$ = str(temp1$,i%,1%)) <> 0 then L51230
                if str(demseq$,1,1) = hex(20)                            ~
                      then demseq$ = str(temp1$,i%,1%)                   ~
                          else demseq$=demseq$ & str(temp1$,i%,1%)
L51230:         next i%
                return
L51250:         errormsg$ = "Demand Types Must Be '1 - 8'"
                goto L51230

        REM *************************************************************~
            *                                                           *~
            * PRINT FINAL SUMMARY                                       *~
            *                                                           *~
            *************************************************************
        final_summary
            if control$(6) = "N" then return
            call "SHOSTAT" ("Printing Final Summary Report")
            pline% = 999%:page% = 0%
            close #50
            call "WORKOPN2" (#50, "INPUT", 0%, f2%(50))
            call "GETNAMES" addr (#50, file$, lib$, vol$)
            call "READFDR" addr (file$, lib$, vol$, 0%, "RC", rec%, u3%)
            rec% = max(1000%, (rec% / 2%))
            select printer (134)
            call "SETPRTSZ" addr(rec%)
            errormsg$ = "* * * * * END OF REPORT * * * * *"
L60100:     if pline% > 60% then gosub print_heading
                read #50, using L60180, printline$, eod goto L60200
                print printline$
                read #50, using L60180, printline$, eod goto L60200
                print printline$
                print
                pline%=pline%+3%
                goto L60100
L60180:              FMT CH(132)

L60200:         print errormsg$

                return

        print_it   /* PRINTS THE DEMAND, PRINTS HEADER WHEN NEEDED */
            if control$(6) = "N" then return
            call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4))
            if f1%(4) = 0 then partdescr$ = " "
            call "DATEFMT" (date$(1%))
            call "DATEFMT" (date$(2%))
            init (" ") printline$
        put printline$ using L60690, demcode$, demline$, type$, priority$,~
               part$, partdescr$, round(demquant,2), date$(1), date$(2)
            write #50, using L60350, printline$
            init (" ") printline$
        put printline$, using L60710, errormsg$
            write #50, using L60350, printline$

L60350:     FMT CH(132)
        return

        print_purge
            if control$(6) = "N" then return
            init ("*") printline$
            write #50, using L60350, printline$
            init (" ") printline$
            put printline$, using L60450, part$
            write #50, using L60350, printline$
L60450: %CLEARING ADVICES FOR PART ######################### FOR OPTIMIZA~
        ~TION
L60460: %                          ################################
            call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4))
            if f1%(4) = 0 then partdescr$ = " "
            init (" ") printline$
            put printline$, using L60460, partdescr$
            write #50, using L60350, printline$
            init ("*") printline$
            write #50, using L60350, printline$
            return

        print_heading
            if page% <> 0% then print using L60660
*          IF MOD(PAGE%,250%) <> 0% THEN 60535
*          CLOSE PRINTER:SELECT PRINTER (134)
*        REM OK TO CONTINUE, PRINT FILE WILL NOT OVERFLOW
            page% = page% +1%
            print page
            print using L60610, page%, date$
            print
            print using L60630
            print using L60660
            pline% = 5
        return

L60610: %PAGE :######                           PLANNING SYSTEM REGENERAT~
        ~ION STATUS REPORT                         RUN DATE : ########
L60630: %DEMAND          LINE  TYPE  PRIORITY  PART NUMBER               ~
        ~DESCRIPTION                          QUANTITY DUE DATE  PLANNED

L60660: %================ ===    =       =     ========================= ~
        ~================================  =========== ======== ========

L60690: %################ ###    #       #     ######################### ~
        ~ ################################ -########.## ######## ########
L60710: %################################################################~
        ~################

        REM PRINT FINAL SUMMARY INFORMATION
        time_summary

            prtdate$ = yymmdd$(today%)
            call "DATEFMT" (prtdate$)

            select printer(134)
            print page
            print using L61200, date$
            print skip (3)
            print using L61270, today%, prtdate$
            print skip (1)
            print using L61210, startdate$(1), starttime$(1)
            print using L61220, startdate$(2), starttime$(2)
            print using L61230, startdate$(3), starttime$(3)
            print using L61240, startdate$(4), starttime$(4)
            print using L61250, startdate$(5), starttime$(5)
            print using L61260, startdate$(6), starttime$(6)

            close printer
            return

L61200: % GLOBAL PLANNING RUN, ELAPSED TIME SUMMARY                 ########
L61210: % RESTORE STARTED                               ########    ########
L61220: % DEMAND PLANNING STARTED                       ########    ########
L61230: % MANUFACTURED PART SHORTAGE CLEARING STARTED   ########    ########
L61240: % PURCHASED PART SHORTAGE CLEARING STARTED      ########    ########
L61250: % FINAL CLEAN UP STARTED                        ########    ########
L61260: % PLANNING RUN FINISHED                         ########    ########
L61270: % PLANNING DATE FOR THIS RUN IS DAY ###  CALENDAR DAY ########

L62000: rem**************************************************************~
            *      modify planning flags                                *~
            *************************************************************

              err% = today%
                 call "PLNFLSUB" (planflags$(), err%)
                    get str(planflags$(),281) using L62110, lta%(),       ~
                                                        pprior$(),dts%()
L62110:         FMT 26*BI(4), 8*CH(1), 8*BI(4)

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

            call "FILEBGON" (#50)
            call "FILEBGON" (#62)
            call "FILEBGON" (#63)

            end
