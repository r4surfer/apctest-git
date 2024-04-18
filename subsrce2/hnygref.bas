        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y   GGG   RRRR   EEEEE  FFFFF          *~
            *  H   H  NN  N  Y   Y  G      R   R  E      F              *~
            *  HHHHH  N N N   YYY   G GGG  RRRR   EEEE   FFFF           *~
            *  H   H  N  NN    Y    G   G  R   R  E      F              *~
            *  H   H  N   N    Y     GGG   R   R  EEEEE  F              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYGREF  - CROSS REF TO GENERIC FILE IF POSSIBLE          *~
            *            ASK FOR OPERATOR INTERVENTION IF MORE THAN ONE *~
            *            IS FOUND                                       *~
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
            * 09/14/83 ! ORIGINAL                                 ! KEN *~
            * 09/15/90 ! Added Item qty on-hand/PIP/ATC/Shelf     ! SID *~
            *          ! display for each item on the list.       !     *~
            * 07/11/91 ! Added the Ability to see Qty Onhand by   ! SID *~
            *          !       Store and ATC-2                    !     *~
            * 05/22/92 ! Try to help out (major housecleaning)    ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        /* Temporary test code - out for test                           */
        sub "HNYGREF" (generic$, #1, #2, f1%)
        /* Temporary test code                                          */

                                 /* #1 -- HNYGENER  */
                                 /* #2 -- HNYMASTR  */

        dim                                                              ~
            cursor%(2),                                                  ~
            day1$6,                      /* PIP/ATC Base Date          */~
            date$8,                      /* PIP/ATC user input date    */~
            disp_atc1$(15)7,             /* ATC-1 for Screen Display   */~
            disp_atc2$(15)7,             /* ATC-2 for Screen Display   */~
            disp_date$8,                 /* System Date                */~
            disp_pip$(15)8,              /* PIP for Screen Display     */~
            disp_shelf$(15)8,            /* SHELF for Screen Display   */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* Error Message Variable     */~
            gendescr$30,                 /* GENERIC DESCRIPTION        */~
            generic$25,                  /* GENERIC PART DESIGNATION   */~
            header$79,                                                   ~
            i$(24)80,                                                    ~
            onhand(1),                   /* OnHand Qty PIPMASTR/HNYQUAN*/~
            onhand$(20)8,                /* OnHand Qty PIPMASTR/HNYQUAN*/~
            kfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            part$(20,2)32,                                               ~
            pip1%(490),                  /* Absolute PIP               */~
            pip2%(490),                  /* PIP adj for CUMF           */~
            atc1%(490),                  /* ATC type 1                 */~
            atc2%(490),                  /* ATC type 2                 */~
            cumf%(490),                  /* SFCUM2                     */~
            plowkey$60,                  /*                            */~
            prev_date$8,                 /* Temp PIP/ATC input date    */~
            prev_store$3,                /* Temp Store Number          */~
            readkey$60,                  /*                            */~
            store$3,                     /* Store Number               */~
            jdate$8,                     /* Date to jump to            */~
            uom$(20)4                    /* UOM from HNYMASTR          */

        dim f1%(64),                     /* Field Status Flags         */~
            f2%(64),                     /* Field Status Flags         */~
            rslt$(64)20                  /* Return code from "OPENFILE"*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************
            mat f2% = con  /* The Variables F2%() and AXD$() should not */
                           /* be modified.  They are an intrinsic part  */
                           /* of the file open subroutine.              */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYGENER ! Generic Part Xref File                   *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! SYSFILE2 ! Planning Calendar Base Date              *~
            * #05 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #07 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #08 ! STORNAME ! Store Master File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #4,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =   20

            select #5, "SFCUM2",                                         ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #7, "PIPMASTR",                                       ~
                        var, indexed, recsize = 2024,                    ~
                        keypos =    2, keylen =  25,                     ~
             alt key  1,keypos =    2, keylen =  26

            select #8,  "STORNAME",                                      ~
                        var, indexed, recsize = 300,                     ~
                        keypos =    1, keylen = 3

        /* Temporary test code - in for test
            SELECT #1, "HNYGENER",                                       ~
                        VAR, INDEXED, RECSIZE = 100,                     ~
                        KEYPOS =   17, KEYLEN =  25,                     ~
             ALT KEY 1, KEYPOS =    1, KEYLEN =  41

            SELECT #2,  "HNYMASTR",                                      ~
                        VAR, INDEXED, RECSIZE = 900,                     ~
                        KEYPOS =    1, KEYLEN = 25
        /* Temporary test code                                          */

            if beenhere% = 1% then L09000

            call "OPENCHCK" (#4, fs%, f2%(4), 0%, rslt$(4))
            call "OPENCHCK" (#5, fs%, f2%(5), 0%, rslt$(5))
            call "OPENCHCK" (#7, fs%, f2%(7), 0%, rslt$(7))
            call "OPENCHCK" (#8, fs%, f2%(8), 0%, rslt$(8))

        /* Temporary test code - in for test
            CALL "OPENCHCK" (#1, FS%, F2%(1), 0%, RSLT$(1))
            CALL "OPENCHCK" (#2, FS%, F2%(2), 0%, RSLT$(2))
        /* Temporary test code                                          */

        /* LOAD CALENDAR */
            f1% = 0%
            call "READ100" (#4, "MONTHS OPEN         ", f1%)
            if f1% = 0 then exit_program
                get #4, using L05810 , day1$
L05810:              FMT XX(32), CH(6)
            close #4

            call "DATE" addr("G-", day1$, date$, index%, ret%)
            index% = index% + 1
            if ret% = 0  and  (index% < 1 or index% > 490) then ret% = 2
            index% = min(490, max(1, index%))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

        /* Temporary test code - in for test
        TESTINPUT
            PLOWKEY$ = HEX(06) & "LAST SLCT:" & GENERIC$
            GENERIC$ = " "
            CALL "PLOWCODE" (#1, STR(GENERIC$,,16), PLOWKEY$, -16%,      ~
                                                           -1.30, F1%(1))
                IF F1%(1) = 0% THEN EXIT_PROGRAM
        /* Temporary test code                                          */

            beenhere% = 1%

            edtmessage$="To select a part, position cursor & hit RETURN"
            date$ = date      : call "DATEFMT" (date$)
            disp_date$ = date : call "DATEFMT" (disp_date$)
            store$, prev_store$ = "ALL"
            prev_date$ = date$
            mode% = 1%

            init (hex(00)) readkey$, plowkey$
            str(readkey$,1,16)= generic$

            call "PLOWALTS" (#1, readkey$, 1%, 16%, f1%)
            if f1% = 0% then endnone
            get #1, using L09300, gendescr$
L09300:         FMT XX(41), CH(30)

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L10060:     gosub startarray
L10070:     if max% > 1% then L10120
               if max% = 0% then endnone
                  fieldnr% = 1%
                  goto setend

L10120: REM * Screen to Display Part Number and On-Hand *
L10150:     gosub part_disp
            errormsg$ = " "

        /* Temporary test code - in for test
                IF KEYHIT%  =  1 THEN GOSUB TESTINPUT
        /* Temporary test code                                          */

                if keyhit% <>  8 then       L10310
                   if store$ = prev_store$ then L10120
                   if store$  = "ALL" then L10260
                   if store$ <> "###" then L10280
L10260:               gosub store_specific_qty
                      goto L10120
L10280:            call "GETCODE" (#8, store$, " ", 0%, 0, f1%(8))
                      if f1%(8) = 0% then L10310
                         goto L10260
L10310:            store$ = prev_store$
                if date$  = prev_date$ then L10400
                   call "DATEOK" (date$, date%, errormsg$)
                      if errormsg$ = " " then L10390
                   if keyhit% = 9% then L10150  /* Error Message */
                      date$  = prev_date$
                      errormsg$ = " "
                         goto L10430            /* Not PF9       */
L10390:               prev_date$ = date$
L10400:         if keyhit%  =  9 then       L11000
L10430:         if keyhit%  = 10 then mode% = 1% + mod(mode%, 2%)
                if keyhit%  = 16 then       endnone
                if keyhit%  =  2 then       L10060
                if keyhit%  <> 5 then       L10500
                   gosub continuearray
                   goto L10070
L10500:         if keyhit% <>  0 then       L10120

            fieldnr% = cursor%(1) - 4
            if fieldnr% < 1 or fieldnr% >  max% then L10120

        setend
            generic$ = part$(fieldnr%, 1%)
            f1% = 1%
            goto exit_program

        endnone
            f1% = 0%
            goto exit_program

L11000: REM *************************************************************~
            * Main Driver for display Parts'  PIP                       *~
            *************************************************************

L11040:     gosub load_pip_atc
L11050:       gosub display_part_pip
                  errormsg$ = " "
                  if keyhit% <>  9 then goto  L11140
                     if date$ = prev_date$ then L11150
                           call "DATEOK" (date$, date%, errormsg$)
                              if errormsg$ <> " " then L11050
                           reload_pip% = 1%
                           prev_date$ = date$
                           goto L11040
L11140:           date$ = prev_date$
L11150:           if keyhit%  = 10 then mode% = 1% + mod(mode%, 2%)
                  if keyhit%  = 11 then L10120
                  if keyhit%  = 16 then goto  endnone
                  if keyhit%  <> 2 then       L11210
                           gosub startarray
                           goto L11040
L11210:           if keyhit%  <> 5 then       L11240
                           gosub continuearray
                           goto  L11040
L11240:           if keyhit% <>  6 then goto  L11280
                           next% = -1%
                           gosub cal_jump_date
                           goto L11040
L11280:           if keyhit% <>  7 then goto L11320
                           next% = 1%
                           gosub cal_jump_date
                           goto L11040
L11320:           if keyhit% <>  0 then goto  L11050 /* Back to Display */

              fieldnr% = cursor%(1) - 4
              if fieldnr% < 1 or fieldnr% > max% then L11050
              goto setend

        cal_jump_date
            if next% = 0% then return
               call "DATUNFMT" (date$)
                  call "DATE" addr("G+", date$, next%, jdate$, return%)
                     if return% <> 0% then L11870
                  date$ = str(jdate$,,6)
                  reload_pip% = 1%
L11870:        call "DATEFMT" (date$)
               prev_date$ = date$
               next% = 0%
            return

        REM *************************************************************~
            *  Load Array for Display                                   *~
            *************************************************************

        startarray
            init (hex(00)) readkey$
            str(readkey$,1,16) = generic$
            max%=0%

        continuearray
            call "PLOWALTS" (#1, readkey$, 1%, 16%, f1%(1))
               if f1%(1) = 0 then return
            init (" ") part$(), onhand$(), uom$()
            max%=0% : reload_pip% = 1%

        loadarray
            max%=max%+1
            get #1, using L30180, part$(max%, 1%)
L30180:         FMT XX(16), CH(25)

        REM * Getting Stocking UOM for a particular Part *
            str(plowkey$,1,25) = part$(max%, 1%)
            call "READ100" (#2, plowkey$, f1%(2))
               if f1%(2) = 0% then L30270
            get #2 using L30250, part$(max%, 2%), uom$(max%)
L30250:         FMT POS(26), CH(32), POS(74), CH(4)

L30270:     if max% >= 15% then store_specific_qty
            call "PLOWALTS" (#1, readkey$, 1%, 16%, f1%(1))
               if f1%(1) <> 0% then loadarray

        store_specific_qty
            usepip% = 0%
            if store$ <> "ALL" then L30550
               store$  = "   "
               sw%     = 1%
               usepip% = 1%
               goto L30610
L30550:     if store$ <> "###" then L30590
               store$  = "   "
               sw%     = 0%
               goto L30610
L30590:     sw% = 1%
            if pos("0123456789" = str(store$,,1)) = 0% then sw% = 0%
L30610:     for i% = 1% to max%
               if usepip% = 0% then L30620
                  call "READ100" (#7, part$(i%, 1%), f1%(7))
                     if f1%(7) = 0% then L30620
                  get #7 using L30616, onhand(1)
L30616:               FMT POS(1987), PD(14,4)
                  goto L30630
L30620:        call "HNYTOTSB" (part$(i%, 1%), store$, " ", onhand(), sw%)
L30630:        convert onhand(1) to onhand$(i%), pic(-#######)
            next i%
            if store$ <> "   " then L30680
               if sw% = 1% then store$ = "ALL"
               if sw% = 0% then store$ = "###"
L30680:     prev_store$ = store$
            return

        REM *************************************************************~
            *      LOAD PIP DATA                                        *~
            *************************************************************

        load_pip_atc
            if reload_pip% = 0% then return

            jdate$ = date$ : call "DATUNFMT" (jdate$)
            call "DATE" addr("G-", day1$, jdate$, f%, ret%)
            if ret% <> 0% then L31120
               f% = f% + 1%
               if f% > 0% and f% < 491% then L31140
L31120:           errormsg$ = "Date not within Planning Calendar"
                  return

L31140:     disp_pip$(), disp_shelf$(), disp_atc1$(), disp_atc2$() = " "
            call "SHOSTAT" ("Acquiring PIP Data, One Moment... ")

            for j% = 1% to max%

            call "READ100" (#7, part$(j%, 1%), f1%(7))
               if f1%(7) = 0% then continue_loop

*          GET #7, USING 31240, PIP1%()
*              FMT POS(27), 490*BI(4)
            call "MXFL4GT" addr(#7, 26%, pip1%(1%), 490%)

            atc1%(490%) = pip1%(490%)
*          FOR KKKK% = 489% TO 1% STEP -1%
*              BASE% = MAX(KKKK% + 1%, TODAY%)
*              ATC1%(KKKK%) = MIN(PIP1%(KKKK%), ATC1%(BASE%))
*          NEXT KKKK%
            call "MXATCSB" addr(atc1%(1%), pip1%(1), atc1%(1%),          ~
                                489%, 1%, today%)

            call "READ100" (#5, part$(j%, 1%), f1%(5))
               if f1%(5) <> 0% then L31400
            mat pip2% = pip1% : mat atc2% = atc1% : goto L31570

L31400
*          GET #5, USING 31410  , CUMF%()    /* CUM SLS FCST ARRAY  */
*              FMT POS(26), 490*BI(4)
            call "MXFL4GT" addr(#5, 25%, cumf%(1%), 490%)

*          FOR KKKK% = 1% TO 490%
*              PIP2%(KKKK%) = PIP1%(KKKK%) - MAX(0%, CUMF%(KKKK%))
*          NEXT KKKK%
            call "MXSFASB" addr(pip2%(1%), pip1%(1), cumf%(1%),          ~
                                1%, 490%, 0%)

            atc2%(490%) = pip2%(490%)
*          FOR KKKK% = 489% TO 1% STEP -1%
*              BASE% = MAX(KKKK% + 1%, TODAY%)
*              ATC2%(KKKK%) = MIN(PIP2%(KKKK%), ATC2%(BASE%))
*          NEXT KKKK%
            call "MXATCSB" addr(atc2%(1%), pip2%(1), atc2%(1%),          ~
                                489%, 1%, today%)

L31570:    convert pip1% (f%) to disp_pip$   (j%), pic(-######)
           convert pip2% (f%) to disp_shelf$ (j%), pic(-######)
           convert atc1% (f%) to disp_atc1$  (j%), pic(-######)
           convert atc2% (f%) to disp_atc2$  (j%), pic(-######)

        continue_loop:
            next j%

            reload_pip% = 0%
            return

        REM *************************************************************~
            *  Part Number/On-Hand/UOM                                  *~
            *************************************************************

        part_disp

            if mode% = 1% then header$="Part Code"                       ~
                          else header$="Part Description"
            str(header$,40) = "On-Hand Today                        UOM"
            if store$ <> "ALL" then L40055
               str(header$,54,21) = "(All Numeric Stores) " : goto L40080
L40055:     if store$ <> "###" then L40070
               str(header$,54,21) = "(All Stores)         " : goto L40080

L40070:     str(header$,54,21) = "(For Store: "& store$ & ")"

L40080:     init(hex(84)) lfac$(), kfac$()
            init(hex(86)) str(kfac$(),1,max%)

L40095:     accept                                                       ~
               at (01,02), "Generic Parts List"                 ,        ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), disp_date$             , ch(08),~
               at (02,02), "Generic Part:"                      ,        ~
               at (02,16), fac(hex(84)), generic$               , ch(16),~
               at (02,33), fac(hex(8c)), gendescr$              , ch(30),~
               at (02,64), "HNYGREF:"                           ,        ~
               at (02,73), fac(hex(8c)), str(cms2v$,,8)         , ch(08),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (05,02), fac(kfac$( 1)), part$( 1%, mode%)    , ch(32),~
               at (06,02), fac(kfac$( 2)), part$( 2%, mode%)    , ch(32),~
               at (07,02), fac(kfac$( 3)), part$( 3%, mode%)    , ch(32),~
               at (08,02), fac(kfac$( 4)), part$( 4%, mode%)    , ch(32),~
               at (09,02), fac(kfac$( 5)), part$( 5%, mode%)    , ch(32),~
               at (10,02), fac(kfac$( 6)), part$( 6%, mode%)    , ch(32),~
               at (11,02), fac(kfac$( 7)), part$( 7%, mode%)    , ch(32),~
               at (12,02), fac(kfac$( 8)), part$( 8%, mode%)    , ch(32),~
               at (13,02), fac(kfac$( 9)), part$( 9%, mode%)    , ch(32),~
               at (14,02), fac(kfac$(10)), part$(10%, mode%)    , ch(32),~
               at (15,02), fac(kfac$(11)), part$(11%, mode%)    , ch(32),~
               at (16,02), fac(kfac$(12)), part$(12%, mode%)    , ch(32),~
               at (17,02), fac(kfac$(13)), part$(13%, mode%)    , ch(32),~
               at (18,02), fac(kfac$(14)), part$(14%, mode%)    , ch(32),~
               at (19,02), fac(kfac$(15)), part$(15%, mode%)    , ch(32),~
                                                                         ~
               at (05,46), fac(lfac$( 1)), onhand$( 1)          , ch(08),~
               at (06,46), fac(lfac$( 2)), onhand$( 2)          , ch(08),~
               at (07,46), fac(lfac$( 3)), onhand$( 3)          , ch(08),~
               at (08,46), fac(lfac$( 4)), onhand$( 4)          , ch(08),~
               at (09,46), fac(lfac$( 5)), onhand$( 5)          , ch(08),~
               at (10,46), fac(lfac$( 6)), onhand$( 6)          , ch(08),~
               at (11,46), fac(lfac$( 7)), onhand$( 7)          , ch(08),~
               at (12,46), fac(lfac$( 8)), onhand$( 8)          , ch(08),~
               at (13,46), fac(lfac$( 9)), onhand$( 9)          , ch(08),~
               at (14,46), fac(lfac$(10)), onhand$(10)          , ch(08),~
               at (15,46), fac(lfac$(11)), onhand$(11)          , ch(08),~
               at (16,46), fac(lfac$(12)), onhand$(12)          , ch(08),~
               at (17,46), fac(lfac$(13)), onhand$(13)          , ch(08),~
               at (18,46), fac(lfac$(14)), onhand$(14)          , ch(08),~
               at (19,46), fac(lfac$(15)), onhand$(15)          , ch(08),~
                                                                         ~
               at (05,77), fac(lfac$( 1)), uom$( 1)             , ch(04),~
               at (06,77), fac(lfac$( 2)), uom$( 2)             , ch(04),~
               at (07,77), fac(lfac$( 3)), uom$( 3)             , ch(04),~
               at (08,77), fac(lfac$( 4)), uom$( 4)             , ch(04),~
               at (09,77), fac(lfac$( 5)), uom$( 5)             , ch(04),~
               at (10,77), fac(lfac$( 6)), uom$( 6)             , ch(04),~
               at (11,77), fac(lfac$( 7)), uom$( 7)             , ch(04),~
               at (12,77), fac(lfac$( 8)), uom$( 8)             , ch(04),~
               at (13,77), fac(lfac$( 9)), uom$( 9)             , ch(04),~
               at (14,77), fac(lfac$(10)), uom$(10)             , ch(04),~
               at (15,77), fac(lfac$(11)), uom$(11)             , ch(04),~
               at (16,77), fac(lfac$(12)), uom$(12)             , ch(04),~
               at (17,77), fac(lfac$(13)), uom$(13)             , ch(04),~
               at (18,77), fac(lfac$(14)), uom$(14)             , ch(04),~
               at (19,77), fac(lfac$(15)), uom$(15)             , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,40), "(10)Part Description"               ,        ~
               at (22,62), "(13)Instructions"                   ,        ~
               at (23,02), "(2)First Page"                      ,        ~
               at (23,20), "(8)Quantity for Store:"             ,        ~
               at (23,43), fac(hex(81)),store$                  , ch(03),~
               at (23,62), "(15)Print Screen"                   ,        ~
               at (24,02), "(5)Next Page"                       ,        ~
               at (24,20), "(9)PIP/ATC for Date:"               ,        ~
               at (24,43), fac(hex(80)),date$                   , ch(08),~
               at (24,62), "(16)Exit, No Select"                ,        ~
                                                                         ~
               keys(hex(0001020508090a100d0f))                  ,        ~
               key (keyhit%)

               if keyhit% <> 13 then L40490
                  call "MANUAL" ("HNYGREF")
                  goto L40095

L40490:        if keyhit% <> 15 then L40510
                  call "PRNTSCRN"
                  goto L40095

L40510:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            * Displaying Parts' PIP                                     *~
            *************************************************************

        display_part_pip
            if mode% = 1% then header$="Part Code"                       ~
                          else header$="Part Description"
            str(header$,43) = "PIP     SHELF        ATC-1      ATC-2"
            init(hex(84)) lfac$(), kfac$()
            init(hex(86)) str(kfac$(),1,max%)

L41055:     accept                                                       ~
               at (01,02), "Generic Parts List"                 ,        ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), disp_date$             , ch(08),~
               at (02,02), "Generic Part:"                      ,        ~
               at (02,16), fac(hex(84)), generic$               , ch(16),~
               at (02,33), fac(hex(8c)), gendescr$              , ch(30),~
               at (02,64), "HNYGREF:"                           ,        ~
               at (02,73), fac(hex(8c)), str(cms2v$,,8)         , ch(08),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (05,02), fac(kfac$( 1)), part$( 1%, mode%)    , ch(32),~
               at (06,02), fac(kfac$( 2)), part$( 2%, mode%)    , ch(32),~
               at (07,02), fac(kfac$( 3)), part$( 3%, mode%)    , ch(32),~
               at (08,02), fac(kfac$( 4)), part$( 4%, mode%)    , ch(32),~
               at (09,02), fac(kfac$( 5)), part$( 5%, mode%)    , ch(32),~
               at (10,02), fac(kfac$( 6)), part$( 6%, mode%)    , ch(32),~
               at (11,02), fac(kfac$( 7)), part$( 7%, mode%)    , ch(32),~
               at (12,02), fac(kfac$( 8)), part$( 8%, mode%)    , ch(32),~
               at (13,02), fac(kfac$( 9)), part$( 9%, mode%)    , ch(32),~
               at (14,02), fac(kfac$(10)), part$(10%, mode%)    , ch(32),~
               at (15,02), fac(kfac$(11)), part$(11%, mode%)    , ch(32),~
               at (16,02), fac(kfac$(12)), part$(12%, mode%)    , ch(32),~
               at (17,02), fac(kfac$(13)), part$(13%, mode%)    , ch(32),~
               at (18,02), fac(kfac$(14)), part$(14%, mode%)    , ch(32),~
               at (19,02), fac(kfac$(15)), part$(15%, mode%)    , ch(32),~
                                                                         ~
               at (05,40), fac(lfac$( 1)), disp_pip$(1)         , ch(08),~
               at (06,40), fac(lfac$( 2)), disp_pip$(2)         , ch(08),~
               at (07,40), fac(lfac$( 3)), disp_pip$(3)         , ch(08),~
               at (08,40), fac(lfac$( 4)), disp_pip$(4)         , ch(08),~
               at (09,40), fac(lfac$( 5)), disp_pip$(5)         , ch(08),~
               at (10,40), fac(lfac$( 6)), disp_pip$(6)         , ch(08),~
               at (11,40), fac(lfac$( 7)), disp_pip$(7)         , ch(08),~
               at (12,40), fac(lfac$( 8)), disp_pip$(8)         , ch(08),~
               at (13,40), fac(lfac$( 9)), disp_pip$(9)         , ch(08),~
               at (14,40), fac(lfac$(10)), disp_pip$(10)        , ch(08),~
               at (15,40), fac(lfac$(11)), disp_pip$(11)        , ch(08),~
               at (16,40), fac(lfac$(12)), disp_pip$(12)        , ch(08),~
               at (17,40), fac(lfac$(13)), disp_pip$(13)        , ch(08),~
               at (18,40), fac(lfac$(14)), disp_pip$(14)        , ch(08),~
               at (19,40), fac(lfac$(15)), disp_pip$(15)        , ch(08),~
                                                                         ~
               at (05,50), fac(lfac$( 1)), disp_shelf$(1)       , ch(08),~
               at (06,50), fac(lfac$( 2)), disp_shelf$(2)       , ch(08),~
               at (07,50), fac(lfac$( 3)), disp_shelf$(3)       , ch(08),~
               at (08,50), fac(lfac$( 4)), disp_shelf$(4)       , ch(08),~
               at (09,50), fac(lfac$( 5)), disp_shelf$(5)       , ch(08),~
               at (10,50), fac(lfac$( 6)), disp_shelf$(6)       , ch(08),~
               at (11,50), fac(lfac$( 7)), disp_shelf$(7)       , ch(08),~
               at (12,50), fac(lfac$( 8)), disp_shelf$(8)       , ch(08),~
               at (13,50), fac(lfac$( 9)), disp_shelf$(9)       , ch(08),~
               at (14,50), fac(lfac$(10)), disp_shelf$(10)      , ch(08),~
               at (15,50), fac(lfac$(11)), disp_shelf$(11)      , ch(08),~
               at (16,50), fac(lfac$(12)), disp_shelf$(12)      , ch(08),~
               at (17,50), fac(lfac$(13)), disp_shelf$(13)      , ch(08),~
               at (18,50), fac(lfac$(14)), disp_shelf$(14)      , ch(08),~
               at (19,50), fac(lfac$(15)), disp_shelf$(15)      , ch(08),~
                                                                         ~
               at (05,63), fac(lfac$( 1)), disp_atc1$(1)        , ch(07),~
               at (06,63), fac(lfac$( 2)), disp_atc1$(2)        , ch(07),~
               at (07,63), fac(lfac$( 3)), disp_atc1$(3)        , ch(07),~
               at (08,63), fac(lfac$( 4)), disp_atc1$(4)        , ch(07),~
               at (09,63), fac(lfac$( 5)), disp_atc1$(5)        , ch(07),~
               at (10,63), fac(lfac$( 6)), disp_atc1$(6)        , ch(07),~
               at (11,63), fac(lfac$( 7)), disp_atc1$(7)        , ch(07),~
               at (12,63), fac(lfac$( 8)), disp_atc1$(8)        , ch(07),~
               at (13,63), fac(lfac$( 9)), disp_atc1$(9)        , ch(07),~
               at (14,63), fac(lfac$(10)), disp_atc1$(10)       , ch(07),~
               at (15,63), fac(lfac$(11)), disp_atc1$(11)       , ch(07),~
               at (16,63), fac(lfac$(12)), disp_atc1$(12)       , ch(07),~
               at (17,63), fac(lfac$(13)), disp_atc1$(13)       , ch(07),~
               at (18,63), fac(lfac$(14)), disp_atc1$(14)       , ch(07),~
               at (19,63), fac(lfac$(15)), disp_atc1$(15)       , ch(07),~
                                                                         ~
               at (05,74), fac(lfac$( 1)), disp_atc2$( 1)       , ch(07),~
               at (06,74), fac(lfac$( 2)), disp_atc2$( 2)       , ch(07),~
               at (07,74), fac(lfac$( 3)), disp_atc2$( 3)       , ch(07),~
               at (08,74), fac(lfac$( 4)), disp_atc2$( 4)       , ch(07),~
               at (09,74), fac(lfac$( 5)), disp_atc2$( 5)       , ch(07),~
               at (10,74), fac(lfac$( 6)), disp_atc2$( 6)       , ch(07),~
               at (11,74), fac(lfac$( 7)), disp_atc2$( 7)       , ch(07),~
               at (12,74), fac(lfac$( 8)), disp_atc2$( 8)       , ch(07),~
               at (13,74), fac(lfac$( 9)), disp_atc2$( 9)       , ch(07),~
               at (14,74), fac(lfac$(10)), disp_atc2$(10)       , ch(07),~
               at (15,74), fac(lfac$(11)), disp_atc2$(11)       , ch(07),~
               at (16,74), fac(lfac$(12)), disp_atc2$(12)       , ch(07),~
               at (17,74), fac(lfac$(13)), disp_atc2$(13)       , ch(07),~
               at (18,74), fac(lfac$(14)), disp_atc2$(14)       , ch(07),~
               at (19,74), fac(lfac$(15)), disp_atc2$(15)       , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,20), "(6)Prev Day"                        ,        ~
               at (22,40), "(10)Part Description"               ,        ~
               at (22,62), "(13)Instructions"                   ,        ~
               at (23,02), "(2)First Page"                      ,        ~
               at (23,20), "(7)Next Day"                        ,        ~
               at (23,40), "(11)On-Hand"                        ,        ~
               at (23,62), "(15)Print Screen"                   ,        ~
               at (24,02), "(5)Next Page"                       ,        ~
               at (24,20), "(9)PIP/ATC for Date:"               ,        ~
               at (24,43), fac(hex(80)),date$                   , ch(08),~
               at (24,62), "(16)Exit, No Select"                ,        ~
                                                                         ~
               keys(hex(0002050607090a0b0d0f10))                ,        ~
               key (keyhit%)

               if keyhit% <> 13 then L41615
                  call "MANUAL" ("HNYGREF")
                  goto L41055

L41615:        if keyhit% <> 15 then L41635
                  call "PRNTSCRN"
                  goto L41055

L41635:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

        /* Temporary test code - in for test
           STOP GENERIC$
            END F1%
        /* Temporary test code                                          */

            end
