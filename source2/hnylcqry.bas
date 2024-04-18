        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       CCC    QQQ   RRRR   Y   Y   *~
            *  H   H  NN  N  Y   Y  L      C   C  Q   Q  R   R   Y Y    *~
            *  HHHHH  N N N   YYY   L      C      Q   Q  RRR      Y     *~
            *  H   H  N  NN    Y    L      C   C  Q Q Q  R  R     Y     *~
            *  H   H  N   N    Y    LLLLL   CCC    QQQ   R   R    Y     *~
            *                                         Q                 *~
            *-----------------------------------------------------------*~
            * HNYLCQRY - Cloned from HNYLOCIN.  Provides Inquiry access *~
            *            to HNYLOCNS file by primary or 4 alternate key *~
            *            keys.                                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/26/85 ! ORIGINAL                                 ! LDJ *~
            * 06/10/86 ! Fixed Bug - Potential FS22 on Data Save. ! LDJ *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/13/87 ! Std Cost Changes (files HNYMASTR/QUAN).  ! ERN *~
            * 02/02/90 ! Cloned from HNYLOCIN, renamed HNYLCQRY,  ! MLJ *~
            *          !  contains inquiry functionality only,    !     *~
            *          !  all file update capabilities removed.   !     *~
            * 03/09/90 ! Increased LOCATION from 200 to 400 for   ! MLJ *~
            *          !  the addition of Variable Fields.        !     *~
            * 07/17/91 ! Re-Worked Caelus Standard Update,        ! SID *~
            *          !  Defaulted 'ALL' for Location/Lot, Added !     *~
            *          !  PF4) Previous, Changed GETCODE to       !     *~
            *          !  PLOWCODE on Location Field, and Avoid   !     *~
            *          !  QTY$ and LOT_QTY$ from getting reset to !     *~
            *          !  'NOT FOUND' when PF8 to PF12 is Pressed.!     *~
            * 08/26/91 ! Fixed PF9/PF12 so that HNYLCINQ would    ! SID *~
            *          !  Plow the Alt Key at the right Starting  !     *~
            *          !  Point.                                  !     *~
            * 12/02/92 ! PRR 12694 Try again to get the PLOWKEY   ! RJH *~
            *          !  right for Location of 'ALL'.            !     *~
            *          ! PRR 12693 Display the inventory quantity !     *~
            *          !  even if the location does not match the !     *~
            *          !  default location.                       !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankline$79,                /* Blank line for sceen dsply */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            error$79,                    /* Error message for NUMTEST  */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            iflag$1,                     /* HNYQUAN -'Not Found' Flag  */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lflag$1,                     /* HNYLOCNS -'Not Found' Flag */~
            literal1$28,                 /* HNYLOCNS Literal - display */~
            literal2$28,                 /* HNYQUAN  Literal - display */~
            literal4$17,                 /* Options Literal - display  */~
            loc$8,                       /* Location Of Part           */~
            locdescr$79,                 /* Location Description       */~
            lot$6,                       /* Lot (Optional)             */~
            lot_qty$10,                  /* Quantity On Hand in Lot    */~
            options$(6)40,               /* Inquiry Opt Literals       */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Code                  */~
            pf4$11,                      /* PF Key Literal             */~
            plowkey$100,                 /* Miscellaneous Read Key     */~
            prev_qty$10,                 /* Temp Qty This Location(Opt)*/~
            prev_lot_qty$10,             /* Temp Qty On Hand in Lot    */~
            readkey$50,                  /* Miscellaneous Read Key     */~
            qty$10,                      /* Quantity This Location(Opt)*/~
            store$3,                     /* Warehouse (Store) Code     */~
            storedescr$30,               /* Warehouse (Store) Code     */~
            temp_loc$8,                  /* Dummy Loc                  */~
            temp_lot$6,                  /* Dummy Lot                  */~
            userid$3                     /* Current User Id            */~

        dim f2%(05),                     /* = 0 if the file is open    */~
            f1%(05),                     /* = 1 if READ was successful */~
            fs%(05),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(05)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see OPENCHCK*/
                     /* The variable AXD$() is not used but required.  */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #3  ! STORNAME ! Store Info File - Name/Address           *~
            * #4  ! HNYLOCNS ! Stock Location Detail File               *~
            * #5  ! LOCATION ! Location File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #2,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #3,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #4,  "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #5,  "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =    4, keylen =  11

            call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(1), rslt$(2), rslt$(3) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            if f2%(1) + f2%(2) + f2%(3) > 0% then L65000
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 300%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 300%, rslt$(5 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            blankline$ = " "
            str(blankline$,62%) = "HNYLCQRY: " & str(cms2v$,,8%)

            edit% = 0%
            edtmessage$  = "To Modify Values, Position Cursor to Desired ~
        ~Value and Press RETURN."

            literal1$ = "Inventory Quantity File  =  "
            literal2$ = "Location  Quantity File  =  "

            literal4$ = "Inquiry Options: "
            options$(1) = "PF  8 - View ALL By Store/Loc/Part/Lot  "
            options$(2) = "PF  9 - View ALL By Store/Part/Lot/Loc  "
            options$(3) = "PF 10 - View ALL By Store/Part/Loc/Lot  "
            options$(4) = "PF 11 - View ALL By Part/Store/Loc/Lot  "
            options$(5) = "PF 12 - View ALL By Store/Lot/Part/Loc  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            edit% = 0%
            init(" ") errormsg$, loc$, lot_qty$, lot$, qty$, partdescr$, ~
                      storedescr$, pf4$
            inpmessage$ = "Enter Part location informations and press RET~
        ~urn."

            for fieldnr% = 1% to 4%
L10140:         gosub'051(fieldnr%)
                      if enabled% =  0    then       L10220
L10160:         gosub'101(fieldnr%)
                      if keyhit%  =  1    then gosub startover
                      if keyhit% <> 4%    then       L10180
                         fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then pf4$ = " "
                         goto L10140
L10180:               if keyhit%  = 16    then       exit_program
                      if keyhit% <>  0    then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then       L10160
L10220:     next fieldnr%
            gosub quantity_calculation


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            edit% = 1%
            inpmessage$ = edtmessage$

            gosub'101(0%)
                  if keyhit%  =  1    then gosub startover
                  if keyhit%  = 16    then       exit_program
                  if keyhit% <>  0    then       L11240
               fieldnr% = cursor%(1) - 5
               if fieldnr% < 1% or fieldnr% > 4% then editmode
            gosub'051(fieldnr%)
L11170:     gosub'101(fieldnr%)
                  if keyhit%  =  1    then gosub startover
                  if keyhit%  = 16    then       exit_program
                  if keyhit% <>  0    then       L11240
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then       L11170
                  gosub quantity_calculation
                  if keyhit% = 0%     then       editmode
L11240:           if keyhit% < 8 or keyhit% > 12 then L11170
                  gosub inquiry_routine
            goto editmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   S C R E E N  1  *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLE fields for the Screen 1 Input.   *~
            *************************************************************

            deffn'051(fieldnr%)
              enabled% = 0%
              errormsg$, inpmessage$ = " "

              on fieldnr% gosub L20160,             /* Warehouse (Store)*/~
                                L20220,             /* Part Code        */~
                                L20280,             /* Location Code    */~
                                L20340              /* Lot (Optional)   */
              return

L20160:   REM Default/Enable for Warehouse (Store) Code...
                enabled% = 1%
                inpmessage$ = "Enter Store/Warehouse Code or Press "     ~
                            & "<RETURN> to Select from the File."
                return

L20220:   REM Default/Enable for Part Code...
                enabled% = 1%
                inpmessage$ = "Enter Part Code or Press <RETURN> to "    ~
                            & "Select from the File."
                return

L20280:   REM Default/Enable for Part Location Code...
                enabled% = 1%
                if loc$ = " " then loc$ = "ALL"
                inpmessage$ = "Enter Location Code, 'ALL' or Press "     ~
                            & "<RETURN> to Select from the File."
                return

L20340:   REM Default/Enable for Part Lot Code...
                enabled% = 1%
                if lot$ = " " then lot$ = "ALL"
                inpmessage$ = "Enter Lot Code, 'ALL' or or Press "       ~
                            & "<RETURN> to Select from the File."
                return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            k% = 0%
            call "STARTOVR" (k%)
            if k% = 1% then return
            return clear all
            store$, part$ = " "
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Load Inventory Quantities from HNYLOCNS and HNYQUAN.      *~
            *************************************************************

        quantity_calculation
            totqty, totlotqty, qty, lot_qty = 0
            qty$, lot_qty$ = " "
            iflag$, lflag$ = "0"

          REM Get HNYLOCNS qty's based on specific user entry...
               plowkey$ = str(store$) & str(part$) & hex(00)
L30120:        call "PLOWALTS" (#4, plowkey$, 1%, 28%, f1%(4))
               if f1%(4) <> 1% then L30250
                  get #4 using L30150, work_lot$, work_loc$, qty
L30150:              FMT POS(471), CH(6), CH(8), POS(573), PD(14,4)
                  if loc$ = "ALL" and lot$ = "ALL" then L30205
                     if loc$ = "ALL" then L30190
                        if work_loc$ <> loc$ then L30120
L30190:        if lot$ = "ALL" then L30205
                  if work_lot$ <> lot$ then L30120
L30205:        lflag$ = "1"
               qty = round (qty, 2)
               totqty = totqty + qty
               goto L30120

L30250:   REM Get HNYQUAN qty's based on specific user entry...
               plowkey$ = str(part$)& str(store$) & hex(00)
L30270:        call "PLOWALTS" (#2, plowkey$, 0%, 28%, f1%(2))
               if f1%(2) <> 1% then L30400
                  get #2 using L30300, work_lot$, work_loc$, lot_qty
L30300:              FMT POS(45), CH(16), CH(8), PD(14,4)
                  if loc$ = "ALL" and lot$ = "ALL" then L30355
*                   IF LOC$ = "ALL" THEN 30340
*                      IF WORK_LOC$ <> LOC$ THEN 30270
               if lot$ = "ALL" then L30355
                  if work_lot$ <> lot$ then L30270
L30355:        iflag$ = "1"
               lot_qty = round(lot_qty, 2)
               totlotqty = totlotqty + lot_qty
               goto L30270

L30400:   REM Convert qty's for display...
               if lflag$ = "1" then L30410
                  qty$ = " Not Found"
                  goto L30412
L30410:        call "CONVERT" (totqty, 2.2, qty$)
L30412:        if iflag$ = "1" then L30420
                  lot_qty$ = " Not Found"
                  goto L30430
L30420:        call "CONVERT" (totlotqty, 2.2, lot_qty$)
L30430:        return

        inquiry_routine
          prev_qty$ = qty$ : prev_lot_qty$ = lot_qty$
          prev_qty  = prev_qty  : prev_lot_qty = lot_qty
          totqty, totlotqty, qty, lot_qty = 0
          qty$, lot_qty$ = " "
          iflag$, lflag$ = "0"
          temp_loc$ = loc$  :  temp_lot$ = lot$
          if str(loc$) = "ALL" then temp_loc$ = all(hex(00))
          if str(lot$) = "ALL" then temp_lot$ = all(hex(00))

          REM Primary Key...
            if keyhit% <> 8% then L35120
               plowkey$ = str(store$) & str(temp_loc$) &  str(part$)     ~
                          & hex(00)
               error$ = "INQUIRY BY STORE/LOC/PART/LOT"
               call "HNYLCINQ" (#4, plowkey$, error$, 0%, 0, f1%(4))
               goto L35380

          REM Alternate 1 Key...
L35120:     if keyhit% <> 9% then L35190
*             IF STR(LOT$) = "ALL" THEN TEMP_LOT$ = "   "
               plowkey$ = str(store$)& str(part$) & str(temp_lot$) &     ~
                          str(loc$) & hex(00)
               str(plowkey$, 1, 42) = addc all(hex(ff))
               error$ = "INQUIRY BY STORE/PART/LOT/LOC"
               call "HNYLCINQ" (#4, plowkey$, error$, 0%, 1, f1%(4))
               goto L35380

          REM Alternate 2 Key...
L35190:     if keyhit% <> 10% then L35260
               plowkey$ = str(store$)& str(part$) & str(temp_loc$)       ~
                          & hex(00)
               error$ = "INQUIRY BY STORE/PART/LOC/LOT"
               call "HNYLCINQ" (#4, plowkey$, error$, 0%, 2, f1%(4))
               goto L35380

          REM Alternate 3 Key...
L35260:     if keyhit% <> 11% then L35330
               plowkey$ = str(part$)& str(store$) & str(temp_loc$)       ~
                          & hex(00)
               error$ = "INQUIRY BY PART/STORE/LOC/LOT"
               call "HNYLCINQ" (#4, plowkey$, error$, 0%, 3, f1%(4))
               goto L35380

          REM Alternate 4 Key...
L35330:     if keyhit% <> 12% then L35380
*             IF STR(LOT$) = "ALL" THEN TEMP_LOT$ = "   "
               plowkey$ = str(store$)& str(temp_lot$) & str(part$) &     ~
                          str(temp_loc$) & hex(00)
               str(plowkey$, 1, 42) = addc all(hex(ff))
               error$ = "INQUIRY BY STORE/LOT/PART/LOC"
               call "HNYLCINQ" (#4, plowkey$, error$, 0%, 4, f1%(4))

L35380:   REM Get HNYLOCNS qty based on key selected...
            errormsg$ = " "
            if f1%(4) = 0% then L35510
               plowkey$ = key(#4)
               call "READ100" (#4, plowkey$, f1%(4))
               if f1%(4) <> 1% then L35510
                  get #4 using L35450, store$, part$, lot$, loc$, qty
L35450:              FMT POS(443), CH(3), CH(25), CH(6), CH(8), POS(573),~
                         PD(14,4)
                  lflag$ = "1"
                  call "CONVERT" (qty, 2.2, qty$)
                  call "DESCRIBE" (#1, part$, partdescr$, 1%, f1%(1))
                  call "DESCRIBE" (#3, store$, storedescr$, 1%, f1%(3))

L35510:   REM Get HNYQUAN qty based on key selected...
            plowkey$ = str(part$) & str(store$) & hex(00)
L35530:     call "PLOWALTS" (#2, plowkey$, 0%, 28%, f1%(2))
            if f1%(2) <> 1% then L35645
               get #2 using L35560, work_lot$, work_loc$, lot_qty
L35560:           FMT POS(45), CH(16), CH(8), PD(14,4)
               if loc$ = "ALL" and lot$ = "ALL" then L35615
                  if loc$ = "ALL" then L35600
                     if work_loc$ <> loc$ then L35530
L35600:        if lot$ = "ALL" then L35615
                  if work_lot$ <> lot$ then L35530
L35615:        iflag$ = "1"
               lot_qty = round(lot_qty, 2)
               totlotqty = totlotqty + lot_qty
               goto L35530
L35645:     if lflag$ = "1" then L35647
               qty$ = prev_qty$ : qty = prev_qty
L35647:     if iflag$ = "1" then L35650
               lot_qty$ = prev_lot_qty$ : lot_qty = prev_lot_qty
               goto L35660
L35650:     call "CONVERT" (totlotqty, 2.2, lot_qty$)
L35660:     return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
              pf4$ = " "
              if fieldnr% > 1% then pf4$ = "(4)Previous"
              init(hex(8c))  lfac$()
                 lfac$(6), lfac$(7) = hex(9c)
              if edit% = 0% then L40130
                 lfac$(6) = hex(84)  :  lfac$(7) = hex(8c)
L40130:       if errormsg$ > " " and fieldnr% > 0% then                  ~
                 lfac$(fieldnr%) = or hex(10)

              on fieldnr% gosub L40220,             /* Store            */~
                                L40220,             /* Part             */~
                                L40220,             /* Location         */~
                                L40220              /* Lot              */
              goto L40260

L40220:   REM Set FAC's for Upper Case Only Input...
                lfac$(fieldnr%) = hex(81)
                return

L40260:     accept                                                       ~
               at (01,02), "Inventory Location Inquiry",                 ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   blankline$           , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
               at (06,02), "Warehouse :"                        ,        ~
               at (06,14), fac(lfac$( 1)), store$               , ch(03),~
               at (06,40), fac(lfac$( 5)), storedescr$          , ch(30),~
               at (07,02), "Part Code :"                        ,        ~
               at (07,14), fac(lfac$( 2)), part$                , ch(25),~
               at (07,40), fac(lfac$( 5)), partdescr$           , ch(32),~
               at (08,02), "Location  :"                        ,        ~
               at (08,14), fac(lfac$( 3)), loc$                 , ch(08),~
               at (08,29), fac(lfac$( 7)), literal2$            , ch(28),~
               at (08,58), fac(lfac$( 7)), qty$                 , ch(10),~
               at (09,02), "Lot (Opt) :"                        ,        ~
               at (09,14), fac(lfac$( 4)), lot$                 , ch(06),~
               at (09,29), fac(lfac$( 7)), literal1$            , ch(28),~
               at (09,58), fac(lfac$( 7)), lot_qty$             , ch(10),~
               at (12,02), fac(lfac$( 6)), literal4$            , ch(17),~
               at (12,22), fac(lfac$( 6)), options$(1)          , ch(40),~
               at (13,22), fac(lfac$( 6)), options$(2)          , ch(40),~
               at (14,22), fac(lfac$( 6)), options$(3)          , ch(40),~
               at (15,22), fac(lfac$( 6)), options$(4)          , ch(40),~
               at (16,22), fac(lfac$( 6)), options$(5)          , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,02), "(1)Start Over",                              ~
               at (23,20), fac(hex(8c)), pf4$                   , ch(11),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010408090a0b0c0d0e0f10)),                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("HNYLCQRY")
                  goto L40260

L40690:        if keyhit% <> 15 then L40730
                  call "PRNTSCRN"
                  goto L40260

L40730:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
                  u3% = u3%

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
              errormsg$ = " "
              on fieldnr% gosub L50140,             /* Warehouse (Store)*/~
                                L50290,             /* Part Code        */~
                                L50470,             /* Loc (Optional)   */~
                                L50710              /* Lot (Optional)   */
              return

L50140:   REM Test Data for Warehouse (Store) Code...
                if store$ = "?" or store$ = " " then L50220
                   if store$ = "001" then L50230
                readkey$ = str(store$)
                call "READ100" (#3, readkey$, f1%(3))
                   if f1%(3) = 1% then L50230
L50220:         storedescr$ = hex(06) & "Valid Warehouses"
L50230:         call "GETCODE" (#3, store$, storedescr$, 1%, .3, f1%(3))
                   if f1%(3) = 1% then return
                errormsg$ = "Warehouse/Store is not defined in the Store ~
        ~File"
                return

L50290:   REM Test Data for Part Code...
                if part$ = "?" or part$ = " " then L50370
                   readkey$ = str(part$)
                   call "READ100" (#1, readkey$, f1%(1))
                      if f1%(1) = 0% then L50400
L50370:         partdescr$ = hex(06) & "Parts on File"
                call "GETCODE" (#1, part$, partdescr$, 1%, .32, f1%(1))
                   if f1%(1) = 1% then L50450
L50400:         readkey$ = str(store$) & str(part$) & hex(00)
                call "PLOWALTS" (#4, readkey$, 1%, 28%, f1%(4))
                   if f1%(4) = 1% then L50450
                errormsg$ = "Part was not found in the Inventory Quantity~
        ~  or Location Quantity File"
L50450:         return

L50470:   REM Test Data for Location Code...
                if loc$ = "ALL" then L50690
                if loc$ = "?" or loc$ = " " then L50531
                   readkey$ = str(store$) & str(loc$)
                   call "READ100" (#5, readkey$, f1%(5))
                      if f1%(5) = 0% then L50531
                   goto L50690
L50531:         plowkey$ = all(hex(00))
                str(plowkey$,1,3) = str(store$,,)
                str(plowkey$,4,8) = str(loc$,,)
                locdescr$ = hex(06)&"Locations on file for store: "&store$
                call "PLOWCODE" (#5, plowkey$, locdescr$, 3%, 0, f1%(5))
                   store$ = str(plowkey$,1,3)
                   loc$ = str(plowkey$,4,8)
                   if f1%(5) = 1% then L50690
                readkey$ = str(store$) & str(part$) & str(loc$) & hex(00)
                call "PLOWALTS" (#4, readkey$, 2%, 36%, f1%(4))
                if f1%(4) = 1% then L50690
                   readkey$ = str(part$) & str(store$) & hex(00)
L50610:            call "PLOWALTS" (#2, readkey$, 0%, 28%, f1%(2))
                   if f1%(2) = 0% then L50670
                      get #2 using L50640, bin_loc$
L50640:                   FMT POS(61), CH(8)
                      if bin_loc$ = loc$ then L50690
                          goto L50610
L50670:         errormsg$ = "Location was not found in the Inventory Quan~
        ~tity or Location Quantity File"
L50690:         return

L50710:   REM Test Data for Lot Code...
                if lot$ = "ALL" then L50920
                if lot$ = "?" then L50780
                   plowkey$=str(store$)& str(part$) & str(lot$) & hex(00)
                   call "PLOWALTS" (#4, plowkey$, 1%, 34%, f1%(4))
                      if f1%(4) = 0% then L50870
                   goto L50920
L50780:         plowkey$ = str(part$) & str(store$) & lot$
                error$ = hex(06) & "Current Lots"
                f1%(2) = -9%
                call "PLOWCODE" (#2, plowkey$, error$, 28%, 0, f1%(2))
                lot$ = str(plowkey$,29)
                   if f1%(2) = 0% then L50870
                plowkey$=str(store$) & str(loc$) & str(part$) & str(lot$)
                call "READ100" (#4, plowkey$, f1%(4))
                   if f1%(4) = 1% then L50920
L50870:         readkey$ = str(part$) & str(store$) & str(lot$)
                call "READ100" (#2, readkey$, f1%(2))
                   if f1%(2) = 1% then L50920
                errormsg$ = "Lot was not found in the Inventory Quantity ~
        ~or Location Quantity File"
L50920:         return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
