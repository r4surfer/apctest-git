        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  V   V  PPPP   RRRR   V   V   SSS   L       CCC   TTTTT   *~
            *  V   V  P   P  R   R  V   V  S      L      C   C    T     *~
            *  V   V  PPPP   RRRR   V   V   SSS   L      C        T     *~
            *   V V   P      R  R    V V       S  L      C   C    T     *~
            *    V    P      R   R    V     SSS   LLLLL   CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPRVSLCT - Cross Reference Inventory to Vendor Price      *~
            *            Catalogue for the passed in Vendor Code.       *~
            *            Allows user to select the Vendor P/N to use.   *~
            *            Returning vendor part number and optionally the*~
            *            selected part code if not passed in.           *~
            *            If Part Code passed in only those Vendor Price *~
            *            records with BOTH the passed in Vendor & Part  *~
            *            codes will be displayed.                       *~
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
            * 05/06/86 ! Original (Cloned from VPRPSLCT)          ! LDJ *~
            * 06/16/86 ! Changed to allow display of single entry ! LDJ *~
            *          !   when in Inquiry Mode (MFLAG=0% or 10%).!     *~
            * 12/18/86 ! Changed to allow display of pending      ! LDJ *~
            *          !   procurements (open P.O. Line Items).   !     *~
            * 09/13/88 ! Added Part #/Description toggle.         ! JIM *~
            * 02/10/93 ! Added Multi-Currency.                    ! JDH *~
            * 03/24/93 ! PRR 12118.  Pending Purchase screen now  ! JDH *~
            *          !   excludes items with zero open qty.     !     *~
            * 06/28/96 ! Add blank date for tests                 ! DER *~
            * 12/20/99 ! Mod to change vendor's price to allow    ! CMG *~
            *          !     five decimal places.  (EWD0001)       !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "VPRVSLCT" (part$,           /* Part Number                */~
                       vendor$,          /* Vendor Code                */~
                       venpart$,         /* Vendor Part Number (return)*/~
                       mflag%,           /* Message Flag; 0%=No Prompt */~
                                         /*   1% = Select for Return ""*/~
                                         /*   2% = Select for Edit   ""*/~
                                         /*  10% = No Prompt   - No PF8*/~
                                         /*  11% = Select for Return ""*/~
                                         /*  12% = Select for Edit   ""*/~
                       only_one$,        /* Only One Record? 'Y' if    */~
                                         /*   only 1 record, 'X' if    */~
                                         /*   none, or 'N' if many.    */~
                                         /*   This is a returned value.*/~
                       #1,               /* VENPRICE File UFB          */~
                       #2,               /* VENDOR   File UFB          */~
                       #4,               /* HNYMASTR FILE UFB          */~
                       #3)               /* HNYPROC  FILE UFB          */

        dim                                                              ~
            blank_date$8,                /* Blank date for tests       */~
            curr$(20)4,                  /* Currency Code              */~
            cursor%(2),                  /* Screen Cursor Position     */~
            date$8,                      /* Today's Date               */~
            descr_map(20),               /* PLOWCODE Argument          */~
            dummy$1,                     /* Dummy Character            */~
            edtmessage$79,               /* Edit Screen Message        */~
            effdate$6,                   /* Next Price Effective Date  */~
            expdate$6,                   /* Next Price Expiration Date */~
            hdr$(3)80,                   /* PLOWCODE Argument          */~
            header$79,                   /* Column Header Line         */~
            i$(24)80,                    /* Screen Image from SCREEN   */~
            incl_excl(2),                /* PLOWCODE Argument          */~
            incl_excl$(2)25,             /* PLOWCODE Argument          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Header Line         */~
            mc_on$1,                     /* Is Multi-Currency Active?  */~
            mc_tbl$1,                    /* Multi-Currency Table for PO*/~
            meas$(20)4,                  /* Vendor Unit of Measure     */~
            mfac$(20)1,                  /* Field Attribute Characters */~
            msg$80,                      /* PLOWCODE Argument          */~
            only_one$1,                  /* Only One Record? Y,N, or X */~
            part$25,                     /* Part Code                  */~
            part$(20)25, pdescr$(20)25,  /* Parts & Descriptions       */~
            pf5$16,                      /* PF5 Screen Literal         */~
            pf8$30,                      /* PF8 Screen Literal         */~
            plowkey$99,                  /* PLOWCODE Argument          */~
            price_stat$(20)21,           /* Vendor Prices - Statutory  */~
            price_curr$(20)21,           /* Vendor Prices - Actual     */~
            readkey$60,                  /* Miscellaneous Read/Plow key*/~
            stat$4,                      /* Statutory Currency Code    */~
            t$(15)1,                     /* Tab Stops                  */~
            toggle$(20)25,               /* Contains the toggled info  */~
            toggle2$(20)4,               /* Toggle Currency/UOM        */~
            vendor$9,                    /* Vendor Code                */~
            vendordescr$32,              /* Vendor Name                */~
            venpart$25,                  /* Vendor Part Number         */~
            venpart$(20)25,              /* Vendor Part Number         */~
            venprice$(20)21,             /* Vendor Prices - Screen     */~
            veneach$(20)10               /* Vendor Prices Converted    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 5 ! VBKLINES ! Backlog line item file                   *~
            * #12 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #45 ! VBKLNCUR ! Currency Line Item Information           *~
            *************************************************************~
            *                                                           *

            select # 5, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28

            select #12,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #45, "VBKLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5, keylen = 28,                         ~
                        alt key 1, keypos = 1, keylen = 32

            if been_here_before% = 1% then L09052
                call "OPENCHCK" (#12, 0%, f2%, 0%, " ")
                been_here_before% = 1%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            blank_date$ = " "  : call "DATUNFMT" (blank_date$)
*        Check for Multi-Currency
            mc_on$ = "N" : stat$ = " "
            readkey$ = "SWITCHS.CUR         "
            call "READ100" (#12, readkey$, f12%)
            if f12% <> 0% then get #12 using L09030, mc_on$, stat$, mc_tbl$
L09030:         FMT POS(21), CH(1), CH(4), POS(28), CH(1)

L09052:     header$="  Vendor Part Number        XXXXXXXXXXXXXXX         ~
        ~            Price/Quantity"
            str(header$,28%,1%) = hex(a4)
            str(header$,54%,1%) = hex(a4)
            str(header$,59%,1%) = hex(a4)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$ = " "
            if mflag% = 1% or mflag% = 11% then                          ~
            edtmessage$="To Select Vendor Part, Position Cursor and Press~
        ~ RETURN, or PF16 to Exit"
            if mflag% = 2% or mflag% = 12% then                          ~
            edtmessage$="To Edit a Vendor Part, Position Cursor and Press~
        ~ RETURN, or PF16 to Exit"
            only_one$ = "X"
            if mflag% < 10% then pf8$ = "(8)View Procurement History"    ~
                            else pf8$ = " "
            init (hex(00)) readkey$
            str(readkey$,1,9)=vendor$
                ll%=9%

            call "PLOWALTS" (#1,readkey$, 2%, ll% ,f1%)
            if f1%=0 then endnone

            init (" ") vendordescr$
            call "DESCRIBE" (#2, vendor$, vendordescr$, 1%, f1%)
                if f1% = 0% then endnone

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            gosub startarray
            if max% > 1% then L10130
               if max% = 1% then only_one$ = "Y"
               if max% = 1% and (mflag% = 0% or mflag% = 10%) then L10130
               fieldnr%=1%
               goto setend

L10130:     gosub'111(0%)
                  if keyhit%  =  2 then gosub startarray
                  if keyhit%  =  5 then gosub continuearray
                  if keyhit%  =  8 then gosub procurement_history
                  if keyhit%  = 14 then gosub pending_procurements
                  if keyhit%  = 16 then       endnone
                  if keyhit% <>  0 then       L10130
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  max% then L10130
            only_one$ = "N"
            goto setend

        procurement_history
            if mflag% > 9% then return
            fieldnr% = cursor%(1) - 5%
            save_part$ = part$
            if fieldnr% < 1 or fieldnr% >  max% then L10310
               part$=part$(fieldnr%)
L10310:     call "HNYPRCSB" (vendor$, part$, 1%, #3, #4, #2, #1)
            part$ = save_part$
            return

        pending_procurements
            call "OPENCHCK" (#5, 0%, f2%, 0%, " ")
            if f2% <> 0% then return
            if mc_on$ = "Y" then call "OPENCHCK" (#45, 0%, f2%, 0%, " ")
            fieldnr% = cursor%(1%) - 5%
            save_part$ = " "
            mat incl_excl = zer
            if fieldnr% < 1% or fieldnr% >  max% then L10440
             save_part$ = part$(fieldnr%)
             incl_excl(01%) =  32.25 : incl_excl$(01%) = save_part$
L10440:     plowkey$ = vendor$
            hdr$(3%) = hex(ac) & "Pending (Not Yet Received) Purchases"  ~
                              & " for Vendor " & vendor$
            str(hdr$(3%),64%) = "VPRVLSCT:" & str(cms2v$,,8%)
            msg$ = hex(06) & "Shown below are the Pending P.O.'s for " & ~
                             "All parts purchased from this Vendor"
            if save_part$ > " " then msg$ = hex(06) & "Shown below are " ~
                & "the Pending P.O.'s for Part " & save_part$
            hdr$(1%) = "  Part Code"
            str(hdr$(1%),28%) = "P.O. Number"
            str(hdr$(1%),44%) = "Itm"
            str(hdr$(1%),48%) = "Ord Qty/Left"
            str(hdr$(1%),61%) = "Unit Price"
            str(hdr$(1%),72%) = "Due Date"
            if mc_on$ = "Y" then str(hdr$(1%),72%) = "Due/Curr"
            descr_map(01%) = 32.25 : descr_map(02%) = 01     /* Part #  */
            descr_map(03%) = 10.16 : descr_map(04%) = 26     /* P.O. #  */
            descr_map(05%) = 29.03 : descr_map(06%) = 42     /* Item #  */
            descr_map(07%) = 93.08 : descr_map(08%) = 47.1040/* Qty     */
            descr_map(09%) =117.08 : descr_map(10%) = 59.1072/* Price   */
            descr_map(11%) =142.061: descr_map(12%) = 70     /*Due Date */
            descr_map(13%) = 57.32 : descr_map(14%) = 1006   /*Part Desc*/
            if mc_on$ <> "Y" then L10597
            descr_map(15%) = -1.04 : descr_map(16%) = 1070   /*Currency */
            descr_map(17%) =-33.08 : descr_map(18%) =1059.1072/*Cur Pric*/
L10597:     descr_map(19%) =109.08 : descr_map(20%)=1048.1040/*Qty Left */
            incl_excl(02%)=-109.08                        /* Remain Qty */
            put incl_excl$(02%) using L10604, 0
L10604:         FMT PD(14,4)

            call "PLOWCODE" (#5, plowkey$, msg$, 9009%,.3, f1%, hdr$(),  ~
              0,-1,incl_excl(), incl_excl$(), "D", "Y", #45, descr_map())
            return

        startarray
            init (hex(00)) str(readkey$,ll%+1%)

        continuearray

            init (" ") part$(), pdescr$(), toggle$(), venpart$(),        ~
                venprice$(), meas$(), veneach$(), t$(), toggle2$(),      ~
                price_curr$(), price_stat$(), curr$()
            init (hex(8c)) lfac$(), mfac$()
            p%, max%=0% : str(header$,29%,15%) = "Our Part Number"
                      if mc_on$ = "Y" then L20156
                          str(header$,55%, 4%) = "UOM " : goto L20170
L20156:               str(header$,55%, 4%) = "Curr"
                      str(header$,60%, 4%) = "Curr"

L20170:     if max%=15% then L20370
L20180:     call "PLOWALTS" (#1,readkey$,2%,ll%,f1%)
            if f1%=0 then L20370
            if part$ > " " and str(key(#1),,25%) <> part$ then L20180
            max% = max% + 1%
            t$(max%) = hex(0b) : mfac$(max%) = hex(86)
            get #1, using L20230, part$(max%), venpart$(max%), prce,      ~
                       meas$(max%), factor, nextprice, effdate$,expdate$,~
                       curr$(max%)
L20230:     FMT XX(9), CH(25),XX(9),CH(25),PD(15,5), XX(12), CH(4),      ~
                PD(14,4),  PD(14,5), CH(6), CH(6), POS(158), CH(4)  /* (EWD0001) */
            toggle$(max%) = part$(max%) /* Initialize to 'part' */
            call "DESCRIBE" (#4, part$(max%), pdescr$(max%), 0%, f1%)
            if curr$(max%) = " " then curr$(max%) = stat$
            toggle2$(max%) = curr$(max%) /* Initialize to 'currency' */
            if mc_on$ <> "Y" then toggle2$(max%) = meas$(max%)
            if factor <= 0 or factor > 1000000 then factor = 1
            if effdate$ = blank_date$ then L20270
            if date < effdate$ or date > expdate$ then L20270
               prce = nextprice
L20270:     call "CONVERT" (prce, 2.5, str(venprice$(max%),1%,10%))
            call "CONVERT" (factor, -2.5, str(venprice$(max%),12%,10%))
            str(venprice$(max%),11%,1%) = "/"
            str(price_curr$(max%)) = str(venprice$(max%))
            str(price_stat$(max%)) = str(venprice$(max%))
            gosub curr_to_stat

            each = round(prce/factor,7)
            call "CONVERT" (each, 2.4, veneach$(max%))
            lfac$(max%) = hex(8c)

            goto L20170

L20370:     if part$(1%)=" " then goto startarray
            return

        endnone
            venpart$=" "
            end

        setend
            venpart$=venpart$(fieldnr%)
            part$=part$(fieldnr%)
            end

        curr_to_stat
            if mc_on$ <> "Y" then return
            if curr$(max%) = stat$ then return
            if max% = 1% then L20850
                if curr$(max%) = curr$(max% - 1%) then L20920
L20850:     call "CURRATSB" (curr$(max%), mc_tbl$, date, cur_factor,    ~
                                                         temp, errormsg$)
            if errormsg$ = " " then L20920
                errormsg$ = " " : temp = 1  /* TEMP is not used */
                price_stat$(max%) = "???????.??"  /* No Rates on file */
                return
L20920:     call "CONVERT" (prce * cur_factor, 2.5,                      ~
                                           str(price_stat$(max%),1%,10%))
            return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
            line2$ = "For Vendor: " & vendor$ & " " & vendordescr$
            str(line2$,62%) = "VPRVSLCT: " & str(cms2v$,,8%)
            if max% > 14% then pf5$ = "(5)Next Screen" else pf5$ = " "

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Vendor Price Catalogue Reference By Vendor",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (05,02), fac(hex(a4)), header$                , ch(79),~
               at (06,02), fac(hex(82)),     dummy$             , ch( 1),~
               at (06,02), fac(mfac$(01)),   t$(01)             , ch( 1),~
               at (07,02), fac(mfac$(02)),   t$(02)             , ch( 1),~
               at (08,02), fac(mfac$(03)),   t$(03)             , ch( 1),~
               at (09,02), fac(mfac$(04)),   t$(04)             , ch( 1),~
               at (10,02), fac(mfac$(05)),   t$(05)             , ch( 1),~
               at (11,02), fac(mfac$(06)),   t$(06)             , ch( 1),~
               at (12,02), fac(mfac$(07)),   t$(07)             , ch( 1),~
               at (13,02), fac(mfac$(08)),   t$(08)             , ch( 1),~
               at (14,02), fac(mfac$(09)),   t$(09)             , ch( 1),~
               at (15,02), fac(mfac$(10)),   t$(10)             , ch( 1),~
               at (16,02), fac(mfac$(11)),   t$(11)             , ch( 1),~
               at (17,02), fac(mfac$(12)),   t$(12)             , ch( 1),~
               at (18,02), fac(mfac$(13)),   t$(13)             , ch( 1),~
               at (19,02), fac(mfac$(14)),   t$(14)             , ch( 1),~
               at (20,02), fac(mfac$(15)),   t$(15)             , ch( 1),~
                                                                         ~
               at (06,04), fac(lfac$(1) ), venpart$  (1)        , ch(25),~
               at (07,04), fac(lfac$(02)), venpart$  (2)        , ch(25),~
               at (08,04), fac(lfac$(03)), venpart$  (3)        , ch(25),~
               at (09,04), fac(lfac$(04)), venpart$  (4)        , ch(25),~
               at (10,04), fac(lfac$(05)), venpart$  (5)        , ch(25),~
               at (11,04), fac(lfac$(6)), venpart$  (6)         , ch(25),~
               at (12,04), fac(lfac$(7)), venpart$  (7)         , ch(25),~
               at (13,04), fac(lfac$(8)), venpart$  (8)         , ch(25),~
               at (14,04), fac(lfac$(9)), venpart$  (9)         , ch(25),~
               at (15,04), fac(lfac$(10)),venpart$  (10)        , ch(25),~
               at (16,04), fac(lfac$(11)),venpart$  (11)        , ch(25),~
               at (17,04), fac(lfac$(12)),venpart$  (12)        , ch(25),~
               at (18,04), fac(lfac$(13)),venpart$  (13)        , ch(25),~
               at (19,04), fac(lfac$(14)),venpart$  (14)        , ch(25),~
               at (20,04), fac(lfac$(15)),venpart$  (15)        , ch(25),~
                                                                         ~
               at (06,30), fac(lfac$( 1)), toggle$( 1)          , ch(25),~
               at (07,30), fac(lfac$( 2)), toggle$( 2)          , ch(25),~
               at (08,30), fac(lfac$( 3)), toggle$( 3)          , ch(25),~
               at (09,30), fac(lfac$( 4)), toggle$( 4)          , ch(25),~
               at (10,30), fac(lfac$( 5)), toggle$( 5)          , ch(25),~
               at (11,30), fac(lfac$( 6)), toggle$( 6)          , ch(25),~
               at (12,30), fac(lfac$( 7)), toggle$( 7)          , ch(25),~
               at (13,30), fac(lfac$( 8)), toggle$( 8)          , ch(25),~
               at (14,30), fac(lfac$( 9)), toggle$( 9)          , ch(25),~
               at (15,30), fac(lfac$(10)), toggle$(10)          , ch(25),~
               at (16,30), fac(lfac$(11)), toggle$(11)          , ch(25),~
               at (17,30), fac(lfac$(12)), toggle$(12)          , ch(25),~
               at (18,30), fac(lfac$(13)), toggle$(13)          , ch(25),~
               at (19,30), fac(lfac$(14)), toggle$(14)          , ch(25),~
               at (20,30), fac(lfac$(15)), toggle$(15)          , ch(25),~
                                                                         ~
               at (06,56), fac(lfac$(01)), toggle2$  (01)       , ch(04),~
               at (07,56), fac(lfac$(02)), toggle2$  (02)       , ch(04),~
               at (08,56), fac(lfac$(03)), toggle2$  (03)       , ch(04),~
               at (09,56), fac(lfac$(04)), toggle2$  (04)       , ch(04),~
               at (10,56), fac(lfac$(05)), toggle2$  (05)       , ch(04),~
               at (11,56), fac(lfac$(06)), toggle2$  (06)       , ch(04),~
               at (12,56), fac(lfac$(07)), toggle2$  (07)       , ch(04),~
               at (13,56), fac(lfac$(08)), toggle2$  (08)       , ch(04),~
               at (14,56), fac(lfac$(09)), toggle2$  (09)       , ch(04),~
               at (15,56), fac(lfac$(10)), toggle2$  (10)       , ch(04),~
               at (16,56), fac(lfac$(11)), toggle2$  (11)       , ch(04),~
               at (17,56), fac(lfac$(12)), toggle2$  (12)       , ch(04),~
               at (18,56), fac(lfac$(13)), toggle2$  (13)       , ch(04),~
               at (19,56), fac(lfac$(14)), toggle2$  (14)       , ch(04),~
               at (20,56), fac(lfac$(15)), toggle2$  (15)       , ch(04),~
                                                                         ~
               at (06,61), fac(lfac$(01)), venprice$ (01)       , ch(20),~
               at (07,61), fac(lfac$(02)), venprice$ (02)       , ch(20),~
               at (08,61), fac(lfac$(03)), venprice$ (03)       , ch(20),~
               at (09,61), fac(lfac$(04)), venprice$ (04)       , ch(20),~
               at (10,61), fac(lfac$(05)), venprice$ (05)       , ch(20),~
               at (11,61), fac(lfac$(06)), venprice$ (06)       , ch(20),~
               at (12,61), fac(lfac$(07)), venprice$ (07)       , ch(20),~
               at (13,61), fac(lfac$(08)), venprice$ (08)       , ch(20),~
               at (14,61), fac(lfac$(09)), venprice$ (09)       , ch(20),~
               at (15,61), fac(lfac$(10)), venprice$ (10)       , ch(20),~
               at (16,61), fac(lfac$(11)), venprice$ (11)       , ch(20),~
               at (17,61), fac(lfac$(12)), venprice$ (12)       , ch(20),~
               at (18,61), fac(lfac$(13)), venprice$ (13)       , ch(20),~
               at (19,61), fac(lfac$(14)), venprice$ (14)       , ch(20),~
               at (20,61), fac(lfac$(15)), venprice$ (15)       , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(2)First Screen",                            ~
               at (22,20), fac(hex(8c)), pf5$                   , ch(16),~
               at (22,40), "(10)Display Toggle",                         ~
               at (23,65), "(15)Print Screen",                           ~
               at (23,20), fac(hex(8c)), pf8$                   , ch(30),~
               at (24,20), "(14)View Pending Purchases",                 ~
               at (24,65), "(16)Return",                                 ~
                                                                         ~
               keys(hex(000205080a0d0e0f10)), key (keyhit%)

               if keyhit% <> 10% then L41290 /* Part/Description toggle */
                    if p% <> 0% then goto L41250
                        p% = 1%
                        str(toggle$()) = str(pdescr$())
                        str(header$,29%,15%) = "Our Description"
                        str(header$,55%, 4%) = "UOM "
                        if mc_on$ <> "Y" then L40110
                            str(header$,60%, 4%) = "Stat"
                            str(toggle2$())  = str(meas$())
                            str(venprice$())=str(price_stat$())
                            goto L40110

L41250:             p% = 0%
                    str(toggle$()) = str(part$())
                    str(header$,29%,15%) = "Our Part Number"
                    if mc_on$ <> "Y" then L40110
                        str(header$,55%, 4%) = "Curr"
                        str(header$,60%, 4%) = "Curr"
                        str(toggle2$()) = str(curr$())
                        str(venprice$()) = str(price_curr$())
                        goto L40110


L41290:        if keyhit% <> 13% then L41330
                  call "MANUAL" ("VPRVSLCT")
                  goto L40110

L41330:        if keyhit% <> 15% then L41370
                  call "PRNTSCRN"
                  goto L40110

L41370:        close ws
               call "SCREEN" addr ("C", f1%, "I", i$(), cursor%())
               return
