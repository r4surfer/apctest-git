        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   PPPP   RRRR    SSS   PPPP   L      IIIII  N   N   *~
            *  C      P   P  R   R  S      P   P  L        I    NN  N   *~
            *  C      PPPP   RRRR    SSS   PPPP   L        I    N N N   *~
            *  C      P      R   R      S  P      L        I    N  NN   *~
            *   CCC   P      R   R   SSS   P      LLLLL  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CPRSPLIN - Maintenance and inquiry of the Specials Pricing*~
            *            in file CPRPRICE.                              *~
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
            * 03/28/85 ! ORIGINAL                                 ! ERN *~
            * 02/02/87 ! Specials Pricing Expansion               ! ERN *~
            * 05/06/87 ! Standard Costing Enhancements            ! ERN *~
            * 11/09/87 ! GETCODE AltKeyDefs on CUSTOMER & HNYMASTR! JIM *~
            * 10/10/88 ! Added PF(3)/Copy Cost Set.               ! JIM *~
            * 02/02/89 ! Proj 7880714 new price set implementation! JIM *~
            * 02/02/89 ! Corrected deletions- 'S' records only.   ! JIM *~
            * 09/15/89 ! Adjusted PLOW screen & PF(3) dup probs.  ! JDH *~
            * 08/06/90 ! Cleared ALL variables on STARTOVER!      ! MJB *~
            *          !  and fixed VOLUME RANGE error message.   !     *~
            * 06/26/96 ! Dim To and From 8 to 10 for century      ! DER *~
            * 09/02/97 !  and add all(hex(00)) to plowkey         ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**
        dim                                                              ~
            blankdate$8,                 /* Blank unfmt date           */~
            cat$4,                       /* Part Category Code         */~
            catdescr$32,                 /* Part Category Description  */~
            custdescr$32,                /* Type Descr or Cust Name    */~
            custnr$9,                    /* Customer Number            */~
            date$8,                      /* Date for Screen Display    */~
            ddate$8,                     /* Delete to date             */~
            descr_map(12),               /* PLOWCODE screen mapping    */~
            disc(10), disc$(10)7,        /* Discount Percentages       */~
            edate$6,                     /* Ending date for deletes    */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error Message              */~
            from$10,                     /* Effective date- From       */~
            fromqty$(10)10,              /* From Quantities- Display   */~
            hdr1$26, hdr2$6, hdr3$18,    /* Columnar Headings          */~
            hfac$(5)1,                   /* Header Field Facs          */~
            incl(1),                     /* PLOWCODE needs this        */~
            incl$(1)1,                   /* PLOWCODE needs this        */~
            infomsg$79,                  /* Input Message              */~
            key$50,                      /* Record Key field           */~
            lastuser$3, lastchanged$8,   /* Change Audit Trial         */~
            lfac$(42)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            partnr$25,                   /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            pc$(10)1,                    /* Over-ride Price Code       */~
            pf$(3)79, pfkeys$(30)1,      /* PF Descrs and Keys         */~
            plowhdr$(3)79,               /* PLOWCODE Heading           */~
            plowkey$100,                 /* Misc use Plow key          */~
            price(10), price$(10)10,     /* Override Prices            */~
            text$40,                     /* Contract, Text             */~
            to$10,                       /* Effective Date- To         */~
            toqty%(10), toqty$(10)10,    /* To Quantities              */~
            type$2,                      /* Customer Type Code         */~
            userid$                      /* User ID playing now        */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CPRPRICE ! Customer Pricing File                    *~
            * #2  ! GENCODES ! General Codes File                       *~
            * #3  ! CUSTOMER ! Customer Master File                     *~
            * #5  ! CATEGORY ! Inventory category codes file            *~
            * #6  ! HNYMASTR ! Inventory Master File                    *~
            * #33 ! DUMMY    ! Dummy for PLOWCODE                       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "CPRPRICE",                                      ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =  1, keylen = 47

            select #2,  "GENCODES",                                      ~
                        varc, indexed,                                   ~
                        recsize =  128,                                  ~
                        keypos =    1, keylen =  24

            select #3,  "CUSTOMER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1200,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #5,  "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =   4

            select #6,  "HNYMASTR",                                      ~
                        varc, indexed,                                   ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #33, "DUMMY", varc, indexed,                          ~
                                 recsize = 5, keypos = 1, keylen = 1

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3),   0%, " ")
            call "OPENCHCK" (#5, 0%, f2%(5),   0%, " ")
            call "OPENCHCK" (#6, 0%, f2%(6),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press (RETURN)."

            str(line2$,62) = "CPRSPLIN: " & cms2v$
            hdr1$ = "Stocking Unit Volume Range"
            hdr2$ = "Disc %"
            hdr3$ = "PC -or- Unit Price"

            plowhdr$(1) = "  Customer  Type Part Code                 " &~
                          "Ctgy   From      To"

            incl(1) = 0 : incl$(1) = " "

            descr_map( 1) =  2.09   :   descr_map( 2) =  1.00
            descr_map( 3) = 11.02   :   descr_map( 4) = 12.00
            descr_map( 5) = 13.25   :   descr_map( 6) = 16.00
            descr_map( 7) = 38.04   :   descr_map( 8) = 42.00
            descr_map( 9) = 42.061  :   descr_map(10) = 47.00
            descr_map(11) = 48.061  :   descr_map(12) = 56.00

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Get Key for Record to be modified. Full Screen edit with  *~
            * field level error correction.                             *~
            *************************************************************

        inputmode
            init(" ") errormsg$, text$, fromqty$(), toqty$(), pc$(),     ~
                      disc$(), price$(), from$, to$, lastchanged$,       ~
                      lastuser$, type$, custnr$, cat$, partnr$,          ~
                      catdescr$, partdescr$, custdescr$
            mat toqty% = zer
            mat disc   = zer
            for i% = 1% to 10% : price(i%) = -1 : next i%
            if str(infomsg$,,4) <> "NOTE" then infomsg$ = " "

            fieldnr% = 0% : screen% = 1%

L10180:     gosub'101(fieldnr%)          /* Get Data Fields            */
                if keyhit%  =  1 then gosub startover
                if keyhit%  =  2 then gosub next_cust_type
                if keyhit%  =  3 then gosub next_cust_code
                if keyhit%  =  4 then gosub next_part_ctgy
                if keyhit%  =  5 then gosub next_part_code
                if keyhit%  = 12 then goto  delete_expired
                if keyhit%  = 16 then goto  L65000
                if keyhit% <>  0 then       L10180
            gosub'151(screen%)           /* Test fields                */
                if errormsg$ <> " " then L10180

            gosub load_data              /* Get record specified       */
            goto  L11000

        next_cust_code
            infomsg$, type$, custdescr$ = " "
            call "READ102" (#3, custnr$, f1%(3))
            if f1%(3) = 1% then goto L10380
                infomsg$ = "At End of Customer File." : return
L10380:     get #3 using L10390, custnr$, custdescr$
L10390:         FMT CH(9), CH(30)
            return

        next_part_code
            infomsg$, cat$, catdescr$ = " "
            call "READ102" (#6, partnr$, f1%(6))
            if f1%(6) = 1% then goto L10470
                infomsg$ = "At End of Parts Master File." : return
L10470:     get #6 using L10480, partnr$, partdescr$
L10480:         FMT CH(25), CH(32)
            return

        next_cust_type
            infomsg$, custnr$, custdescr$ = " "
            plowkey$ = "CUS TYPES" & type$
            call "READ102" (#2, plowkey$, f1%(2))
            if f1%(2) = 1% then goto L10570
L10560:         infomsg$ = "At End of Customer Type Codes File." : return
L10570:     get #2 using L10580, plowkey$
L10580:         FMT CH(9)
            if str(plowkey$,,9) <> "CUS TYPES" then goto L10560
            get #2 using L10610, type$, custdescr$
L10610:         FMT XX(9), CH(15), CH(30)
            return

        next_part_ctgy
            infomsg$, partnr$, partdescr$ = " "
            call "READ102" (#5, cat$, f1%(5))
            if f1%(5) = 1% then goto L10690
                infomsg$ = "At end of Part Category Codes File." : return
L10690:     get #5 using L10700, cat$, catdescr$
L10700:         FMT CH(4), CH(30)
            return

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Get data for the record selected.  Full screen unless we  *~
            * get an error.                                             *~
            *************************************************************

            screen%  = 2%
L11080:     fieldnr% = 0
L11090:     gosub'111(fieldnr%)          /* Full screen edit           */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 12% then gosub delete_record
            gosub'151(2%)                /* Test data fields           */
                if errormsg$ <> " " then goto L11090
                if keyhit% = 16% then datasave
                goto L11080


        delete_record
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Press PF-16 to Delete the above record",    ~
                            "-or-", "Press (RETURN) to cancel delete.")
            if keyhit1% <> 16% then return
                call "DATUFMTC" (from$)
                key$ = "S" & str(custnr$) & str(type$) & str(partnr$) &  ~
                             str(cat$) & from$
                call "DELETE" (#1, key$, 47%)
                return clear all
                goto inputmode


        REM *************************************************************~
            *                    D E L E T E                            *~
            * --------------------------------------------------------- *~
            * Remove expired records (to date specified) from file.     *~
            *************************************************************
        delete_expired

            call "DATE" addr("G+", date, -1%, ddate$, ret%)
            call "DATEFMT" (ddate$)
            infomsg$, errormsg$ = " "

L15120:     accept                                                       ~
               at (01,02), "Special Pricing Management: DELETE RECORDS", ~
               at (01,67), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(a4)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "DELETE THRU DATE: ",                         ~
               at (06,22), fac(hex(81)), ddate$,                         ~
                                                                         ~
               at(08,02), "All records that have 'TO' dates on or      ",~
               at(09,02), "before the DELETE THRU DATE will be removed ",~
               at(10,02), "from the Special Pricing file if you choose ",~
               at(11,02), "to proceed.                                 ",~
               at(21,02), fac(hex(ac)), infomsg$,                        ~
               at(22,02), "(ENTER) Test date entered.",                  ~
               at(22,52), "(13)Instructions",                            ~
               at(23,52), "(15)Print Screen",                            ~
               at(24,23), "(16)Exit Deletion Routine",                   ~
               at(24,52), "(32)Proceed with Deletions",                  ~
                     keys(hex(000d0f1020)),                              ~
                     key (keyhit%)

                if keyhit% <> 13% then L15390
                     call "MANUAL" ("CPRSPLIN") : goto L15120

L15390:         if keyhit% <> 15% then L15420
                     call "PRNTSCRN"  :  goto L15120

L15420:     if keyhit% = 16% then goto inputmode
            infomsg$, errormsg$ = " "
            call "DATEOK" (ddate$, ret%, errormsg$)
            if errormsg$ <> " " then goto L15120
            if keyhit% = 32% then goto L15530
                call "DATUNFMT" (ddate$)
                if ddate$ >= date then infomsg$ =                        ~
                           "WARNING: Delete-To-Date is Today or after."
                call "DATEFMT" (ddate$)
                goto L15120

L15530
*        GO AHEAD AND DELETE THESE RECORDS
            print at(20,02), "DELETING RECORDS....."
            call "DATUNFMT" (ddate$)
            init(hex(00)) plowkey$
            deleted% = 0%

            key$ = "S" & hex(000000000000)/* Specials records only */
L15600:     call "READ102" (#1, key$, f1%(1))
            if f1%(1) = 1% then goto L15650
                convert deleted% to infomsg$, pic(####0)
                infomsg$ = "NOTE: " & infomsg$ & " RECORDS WERE DELETED."
                goto inputmode
L15650:     get #1 using L15660, key$, edate$
L15660:         FMT CH(47), CH(6)
            if edate$ > ddate$ then goto L15600
                print at(21,02), key$
                call "DELETE" (#1, key$, 47%)
                deleted% = deleted% + 1%
                goto L15600


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub save_data
            goto inputmode


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
L29918:     keyhit1% = 2%
            call "STARTOVR"(keyhit1%)
            if keyhit1% = 1% then return
                if keyhit1% <> 0% then L29918
                     return clear all
                     goto inputmode

        REM *************************************************************~
            *                L O A D   D A T A                          *~
            * --------------------------------------------------------- *~
            * Retrieve desired record from disk.  If not found set up   *~
            * as if it was.                                             *~
            *************************************************************
        load_data
            call "DATUFMTC" (from$)
            key$ = "S" & str(custnr$) & str(type$) & str(partnr$) &      ~
                         str(cat$)    & str(from$)
            call "READ100" (#1, key$, f1%(1))
            call "DATFMTC" (from$)
            infomsg$ = "New Record."
            if f1%(1) = 0% then return

            infomsg$ = "Existing Record."
        get_cprprice_data
            get #1 using L30180, to$, text$, lastuser$, lastchanged$,     ~
                                toqty%(), pc$(), price(), disc()
L30180:         FMT XX(47), CH(6), CH(40), CH(3), CH(6), 10*BI(4),       ~
                    10*CH(1), 10*PD(14,4), 10*PD(14,4)
            call "DATFMTC" (to$)
            call "DATEFMT" (lastchanged$)
            init (" ") fromqty$() : fromqty$(1) = "         0"
            for i% = 1% to 10%
                if toqty%(i%) = 0% then L30330
                     convert toqty%(i%) to toqty$(i%), pic(##,###,##0)
                     call "CONVERT" (price(i%), 0.4, price$(i%))
                     if price(i%) = -1 then price$(i%) = " "
                     convert disc  (i%) to disc$ (i%), pic(-###.##)
                     if i% = 1% then L30330
                          temp% = toqty%(i%-1) + 1%
                          convert temp% to fromqty$(i%), pic(##,###,##0)
L30330:     next i%
            return


        REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write data to disk.                                       *~
            *************************************************************
        save_data
            call "DATUFMTC" (to$  )
            call "DATUFMTC" (from$)
            key$ = "S" & str(custnr$) & str(type$) & str(partnr$) &      ~
                         str(cat$)    & str(from$)
            call "READ101" (#1, key$, f1%(1))
            put #1 using L31130, key$, to$, text$, userid$, date,         ~
                                toqty%(), pc$(), price(), disc(), " "
L31130:         FMT CH(47), CH(6), CH(40), CH(3), CH(6), 10*BI(4),       ~
                    10*CH(1), 10*PD(14,4), 10*PD(14,4), CH(188)
            if f1%(1) = 0% then  write #1  else  rewrite #1
            return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Gets which Record is to be edited.  If FIELDNR% = 0 then  *~
            * edit is full screen, otherwise a particuliar field has    *~
            * been found to be in error.                                *~
            *************************************************************

        deffn'101(fieldnr%)
            edit% = 1%              /* Input Mode */
            init (hex(8c)) lfac$()
            if errormsg$ = " " then init(hex(81)) hfac$()
            if infomsg$  = " " then                                      ~
                infomsg$ = "Put a '?' in a field to look at valid codes."
            gosub setpf_input
            goto L40270

        deffn'111(fieldnr%)
            edit% = 2%              /* Edit Mode */
            gosub setpf_edit
            if errormsg$ <> " " then goto L40270
                init(hex(84)) hfac$()        /* Header Variables       */
                init(hex(82)) lfac$()
                lfac$( 1), lfac$( 5), lfac$( 9), lfac$(13), lfac$(17),   ~
                           lfac$(21), lfac$(25), lfac$(29), lfac$(33),   ~
                           lfac$(37), lfac$(41)             = hex(81)
                lfac$( 2)  = hex(80)


L40270: accept                                                           ~
           at (01,02), "Special Pricing Management",                     ~
           at (01,66), "Today:",                                         ~
           at (01,73), fac(hex(8c)), date$                      , ch(08),~
           at (02,02), fac(hex(ac)), line2$                     , ch(79),~
           at (03,02), fac(hex(94)), errormsg$                  , ch(79),~
                                                                         ~
           at (04,02), "CUSTOMER Type Code XX -or- CODE  ",              ~
           at (04,21), fac(hfac$( 1)), type$                    , ch(02),~
           at (04,36), fac(hfac$( 2)), custnr$                  , ch(09),~
           at (04,49), fac(hex(8c)),   custdescr$               , ch(32),~
           at (05,02), "and PART Category",                              ~
           at (06,02), "         -or- Code",                             ~
           at (05,21), fac(hfac$( 3)), cat$                     , ch(04),~
           at (05,49), fac(hex(8c)),   catdescr$                , ch(32),~
           at (06,21), fac(hfac$( 4)), partnr$                  , ch(25),~
           at (06,49), fac(hex(8c)),   partdescr$               , ch(32),~
           at (07,02), "EFFECTIVE DATES From            to"     ,        ~
           at (07,23), fac(hfac$( 5)), from$                    , ch(10),~
           at (07,37), fac(lfac$( 1)), to$                      , ch(10),~
           at (07,49), "Last Changed MM/DD/YY by XXX",                   ~
           at (07,62), fac(hex(8c)), lastchanged$               , ch(08),~
           at (07,74), fac(hex(8c)), lastuser$                  , ch(03),~
           at (08,02), "Contract # / Text",                              ~
           at (08,21), fac(lfac$(2)),  text$                    , ch(40),~
                                                                         ~
           at (10,02), fac(hex(ac)), str(hdr1$),                         ~
           at (10,31), fac(hex(ac)), str(hdr2$),                         ~
           at (10,40), fac(hex(ac)), str(hdr3$),                         ~
           at (11,62), "!               ",                               ~
           at (12,62), "!               ",                               ~
           at (13,62), "!               ",                               ~
           at (14,62), "! Note: Discount",                               ~
           at (15,62), "! is applied to ",                               ~
           at (16,62), "! whatever unit ",                               ~
           at (17,62), "! price derived.",                               ~
           at (18,62), "!               ",                               ~
           at (19,62), "!               ",                               ~
           at (20,62), "!               ",                               ~
                                                                         ~
           at (11,04), fac(hex(8c))   , fromqty$( 1)            , ch(10),~
           at (11,15), "-",                                              ~
           at (11,17), fac(lfac$( 3)),    toqty$( 1)            , ch(10),~
           at (11,30), fac(lfac$( 4)),     disc$( 1)            , ch(07),~
           at (11,41), fac(lfac$( 5)),       pc$( 1)            , ch(01),~
           at (11,45), fac(lfac$( 6)),    price$( 1)            , ch(10),~
                                                                         ~
           at (12,04), fac(hex(8c))   , fromqty$( 2)            , ch(10),~
           at (12,15), "-",                                              ~
           at (12,17), fac(lfac$( 7)),    toqty$( 2)            , ch(10),~
           at (12,30), fac(lfac$( 8)),     disc$( 2)            , ch(07),~
           at (12,41), fac(lfac$( 9)),       pc$( 2)            , ch(01),~
           at (12,45), fac(lfac$(10)),    price$( 2)            , ch(10),~
                                                                         ~
           at (13,04), fac(hex(8c))   , fromqty$( 3)            , ch(10),~
           at (13,15), "-",                                              ~
           at (13,17), fac(lfac$(11)),    toqty$( 3)            , ch(10),~
           at (13,30), fac(lfac$(12)),     disc$( 3)            , ch(07),~
           at (13,41), fac(lfac$(13)),       pc$( 3)            , ch(01),~
           at (13,45), fac(lfac$(14)),    price$( 3)            , ch(10),~
                                                                         ~
           at (14,04), fac(hex(8c))   , fromqty$( 4)            , ch(10),~
           at (14,15), "-",                                              ~
           at (14,17), fac(lfac$(15)),    toqty$( 4)            , ch(10),~
           at (14,30), fac(lfac$(16)),     disc$( 4)            , ch(07),~
           at (14,41), fac(lfac$(17)),       pc$( 4)            , ch(01),~
           at (14,45), fac(lfac$(18)),    price$( 4)            , ch(10),~
                                                                         ~
           at (15,04), fac(hex(8c))   , fromqty$( 5)            , ch(10),~
           at (15,15), "-",                                              ~
           at (15,17), fac(lfac$(19)),    toqty$( 5)            , ch(10),~
           at (15,30), fac(lfac$(20)),     disc$( 5)            , ch(07),~
           at (15,41), fac(lfac$(21)),       pc$( 5)            , ch(01),~
           at (15,45), fac(lfac$(22)),    price$( 5)            , ch(10),~
                                                                         ~
           at (16,04), fac(hex(8c))   , fromqty$( 6)            , ch(10),~
           at (16,15), "-",                                              ~
           at (16,17), fac(lfac$(23)),    toqty$( 6)            , ch(10),~
           at (16,30), fac(lfac$(24)),     disc$( 6)            , ch(07),~
           at (16,41), fac(lfac$(25)),       pc$( 6)            , ch(01),~
           at (16,45), fac(lfac$(26)),    price$( 6)            , ch(10),~
                                                                         ~
           at (17,04), fac(hex(8c))   , fromqty$( 7)            , ch(10),~
           at (17,15), "-",                                              ~
           at (17,17), fac(lfac$(27)),    toqty$( 7)            , ch(10),~
           at (17,30), fac(lfac$(28)),     disc$( 7)            , ch(07),~
           at (17,41), fac(lfac$(29)),       pc$( 7)            , ch(01),~
           at (17,45), fac(lfac$(30)),    price$( 7)            , ch(10),~
                                                                         ~
           at (18,04), fac(hex(8c))   , fromqty$( 8)            , ch(10),~
           at (18,15), "-",                                              ~
           at (18,17), fac(lfac$(31)),    toqty$( 8)            , ch(10),~
           at (18,30), fac(lfac$(32)),     disc$( 8)            , ch(07),~
           at (18,41), fac(lfac$(33)),       pc$( 8)            , ch(01),~
           at (18,45), fac(lfac$(34)),    price$( 8)            , ch(10),~
                                                                         ~
           at (19,04), fac(hex(8c))   , fromqty$( 9)            , ch(10),~
           at (19,15), "-",                                              ~
           at (19,17), fac(lfac$(35)),    toqty$( 9)            , ch(10),~
           at (19,30), fac(lfac$(36)),     disc$( 9)            , ch(07),~
           at (19,41), fac(lfac$(37)),       pc$( 9)            , ch(01),~
           at (19,45), fac(lfac$(38)),    price$( 9)            , ch(10),~
                                                                         ~
           at (20,04), fac(hex(8c))   , fromqty$(10)            , ch(10),~
           at (20,15), "-",                                              ~
           at (20,17), fac(lfac$(39)),    toqty$(10)            , ch(10),~
           at (20,30), fac(lfac$(40)),     disc$(10)            , ch(07),~
           at (20,41), fac(lfac$(41)),       pc$(10)            , ch(01),~
           at (20,45), fac(lfac$(42)),    price$(10)            , ch(10),~
                                                                         ~
           at (21,02), fac(hex(a4)),   infomsg$                 , ch(79),~
           at (22,02), fac(hex(8c)),   pf$(1)                   , ch(79),~
           at (23,02), fac(hex(8c)),   pf$(2)                   , ch(79),~
           at (24,02), fac(hex(8c)),   pf$(3)                   , ch(79),~
               keys(str(pfkeys$())), key (keyhit%)

               if keyhit% <> 13 then L41470
                  call "MANUAL" ("CPRSPLIN")
                  goto L40270

L41470:        if keyhit% <> 15 then L41510
                  call "PRNTSCRN"
                  goto L40270

L41510:         if keyhit% <> 14 then goto L41650
                     plowkey$ = "S" & str(custnr$) & str(type$) &        ~
                                str(partnr$) & str(cat$) & all(hex(00))
                     call "PLOWCODE" (#1, plowkey$, " ", 9001%, 0.53,    ~
                                      f1%(1), plowhdr$(), 0, 0, incl(),  ~
                                      incl$(), "d", " ", #33, descr_map())
                     if f1%(1) = 0% then L40270
                          custnr$ = str(plowkey$, 2, 9)
                          type$   = str(plowkey$,11, 2)
                          partnr$ = str(plowkey$,13,25)
                          cat$    = str(plowkey$,38, 4)
                          from$   = str(plowkey$,42, 6)
                          keyhit% = 0%
                          return

L41650:         if keyhit% <> 3% or edit% = 1% then return
                     plowkey$ = "S" & all(hex(00))
                     call "PLOWCODE" (#1, plowkey$, " ", 9001%, 0.53,    ~
                                      f1%(1), plowhdr$(), 0, 0, incl(),  ~
                                      incl$(), "d", " ", #33, descr_map())
                     if f1%(1) <> 0% then gosub get_cprprice_data
                     lastuser$, lastchanged$ = " "
                     return

        setpf_input
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Next Customer Type  (4)Next Part Cate" &        ~
                     "gory (12)Deletions     (15)Print Screen"
            pf$(3) = "(3)Next Customer Code  (5)Next Part Code" &        ~
                     "     (14)Find Record   (16)Exit Program"
            str(pfkeys$()) =  hex(0001020304050c0d0e0f10)
            return

        setpf_edit
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Copy Price Set                       " &        ~
                     "(12)Delete Record      (16)Save Data   "
            str(pfkeys$()) =  hex(0001030c0d0f10)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests data for all items on screen. If SCREEN% = 1 then   *~
            * test is for the key elements; If SCREEN% = 2 then the     *~
            * data elements are tested.                                 *~
            *************************************************************

        deffn'151(screen%)
            errormsg$ = " "
            fieldnr%  = 0%
            init(hex(8c)) lfac$(), hfac$()
            if screen% = 2% then goto L50790


*        Test Data for CUSTOMER TYPE CODE / CUSTOMER NUMBER.
            if type$ = " " and custnr$ = " " then L50170 else L50200
L50170:         errormsg$ = "Customer Type & Code cannot both be blank."
L50180:         init (hex(81)) hfac$()
                return
L50200:     if type$ <> " " and custnr$ <> " " then L50210 else L50240
L50210:         errormsg$ = "Enter Customer Type -OR- Code, not both."
                goto L50180

L50240:     custdescr$ = " "
            if type$ = " " then goto L50340
                if type$  = "?" then type$ = " "
                plowkey$ = "CUS TYPES" & type$
                call "PLOWCODE" (#2, plowkey$, custdescr$, 9%, .3, f1%(2))
                if f1%(2) = 1% then goto L50320
                     errormsg$ = "Invalid Customer Type Code."
                     hfac$(1) = hex(81)  : return
L50320:         type$ = str(plowkey$,10) : goto L50400

L50340:         if custnr$ = "?" then custnr$ = " "
                call "GETCODE" (#3, custnr$, custdescr$, 0%, 4.3, f1%(3))
                if f1%(3) = 1 then goto L50400
                     errormsg$ = "Invalid Customer Code."
                     hfac$(2) = hex(81) : return

L50400
*        Test Data for PART CATEGORY CODE / PART NUMBER
            if cat$ = " " and partnr$ = " " then L50420 else L50450
L50420:         errormsg$ = "Part Category & Code cannot both be blank."
L50430:         hfac$(3), hfac$(4) = hex(81)
                return
L50450:     if cat$ <> " " and partnr$ <> " " then L50460 else L50490
L50460:         errormsg$ = "Enter Part Category -OR- Code, NOT both."
                goto L50430

L50490:     catdescr$, partdescr$ = " "
            if cat$ = " " then goto L50570
                if cat$ = "?" then cat$ = " "
                call "GETCODE" (#5, cat$, catdescr$, 0%, .3, f1%(5))
                if f1%(5) = 1% then goto L50630
                     errormsg$ = "Invalid Part Category Code."
                     hfac$(3) = hex(81) : return

L50570:         if partnr$ = "?" then partnr$ = " "
                call "GETCODE" (#6, partnr$, partdescr$, 0%, 2.3, f1%(6))
                if f1%(6) = 1% then goto L50630
                     errormsg$ = "Invalid Part Code."
                     hfac$(4)  = hex(81) : return

L50630: /* Test data for FROM date                                     */
            if from$ = " " then from$ = date$
            if from$ = "?" then goto L50680
            call "DATEOKC" (from$, date%, errormsg$)
            if errormsg$ = " " then return
L50680:         plowkey$ = "S" & str(custnr$) & str(type$) &             ~
                                 str(partnr$) & str(cat$) & all(hex(00))
                call "PLOWCODE" (#1, plowkey$, " ", 9041%, 0.53,         ~
                                      f1%(1%), plowhdr$(), 0, 0, incl(), ~
                                      incl$(), "d", " ", #33, descr_map())
                if f1%(1) = 1% then from$ = str(plowkey$, 42, 6)
                if f1%(1) = 1% then call "DATFMTC" (from$)
                if f1%(1) = 1% then return
                     errormsg$ = "Invalid entry for FROM date."
                     hfac$(5) = hex(81) : return


*       ********  S E C O N D   S C R E E N   S E C T I O N  **********
L50790: /* Test data for TO DATE                                       */
            if to$ = " " or to$ = blankdate$ then to$ = "20991231"
            call "DATEOKC" (to$, date%, errormsg$)
            if errormsg$ = " " then goto L50840
                lfac$(1) = hex(81) : return
L50840
*        Make sure that FROM DATE <= TO DATE
            call "DATUFMTC" (from$)
            call "DATUFMTC" (to$  )
            if from$ <= to$ then goto L50900
                errormsg$ = "FROM DATE cannot be less than TO DATE."
                lfac$(1)  = hex(81)
L50900:     call "DATFMTC" (from$)
            call "DATFMTC" (to$  )
            if errormsg$ <> " " then return

        /* Test data for TABLE ENTRIES                                 */
*        Let's make sure that an attempt was made to enter something
            for i% = 1% to 10% : if toqty$(i%)<> " " then L51040 : next i%
                errormsg$ = "You must enter at least one VOLUME range."
                init(hex(82)) lfac$()
                lfac$( 1), lfac$( 5), lfac$( 9), lfac$(13), lfac$(17),   ~
                                      lfac$(21), lfac$(25)      = hex(81)
                lfac$( 2)  = hex(80)
                return

L51040
*        Pack the array to the top (discard non-entries)
            for i% = 1% to 9%
                count% = 0%
L51070:         if toqty$(i%) <> " " then L51170
                     for i1% = i% to 9%
                          toqty$(i1%) = toqty$(i1%+1%)
                          pc$   (i1%) = pc$   (i1%+1%)
                          disc$ (i1%) = disc$ (i1%+1%)
                          price$(i1%) = price$(i1%+1%)
                     next i1%
                     toqty$(10) = " "
                     count% = count% + 1%
                     if count% < 10% - i% then L51070
L51170:     next i%
            for i% = 1% to 10%
                if toqty$(i%) <> " " then L51230
                     fromqty$(i%), price$(i%), pc$(i%), disc$(i%) = " "
                     toqty%(i%), disc(i%) = 0
                     price   (i%) = -1
L51230:     next i%

        for i% = 1% to 10%  :  if toqty$(i%) = " " then L51820
*        First Check the TO QUANTITY
            tran (str(toqty$(i%)), " ,") replacing
            call "SPCSMASH" (toqty$(i%))
            convert toqty$(i%) to temp, data goto L51310
            if temp <= 0 or temp > 99999999  then L51310
            toqty%(i%) = temp
            goto L51340
L51310:         errormsg$ = "Invalid numeric entry (Range to 99,999,999)."
L51320:         lfac$(i%*4-1) = hex(82)
                return
L51340:     if toqty%(i%) > 0% then L51360
                errormsg$ = "Quantities must be positives" : goto L51320
L51360:     convert toqty%(i%) to toqty$(i%), pic(##,###,###)
            if i% = 1% then L51420
                if toqty%(i%) > toqty%(i%-1%) then L51420
                     errormsg$ = "Ranges must be in ascending order."
                     goto L51320

L51420
*        Now see that another value has been entered
            if disc$(i%) <> " " or pc$(i%) <> " " or price$(i%) <> " "   ~
                                                              then L51510
                errormsg$ = "You must enter at least one of these fields."
                lfac$(i%*4  ) = hex(82)            /* Discount   */
                lfac$(i%*4+1) = hex(81)            /* Price Code */
                lfac$(i%*4+2) = hex(82)            /* Price      */
                return

L51510: /* Test data for DISCOUNT PERCENTAGE                           */
            if disc$(i%) = " " then disc$(i%) = "0"
            convert disc$(i%) to disc(i%), data goto L51560
            disc(i%) = round(disc(i%), 2)
            goto L51580
L51560:         errormsg$ = "Invalid entry for Discount Percentage."
                lfac$(i%*4)  =  hex(82) :  return
L51580:     if abs(disc(i%)) < 999 then goto L51610
                errormsg$ = "Discount can not be greater than 999."
                lfac$(i%*4)  = hex(82)  :  return
L51610:     convert disc(i%) to disc$(i%), pic(-##0.00)

        /* Test data for OVER-RIDE PRICE CODE                          */
            if pc$(i%) = " " then goto L51710
            price$(i%) = " " : price(i%) = -1  /* Can't enter both     */
                if (pc$(i%) >= "A" and pc$(i%) <= "Q")  or               ~
                   (pc$(i%) >= "0" and pc$(i%) <= "8")  then goto L51710
                     errormsg$ = "Price Code must be ' ',  A-Q, or 0-8."
                     lfac$(i%*4+1)  = hex(81)  :  return

L51710: /* Test data for UNIT PRICE OVERRIDE                           */
            if price$(i%) <> " " then L51740
                price(i%) = -1 : goto L51820
L51740:     convert price$(i%) to price(i%), data goto L51750 : goto L51770
L51750:         errormsg$ = "Illegal entry for unit price."
L51760:         lfac$(i%*4+2)  = hex(81)  :  return
L51770:     if price(i%) >= 0 then L51790
                errormsg$ = "Unit price may not be negative." :goto L51760
L51790:     if price(i%) > 9e10 then L51750
            call "CONVERT" (price(i%), 0.4, price$(i%))

L51820
*        Now concoct the 'From Range' Variable and do it again, Sam
            if i% = 1% then fromqty$(i%) = "         0"
            if i% = 1% then L51890
            if toqty$(i%) = " " then L51890
                temp% = toqty%(i%-1%) + 1%
                convert temp% to fromqty$(i%), pic(##,###,##0)

L51890: next i%
        return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files implicitly.  Routine also displays   *~
            * a message (only if in foreground) while returning to the  *~
            * appropriate next program (out of this program's control). *~
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

            end
