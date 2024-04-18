        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      X   X  TTTTT  FFFFF  RRRR   IIIII  N   N   *~
            *  G      L       X X     T    F      R   R    I    NN  N   *~
            *  G GGG  L        X      T    FFFF   RRRR     I    N N N   *~
            *  G   G  L       X X     T    F      R   R    I    N  NN   *~
            *   GGG   LLLLL  X   X    T    F      R   R  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLXTFRIN - THIS PROGRAM MAINTAINS MANAGEMENT TRANSFER     *~
            *            FACTORS FOR MANAGEMENT REPORTING.  THESE       *~
            *            FACTORS ARE BY CUSTOMER TYPE/CODE, PART        *~
            *            CATEGORY/CODE, AND EFFECTIVITY DATE.           *~
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
            * 11/12/90 ! ORIGINAL                                 ! JDH *~
	    * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cat$4,                       /* Part Category Code         */~
            catdescr$32,                 /* Part Category Description  */~
            custdescr$32,                /* Type Descr or Cust Name    */~
            custnr$9,                    /* Customer Number            */~
            date$8,                      /* Date for Screen Display    */~
            ddate$8,                     /* Delete to date             */~
            descr_map(12),               /* PLOWCODE screen mapping    */~
            edate$6,                     /* Ending date for deletes    */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error Message              */~
            factor$6,                    /* Management Transfer Factor */~
            from$8,                      /* Effective date- From       */~
            hfac$(5)1,                   /* Header Field Facs          */~
            incl(1),                     /* PLOWCODE needs this        */~
            incl$(1)1,                   /* PLOWCODE needs this        */~
            infomsg$79,                  /* Input Message              */~
            key$50,                      /* Record Key field           */~
            lastuser$3, lastchanged$8,   /* Change Audit Trial         */~
            lfac$(2)1,                   /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            partnr$25,                   /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            pf$(3)79, pfkeys$(30)1,      /* PF Descrs and Keys         */~
            plowhdr$(3)79,               /* PLOWCODE Heading           */~
            plowkey$100,                 /* Misc use Plow key          */~
            to$8,                        /* Effective Date- To         */~
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
            * #01 ! MGTFCTR2 ! Management Transfer Factors File         *~
            * #02 ! GENCODES ! General Codes File                       *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            * #05 ! CATEGORY ! Inventory category codes file            *~
            * #06 ! HNYMASTR ! Inventory Master File                    *~
            * #33 ! DUMMY    ! Dummy for PLOWCODE                       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "MGTFCTR2",                                      ~
                        varc, indexed,                                   ~
                        recsize =  100,                                  ~
                        keypos =  1, keylen = 46

            select #02, "GENCODES",                                      ~
                        varc, indexed,                                   ~
                        recsize =  128,                                  ~
                        keypos =    1, keylen =  24

            select #03, "CUSTOMER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1200,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #05, "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =   4

            select #06, "HNYMASTR",                                      ~
                        varc, indexed,                                   ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #33, "DUMMY", varc, indexed,                          ~
                                 recsize = 5, keypos = 1, keylen = 1

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, 0%, f2%(1), 100%, " ")
            call "OPENCHCK" (#02, 0%, f2%(2),   0%, " ")
            call "OPENCHCK" (#03, 0%, f2%(3),   0%, " ")
            call "OPENCHCK" (#05, 0%, f2%(5),   0%, " ")
            call "OPENCHCK" (#06, 0%, f2%(6),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press (RETURN)."

            str(line2$,62) = "GLXTFRIN: " & cms2v$

            plowhdr$(1) = "  Customer  Type Part Code                 " &~
                          "Ctgy   From      To"

            incl(1) = 0 : incl$(1) = " "

            descr_map( 1) =  1.09   :   descr_map( 2) =  1.00
            descr_map( 3) = 10.02   :   descr_map( 4) = 12.00
            descr_map( 5) = 12.25   :   descr_map( 6) = 16.00
            descr_map( 7) = 37.04   :   descr_map( 8) = 42.00
            descr_map( 9) = 41.061  :   descr_map(10) = 47.00
            descr_map(11) = 47.061  :   descr_map(12) = 56.00

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Get Key for Record to be modified. Full Screen edit with  *~
            * field level error correction.                             *~
            *************************************************************

        inputmode
            init(" ") errormsg$, factor$, from$, to$, lastchanged$,      ~
                      lastuser$
            call "ALLFREE"
            if str(infomsg$,,4) <> "NOTE" then infomsg$ = " "

            fieldnr% = 0% : screen% = 1%

L10180:     gosub'101(fieldnr%)          /* Get Data Fields            */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then gosub next_cust_type
                if keyhit%  =  3% then gosub next_cust_code
                if keyhit%  =  4% then gosub next_part_ctgy
                if keyhit%  =  5% then gosub next_part_code
                if keyhit%  = 12% then goto  delete_expired
                if keyhit%  = 16% then goto  L65000
                if keyhit% <>  0% then       L10180
            gosub'151(screen%)           /* Test fields                */
                if errormsg$ <> " " then L10180

            gosub load_data              /* Get record specified       */
            goto  L11000

        next_cust_code
            infomsg$, type$, custdescr$ = " "
            call "READ102" (#03, custnr$, f1%(3))
            if f1%(3) = 1% then goto L10380
                infomsg$ = "At End of Customer File." : return
L10380:     get #03 using L10390, custnr$, custdescr$
L10390:         FMT CH(9), CH(30)
            return

        next_part_code
            infomsg$, cat$, catdescr$ = " "
            call "READ102" (#06, partnr$, f1%(6))
            if f1%(6) = 1% then goto L10470
                infomsg$ = "At End of Parts Master File." : return
L10470:     get #06 using L10480, partnr$, partdescr$
L10480:         FMT CH(25), CH(32)
            return

        next_cust_type
            infomsg$, custnr$, custdescr$ = " "
            plowkey$ = "CUS TYPES" & type$
            call "READ102" (#02, plowkey$, f1%(2))
            if f1%(2) = 1% then goto L10570
L10560:         infomsg$ = "At End of Customer Type Codes File." : return
L10570:     get #02 using L10580, plowkey$
L10580:         FMT CH(9)
            if str(plowkey$,,9) <> "CUS TYPES" then goto L10560
            get #2 using L10610, type$, custdescr$
L10610:         FMT XX(9), CH(15), CH(30)
            return

        next_part_ctgy
            infomsg$, partnr$, partdescr$ = " "
            call "READ102" (#05, cat$, f1%(5))
            if f1%(5) = 1% then goto L10690
                infomsg$ = "At end of Part Category Codes File." : return
L10690:     get #05 using L10700, cat$, catdescr$
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
                            "Press PF-28 to Delete the above record",    ~
                            "-or-", "Press (RETURN) to cancel delete.")
            if keyhit1% <> 28% then return
                call "DATUNFMT" (from$)
                key$ = str(custnr$) & str(type$) & str(partnr$) &        ~
                             str(cat$) & from$
                call "DELETE" (#01, key$, 46%)
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
               at (01,02), "Management Transfer Factors: DELETE RECORDS",~
               at (01,67), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "DELETE THRU DATE: ",                         ~
               at (06,22), fac(hex(81)), ddate$,                         ~
                                                                         ~
               at(08,02), "All records that have 'TO' dates on or      ",~
               at(09,02), "before the DELETE THRU DATE will be removed ",~
               at(10,02), "from the Transfer Factors file if you choose",~
               at(11,02), "to proceed.                                 ",~
               at(21,02), fac(hex(ac)), infomsg$,                        ~
               at(22,02), "(RETURN) Test date entered.",                 ~
               at(22,65), "(13)Instructions",                            ~
               at(23,65), "(15)Print Screen",                            ~
               at(24,44), "(28)DELETE Records",                          ~
               at(24,65), "(16)ABORT Delete",                            ~
                     keys(hex(000d0f101c)),                              ~
                     key (keyhit%)

                if keyhit% <> 13% then L15390
                     call "MANUAL" ("GLXTFRIN") : goto L15120

L15390:         if keyhit% <> 15% then L15420
                     call "PRNTSCRN"  :  goto L15120

L15420:     if keyhit% = 16% then goto inputmode
            infomsg$, errormsg$ = " "
            call "DATEOKC" (ddate$, ret%, errormsg$)
            if errormsg$ <> " " then goto L15120
            if keyhit% = 28% then goto L15530
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

            key$ = hex(00)
L15600:     call "READ102" (#01, key$, f1%(1))
            if f1%(1) = 1% then goto L15650
                convert deleted% to infomsg$, pic(####0)
                infomsg$ = "NOTE: " & infomsg$ & " RECORDS WERE DELETED."
                goto inputmode
L15650:     get #01 using L15660, key$, edate$
L15660:         FMT CH(46), CH(6)
            if edate$ > ddate$ then goto L15600
                print at(21,02), key$
                call "DELETE" (#01, key$, 46%)
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
            call "DATUNFMT" (from$)
            key$ = str(custnr$) & str(type$) & str(partnr$) & str(cat$)  ~
                                                             & str(from$)
            call "READ100" (#01, key$, f1%(1))
            call "DATEFMT" (from$)
            infomsg$ = "New Record."
            if f1%(1) = 0% then return

            infomsg$ = "Existing Record."
        get_data
            get #01 using L30180, to$, lastuser$, lastchanged$, factor
L30180:         FMT XX(46), CH(6), CH(3), CH(6), PD(14,4)
            call "DATEFMT" (to$)
            call "DATEFMT" (lastchanged$)
            call "CONVERT" (factor, -2.2, factor$)
            return


        REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write data to disk.                                       *~
            *************************************************************
        save_data
            call "DATUNFMT" (to$  )
            call "DATUNFMT" (from$)
            key$ = str(custnr$) & str(type$) & str(partnr$) &            ~
                                                   str(cat$) & str(from$)
            call "READ101" (#01, key$, f1%(1))
            put #01 using L31130, key$, to$, userid$, date, factor, " "
L31130:         FMT CH(46), CH(6), CH(3), CH(6), PD(14,4), CH(31)
            if f1%(1) = 0% then  write #01  else  rewrite #01
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
                lfac$( 1)  = hex(81)
                lfac$( 2)  = hex(82)


L40270: accept                                                           ~
           at (01,02), "Management Transfer Factors",                    ~
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
           at (07,02), "EFFECTIVE DATES    From          to"    ,        ~
           at (07,26), fac(hfac$( 5)), from$                    , ch(08),~
           at (07,38), fac(lfac$( 1)), to$                      , ch(08),~
           at (07,49), "Last Changed MM/DD/YY by XXX",                   ~
           at (07,62), fac(hex(8c)), lastchanged$               , ch(08),~
           at (07,74), fac(hex(8c)), lastuser$                  , ch(03),~
                                                                         ~
           at (09,02), "Transfer Factor",                                ~
           at (09,21), fac(lfac$( 2)), factor$                  , ch(06),~
                                                                         ~
           at (21,02), fac(hex(a4)),   infomsg$                 , ch(79),~
           at (22,02), fac(hex(8c)),   pf$(1)                   , ch(79),~
           at (23,02), fac(hex(8c)),   pf$(2)                   , ch(79),~
           at (24,02), fac(hex(8c)),   pf$(3)                   , ch(79),~
               keys(str(pfkeys$())), key (keyhit%)

               if keyhit% <> 13% then L41470
                  call "MANUAL" ("GLXTFRIN")
                  goto L40270

L41470:        if keyhit% <> 15% then L41510
                  call "PRNTSCRN"
                  goto L40270

L41510:         if keyhit% <> 14% then goto L41650
                     plowkey$ = str(custnr$) & str(type$) &              ~
                                str(partnr$) & str(cat$)
                     call "PLOWCODE" (#01, plowkey$, " ", 9000%, 0.53,   ~
                                      f1%(1), plowhdr$(), 0, 0, incl(),  ~
                                      incl$(), "d", " ", #33, descr_map())
                     if f1%(1) = 0% then L40270
                          custnr$ = str(plowkey$, 1, 9)
                          type$   = str(plowkey$,10, 2)
                          partnr$ = str(plowkey$,12,25)
                          cat$    = str(plowkey$,37, 4)
                          from$   = str(plowkey$,41, 6)
                          call "DATEFMT" (from$)
                          keyhit% = 0%
                          return

L41650:         if keyhit% <> 3% or edit% = 1% then return
                     plowkey$ = hex(00)
                     call "PLOWCODE" (#01, plowkey$, " ", 9000%, 0.53,   ~
                                      f1%(1), plowhdr$(), 0, 0, incl(),  ~
                                      incl$(), "d", " ", #33, descr_map())
                     if f1%(1) <> 0% then gosub get_data
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
            pf$(3) = "(3)Copy                                 " &        ~
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
                custdescr$ = hex(06) & "Select Customer Type Code."
                call "PLOWCODE" (#02, plowkey$, custdescr$, 9%, .3,      ~
                                                                  f1%(2))
                if f1%(2) = 1% then goto L50320
                     errormsg$ = "Invalid Customer Type Code."
                     hfac$(1) = hex(81)  : return
L50320:         type$ = str(plowkey$,10) : goto L50400

L50340:         if custnr$ = "?" then custnr$ = " "
                custdescr$ = hex(0684) & "Select Customer Code."
                call "GETCODE" (#03, custnr$, custdescr$, 0%, 4.3, f1%(3))
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
                catdescr$ = hex(0684) & "Select Part Category Code."
                call "GETCODE" (#05, cat$, catdescr$, 0%, .3, f1%(5))
                if f1%(5) = 1% then goto L50630
                     errormsg$ = "Invalid Part Category Code."
                     hfac$(3) = hex(81) : return

L50570:         if partnr$ = "?" then partnr$ = " "
                partdescr$ = hex(0684) & "Select Part Code."
                call "GETCODE" (#06, partnr$, partdescr$, 0%, 2.3, f1%(6))
                if f1%(6) = 1% then goto L50630
                     errormsg$ = "Invalid Part Code."
                     hfac$(4)  = hex(81) : return

L50630: /* Test data for FROM date                                     */
            if from$ = " " then from$ = date$
            if from$ = "?" then goto L50680
            call "DATEOKC" (from$, date%, errormsg$)
            if errormsg$ = " " then return
L50680:         plowkey$ = str(custnr$) & str(type$) & str(partnr$) &    ~
                                                                str(cat$)
                call "PLOWCODE" (#01, plowkey$, " ", 40%, 0, f1%(1))
                if f1%(1) = 1% then from$ = str(plowkey$, 41, 6)
                if f1%(1) = 1% then call "DATEFMT" (from$)
                if f1%(1) = 1% then return
                     errormsg$ = "Invalid entry for FROM date."
                     hfac$(5) = hex(81) : return


*       ********  S E C O N D   S C R E E N   S E C T I O N  **********
L50790: /* Test data for TO DATE                                       */
            if to$ = " " then to$ = "20991231"
            call "DATEOKC" (to$, date%, errormsg$)
            if errormsg$ = " " then goto L50840
                lfac$(1) = hex(81) : return
L50840
*        Make sure that FROM DATE <= TO DATE
            call "DATUNFMT" (from$)
            call "DATUNFMT" (to$  )
            if from$ <= to$ then goto L50900
                errormsg$ = "FROM DATE cannot be less than TO DATE."
                lfac$(1)  = hex(81)
L50900:     call "DATEFMT" (from$)
            call "DATEFMT" (to$  )
            if errormsg$ <> " " then return

*        Now test that the Transfer Tactor is OK
            call "NUMTEST" (factor$, -99.99, 99.99, errormsg$, 2.2,      ~
                            factor)
            lfac$(2) = hex(82)
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
