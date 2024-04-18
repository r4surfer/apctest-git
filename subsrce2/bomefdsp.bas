        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M  EEEEE  FFFFF  DDD    SSSS  PPPP     *~
            *  B   B  O   O  MM MM  E      F      D  D  S      P   P    *~
            *  BBBB   O   O  M M M  EEE    FFF    D   D   S    PPPP     *~
            *  B   B  O   O  M   M  E      F      D  D      S  P        *~
            *  BBBB    OOO   M   M  EEEEE  F      DDD   SSSS   P        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMEFDSP - Displays data about BOM effectivity.  Similar  *~
            *            to SETBOM without update capability.           *~
            *            CALENDAR.                                      *~
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
            * 09/10/93 ! ORIGINAL (Cloned from SETBOM)            ! JDH *~
            * 07/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "BOMEFDSP" (part$, #01, #02, #03, #04, #05, #06)

        dim                                                              ~
            blankline$79,                /* FOR SCREEN DISPLAY         */~
            boms$(490)3,                 /* BOMS IN EFFECT             */~
            bom$3,                       /* BOM ID FOR SET             */~
            calend$8,                    /* END OF PLANNING CALENDAR   */~
            calstart$8,                  /* START OF CALENDAR          */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dd%(490),                    /* PRODUCTION CALENDAR        */~
            dd$(31)3,                    /* DOW FOR DAY TO DAY DISPLAY */~
            descr$(2)18,                 /* FULL DATE DESCRIPTION      */~
            dow$(490)3,                  /* PRODUCTION CALENDAR        */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            end$8,                       /* END DATE FOR SET           */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lastpart$25,                 /* Last Part Displayed        */~
            line$(100)75,                /* SCREEN                     */~
            line2$79,                    /* Screen Line 2              */~
            mm%(490),                    /* PRODUCTION CALENDAR        */~
            month$(12)9,                 /* CALENDAR MONTHS DESCRIPTION*/~
            part$25,                     /* ENTER DESIRED PART CODE    */~
            partdescr$32,                /* ENTER DESIRED PART CODE    */~
            pfkey4$13,                   /* PREVIUOS SCREEN KEY        */~
            pfkey5$13,                   /* NEXT SCREEN KEY            */~
            pfkey16$16,                  /* Save/Return KEY            */~
            readkey$40,                  /* FOR PLOWS                  */~
            rtes$(490)3,                 /* RTES IN EFFECT             */~
            rte$3,                       /* BILLS ATTACHED ROUTE ID    */~
            seebom$(31)3,                /* DAY DISPLAY                */~
            seerte$(31)3,                /* DAY DISPLAY                */~
            start$8,                     /* START DATE FOR SET         */~
            title$75,                    /* FOR SCREEN DISPLAY         */~
            tdate$8,                     /* Temp. Date                 */~
            udate$8,                     /* Unfmted Text date          */~
            yy%(490),                    /* PRODUCTION CALENDAR        */~
            yymmdd$(490)6                /* PRODUCTION CALENDAR        */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

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

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! ENGMASTR ! Engineering Master Filer                 *~
            * #02 ! BOMMASTR ! BOM relationship file                    *~
            * #03 ! RTEMASTR ! Production routing master file           *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! CALMASTR ! Planning Production Calendar File        *~
            * #06 ! STCBOMXF ! Standard Cost Set / BOM-RTE X-Ref        *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if been_here_before% = 1% then L10000
                been_here_before% = 1%

            date$ = date
            call "DATEFMT" (date$)
            gosub load_calendar

            edtmessage$ = " "

            title$ = "BOM ID  (RTE)      Start Date                    En~
        ~d Date"
            blankline$ = " "

            month$(01%) = "January  "
            month$(02%) = "February "
            month$(03%) = "March    "
            month$(04%) = "April    "
            month$(05%) = "May      "
            month$(06%) = "June     "
            month$(07%) = "July     "
            month$(08%) = "August   "
            month$(09%) = "September"
            month$(10%) = "October  "
            month$(11%) = "November "
            month$(12%) = "December "

            str(line2$,62%) = "BOMEFDSP: " & str(cms2v$,,8%)

            lastpart$ = " "

L10000: REM *************************************************************~
            *       S T A R T S   T H E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES STARTING THE DISPLAY PROCESS.                     *~
            *************************************************************


            if lastpart$ = part$ then L11000
                lastpart$ = part$

            init(" ") errormsg$, inpmessage$, partdescr$, bom$,          ~
                      boms$(), line$(), rte$, start$, end$
            end$ = calend$
            start$ = date$

            call "DESCRIBE" (#4, part$, partdescr$, 0%, f1%(4%))
            gosub L30000

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            l% = home%
L11070:     gosub L41000
            errormsg$ = " "
                  if keyhit%  =  2% then l% = 0%
                  if keyhit%  =  4% then l% = max(0%, l% - 6%)
                  if keyhit%  =  5% then l% = min(max(0%, maxlines%-6%), ~
                                                                 l% + 6%)
                  if keyhit%  =  9% then gosub see_by_day
                  if keyhit%  = 16% then       end
            goto L11070

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

L30000: REM *************************************************************~
            *            L O A D    E N G I N E E R I N G               *~
            *                        D A T A                            *~
            * AND FORMAT THE MAIN SCREEN.                               *~
            *************************************************************

           display at(12,27), "Loading Effectivity Dates"

           call "READ100" (#1, str(part$,,25%) & "1001", f1%(1%))
                     if f1%(1%) <> 1% then goto L30370

           get #1, using L30120,   boms$()
L30120:    FMT XX(29), 490*CH(3)

*        FORMAT_SCREEN
           l%, x% = 1%
           line$() = " "        /* format LINE$() into a summary of */
           for i% = 2% to 491%           /* change activity */
            if i% = 491% then L30200
            if boms$(i%) = boms$(x%) then L30320
L30200:         gosub'99(x%, i%-1%)
                rte$ = " "
                readkey$ = str(part$,,25%) & str(boms$(x%),,3%) & "  0"
                call "READ100" (#2, readkey$, f1%(2%))
                     if f1%(2%) <> 0% then get #2, using L30250, rte$
L30250:              FMT POS(87), CH(3)    /* get the route */
                     if f1%(2%) = 0% then errormsg$ = "WARNING: BOM " &  ~
                          boms$(x%) & " IS *NOT* ON FILE"
                     if f1%(2%) = 0% and boms$(x%) = " " then errormsg$ =~
                "Warning: At least part of this schedule has *NO* BOM in ~
        ~effect."   /* The literal 'Warning:' is used later for PF Keys */
                put line$(l%), using L30600, boms$(x%), rte$, descr$(1%), ~
                                                            descr$(2%)
                for dl% = x% to i% - 1% : rtes$(dl%) = rte$ : next dl%
                x% = i%
                l% = min(l%+1%, 100%)
L30320:    next i%
           str(line$(home%+1),68%,8%) = hex(843e20) & "Today"
           maxlines% = l% - 1%
        return

L30370
*       * NOT ON FILE, TELL THEM ***************************************
           l%, x% = 1%
           gosub'99 (1%, 490%)
           put line$(1%), using L30600, " ", " ", descr$(1%), descr$(2%)
           line$(4%) = hex(84) & "Please Note The Following:"

           line$(5%)= "This part currently does not have any effectivity ~
        ~dates established."
           line$(6%)= "Unless a BOM is set as effective, Planning this pa~
        ~rt is NOT possible."
           start$ = calstart$ : end$ = calend$
           maxlines% = 1%
        return


L30600: %###     (###)      ##################            ###############~
        ~###

        deffn'99 (start%, end%)   /* FORMAT DATE RANGE */
            if today% >= start% and today% <= end% then home% = l% - 1%
            descr$(1%) = month$(mm%(start%))
            descr$(2%) = month$(mm%(end%))
            put str(descr$(1%), len(descr$(1%)) + 2%), using L30720,      ~
                               dd%(start%), yy%(start%)
            put str(descr$(2%), len(descr$(2%)) + 2%), using L30720,      ~
                               dd%(end%), yy%(end%)
        return
L30720: %##, ####

        rem**************************************************************~
            *      l o a d   c a l m a s t r    d a t a                 *~
            *                                                           *~
            * loads and formats the production calendar.                *~
            *************************************************************

        load_calendar
            call "READ100" (#5,  "10", f1%(5%))
                if f1%(5%) = 0% then bad_calendar
            get #5, using L33110, str(yymmdd$(),1%,1470%)
L33110:         FMT XX(2), CH(1470)

            call "READ100" (#5,  "11", f1%(5%))
                if f1%(5%) = 0% then bad_calendar
            get #5, using L33160, str(yymmdd$(),1471%,1470%)
L33160:         FMT XX(2), CH(1470)

            call "READ100" (#5,  "20", f1%(5%))
                if f1%(5%) = 0% then bad_calendar
            get #5, using L33210, yy%()
L33210:         FMT XX(2), 490*BI(4)

            call "READ100" (#5,  "30", f1%(5%))
                if f1%(5%) = 0% then bad_calendar
            get #5, using L33260, mm%()
L33260:         FMT XX(2), 490*BI(4)

            call "READ100" (#5,  "40", f1%(5%))
                if f1%(5%) = 0% then bad_calendar
            get #5, using L33310, dd%()
L33310:         FMT XX(2), 490*BI(4)

            call "READ100" (#5,  "50", f1%(5%))
                if f1%(5%) = 0% then bad_calendar
            get #5, using L33360, dow$()
L33360:         FMT XX(2), 490*CH(3)

            today% = 0%
            for i% = 1% to 490%
                if yymmdd$(i%) <> date then L33430
                     today% = i%
                     i% = 490%
L33430:     next i%

            calstart$ = yymmdd$(1%)
            calend$   = yymmdd$(490%)
            call "DATEFMT" (calstart$)
            call "DATEFMT" (calend$)
            if today% <> 0 then return

        bad_calendar
        stop " PRODUCTION CALENDAR IS INVALID. PRESS ENTER TO EXIT & FIX."
        goto L65000

L41000: REM *************************************************************~
            *      S H O W   S U M M A R Y  /  A L L O W   S E T        *~
            *                                                           *~
            * MAIN SCREEN.                                              *~
            *************************************************************
            pfkey4$, pfkey5$ = " "
            if l% <> 0% then pfkey4$ = "(4)Prev Dates"
            if line$(l%+11%) <> " " then pfkey5$ = "(5)Next Dates"

            pfkey16$ = "(16)Return"

L41060: accept                                                           ~
               at (01,22), "**** VIEW BOM EFFECTIVITY DATES ****",       ~
               at (02,02),                                               ~
        "+---------------------------------------------------------------~
        ~--------------+",                                                ~
               at (03,02),                                               ~
        "!Below Are Current/Past/Future BOMs In Effect.                  ~
        ~              !",                                                ~
               at (03,61), fac(hex(8c)), str(line2$,62%)        , ch(18),~
               at (04,02),                                               ~
        "!  (Use the PF KEYS defined below to review portions of structur~
        ~e not shown)  !",                                                ~
               at (05,02), "!", at (05,80), "!",                         ~
               at (06,02), "!", at (06,80), "!",                         ~
               at (05,04), fac(hex(84)), part$                  , ch(25),~
               at (05,30), fac(hex(84)), partdescr$             , ch(32),~
               at (05,64), "Date :",                                     ~
               at (05,71), fac(hex(8c)), date$                  , ch(08),~
               at (06,04), fac(hex(ac)), title$                 , ch(75),~
               at (07,02), "!", at(07,80), "!",                          ~
               at (08,02), "!", at(08,80), "!",                          ~
               at (09,02), "!", at(09,80), "!",                          ~
               at (10,02), "!", at(10,80), "!",                          ~
               at (11,02), "!", at(11,80), "!",                          ~
               at (12,02), "!", at(12,80), "!",                          ~
               at (13,02), "!", at(13,80), "!",                          ~
               at (14,02), "!", at(14,80), "!",                          ~
               at (15,02), "!", at(15,80), "!",                          ~
               at (16,02), "!", at(16,80), "!",                          ~
                                                                         ~
               at (07,04), fac(hex(8c)), line$(l% + 01%)        , ch(75),~
               at (08,04), fac(hex(8c)), line$(l% + 02%)        , ch(75),~
               at (09,04), fac(hex(8c)), line$(l% + 03%)        , ch(75),~
               at (10,04), fac(hex(8c)), line$(l% + 04%)        , ch(75),~
               at (11,04), fac(hex(8c)), line$(l% + 05%)        , ch(75),~
               at (12,04), fac(hex(8c)), line$(l% + 06%)        , ch(75),~
               at (13,04), fac(hex(8c)), line$(l% + 07%)        , ch(75),~
               at (14,04), fac(hex(8c)), line$(l% + 08%)        , ch(75),~
               at (15,04), fac(hex(8c)), line$(l% + 09%)        , ch(75),~
               at (16,04), fac(hex(8c)), line$(l% + 10%)        , ch(75),~
               at (07,43), "Thru...",                                    ~
                                                                         ~
               at (17,02),                                               ~
        "+---------------------------------------------------------------~
        ~--------------+",                                                ~
               at (18,02), "!  The Current Prod. Calendar Starts on",    ~
               at (18,42), fac(hex(84)), calstart$              , ch(08),~
               at (18,51), "And Continues Thru",                         ~
               at (18,70), fac(hex(84)), calend$                , ch(08),~
               at (18,80), "!",                                          ~
               at (19,02),                                               ~
        "+---------------------------------------------------------------~
        ~--------------+",                                                ~
               at (21,02), fac(hex(ac)), edtmessage$            , ch(79),~
               at (22,02), "(2)First Effect. Dates",                     ~
               at (22,35), "(9)Show By Day",                             ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfkey4$                , ch(13),~
               at (24,02), fac(hex(8c)), pfkey5$                , ch(13),~
               at (23,65), "(15)Print Screen",                           ~
               at (23,35), "(14)See BOMs On File",                       ~
               at (24,65), fac(hex(84)), pfkey16$               , ch(16),~
                                                                         ~
               keys(hex(ff020405090d0e0f10ffff)),                        ~
               key (keyhit%)

               if keyhit% <> 13% then L41780
                  call "MANUAL" ("BOMEFDSP")
                  goto L41060

L41780:        if keyhit% <> 14% then L41820
                  call "BOMSRTES" (part$, #4, #2, #3, #6)
                  goto L41060

L41820:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41060

        REM *************************************************************~
           *    S E E   C A P A C I T I E S   B Y   M O N T H           *~
           **************************************************************

        see_by_day

L42060:     tdate$ = date
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,5%,2%) to mo%
            convert str(udate$,1%,4%) to yr%
L42080:     f% = 0%
            for i% = 1% to 490%
                if yy%(i%) < yr% then L42150
                if yy%(i%) > yr% then goto L42160
                if mm%(i%) < mo% then L42150
                if mm%(i%) > mo% then goto L42160
                if f% = 0% then f% = i%
L42150:     next i%
L42160:     ld% = i% - 1%

            j% = 0
            init(" ")  dd$(), seebom$(), seerte$()

            for i% = f% to ld%
                j% = j% + 1%
                seebom$(j%) = boms$(i%)
                seerte$(j%) = rtes$(i%)
                dd$(j%) = dow$(i%)
            next i%

L42290: accept                                                           ~
               at (01,02), "BOM & RTE EFFECTIVE: ",                      ~
               at (01,23), fac(hex(84)), part$                  , ch(25),~
               at (01,49), "Month Of:",                                  ~
               at (01,58), fac(hex(84)), month$(mo%)            , ch(09),~
               at (01,71), fac(hex(84)), yr%, pic(####),                 ~
               at(02,16), fac(hex(84)), partdescr$, ch(32),              ~
               at (03,03),                                               ~
        "Day       BOM    RTE                  ! Day        BOM    RTE   ~
        ~    ",                                                           ~
               at (04,03), "01", at (04,41), "!  16",                    ~
               at (05,03), "02", at (05,41), "!  17",                    ~
               at (06,03), "03", at (06,41), "!  18",                    ~
               at (07,03), "04", at (07,41), "!  19",                    ~
               at (08,03), "05", at (08,41), "!  20",                    ~
               at (09,03), "06", at (09,41), "!  21",                    ~
               at (10,03), "07", at (10,41), "!  22",                    ~
               at (11,03), "08", at (11,41), "!  23",                    ~
               at (12,03), "09", at (12,41), "!  24",                    ~
               at (13,03), "10", at (13,41), "!  25",                    ~
               at (14,03), "11", at (14,41), "!  26",                    ~
               at (15,03), "12", at (15,41), "!  27",                    ~
               at (16,03), "13", at (16,41), "!  28",                    ~
               at (17,03), "14", at (17,41), "!  29",                    ~
               at (18,03), "15", at (18,41), "!  30",                    ~
                                 at (19,41), "!  31",                    ~
                                                                         ~
           at(04,13), fac(hex(84)), seebom$(01%)            ,ch(03)     ,~
           at(05,13), fac(hex(84)), seebom$(02%)            ,ch(03)     ,~
           at(06,13), fac(hex(84)), seebom$(03%)            ,ch(03)     ,~
           at(07,13), fac(hex(84)), seebom$(04%)            ,ch(03)     ,~
           at(08,13), fac(hex(84)), seebom$(05%)            ,ch(03)     ,~
           at(09,13), fac(hex(84)), seebom$(06%)            ,ch(03)     ,~
           at(10,13), fac(hex(84)), seebom$(07%)            ,ch(03)     ,~
           at(11,13), fac(hex(84)), seebom$(08%)            ,ch(03)     ,~
           at(12,13), fac(hex(84)), seebom$(09%)            ,ch(03)     ,~
           at(13,13), fac(hex(84)), seebom$(10%)            ,ch(03)     ,~
           at(14,13), fac(hex(84)), seebom$(11%)            ,ch(03)     ,~
           at(15,13), fac(hex(84)), seebom$(12%)            ,ch(03)     ,~
           at(16,13), fac(hex(84)), seebom$(13%)            ,ch(03)     ,~
           at(17,13), fac(hex(84)), seebom$(14%)            ,ch(03)     ,~
           at(18,13), fac(hex(84)), seebom$(15%)            ,ch(03)     ,~
           at(04,54), fac(hex(84)), seebom$(16%)            ,ch(03)     ,~
           at(05,54), fac(hex(84)), seebom$(17%)            ,ch(03)     ,~
           at(06,54), fac(hex(84)), seebom$(18%)            ,ch(03)     ,~
           at(07,54), fac(hex(84)), seebom$(19%)            ,ch(03)     ,~
           at(08,54), fac(hex(84)), seebom$(20%)            ,ch(03)     ,~
           at(09,54), fac(hex(84)), seebom$(21%)            ,ch(03)     ,~
           at(10,54), fac(hex(84)), seebom$(22%)            ,ch(03)     ,~
           at(11,54), fac(hex(84)), seebom$(23%)            ,ch(03)     ,~
           at(12,54), fac(hex(84)), seebom$(24%)            ,ch(03)     ,~
           at(13,54), fac(hex(84)), seebom$(25%)            ,ch(03)     ,~
           at(14,54), fac(hex(84)), seebom$(26%)            ,ch(03)     ,~
           at(15,54), fac(hex(84)), seebom$(27%)            ,ch(03)     ,~
           at(16,54), fac(hex(84)), seebom$(28%)            ,ch(03)     ,~
           at(17,54), fac(hex(84)), seebom$(29%)            ,ch(03)     ,~
           at(18,54), fac(hex(84)), seebom$(30%)            ,ch(03)     ,~
           at(19,54), fac(hex(84)), seebom$(31%)            ,ch(03)     ,~
                                                                         ~
           at(04,20), fac(hex(84)), seerte$(01%)            ,ch(03)     ,~
           at(05,20), fac(hex(84)), seerte$(02%)            ,ch(03)     ,~
           at(06,20), fac(hex(84)), seerte$(03%)            ,ch(03)     ,~
           at(07,20), fac(hex(84)), seerte$(04%)            ,ch(03)     ,~
           at(08,20), fac(hex(84)), seerte$(05%)            ,ch(03)     ,~
           at(09,20), fac(hex(84)), seerte$(06%)            ,ch(03)     ,~
           at(10,20), fac(hex(84)), seerte$(07%)            ,ch(03)     ,~
           at(11,20), fac(hex(84)), seerte$(08%)            ,ch(03)     ,~
           at(12,20), fac(hex(84)), seerte$(09%)            ,ch(03)     ,~
           at(13,20), fac(hex(84)), seerte$(10%)            ,ch(03)     ,~
           at(14,20), fac(hex(84)), seerte$(11%)            ,ch(03)     ,~
           at(15,20), fac(hex(84)), seerte$(12%)            ,ch(03)     ,~
           at(16,20), fac(hex(84)), seerte$(13%)            ,ch(03)     ,~
           at(17,20), fac(hex(84)), seerte$(14%)            ,ch(03)     ,~
           at(18,20), fac(hex(84)), seerte$(15%)            ,ch(03)     ,~
           at(04,61), fac(hex(84)), seerte$(16%)            ,ch(03)     ,~
           at(05,61), fac(hex(84)), seerte$(17%)            ,ch(03)     ,~
           at(06,61), fac(hex(84)), seerte$(18%)            ,ch(03)     ,~
           at(07,61), fac(hex(84)), seerte$(19%)            ,ch(03)     ,~
           at(08,61), fac(hex(84)), seerte$(20%)            ,ch(03)     ,~
           at(09,61), fac(hex(84)), seerte$(21%)            ,ch(03)     ,~
           at(10,61), fac(hex(84)), seerte$(22%)            ,ch(03)     ,~
           at(11,61), fac(hex(84)), seerte$(23%)            ,ch(03)     ,~
           at(12,61), fac(hex(84)), seerte$(24%)            ,ch(03)     ,~
           at(13,61), fac(hex(84)), seerte$(25%)            ,ch(03)     ,~
           at(14,61), fac(hex(84)), seerte$(26%)            ,ch(03)     ,~
           at(15,61), fac(hex(84)), seerte$(27%)            ,ch(03)     ,~
           at(16,61), fac(hex(84)), seerte$(28%)            ,ch(03)     ,~
           at(17,61), fac(hex(84)), seerte$(29%)            ,ch(03)     ,~
           at(18,61), fac(hex(84)), seerte$(30%)            ,ch(03)     ,~
           at(19,61), fac(hex(84)), seerte$(31%)            ,ch(03)     ,~
                                                                         ~
            at(04,06), fac(hex(84)),  dd$(01%)              ,ch(03)     ,~
            at(05,06), fac(hex(84)),  dd$(02%)              ,ch(03)     ,~
            at(06,06), fac(hex(84)),  dd$(03%)              ,ch(03)     ,~
            at(07,06), fac(hex(84)),  dd$(04%)              ,ch(03)     ,~
            at(08,06), fac(hex(84)),  dd$(05%)              ,ch(03)     ,~
            at(09,06), fac(hex(84)),  dd$(06%)              ,ch(03)     ,~
            at(10,06), fac(hex(84)),  dd$(07%)              ,ch(03)     ,~
            at(11,06), fac(hex(84)),  dd$(08%)              ,ch(03)     ,~
            at(12,06), fac(hex(84)),  dd$(09%)              ,ch(03)     ,~
            at(13,06), fac(hex(84)),  dd$(10%)              ,ch(03)     ,~
            at(14,06), fac(hex(84)),  dd$(11%)              ,ch(03)     ,~
            at(15,06), fac(hex(84)),  dd$(12%)              ,ch(03)     ,~
            at(16,06), fac(hex(84)),  dd$(13%)              ,ch(03)     ,~
            at(17,06), fac(hex(84)),  dd$(14%)              ,ch(03)     ,~
            at(18,06), fac(hex(84)),  dd$(15%)              ,ch(03)     ,~
            at(04,47), fac(hex(84)),  dd$(16%)              ,ch(03)     ,~
            at(05,47), fac(hex(84)),  dd$(17%)              ,ch(03)     ,~
            at(06,47), fac(hex(84)),  dd$(18%)              ,ch(03)     ,~
            at(07,47), fac(hex(84)),  dd$(19%)              ,ch(03)     ,~
            at(08,47), fac(hex(84)),  dd$(20%)              ,ch(03)     ,~
            at(09,47), fac(hex(84)),  dd$(21%)              ,ch(03)     ,~
            at(10,47), fac(hex(84)),  dd$(22%)              ,ch(03)     ,~
            at(11,47), fac(hex(84)),  dd$(23%)              ,ch(03)     ,~
            at(12,47), fac(hex(84)),  dd$(24%)              ,ch(03)     ,~
            at(13,47), fac(hex(84)),  dd$(25%)              ,ch(03)     ,~
            at(14,47), fac(hex(84)),  dd$(26%)              ,ch(03)     ,~
            at(15,47), fac(hex(84)),  dd$(27%)              ,ch(03)     ,~
            at(16,47), fac(hex(84)),  dd$(28%)              ,ch(03)     ,~
            at(17,47), fac(hex(84)),  dd$(29%)              ,ch(03)     ,~
            at(18,47), fac(hex(84)),  dd$(30%)              ,ch(03)     ,~
            at(19,47), fac(hex(84)),  dd$(31%)              ,ch(03)     ,~
                                                                         ~
            at (21,02), fac(hex(a4)), blankline$            ,ch(79)     ,~
            at (22,02), "(2)First Month   (4)Prev Month                ",~
            at (22,65), "(13)Instructions",                              ~
            at (24,02), "                 (5)Next Month",                ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), "(16)Return",                                    ~
                                                                         ~
                keys(hex(000204050d0f10)),                               ~
                key(keypressed%)

            if keypressed% = 16% then return
            if keypressed% = 0% then return
            if keypressed% = 2% then goto L42060

            if keypressed% <> 4% then goto L43730
                if f% = 1% then goto L42290
                mo% = mm%(f%-1%)
                yr% = yy%(f%-1%)
                goto L42080

L43730:     if keypressed% <> 5% then goto L43790
                if ld% = 490% then goto L42290
                mo% = mm%(ld%+1%)
                yr% = yy%(ld%+1%)
                goto L42080

L43790:     if keypressed% <> 13% then goto L43830
                call "MANUAL" ("BOMEFDSP")
                goto L42290

L43830:     if keypressed% <> 15% then goto L42290
                call "PRNTSCRN"
                goto L42290

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


            end
