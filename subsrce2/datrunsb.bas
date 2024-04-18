        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  DDDD    AAA   TTTTT  RRRR   U   U  N   N   SSS   BBBB    *~
            *  D   D  A   A    T    R   R  U   U  NN  N  S      B   B   *~
            *  D   D  AAAAA    T    RRRR   U   U  N N N   SSS   BBBB    *~
            *  D   D  A   A    T    R   R  U   U  N  NN      S  B   B   *~
            *  DDDD   A   A    T    R   R   UUU   N   N   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATRUNSB - ALLOWS ENTRY OF DUE DATE AND QUANTITIES FOR    *~
            *            CREATION OF MULTIPLE SALES ORDER LINES FOR THE *~
            *            PART PASSED.  CHANNELS PASSED ALLOW CALL TO    *~
            *            PIPATCSB FOR ATC INFORMATION DURING ENTRY.     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/23/94 ! ORIGINAL                                 ! MLJ *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "DATRUNSB" (customer$,    /* Customer Number               */~
                        so$,          /* Sales Order Number            */~
                        cl%,          /* Next SO Seq #                 */~
                        c%,           /* Next SO Line #                */~
                        part$,        /* Part Number                   */~
                        orderdate$,   /* SO Order Date                 */~
                        dr_date$(),   /* Due Date Array Passed Back    */~
                        dr_qty$(),    /* Quantity Array Passed back    */~
                        dr_ship$(),   /* Ship Date Array Passed Back   */~
                        offset%,      /* Ship Date Offset (BCKFLAGS)   */~
                        #24,          /* PIPMASTR                      */~
                        #04,          /* HNYMASTR                      */~
                        #28,          /* SFCUM2                        */~
                        #19,          /* CALMASTR                      */~
                        #25,          /* PIPIN                         */~
                        #26,          /* PIPOUT                        */~
                        #29,          /* HNYDETAL                      */~
                        #18,          /* DEMMASTR                      */~
                        #27,          /* PIPCROSS                      */~
                        ret%)         /* 16% = Create Multiple Lines   */~
                                      /* 32% = Abort Multi Line Create */
        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            customer$9,                  /* Custoimer Number           */~
            blankdate$8,                 /* Blank date for comparison  */~
            date$8,                      /* Current Date               */~
            descr$34,                    /* Part Number                */~
            dr_date$(36)8,               /* Due Date Array             */~
            dr_qty$(36)10,               /* Quantity Array             */~
            dr_ship$(36)8,               /* Ship Date Array            */~
            drfac$(2)1,                  /* Field Attribute Characters */~
            errormsg$79,                 /* Error message              */~
            excess$3,                    /* # of Excess Lines          */~
            inpmessage$79,               /* Informational Message      */~
            i$(24)80,                    /* Screen Image               */~
            line$3,                      /* Copying From Line #        */~
            line2$79,                    /* Screen Line #2             */~
            od$8,                        /* Formatted Order Date       */~
            offset$3,                    /* Offset Days                */~
            orderdate$8,                 /* SO Order Date              */~
            partdescr$47,                /* Part & Description         */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkey$32,                    /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            weekdayh$9                   /* Ship Date Day              */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

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
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            convert cl%-1% to line$, pic(###)

            line2$ = "Customer: " & str(customer$) & " Sales Order: "   &~
                                                               str(so$)
            str(line2$,62%) = "DATRUNSB: " & str(cms2v$,,8%)

            call "DESCRIBE" (#04, str(part$), descr$, 1%, f1%(4%))
                if f1%(4%) = 0% then ns% = 1%
            if ns% = 1% then descr$ = "(Non-Stocked)"
            partdescr$ = part$ & " " & descr$

        REM *************************************************************~
            *               I N P U T   M O D E                         *~
            *-----------------------------------------------------------*~
            * Handles normal input for Dates and Quanntities.           *~
            *************************************************************

        inputmode
            init(" ") dr_date$(), dr_qty$(), dr_ship$(), errormsg$
            done%, ret% = 0%
L10090:     edit% = 1%

L10110:     gosub'101(edit%)
                if keyhit% =  1% then inputmode
                if keyhit% = 32% then abort_daterun
                if keyhit% <> 0% then L10110
            gosub L50000
                if errormsg$ <> " " then L10110


        editmode
            edit% = 2%
            gosub'101(edit%)
                if keyhit% =  1% then inputmode
                if keyhit% = 16% then exit_program
                if keyhit% =  0% then L10090
            goto editmode

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

        REM *************************************************************~
            *            D A T E   R U N   S C R E E N                  *~
            *-----------------------------------------------------------*~
            * Due dates & qtys for auto-creation of multiple SO lines.  *~
            *************************************************************

        deffn'101(edit%)
            if edit% <> 1% then L40110
                drfac$(1%) = hex(81)
                drfac$(2%) = hex(82)
                goto L40120
L40110:     init(hex(8c)) drfac$()
L40120:     gosub setpf1

L40140:     accept                                                       ~
               at (01,02), "Sales Order Management: Multiple Line Entry",~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Copying From Line:",                         ~
               at (05,22), fac(hex(8c)), line$                  , ch(03),~
               at (05,27), "Part:",                                      ~
               at (05,33), fac(hex(8c)), partdescr$             , ch(47),~
                                                                         ~
               at (07,02), "Due Date",                                   ~
               at (07,12), "Quantity",                                   ~
               at (07,23), "Due Date",                                   ~
               at (07,33), "Quantity",                                   ~
               at (07,44), "Due Date",                                   ~
               at (07,54), "Quantity",                                   ~
                                                                         ~
               at (08,02), fac(drfac$(1%)),  dr_date$( 1%)      , ch(08),~
               at (08,12), fac(drfac$(2%)),  dr_qty$( 1%)       , ch(10),~
               at (08,23), fac(drfac$(1%)),  dr_date$( 2%)      , ch(08),~
               at (08,33), fac(drfac$(2%)),  dr_qty$( 2%)       , ch(10),~
               at (08,44), fac(drfac$(1%)),  dr_date$( 3%)      , ch(08),~
               at (08,54), fac(drfac$(2%)),  dr_qty$( 3%)       , ch(10),~
                                                                         ~
               at (09,02), fac(drfac$(1%)),  dr_date$( 4%)      , ch(08),~
               at (09,12), fac(drfac$(2%)),  dr_qty$( 4%)       , ch(10),~
               at (09,23), fac(drfac$(1%)),  dr_date$( 5%)      , ch(08),~
               at (09,33), fac(drfac$(2%)),  dr_qty$( 5%)       , ch(10),~
               at (09,44), fac(drfac$(1%)),  dr_date$( 6%)      , ch(08),~
               at (09,54), fac(drfac$(2%)),  dr_qty$( 6%)       , ch(10),~
                                                                         ~
               at (10,02), fac(drfac$(1%)),  dr_date$( 7%)      , ch(08),~
               at (10,12), fac(drfac$(2%)),  dr_qty$( 7%)       , ch(10),~
               at (10,23), fac(drfac$(1%)),  dr_date$( 8%)      , ch(08),~
               at (10,33), fac(drfac$(2%)),  dr_qty$( 8%)       , ch(10),~
               at (10,44), fac(drfac$(1%)),  dr_date$( 9%)      , ch(08),~
               at (10,54), fac(drfac$(2%)),  dr_qty$( 9%)       , ch(10),~
                                                                         ~
               at (11,02), fac(drfac$(1%)),  dr_date$(10%)      , ch(08),~
               at (11,12), fac(drfac$(2%)),  dr_qty$(10%)       , ch(10),~
               at (11,23), fac(drfac$(1%)),  dr_date$(11%)      , ch(08),~
               at (11,33), fac(drfac$(2%)),  dr_qty$(11%)       , ch(10),~
               at (11,44), fac(drfac$(1%)),  dr_date$(12%)      , ch(08),~
               at (11,54), fac(drfac$(1%)),  dr_qty$(12%)       , ch(10),~
                                                                         ~
               at (12,02), fac(drfac$(1%)),  dr_date$(13%)      , ch(08),~
               at (12,12), fac(drfac$(2%)),  dr_qty$(13%)       , ch(10),~
               at (12,23), fac(drfac$(1%)),  dr_date$(14%)      , ch(08),~
               at (12,33), fac(drfac$(2%)),  dr_qty$(14%)       , ch(10),~
               at (12,44), fac(drfac$(1%)),  dr_date$(15%)      , ch(08),~
               at (12,54), fac(drfac$(2%)),  dr_qty$(15%)       , ch(10),~
                                                                         ~
               at (13,02), fac(drfac$(1%)),  dr_date$(16%)      , ch(08),~
               at (13,12), fac(drfac$(2%)),  dr_qty$(16%)       , ch(10),~
               at (13,23), fac(drfac$(1%)),  dr_date$(17%)      , ch(08),~
               at (13,33), fac(drfac$(2%)),  dr_qty$(17%)       , ch(10),~
               at (13,44), fac(drfac$(1%)),  dr_date$(18%)      , ch(08),~
               at (13,54), fac(drfac$(2%)),  dr_qty$(18%)       , ch(10),~
                                                                         ~
               at (14,02), fac(drfac$(1%)),  dr_date$(19%)      , ch(08),~
               at (14,12), fac(drfac$(2%)),  dr_qty$(19%)       , ch(10),~
               at (14,23), fac(drfac$(1%)),  dr_date$(20%)      , ch(08),~
               at (14,33), fac(drfac$(2%)),  dr_qty$(20%)       , ch(10),~
               at (14,44), fac(drfac$(1%)),  dr_date$(21%)      , ch(08),~
               at (14,54), fac(drfac$(2%)),  dr_qty$(21%)       , ch(10),~
                                                                         ~
               at (15,02), fac(drfac$(1%)),  dr_date$(22%)      , ch(08),~
               at (15,12), fac(drfac$(2%)),  dr_qty$(22%)       , ch(10),~
               at (15,23), fac(drfac$(1%)),  dr_date$(23%)      , ch(08),~
               at (15,33), fac(drfac$(2%)),  dr_qty$(23%)       , ch(10),~
               at (15,44), fac(drfac$(1%)),  dr_date$(24%)      , ch(08),~
               at (15,54), fac(drfac$(2%)),  dr_qty$(24%)       , ch(10),~
                                                                         ~
               at (16,02), fac(drfac$(1%)),  dr_date$(25%)      , ch(08),~
               at (16,12), fac(drfac$(2%)),  dr_qty$(25%)       , ch(10),~
               at (16,23), fac(drfac$(1%)),  dr_date$(26%)      , ch(08),~
               at (16,33), fac(drfac$(2%)),  dr_qty$(26%)       , ch(10),~
               at (16,44), fac(drfac$(1%)),  dr_date$(27%)      , ch(08),~
               at (16,54), fac(drfac$(2%)),  dr_qty$(27%)       , ch(10),~
                                                                         ~
               at (17,02), fac(drfac$(1%)),  dr_date$(28%)      , ch(08),~
               at (17,12), fac(drfac$(2%)),  dr_qty$(28%)       , ch(10),~
               at (17,23), fac(drfac$(1%)),  dr_date$(29%)      , ch(08),~
               at (17,33), fac(drfac$(2%)),  dr_qty$(29%)       , ch(10),~
               at (17,44), fac(drfac$(1%)),  dr_date$(30%)      , ch(08),~
               at (17,54), fac(drfac$(2%)),  dr_qty$(30%)       , ch(10),~
                                                                         ~
               at (18,02), fac(drfac$(1%)),  dr_date$(31%)      , ch(08),~
               at (18,12), fac(drfac$(2%)),  dr_qty$(31%)       , ch(10),~
               at (18,23), fac(drfac$(1%)),  dr_date$(32%)      , ch(08),~
               at (18,33), fac(drfac$(2%)),  dr_qty$(32%)       , ch(10),~
               at (18,44), fac(drfac$(1%)),  dr_date$(33%)      , ch(08),~
               at (18,54), fac(drfac$(2%)),  dr_qty$(33%)       , ch(10),~
                                                                         ~
               at (19,02), fac(drfac$(1%)),  dr_date$(34%)      , ch(08),~
               at (19,12), fac(drfac$(2%)),  dr_qty$(34%)       , ch(10),~
               at (19,23), fac(drfac$(1%)),  dr_date$(35%)      , ch(08),~
               at (19,33), fac(drfac$(2%)),  dr_qty$(35%)       , ch(10),~
               at (19,44), fac(drfac$(1%)),  dr_date$(36%)      , ch(08),~
               at (19,54), fac(drfac$(2%)),  dr_qty$(36%)       , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 8% then L41280
                   call "PIPATCSB" (part$, #24, #04, #28, #19, #25, #26, ~
                                    #29, #18, #27)
                   goto L40140

L41280:        if keyhit% <> 13% then L41320
                   call "MANUAL" ("DATRUNSB")
                   goto L40140

L41320:        if keyhit% <> 15% then L41360
                   call "PRNTSCRN"
                   goto L40140

L41360:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               return

        setpf1
           if edit% = 2% then L42140
               inpmessage$ = "Enter Due Dates and Quantities for Multip"&~
                             "le Sales Order Lines."
           pf$(1%) ="(1)Restart                                        "&~
                    "             (13)Instructions"
           pf$(2%) ="                  (8)See ATC                      "&~
                    "             (15)Print Screen"
           pf$(3%) ="                                                  "&~
                    "             (32)Abort       "

           pfkey$ = hex(01080d0f2000)
           return

L42140:    pf$(1%) ="(1)Restart                                        "&~
                    "             (13)Instructions"
           pf$(2%) ="                                                  "&~
                    "             (15)Print Screen"
           pf$(3%) ="                                                  "&~
                    "             (16)Create Lines"

           pfkey$ = hex(01ff0d0f1000)
           inpmessage$ = "Press RETURN for Full Screen Edit Capability."

           return

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        REM Get info which is common to all So lines...
            clines% = 0%  :  offset = 1
            if done% = 1% then L50350

            REM Is there an offset for ship date calculation? ...
                if offset% > 0% then L50210
L50120:         u3% = 2%
                call "ASKNUMBR" (u3%, "***  OFFSET DAYS ***",            ~
                     "Ship Date offset is blank or 0 in Sales Order Beh"&~
                     "avior Flags.", 1, 999, offset, 0.0)
                if u3% = 1% then abort_daterun
                    if u3% <> 0% then L50120
                        call "CONVERT" (offset, 0.0, offset$)
                        convert offset$ to offset%, data goto L50210

L50210:     REM Is this a non-stocked part? ...
                if ns% = 1% then L50350

            REM Get part minimum quantity and minimum increment...,
                minsoqty, minsoinc = 0
                get #4 using L50270, minsoqty, minsoinc
L50270:             FMT POS(706), 2*PD(14,4)

            REM Prepare Original Order Date...
                od$ = orderdate$
                call "DATUNFMT" (od$)

                done% = 1%

L50350: REM Test Due Dates, Quantities & Calc Ship Date...
            for i% = 1% to 36%
                if (dr_date$(i%) = " " or dr_date$(i%) = blankdate$) ~
                    and dr_qty$ (i%) = " " then L50890

                call "DATEOK" (dr_date$(i%), u3%, errormsg$)
                    if errormsg$ <> " " then return

                call "DATUNFMT" (dr_date$(i%))
                if dr_date$(i%) >= od$ then L50510
                call "DATEFMT" (dr_date$(i%))
                    errormsg$ = "Due Date: " & dr_date$(i%)  &           ~
                                " must be on or before " & orderdate$
                    return

L50510:         call "DATEFMT" (dr_date$(i%))
                convert dr_qty$(i%) to dr_qty, data goto L50540
                goto L50570
L50540:             errormsg$ = "Invalid Order Quantity: " & dr_qty$(i%)
                    return

L50570:         if dr_qty >= 0 then L50620
                    errormsg$ = "Order Quantity can't be negative: " &   ~
                                dr_qty$(i%)
                    return

L50620:         call "CONVERT" (dr_qty,-2.2, dr_qty$(i%))
                call "BCKMINSB" (dr_qty, minsoqty, minsoinc, u3%)
                    if u3% =  16% then L50680
                        errormsg$ = hex(00)
                        return

L50680:         call "DATUNFMT" (dr_date$(i%))
                call "DATE" addr ("G+", dr_date$(i%), -offset%,          ~
                                                     dr_ship$(i%) , u3%)
                call "DATE" addr ("GD", str(dr_ship$(i%),,6%),           ~
                                                         weekdayh$, u3%)
                call "DATEFMT" (dr_date$(i%))
                if weekdayh$ <> "SATURDAY" and  weekdayh$ <> "SUNDAY"    ~
                                                              then L50860
                u3% = 2%
                call "ASKUSER" (u3%, "*****  WEEKEND  *****",            ~
                     "Calculated Ship Date for Due Date " & dr_date$(i%)&~
                     " falls on a weekend.", "Press PF-1 to Accept,"    &~
                     " -OR-", "Press RETURN to re-enter due date.")
                if u3% = 1% then L50860
                    errormsg$ = "Ship Date for Due Date: "& dr_date$(i%) ~
                                 & " falls on a weekend"
                    return

L50860:         clines% = clines% + 1%
                call "DATEFMT" (dr_ship$(i%))

L50890:     next i%

        REM Check for excessive lines...
            total_lines%, excess% = 0%

            total_lines% = (c%-1%) + clines%
            if total_lines% <= 100% then return

            excess% = ((c%-1%) + clines%) - 100%
            convert excess% to excess$, pic(###)
L50990:     u3% = 2%
            call "ASKUSER" (u3%, "*** EXCESSIVE LINES ***",              ~
                 "You have exceeded the allowable number of lines by: " &~
                 excess$, "Press RETURN to automatically remove excess "&~
                 "line(s) -OR-", "Press PF1 to edit.")
            if u3% = 0% then L51080
                if u3% <> 1% then L50990
                    errormsg$ = "Exceed allowable lines by: " &  excess$
                    return
L51080:     e% = clines%
            dr_date$(e%), dr_qty$(e%) = " "
            clines% = clines% - 1%
            excess% = excess% - 1%
            if excess% <> 0% then L51080
                return

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        abort_daterun
             ret% = 32%
             end

        exit_program
             ret% = 16%
             end
