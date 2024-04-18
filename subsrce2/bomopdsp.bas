        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBBB   OOO   M   M   OOO   PPPP  DDDD    SSSSS  PPPPP   *~
            *  B   B  O   O  MM MM  O   O  P   P D    D  S      P   P   *~
            *  BBBBB  O   O  M M M  O   O  PPPP  D    D  SSSSS  PPPPP   *~
            *  B   B  O   O  M   M  O   O  P     D    D      S  P       *~
            *  BBBBB   OOO   M   M   OOO   P      DDD    SSSSS  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMOPDSP - Displays OPTIONS SELECTED FOR S.O. Line Seq.   *~
            *     Number passed by the caller.                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/06/87 ! Original                                 ! MDE *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "BOMOPDSP"                   /* Job Materials Ledger S/R   */~
                (so$,                    /* Sales Oreder Number        */~
                sline$,                  /* Sales Order Line Seq. No.  */~
                sopart$,                 /* Sales Order Part Number    */~
                #01,                     /* HNYMASTER UFB              */~
                #02)                     /* BOMSPEC  UFB               */





        dim                                                              ~
            column1$79, column2$79, c$1, /* Screen column headers, FAC */~
            cursor%(2),                  /* Cursor location for edit   */~
            d$1,                         /* Data FAC                   */~
            date$8,                      /* Date for screen display    */~
            demline$3,                   /* S.O. Line Number           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            keytab$17,                   /* PF keys enabled            */~
            line2$79,                    /* Second Line of Screen Headr*/~
            part$(15,2)25,               /* Part Numbers & descriptions*/~
            partdesc$(15)32,             /*      Descriptions          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf2$16,                      /* PF 2 Screen Literal        */~
            p1$25,p2$25,                 /* Parts                      */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            qty$(15)10,                  /* Quantity moved to job      */~
            slso$16                      /* Demand Code In BOMSPEC     */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * #02 ! BOMSPEC  ! OPTION List File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#02, fs%( 2), f2%( 2), 0%, rslt$( 2))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            str(line2$,,36) = "For Part : " & sopart$
            str(line2$,61%) = "BOMOPDSP : " & str(cms2v$,,8%)
            date$ = date : call "DATEFMT" (date$)
            c$ = hex(ac) : d$ = hex(84)
            column1$ = " Option Part               " & c$ & " Option"  & ~
                " Description              " & c$ & "       Quantity " & ~
                hex(8c)

            column2$ = " Option Part               " & c$ & " Replac"  & ~
                "ed Part                   " & c$ & "       Quantity " & ~
                hex(8c)

L10200: REM Start reading BOMSPEC at the beginning of SO/LINE Passed
            init (hex(00)) plowkey$
            p%, endofjob% = 0%
            plowkey$ = str(so$,1,16) & sline$ & hex(00000000)

        REM Initialize all arrays/fields used for screen-loads
L10450:    p% = p% + 1%           /* Bump screen page number up one */
           init (" ") qty$(),part$(),partdesc$(),errormsg$,slso$
           i% = 0%
           init (hex(00)) p1$, p2$
           if p% >= 2% then nxt /* For Second Screen */

            init (hex(00)) p1$, p2$
        REM Read a screen-load of records for the S.O. passed in
            i% = 1%   /* Starting With 1 */
            call "PLOWALTS" (#2, plowkey$, 1%, 19%, f1%(2))
            if f1%(2)= 0% then  L65000    /* None For This S.O. */
            get #2 using L11520,p1$,slso$,demline$, p2$,qty

L11520:       FMT CH(25),POS(57),CH(16), POS(73), CH(3),POS(80), CH(25), ~
                PD(14,4)
              if p2$ = " " then partdesc$(i%) = "No Replacement Selected"
              if str(demline$,,3) <> str(sline$,,3) then L11710
              if slso$ <> so$ then L11710
              part$(i%,2%) = p2$
              part$(i%,1%) = p1$
              call "CONVERT" (qty, 0.0, qty$(i%))

                call "READ100" (#1,p2$,f1%(1)) /* Part Description */
                if f1%(1)= 0% then nxt
                get #1 using L11660, partdesc$(i%)
L11660:         FMT POS(26), CH(32)

         nxt
            init (hex(00)) p1$, p2$
            i% = i% + 1%
            if i% > 15% then L13000 /* More Than One Screen-Full */
L11710:     call "READNEXT" (#2, f1%(2))
            if f1%(2) = 0% then L11850
            get #2 using L11520,p1$,slso$,demline$,p2$, qty


            if slso$ <> so$ then L11710
            if p2$ = " " then partdesc$(i%) = "No Replacement Selected"
            if str(demline$,,3) <> str(sline$,,3) then L11710
            call "CONVERT" (qty,0.0,qty$(i%))
            part$(i%,2%) = p2$
            part$(i%,1%) = p1$
            call "READ100" (#1, p2$, f1%(1))
              if f1%(1)= 0% then nxt
                 get #1 using L11660,partdesc$(i%)
                   goto nxt

L11850: REM  END OF BOMSPEC RECORDS
            endofjob% = 1%

         /* Set Up Special Pf Keys For Paging As Needed */

L13000:     pf2$, pf5$ = " " : pf16$ = "(16)Return"
            keytab$ = hex(ffffffffffffff08ff0affff0d0e0f1000)
            if p% = 1% then goto L13200
                pf2$ = "(2)First Page" : str(keytab$, 2, 1) = hex(02)
L13200:     if endofjob% = 1% then goto toggle_part
                pf5$ = "(5)Next Page" : str(keytab$, 5, 1) = hex(05)

        toggle_part
            errormsg$ = " "
        REM OK, the PF's are set. Display the screen & respond to keys
            gosub L40040                /* Display 1st screen of data */
            if keyhit% =  2% then goto L10200
            if keyhit% =  5% then goto L10450
            if keyhit% =  8% then goto toggle_description
            if keyhit% =  16% then end
            goto toggle_part

        toggle_description
            errormsg$ = " "
        REM Display the alternative screen- Descriptions, Stores, Lots
            gosub L45040         /* Display 2nd screen of data */
            if keyhit% =  2% then goto L10200
            if keyhit% =  5% then goto L10450
            if keyhit% =  8% then goto toggle_part
            if keyhit% =  16% then end
            goto toggle_description

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *     'N O R M A L'   S C R E E N   P A G E   (PARTS)       *~
            *************************************************************

L40040:     accept                                                       ~
               at (01,02), "Options Selected For S.O.#:",                ~
               at (01,30), fac(hex(8c)), so$                    , ch(16),~
               at (01,46), "Seq. No. : "                        ,        ~
               at (01,57), fac(hex(8c)), sline$                 , ch(03),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(c$),      column1$               , ch(79),~
                                                                         ~
               at (05,04), fac(d$),      part$ (1 ,2)           , ch(25),~
               at (05,31), fac(d$),      partdesc$(1)           , ch(32),~
               at (05,65), fac(d$),      qty$     (1)           , ch(10),~
                                                                         ~
               at (06,04), fac(d$),      part$ (2 ,2)           , ch(25),~
               at (06,31), fac(d$),      partdesc$(2)           , ch(32),~
               at (06,65), fac(d$),      qty$     (2)           , ch(10),~
                                                                         ~
               at (07,04), fac(d$),      part$ (3 ,2)           , ch(25),~
               at (07,31), fac(d$),      partdesc$(3)           , ch(32),~
               at (07,65), fac(d$),      qty$     (3)           , ch(10),~
                                                                         ~
               at (08,04), fac(d$),      part$ (4 ,2)           , ch(25),~
               at (08,31), fac(d$),      partdesc$(4)           , ch(32),~
               at (08,65), fac(d$),      qty$     (4)           , ch(10),~
                                                                         ~
               at (09,04), fac(d$),      part$ (5 ,2)           , ch(25),~
               at (09,31), fac(d$),      partdesc$(5)           , ch(32),~
               at (09,65), fac(d$),      qty$     (5)           , ch(10),~
                                                                         ~
               at (10,04), fac(d$),      part$ (6 ,2)           , ch(25),~
               at (10,31), fac(d$),      partdesc$(6)           , ch(32),~
               at (10,65), fac(d$),      qty$     (6)           , ch(10),~
                                                                         ~
               at (11,04), fac(d$),      part$ (7 ,2)           , ch(25),~
               at (11,31), fac(d$),      partdesc$(7)           , ch(32),~
               at (11,65), fac(d$),      qty$     (7)           , ch(10),~
                                                                         ~
               at (12,04), fac(d$),      part$ (8 ,2)           , ch(25),~
               at (12,31), fac(d$),      partdesc$(8)           , ch(32),~
               at (12,65), fac(d$),      qty$     (8)           , ch(10),~
                                                                         ~
               at (13,04), fac(d$),      part$ (9 ,2)           , ch(25),~
               at (13,31), fac(d$),      partdesc$(9)           , ch(32),~
               at (13,65), fac(d$),      qty$     (9)           , ch(10),~
                                                                         ~
               at (14,04), fac(d$),      part$ (10, 2)          , ch(25),~
               at (14,31), fac(d$),      partdesc$(10)          , ch(32),~
               at (14,65), fac(d$),      qty$     (10)          , ch(10),~
                                                                         ~
               at (15,04), fac(d$),      part$ (11, 2)          , ch(25),~
               at (15,31), fac(d$),      partdesc$(11)          , ch(32),~
               at (15,65), fac(d$),      qty$     (11)          , ch(10),~
                                                                         ~
               at (16,04), fac(d$),      part$ (12, 2)          , ch(25),~
               at (16,31), fac(d$),      partdesc$(12)          , ch(32),~
               at (16,65), fac(d$),      qty$     (12)          , ch(10),~
                                                                         ~
               at (17,04), fac(d$),      part$ (13, 2)          , ch(25),~
               at (17,31), fac(d$),      partdesc$(13)          , ch(32),~
               at (17,65), fac(d$),      qty$     (13)          , ch(10),~
                                                                         ~
               at (18,04), fac(d$),      part$ (14, 2)          , ch(25),~
               at (18,31), fac(d$),      partdesc$(14)          , ch(32),~
               at (18,65), fac(d$),      qty$     (14)          , ch(10),~
                                                                         ~
               at (19,04), fac(d$),      part$ (15, 2)          , ch(25),~
               at (19,31), fac(d$),      partdesc$(15)          , ch(32),~
               at (19,65), fac(d$),      qty$     (15)          , ch(10),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,40), "                      ",                     ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pf2$                           ,~
               at (23,40), "                      ",                     ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf5$                           ,~
               at (24,22), "(8)Show Replaced Parts",                     ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L41380
                  call "MANUAL" ("BOMOPDSP")
                  goto L40040

L41380:        if keyhit% <> 15 then L41420
                  call "PRNTSCRN"
                  goto L40040

L41420:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *'A L T E R N A T E'   S C R E E N   P A G E  (DESCRIPTIONS)*~
            *************************************************************

L45040:     accept                                                       ~
               at (01,02), "Options & Replaced Parts S.O.#"             ,~
               at (01,32), fac(hex(8c)), so$                    , ch(16),~
               at (01,46), "Seq. No. : "                        ,        ~
               at (01,57), fac(hex(8c)), sline$                 , ch(03),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(c$),      column2$               , ch(79),~
                                                                         ~
               at (05,04), fac(d$),      part$ (1 ,2)           , ch(25),~
               at (05,31), fac(d$),      part$ (1 ,1)           , ch(25),~
               at (05,65), fac(d$),      qty$     (1)           , ch(10),~
                                                                         ~
               at (06,04), fac(d$),      part$ (2 ,2)           , ch(25),~
               at (06,31), fac(d$),      part$ (2 ,1)           , ch(25),~
               at (06,65), fac(d$),      qty$     (2)           , ch(10),~
                                                                         ~
               at (07,04), fac(d$),      part$ (3 ,2)           , ch(25),~
               at (07,31), fac(d$),      part$ (3 ,1)           , ch(25),~
               at (07,65), fac(d$),      qty$     (3)           , ch(10),~
                                                                         ~
               at (08,04), fac(d$),      part$ (4 ,2)           , ch(25),~
               at (08,31), fac(d$),      part$ (4 ,1)           , ch(25),~
               at (08,65), fac(d$),      qty$     (4)           , ch(10),~
                                                                         ~
               at (09,04), fac(d$),      part$ (5 ,2)           , ch(25),~
               at (09,31), fac(d$),      part$ (5 ,1)           , ch(25),~
               at (09,65), fac(d$),      qty$     (5)           , ch(10),~
                                                                         ~
               at (10,04), fac(d$),      part$ (6 ,2)           , ch(25),~
               at (10,31), fac(d$),      part$ (6 ,1)           , ch(25),~
               at (10,65), fac(d$),      qty$     (6)           , ch(10),~
                                                                         ~
               at (11,04), fac(d$),      part$ (7 ,2)           , ch(25),~
               at (11,31), fac(d$),      part$ (7 ,1)           , ch(25),~
               at (11,65), fac(d$),      qty$     (7)           , ch(10),~
                                                                         ~
               at (12,04), fac(d$),      part$ (8 ,2)           , ch(25),~
               at (12,31), fac(d$),      part$ (8 ,1)           , ch(25),~
               at (12,65), fac(d$),      qty$     (8)           , ch(10),~
                                                                         ~
               at (13,04), fac(d$),      part$ (9 ,2)           , ch(25),~
               at (13,31), fac(d$),      part$ (9 ,1)           , ch(25),~
               at (13,65), fac(d$),      qty$     (9)           , ch(10),~
                                                                         ~
               at (14,04), fac(d$),      part$ (10, 2)          , ch(25),~
               at (14,31), fac(d$),      part$ (10, 1)          , ch(25),~
               at (14,65), fac(d$),      qty$     (10)          , ch(10),~
                                                                         ~
               at (15,04), fac(d$),      part$ (11, 2)          , ch(25),~
               at (15,31), fac(d$),      part$ (11, 1)          , ch(25),~
               at (15,65), fac(d$),      qty$     (11)          , ch(10),~
                                                                         ~
               at (16,04), fac(d$),      part$ (12, 2)          , ch(25),~
               at (16,31), fac(d$),      part$ (12, 1)          , ch(25),~
               at (16,65), fac(d$),      qty$     (12)          , ch(10),~
                                                                         ~
               at (17,04), fac(d$),      part$ (13, 2)          , ch(25),~
               at (17,31), fac(d$),      part$ (13, 1)          , ch(25),~
               at (17,65), fac(d$),      qty$     (13)          , ch(10),~
                                                                         ~
               at (18,04), fac(d$),      part$ (14, 2)          , ch(25),~
               at (18,31), fac(d$),      part$ (14, 1)          , ch(25),~
               at (18,65), fac(d$),      qty$     (14)          , ch(10),~
                                                                         ~
               at (19,04), fac(d$),      part$ (15, 2)          , ch(25),~
               at (19,31), fac(d$),      part$ (15, 1)          , ch(25),~
               at (19,65), fac(d$),      qty$     (15)          , ch(10),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,40), "                      ",                     ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pf2$                           ,~
               at (23,40), "                      ",                     ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf5$                           ,~
               at (24,22), "(8)Show Option List",                        ~
               at (24,65), fac(hex(84)), pf16$                          ,~
               keys (keytab$), key (keyhit%)

               if keyhit% <> 13 then L46520
                  call "MANUAL" ("BOMOPDSP")
                  goto L45040

L46520:        if keyhit% <> 15 then L46560
                  call "PRNTSCRN"
                  goto L45040

L46560:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
