        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M   SSS   RRRR   TTTTT  EEEEE   SSS    *~
            *  B   B  O   O  MM MM  S      R   R    T    E      S       *~
            *  BBBB   O   O  M M M   SSS   RRRR     T    EEEE    SSS    *~
            *  B   B  O   O  M   M      S  R   R    T    E          S   *~
            *  BBBB    OOO   M   M   SSS   R   R    T    EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMSRTES - SHOWS ALL RTE, BILLS, AND COST DETAIL SETS     *~
            *            ASSOCIATED WITH THE PASSED IN PART             *~
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
            * 02/04/84 ! ORIGINAL                                 ! HES *~
            * 03/30/87 ! Modified for STC project (Rewrite).      ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        sub "BOMSRTES"   (part$,         /* Part Number to Display     */~
                          #4,            /* HNYMASTR Channel           */~
                          #1,            /* BOMMASTR Channel           */~
                          #2,            /* RTEMASTR Channel           */~
                          #3 )           /* STCBOMXF Channel           */

        dim                                                              ~
            bom$(100)3,                  /* BOMs on file               */~
            bomid$3,                     /* BOM                        */~
            bomrte$(100)3,               /* Route IDs referenced       */~
            date$8,                      /* Date for screen display    */~
            hdr$17,                      /* Screen Column Headings     */~
            inpmessage$79,               /* Input message              */~
            line$(100)79,                /* Screen Display             */~
            line2$79,                    /* Second Screen Line         */~
            p%(1),                       /* Receiver for Search        */~
            part$25, partdescr$34,       /* Part & Description         */~
            readkey$99,                  /* A Read Key                 */~
            rteid$3,                     /* Route ID                   */~
            rte$(100)3,                  /* Route structures on file   */~
            set$8,                       /* Cost Set ID                */~
            type$10                      /* Part type / Description    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! BOMMASTR ! Bill of Materials Relationship File      *~
            * # 2 ! RTEMASTR ! Standard & Alt Work Center Routings      *~
            * # 3 ! STCBOMXF ! Standard Cost BOM/Route X-Ref            *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************

        REM *************************************************************~
            *             I N I T I A L I Z A T I O N                   *~
            * --------------------------------------------------------- *~
            * Set up program variables                                  *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            str(line2$,62%) = "BOMSRTES: " & str(cms2v$,,8)
            hdr$ = "BOM RTE Cost Set"

        REM *************************************************************~
            *      M A I N   P R O C E S S I N G   S E C T I O N        *~
            * --------------------------------------------------------- *~
            * Prepare listing of Cost Sets, BOMs and Routes.            *~
            *************************************************************

        init(" ") inpmessage$, partdescr$, rte$(), bom$(), bomrte$(),    ~
                  line$(), type$

        call "GETCODE" (#4, part$, partdescr$, 1%, 99, f1%)
        if f1% = 0% then goto L65000
            get #4, using L10120, type$
L10120:         FMT XX(179), CH(10)
            convert type$ to type%, data goto L10180
            if type% = 0% or type% > 499% then type$ = "Manufactrd"      ~
                                          else type$ = "Purchased "

L10180
*        Load up all ROUTES for this part
            readkey$ = part$
            rte%     = 0%
L10210:     call "PLOWNEXT" (#2, readkey$, 25%, f1%)
            if f1% = 0% then L10290
                rte% = rte% + 1%
                get #2, using L10250, rte$(rte%)
L10250:              FMT XX(29), CH(3)
                str(readkey$,29,3) = "999"
                goto L10210

L10290
*        Load up all BILLS and Bill/Route Relations
            readkey$ = part$
            bom% = 0%
L10320:     call "PLOWNEXT" (#1, readkey$, 25%, f1%)
            if f1% = 0% then L10420
                if str(readkey$,29,3) <> "  0" then L10320
                bom% = bom% + 1%
                get #1 using L10370, bom$(bom%), bomrte$(bom%)
L10370:              FMT XX(50), CH(3), POS(87), CH(3)
                str(readkey$,29,3) = "999"
                goto L10320


L10420
*        Load up all STD COST SETS for this part.
            readkey$ = str(part$,,25) & hex(00)
            line%    = 0%  :  offset% = 99%
L10450:     call "PLOWALTS" (#3, readkey$, 1%, 25%, f1%)
            if f1% = 0% then L10610
                get #3 using L10480, bomid$, set$, rteid$
L10480:              FMT POS(26), CH(3), CH(8), POS(62), CH(3)
                offset% = offset% + 20%
                if offset% < 62% then L10530
                     offset% = 1%
                     line%   = line% + 1%
L10530:         str(line$(line%), offset%) = str(bomid$) & " " &         ~
                                             str(rteid$) & " " & set$
                search str(rte$()) = rteid$ to p%() step 3
                if p%(1) <> 0% then str(rte$(),p%(1),3) = " "
                search str(bom$()) = bomid$ to p%() step 3
                if p%(1) <> 0% then str(bom$(),p%(1),3) = " "
                goto L10450

L10610
*        Now put out BOMs / RTEs with no Costs Sets
            if bom% = 0% then L10750
            for i%  = 1% to bom%
                if bom$(i%) = " " or bomrte$(i%) = " " then L10730
                     offset% = offset% + 20%
                     if offset% < 62% then L10690
                          offset% = 1%
                          line%   = line% + 1%
L10690:              str(line$(line%), offset%) = str(bom$(i%)) & " " &  ~
                                                  bomrte$ (i%)
                     search str(rte$()) = rteid$ to p%() step 3
                     if p%(1) > 0% then str(rte$(),p%(1),3) = " "
L10730:     next i%

L10750
*        Now Unattached BOMs
            if bom% = 0% then L10860
            for i%  = 1% to bom%
                if bom$(i%) = " " or bomrte$(i%) <> " " then L10840
                offset% = offset% + 20%
                if offset% < 62% then L10830
                     offset% = 1%
                     line%   = line% + 1%
L10830:         str(line$(line%), offset%) = bom$(i%)
L10840:     next i%

L10860
*        Now unattached Routes
            if rte% = 0% then L10970
            for i%  = 1% to rte%
                if rte$(i%) = " " then L10950
                offset% = offset% + 20%
                if offset% < 62% then L10940
                     offset% = 1%
                     line%   = line% + 1%
L10940:         str(line$(line%), offset%) = "    " & rte$(i%)
L10950:     next i%

L10970
*        And finally set up for display
            if line% = 0% then                                           ~
                  line$(1) = "There are no BOMS or Routes for this Part."
            last% =  max(0%, line% - 10%)
            line% = 0%

L40000: REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

L40060:     accept                                                       ~
               at (01,02), "PART'S BOM/RTE/STC STRUCTURE",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "Part: ",                                     ~
               at (03,09), fac(hex(84)),   part$                , ch(25),~
               at (03,35), fac(hex(8c)),   partdescr$           , ch(34),~
               at (03,71), fac(hex(8c)),   type$                , ch(10),~
                                                                         ~
               at (05,02), fac(hex(ac)),   hdr$                 , ch(17),~
               at (05,22), fac(hex(ac)),   hdr$                 , ch(17),~
               at (05,42), fac(hex(ac)),   hdr$                 , ch(17),~
               at (05,62), fac(hex(ac)),   hdr$                 , ch(17),~
                                                                         ~
               at (06,02), fac(hex(8c)),   line$(line%+1 )      , ch(79),~
               at (07,02), fac(hex(8c)),   line$(line%+2 )      , ch(79),~
               at (08,02), fac(hex(8c)),   line$(line%+3 )      , ch(79),~
               at (09,02), fac(hex(8c)),   line$(line%+4 )      , ch(79),~
               at (10,02), fac(hex(8c)),   line$(line%+5 )      , ch(79),~
               at (11,02), fac(hex(8c)),   line$(line%+6 )      , ch(79),~
               at (12,02), fac(hex(8c)),   line$(line%+7 )      , ch(79),~
               at (13,02), fac(hex(8c)),   line$(line%+8 )      , ch(79),~
               at (14,02), fac(hex(8c)),   line$(line%+9 )      , ch(79),~
               at (15,02), fac(hex(8c)),   line$(line%+10)      , ch(79),~
               at (16,02), fac(hex(8c)),   line$(line%+11)      , ch(79),~
               at (17,02), fac(hex(8c)),   line$(line%+12)      , ch(79),~
               at (18,02), fac(hex(8c)),   line$(line%+13)      , ch(79),~
               at (19,02), fac(hex(8c)),   line$(line%+14)      , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), "(2)First  (4)Previous  (6)Down",             ~
               at (24,02), "(3)Last   (5)Next      (7)Up",               ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                     keys(hex(0203040506070d0f10)), key(keyhit%)

               if keyhit% <> 13% then L40480
                  call "MANUAL" ("BOMSRTES")
                  goto L40060

L40480:        if keyhit% <> 15% then L40520
                  call "PRNTSCRN"
                  goto L40060

L40520:        if keyhit% = 16 then L65000
               if keyhit% =  2 then line% = 0%
               if keyhit% =  3 then line% = last%
               if keyhit% =  4 then line% = max(0%, line% - 10%)
               if keyhit% =  5 then line% = min(last%, line% + 10%)
               if keyhit% =  6 then line% = max(0%, line% - 1%)
               if keyhit% =  7 then line% = min(last%, line% + 1%)
               goto L40000

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end

