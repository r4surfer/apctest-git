        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  H   H  NN  N  Y   Y  P   P  U   U  R   R  G      E       *~
            *  HHHHH  N N N   YYY   PPPP   U   U  RRRR   G GGG  EEEE    *~
            *  H   H  N  NN    Y    P      U   U  R   R  G   G  E       *~
            *  H   H  N   N    Y    P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPURGE - Kill HNYQUAN records that have zero on-hand,   *~
            *            back-order, on-order, committed and in-process *~
            *            quantities.  Also Kills any HNYLOCNS records   *~
            *            (Part Locations) tied to a PART/STORE/LOT combo*~
            *            thats being deleted from HNYQUAN.              *~
            *            Only items meeting user selected criteria are  *~
            *            deleted (must be within ranges set).           *~
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
            * 08/17/84 ! ORIGINAL                                 ! DSH *~
            * 02/13/86 ! Added HNYLOCNS file & Deletion thereof.  ! LDJ *~
            * 02/26/87 ! Added Serial # logic                     ! JRH *~
            * 05/13/87 ! HNYPOOL/HNYMASTR record length mods(whew)! JIM *~
            * 04/01/88 ! Now deletes text assoc. w/ HNYQUAN recs. ! RJM *~
            * 02/13/90 ! Added 4th alt. Key for HNYLOCNS file.    ! JEF *~
            * 07/17/92 ! Added Cycle Count Files deletion logic.  ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankline$79,                /* P.F. KEYS ACTIVE           */~
            box$(600)1,                  /* BLANK BOX FOR ITEM PURGE   */~
            cycle$1,                     /*Purge With Cycle Count Y/N/K*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deletekey$44,                /* Misc Delete Key            */~
            detail$1,                    /* PURGE WITH DETAIL Y/N      */~
            endlot$6,                    /*   ENDING LOT NUMBER:       */~
            endpart$25,                  /*   ENDING PART CODE:        */~
            endstore$3,                  /*   ENDING STORE CODE:       */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            heading$79,                  /* TITLE FOR PURGE SCREEN     */~
            inputmsg$79,                 /* INPUT MESSAGE              */~
            keyhits$32,                  /* PF KEY ENABLE SWITCH       */~
            lfac$(15)1,                  /* FIELD ATTRIBUTES           */~
            line$(600)77,                /* PART, DESCRIPT, STORE, LOT */~
            line2$79,                    /* SCREEN LINE 2              */~
            lot$16,                      /* LOT NUMBER                 */~
            mode$1,                      /* MODE AUTO OR PROMPT        */~
            page$2,                      /* SCREEN NUMBER OF PURGE     */~
            part$25,                     /* PART NUMBER                */~
            pfkey$(32)20,                /* PF KEY PROMPTS             */~
            plowkey$99,                  /* PLOWKEY FOR HNYQUAN        */~
            plowsave$99,                 /* PLOWKEY SAVE AREA          */~
            purgekey$99,                 /* PLOWKEY FOR HNYQUAN        */~
            quantxtid$(600)4,            /* TEXT ID'S FOR HNYQUAN      */~
            readkey$100,                 /* Misc Plow Key              */~
            serial$1,                    /* Serial # flag              */~
            startlot$6,                  /* STARTING LOT NUMBER:       */~
            startpart$25,                /* STARTING PART CODE:        */~
            startstore$3,                /* STARTING STORE CODE:       */~
            store$3,                     /* STORE NUMBER               */~
            textid$4,                    /* TEMP TEXT ID VARIABLE      */~
            xitle$79                     /* TITLE FOR PURGE SCREEN     */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #1  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #2  ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #3  ! HNYDETAL ! Inventory Transaction History Detail file*~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! HNYLOCNS ! Stock location master file               *~
            * #6  ! SERMASTR ! Serial number master file                *~
            * #7  ! TXTFILE  ! System Text File                         *~
            * #23 ! HNYCCMST ! Cycle Count Master File                  *~
            * #24 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select #02, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  38                      ~

            select #03, "HNYDETAL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #04, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup

            select #5,  "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42          ~

            select #06, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #07, "TXTFILE",                                       ~
                        varc,    indexed,  recsize = 2024,               ~
                        keypos =   1, keylen =  11


            select #23, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #24, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize = 436,               ~
                        keypos =    1,  keylen = 41,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, "SHARE", f2%(05), rslt$(05), axd$(05))
            call "OPENFILE" (#06, "SHARE", f2%(06), rslt$(06), axd$(06))
            call "OPENFILE" (#23, "SHARE", f2%(23), rslt$(23), axd$(23))
            call "OPENFILE" (#24, "SHARE", f2%(24), rslt$(24), axd$(24))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
                call "DATEFMT" (date$)

            blankline$ = "P.F. Keys Active:"
                str(heading$,03,09) = "Part Code"
                str(heading$,29,16) = "Part Description"
                str(heading$,62,07) = "Str/Lot"
                str(heading$,73,07) = "Det Ser"
            init(hex(ff)) keyhits$
                str(keyhits$,,9) = hex(01ffffffff0d0e0f10)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") inputmsg$, endpart$, endstore$, endlot$
                startstore$, startlot$  = "ALL"
                startpart$ = "FIRST"
                endpart$ = "LAST"
                mode$ = "P"
                serial$, detail$, cycle$ = "N"
                inputmsg$ = "Please specify the ranges for searching for ~
        ~purgable on-hand records."

L10150:     gosub L40000
                  if keyhit% <>  0 then L10150
            gosub L50000
                  if errormsg$ <> " " then L10150
                  gosub setup_key

L11000: REM *************************************************************~
            * CYCLE THROUGH AND DELETE                                  *~
            *************************************************************

            gosub in_search_of

            if mode$ <> "A" then L11100
               init ("X") str(box$(),1,rec%)
               goto L19000

L11100:     gosub L41000
                  if keyhit% <> 16 then L11100

L19000: REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

            call "SHOSTAT" ("Deleting On Hand Records & Inventory " &    ~
                "Locations")

            for x% = 1% to rec%
                if line$(x%) = "** End of File **" then inputmode
                if box$(x%) = " " then L19360

                init(hex(00)) purgekey$
                str(purgekey$,,34) = str(line$(x%),1,25)                 ~
                           & str(line$(x%),60,3) & str(line$(x%),64,6)
                call "DELETE" (#1, purgekey$, 34%)
                if quantxtid$(x%) = hex(000000)                          ~
                     or quantxtid$(x%) = hex(202020)                     ~
                     or quantxtid$(x%) = hex(ffffff) then L19250
                call "TXTFUTIL" (#7, f2%(7), "DELE", quantxtid$(x%))
L19250:         init(hex(00)) str(purgekey$,35)
                call "DELETE" (#2, purgekey$, 34%)
                if detail$ = "K" then L19292
                   init(hex(00)) str(purgekey$,35)
                   call "DELETE" (#3, purgekey$, 34%)
L19292:         if cycle$ = "K" then L19305
                   init(hex(00)) str(purgekey$,35)
                   call "DELETE" (#23, purgekey$, 34%)
                   deletekey$ = "P" &  purgekey$
                   init(hex(20)) str(deletekey$,36%)
                   call "REDALT1" (#24, deletekey$, 1%, f1%(24%))
                   if f1%(24%) = 0 then L19305  /* Only Delete Parts from*/
                   delete #24                  /* a Pre-Active Session  */
L19305:         readkey$ = str(line$(x%),60,3) & str(line$(x%),1,25) &   ~
                           str(line$(x%),64,6) & hex(00)
L19320:         call "PLOWAL1" (#5, readkey$, 1%, 34%, f1%(5))
                if f1%(5) = 0% then L19360
                delete #5
                goto L19320
L19360:     next x%
                if str(plowkey$,1,1) <> hex(ff) then L11000
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
            k% = 2%
            call "STARTOVR" (k%)
            if k% = 1% then return
            return clear all
            init(" ") errormsg$
            goto inputmode

        REM *************************************************************~
            * SET UP INITIAL PLOWKEY                                    *~
            *************************************************************

        setup_key

            init(hex(00)) plowkey$

                if startpart$ <> "ALL" then L30120
                    init(hex(00)) startpart$
                    init(hex(ff)) endpart$
                       goto L30140
L30120:         if startpart$ = "FIRST" then init(hex(00)) startpart$
                if endpart$ = "LAST" then init(hex(ff)) endpart$

L30140:         if startstore$ <> "ALL" then L30180
                    init(hex(00)) startstore$
                    init(hex(ff)) endstore$

L30180:         if startlot$ <> "ALL" then L30220
                    init(hex(00)) startlot$
                    init(hex(ff)) endlot$

L30220:     str(plowkey$,1,28) = str(startpart$,1,25) &                  ~
                str(startstore$,1,3)
            plowsave$ = plowkey$
            return

        REM *************************************************************~
            *     R E T R I E V E   P U R G A B L E   R E C O R D S     *~
            *************************************************************

        in_search_of

                init(" ") line$(), box$(), errormsg$
                q%, l%, rec% = 0%

L31090: REM Major search loop. Restore key & search HNYQUAN file *********
                if q% = 0% then call "SHOSTAT"                           ~
                     ("Selecting inventory records")
                q% = 1%
                plowkey$ = plowsave$
                call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
                plowsave$ = plowkey$
                    if f1%(1) <> 0% then L31200
L31170:                init (hex(ff)) plowkey$, plowsave$
                       goto L31820

L31200:     get #1, using L31210, part$, store$, lot$
L31210:         FMT POS(17), CH(25), CH(3), CH(16)

            if part$ < startpart$ then L31090
            if part$ > endpart$ then L31170

                if store$ < startstore$ then L31090
                if store$ <= endstore$  then L31310
                   init (hex(ff)) str(plowkey$,26), str(plowsave$,26)
                   goto L31090

L31310:         if lot$ < startlot$ then L31090
                if lot$ <= endlot$  then L31360
                   init (hex(ff)) str(plowkey$,29), str(plowsave$,29)
                   goto L31090

L31360:     get #1, using L31390, onhand, back, order, commit, inproc,    ~
                                 textid$

L31390:         FMT POS(69), 5*PD(14,4), POS(418), CH(4)
                if onhand <> 0 then L31090
                if back   <> 0 then L31090
                if order  <> 0 then L31090
                if commit <> 0 then L31090
                if inproc <> 0 then L31090

               init (hex(00)) str(plowkey$,35)
               call "PLOWNEXT" (#3, plowkey$, 34%, f1%(3))
               if f1%(3) = 0 then L31494
               if detail$ = "N" then L31090

L31494:         init (hex(00)) plowkey$  /* Check the Cycle Count Mstr */
                str(plowkey$,,25) = part$
                call "PLOWNEXT" (#23, plowkey$, 25%, f1%(23%))
                if f1%(23%) = 0% then L31510
                if cycle$ = "N" then L31090

L31510:         init (hex(00)) plowkey$
                str(plowkey$,,25) = part$
                call "PLOWNEXT" (#6, plowkey$, 25%, f1%(6))
                if f1%(6) = 0% then L31660
                     if serial$ = "Y" then L31660
                     if serial$ = "N" then L31090
L31570:              q%, u3% = 0%
                     call "ASKUSER" (u3%, "*** SERIAL #'D PART ***",     ~
                          "Part " & part$ & " is Serial numbered",       ~
                          "Press PF(16) to PURGE it & continue"        & ~
                          "                     -- OR --",               ~
                          "Press (RETURN) to continue WITHOUT purging it")
                     if u3% =   0% then L31090
                     if u3% <> 16% then L31570

L31660:     rec% = min((rec% + 1%),598)
               str(line$(rec%),01,25) = part$
               call "DESCRIBE" (#4, part$, str(line$(rec%),27,32), 0%,   ~
                                                                  f1%(4))
               quantxtid$(rec%) = textid$
               str(line$(rec%),60,3) = store$
               str(line$(rec%),64,6) = lot$

               if f1%(3) <> 0 then str(line$(rec%),72,1) = "Y"
               if f1%(6) <> 0 then str(line$(rec%),76,1) = "Y"

               if rec% < 598% then L31090

L31820:     if rec% <> 0 then L31870
               return clear
               errormsg$ = "No purgable records found."
               goto inputmode

L31870:     if rec% < 598 then L31930
            if str(plowkey$,1,1) = hex(ff) then L31930
               errormsg$ = "More records may be found, search will contin~
        ~ue after processing."
               return

L31930:     rec% = min(rec%, 598%)
               line$(rec%+2%) = "** End of File **"
               return

L40000: REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *************************************************************
            str(line2$,62%) = "HNYPURGE: " & str(cms2v$,,8%)
L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "Selecting search range for purgable on-hand records", ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), inputmsg$              , ch(79),~
                                                                         ~
               at (07,02),                                               ~
                  "Starting Part Code:",                                 ~
               at (07,30), fac(hex(81)), startpart$             , ch(25),~
               at (08,02),                                               ~
                  "  Ending Part Code:",                                 ~
               at (08,30), fac(hex(81)), endpart$               , ch(25),~
               at (10,02),                                               ~
                  "Starting Store Code:",                                ~
               at (10,30), fac(hex(81)), startstore$            , ch(03),~
               at (11,02),                                               ~
                  "  Ending Store Code:",                                ~
               at (11,30), fac(hex(81)), endstore$              , ch(03),~
               at (13,02),                                               ~
                  "Starting Lot Number:",                                ~
               at (13,30), fac(hex(81)), startlot$              , ch(06),~
               at (14,02),                                               ~
                  "  Ending Lot Number:",                                ~
               at (14,30), fac(hex(81)), endlot$                , ch(06),~
               at (16,02),                                               ~
               "Purge if details? (N=No, Y=Yes, K=Yes but Keep Details)",~
               at (16,57), ":"                                  ,        ~
               at (16,59), fac(hex(81)), detail$                , ch(01),~
               at (17,02),                                               ~
               "Purge if Serial #s? (N=No, Y=Yes, S=Stop & ask):",       ~
               at (17,59), fac(hex(81)), serial$                , ch(01),~
               at (18,02),                                               ~
               "Purge if Cycle Count?(N=No, Y=Yes, K=Yes & Keep Details",~
               at (18,57), ":"                                  ,        ~
               at (18,59), fac(hex(81)), cycle$                 , ch(01),~
               at (20,02),                                               ~
                  "Purge mode (P=Prompt, A=Automatic):",                 ~
               at (20,59), fac(hex(81)), mode$                  , ch(01),~
                                                                         ~
               at (22,02), fac(hex(ac)), blankline$             , ch(79),~
                                                                         ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20),                                               ~
                  "(13)Instructions",                                    ~
               at (23,40),                                               ~
                  "(15)Print Screen",                                    ~
               at (23,60),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 1 then L40690
                  gosub startover
                  goto L40260

L40690:        if keyhit% <> 13 then L40730
                  call "MANUAL" ("HNYPURGE")
                  goto L40260

L40730:        if keyhit% <> 15 then L40761
                  call "PRNTSCRN"
                  goto L40260

L40761:        init(" ") errormsg$

               if keyhit% <> 16 then return
                  return clear
                  goto L65000

L41000: REM *************************************************************~
            *       D I S P L A Y    P U R G A B L E    I T E M S       *~
            *************************************************************

            init(" ") pfkey$(2),pfkey$(3),pfkey$(4),pfkey$(5),inputmsg$

               str(keyhits$,02,04) = hex(ffffffff)
               inputmsg$ = "Select the records to be purged by placing an~
        ~y character in the boxes."

            init(hex(9c)) lfac$()
               for x% = 1 to 15
                  if line$((l%*15)+x%) = " "  then L41150
                     lfac$(x%) = hex(80)
               next x%
                  pfkey$(3) = "(3)Last Page"
                  pfkey$(5) = "(5)Next Page"
                  str(keyhits$,04,02) = hex(0305)

L41150:        if l% = 0 then L41190
                  pfkey$(2) = "(2)First Page"
                  pfkey$(4) = "(4)Prev Page"
                  str(keyhits$,02,02) = hex(0204)

L41190:        convert (l% + 1) to page$, pic(##)
                  xitle$ = " ** PURGABLE INVENTORY ON-HAND RECORDS ** "  ~
                                   &  hex(06) & "    DATE:  " & date$    ~
                          & "   "  &  hex(06) & "    PAGE:  " & page$

            accept                                                       ~
               at (01,02), fac(hex(ac)), xitle$                 , ch(79),~
                                                                         ~
               at (03,02), fac(hex(84)), inputmsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(a4)), heading$               , ch(79),~
               at (06,02), fac(lfac$(01)), box$((l%*15)+01)     , ch(01),~
               at (07,02), fac(lfac$(02)), box$((l%*15)+02)     , ch(01),~
               at (08,02), fac(lfac$(03)), box$((l%*15)+03)     , ch(01),~
               at (09,02), fac(lfac$(04)), box$((l%*15)+04)     , ch(01),~
               at (10,02), fac(lfac$(05)), box$((l%*15)+05)     , ch(01),~
               at (11,02), fac(lfac$(06)), box$((l%*15)+06)     , ch(01),~
               at (12,02), fac(lfac$(07)), box$((l%*15)+07)     , ch(01),~
               at (13,02), fac(lfac$(08)), box$((l%*15)+08)     , ch(01),~
               at (14,02), fac(lfac$(09)), box$((l%*15)+09)     , ch(01),~
               at (15,02), fac(lfac$(10)), box$((l%*15)+10)     , ch(01),~
               at (16,02), fac(lfac$(11)), box$((l%*15)+11)     , ch(01),~
               at (17,02), fac(lfac$(12)), box$((l%*15)+12)     , ch(01),~
               at (18,02), fac(lfac$(13)), box$((l%*15)+13)     , ch(01),~
               at (19,02), fac(lfac$(14)), box$((l%*15)+14)     , ch(01),~
               at (20,02), fac(lfac$(15)), box$((l%*15)+15)     , ch(01),~
                                                                         ~
               at (06,04), fac(hex(8c)), line$((l%*15)+01)      , ch(77),~
               at (07,04), fac(hex(8c)), line$((l%*15)+02)      , ch(77),~
               at (08,04), fac(hex(8c)), line$((l%*15)+03)      , ch(77),~
               at (09,04), fac(hex(8c)), line$((l%*15)+04)      , ch(77),~
               at (10,04), fac(hex(8c)), line$((l%*15)+05)      , ch(77),~
               at (11,04), fac(hex(8c)), line$((l%*15)+06)      , ch(77),~
               at (12,04), fac(hex(8c)), line$((l%*15)+07)      , ch(77),~
               at (13,04), fac(hex(8c)), line$((l%*15)+08)      , ch(77),~
               at (14,04), fac(hex(8c)), line$((l%*15)+09)      , ch(77),~
               at (15,04), fac(hex(8c)), line$((l%*15)+10)      , ch(77),~
               at (16,04), fac(hex(8c)), line$((l%*15)+11)      , ch(77),~
               at (17,04), fac(hex(8c)), line$((l%*15)+12)      , ch(77),~
               at (18,04), fac(hex(8c)), line$((l%*15)+13)      , ch(77),~
               at (19,04), fac(hex(8c)), line$((l%*15)+14)      , ch(77),~
               at (20,04), fac(hex(8c)), line$((l%*15)+15)      , ch(77),~
                                                                         ~
               at (22,02), fac(hex(ac)), blankline$             , ch(79),~
               at (23,02),                                               ~
                   "(1)Start Over",                                      ~
               at (23,46),                                               ~
                   "(13)Instructions",                                   ~
               at (23,63),                                               ~
                   "(15)Print Screen",                                   ~
               at (24,46),                                               ~
                   "(14)Mark All",                                       ~
               at (24,63),                                               ~
                   "(16)PURGE Records",                                  ~
               at (23,16), fac(hex(8c)), pfkey$(2)              , ch(13),~
               at (24,16), fac(hex(8c)), pfkey$(3)              , ch(13),~
               at (23,31), fac(hex(8c)), pfkey$(4)              , ch(13),~
               at (24,31), fac(hex(8c)), pfkey$(5)              , ch(13),~
                                                                         ~
                     keys(keyhits$),                                     ~
                     key (keyhit%)

               if keyhit% <> 1% then L41790
                     gosub startover
                     goto L41000

L41790:        if keyhit% <> 2% then L41822
                     l% = 0%
                     goto L41000

L41822:        if keyhit% <> 4% then L41830
                     l% = max((l%-1),0)
                     goto L41000

L41830:        if keyhit% <> 3% then L41862
                     l% = int(rec%/15%)
                     goto L41000

L41862:        if keyhit% <> 5% then L41870
                     l% = min((l%+1),39)
                     goto L41000

L41870:        if keyhit% <> 14% then L41902
               init ("X") str(box$(),1,rec%)
                     goto L41000

L41902:        if keyhit% <> 13% then L41910
                     call "MANUAL" ("HNYPURGE")
                     goto L41000

L41910:        if keyhit% <> 15% then return
                     call "PRNTSCRN"
                     goto L41000


L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *************************************************************

            init(" ") errormsg$

            if startpart$ <> "ALL" then L50081
               startpart$ = "FIRST"
L50070:        endpart$ = "LAST"
               goto L50160
L50081:        if startpart$ = "FIRST" and endpart$=" " then L50070
               if startpart$ = "FIRST" and endpart$="LAST" then L50160
                 if endpart$ <> " " then L50105
                    endpart$ = startpart$
L50105:        if startpart$ = "FIRST" then L50160
               if endpart$ = "LAST" then L50160
                 if startpart$ <= endpart$ then L50160
                 errormsg$ = "Ending part code must be greater than start~
        ~ing part code."
                 return

L50160:     if startstore$ <> "ALL" then L50190
               init(" ") endstore$
               goto L50260
L50190:          if endstore$ <> " " then L50210
                    endstore$ = startstore$
L50210:          if startstore$ <= endstore$ then L50260
                 errormsg$ = "Ending store code must be greater than star~
        ~ting store code."
                 return

L50260:     if startlot$ <> "ALL" then L50290
               init(" ") endlot$
               goto L50360
L50290:          if endlot$ <> " " then L50310
                    endlot$ = startlot$
L50310:          if startlot$ <= endlot$ then L50360
                 errormsg$ = "Ending lot number must be greater than star~
        ~ting lot number."
                 return

L50360:     if detail$ = "Y" then L50401
            if detail$ = "N" then L50401
            if detail$ = "K" then L50401
L50380:        errormsg$ = "DETAIL: Enter 'Y', 'N' or 'K', please."
               return

L50401:     if serial$ = "Y" then L50407
            if serial$ = "N" then L50407
            if serial$ = "S" then L50407
               errormsg$ = "SERIAL: Enter 'Y', 'N' or 'S', please."
               return

L50407:     p% = pos("YNK" = cycle$)
            if p% = 0% then goto L50380

            if mode$ = "A" then return
            if mode$ = "P" then return
               errormsg$ = "MODE: Enter 'A' or 'P', please."
               return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One moment please")

            end
