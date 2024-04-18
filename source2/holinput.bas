        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H   OOO   L      IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  H   H  O   O  L        I    NN  N  P   P  U   U    T     *~
            *  HHHHH  O   O  L        I    N N N  PPPP   U   U    T     *~
            *  H   H  O   O  L        I    N  NN  P      U   U    T     *~
            *  H   H   OOO   LLLLL  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HOLINPUT - Allows user to indicate dates that the plant   *~
            *            will be closed due to holidays.  Can optionally*~
            *            update WC capacity with this info.             *~
            *            This list is refered to when defining W.C.     *~
            *            capacity in WCINPUT.                           *~
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
            * 12/07/82 ! ORIGINAL                                 ! GLW *~
            * 09/17/85 ! Eliminated HOLMASTR, SYSFILE2 instead    ! HES *~
            * 08/09/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim avail%(490),                 /*                            */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dd$(31)3,                    /*                            */~
            dh$(31)1,                    /*                            */~
            dow$(490)3,                  /* DAYS OF WEEK FROM PLAN CAL */~
            dow$21,                      /* DAYS OF WEEK, 3 CHAR EACH  */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$101,                  /* FILLER TEXT FOR HOLMASTR   */~
            h$(490)1,                    /* AS LOADED                  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            mm%(490),                    /* MONTH OF EACH DAY IN CALDR */~
            modate$(12)9,                /*                            */~
            packed$245,                  /* WORK VARIABLE              */~
            plant$20,                    /* KEY FOR HOLIDAY RCRD IN SYS*/~
            readkey$40,                  /* WORK VARIABLE FOR READS    */~
            record1$59,                  /* FIRST PART OF WC RECORD    */~
            record2$(5)197,              /* YEAR OF EACH DAY IN CALDR  */~
            tdate$8,                     /* Temporary Date Variable    */~
            yy%(490)                     /*                            */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

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
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #20 ! SYSFILE2 ! SYSTEM CATCH ALL FILE                    *~
            * #11 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            *************************************************************


           select #11, "WCMASTR",                                        ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 2024,                                   ~
                       keypos =  2, keylen = 5,                          ~
                       alt key 1, keypos = 1 , keylen = 6

           select #12, "CALMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1962,                                   ~
                       keypos = 1, keylen = 2

           select #20, "SYSFILE2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1, keylen = 20


           call "SHOSTAT" ("Preparing For Plant Holiday Schedule Manageme~
        ~nt")

           call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
           call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
           call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))

           REM Create Files As Required...
           if f2%(20) = 0 then L02450
           call "OPENFILE" (#20, "OUTPT", f2%(20), rslt$(20), axd$(20))
                close #20
           call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))

L02450:    if f2%(12) = 0 then L09000
           stop "Planning Calendar Must Be Set Up First" : goto L65000

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            modate$(01) = "JANUARY  "
            modate$(02) = "FEBRUARY "
            modate$(03) = "MARCH    "
            modate$(04) = "APRIL    "
            modate$(05) = "MAY      "
            modate$(06) = "JUNE     "
            modate$(07) = "JULY     "
            modate$(08) = "AUGUST   "
            modate$(09) = "SEPTEMBER"
            modate$(10) = "OCTOBER  "
            modate$(11) = "NOVEMBER "
            modate$(12) = "DECEMBER "

            plant$ = "HOLIDAY SCHEDULE"  /* NOTE!!! HARDWIRED PLANT    */

            dow$="MONTUEWEDTHUFRISATSUN"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, filler$

            gosub the_input_screen_and_other_things_section

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L30000     /* WRITE THE DATA  */
            goto L65000

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

L29938:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29962, L29968, L29972
               return

L29962:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29968:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29972:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29938

L30000: REM *************************************************************~
            *      WRITE HOLIDAY SCHEDULE DATA TO FILE                  *~
            *                                                           *~
            *************************************************************

            if errormsg$ <> " " then L65000
            for i% = 1 to 490
                if h$(i%) = " " then h$(i%) = "0" else h$(i%) = "1"
            next i%
            hexpack packed$ from str(h$())
            call "READ101" (#20, plant$, f1%(20))
                if f1%(20) <> 0 then delete #20
            write #20, using L30130, plant$, packed$, " "
L30130:     FMT CH(20), CH(245), CH(235)
            return

L31000: rem**************************************************************~
            *      load existing holiday schedule from file             *~
            *       and calendar info from file                         *~
            *************************************************************

            init(" ") h$()
            call "READ100" (#20, plant$, f1%(20))
                if f1%(20) = 0 then L31150
            get #20, using L31090, packed$
L31090:     FMT XX(20), CH(245)
            hexunpack packed$ to h$()
            for i% = 1 to 490
                if h$(i%) = "0" then h$(i%) = " " else h$(i%) = "X"
            next i%

L31150:     call "READ100" (#12, "30", f1%(12))
                if f1%(12) = 0 then L31250
                get #12, using L31180, mm%()
L31180:         FMT XX(2), 490*BI(4)
            call "READ100" (#12, "50", f1%(1))
                if f1%(12) = 0 then L31250
                get #12, using L31220, dow$()
L31220:         FMT XX(2), 490*CH(3)
            call "READ100" (#12, "20", f1%(1))
                if f1%(12) <> 0 then L31280
L31250:         errormsg$ = "THERE IS NO CALENDAR, PLEASE EXIT & CREATE O~
        ~NE"
                goto L65000
L31280:         get #12, using L31290, yy%()
L31290:         FMT XX(2), 490*BI(4)
                return

        REM *************************************************************~
            *      UPDATE ALL WORK CENTERS FOR NEW HOLIDAY SCHEDULE     *~
            *                                                           *~
            *************************************************************

        changewcs
           readkey$ = " "
L33070:    call "PLOWNXT1" (#11, readkey$, 0%, f1%(11))
                 if f1%(11) = 0 then datasave
           get #11, using L33100, record1$, avail%(), record2$()
L33100:    FMT CH(59), 490*BI(2), 5*CH(197)
           call "SHOSTAT" ("Updating Work Center: " & str(readkey$,,4))

           for i% = 1 to 490
               if h$(i%) <> " " then avail%(i%) = 0
           next i%
           rewrite #11, using L33100, record1$, avail%(), record2$()
           goto L33070

L40000: REM *************************************************************~
            *                  E N T E R   D A T A                      *~
            *                                                           *~
            * INPUTS DATA FOR THE ITEMS ON PAGE 1.                      *~
            *************************************************************

            the_input_screen_and_other_things_section
                  errormsg$ = " "

            gosub L31000     /* LOAD EXISTING DATA IF THERE  */

            call "DATEFMT"(date, 0%, tdate$)
            convert str(tdate$,5%,2%) to mo%
            convert str(tdate$,1%,4%) to yr%

L40130:     f% = 0%
            for i = 1 to 490
            if yy%(i) < yr% then L40200
            if yy%(i) > yr% then L40230
            if mm%(i) < mo% then L40200
            if mm%(i) > mo% then L40230
            if f% =  0 then f% = i
L40200:     next i
            i = 491

L40230:     l% = i - 1

            j = 0
            init(" ") dh$(), dd$()
            for i = f% to l%
                j = j + 1
                dh$(j) = h$(i)
                dd$(j) = dow$(i)
            next i

L40330: accept                                                           ~
               at (01,09), "HOLIDAY SCHEDULE",                           ~
               at (01,44), "MONTH OF:",                                  ~
               at (01,54), fac(hex(84)), modate$(mo%)           , ch(09),~
               at (01,68), fac(hex(84)), yr%, pic(####),                   ~
               at (03,03),                                               ~
        "DAY   PLANT CLOSED                    ! DAY   PLANT CLOSED",    ~
               at (04,03), "01",                 at (04,41), "!  16",    ~
               at (05,03), "02",                 at (05,41), "!  17",    ~
               at (06,03), "03",                 at (06,41), "!  18",    ~
               at (06,64), "****************",   at (07,03), "04",       ~
               at (07,41), "!  19",                                      ~
               at (07,64), "* move cursor  *",   at (08,03), "05",       ~
               at (08,41), "!  20",                                      ~
               at (08,64), "* to day and   *",   at (09,03), "06",       ~
               at (09,41), "!  21",                                      ~
               at (09,64), "* press ENTER  *",   at (10,03), "07",       ~
               at (10,41), "!  22",                                      ~
               at (10,64), "* to chang the *",   at (11,03), "08",       ~
               at (11,41), "!  23",                                      ~
               at (11,64), "* marker       *",   at (12,03), "09",       ~
               at (12,41), "!  24",                                      ~
               at (12,64), "* now shown    *",   at (13,03), "10",       ~
               at (13,41), "!  25",                                      ~
               at (13,64), "****************",   at (14,03), "11",       ~
               at (14,41), "!  26",              at (15,03), "12",       ~
               at (15,41), "!  27",              at (16,03), "13",       ~
               at (16,41), "!  28",              at (17,03), "14",       ~
               at (17,41), "!  29",              at (18,03), "15",       ~
               at (18,41), "!  30",              at (19,41), "!  31",    ~
               at (20,03),                                               ~
        "An X beside the day shows that the plant will be CLOSED due to a~
        ~ holiday.",                                                      ~
               at (21,03),                                               ~
        "The marked days will override the normal hrs worked when enterin~
        ~g hrs for WCs",                                                  ~
               at (23,03),                                               ~
        "Use PF keys:   (4)PREV MONTH   (5)NEXT MONTH  (14)SAVE DATA, CHG~
        ~ ALL CUR WCS",                                                   ~
               at (24,03),                                               ~
        "   for     :   (12)EXIT PGM    (15)PRT SCRN   (16)SAVE DATA, DO ~
        ~NOT CHG WCS",                                                    ~
            at(04,14), fac(hex(84)), dh$(01), ch(1),                     ~
            at(05,14), fac(hex(84)), dh$(02), ch(1),                     ~
            at(06,14), fac(hex(84)), dh$(03), ch(1),                     ~
            at(07,14), fac(hex(84)), dh$(04), ch(1),                     ~
            at(08,14), fac(hex(84)), dh$(05), ch(1),                     ~
            at(09,14), fac(hex(84)), dh$(06), ch(1),                     ~
            at(10,14), fac(hex(84)), dh$(07), ch(1),                     ~
            at(11,14), fac(hex(84)), dh$(08), ch(1),                     ~
            at(12,14), fac(hex(84)), dh$(09), ch(1),                     ~
            at(13,14), fac(hex(84)), dh$(10), ch(1),                     ~
            at(14,14), fac(hex(84)), dh$(11), ch(1),                     ~
            at(15,14), fac(hex(84)), dh$(12), ch(1),                     ~
            at(16,14), fac(hex(84)), dh$(13), ch(1),                     ~
            at(17,14), fac(hex(84)), dh$(14), ch(1),                     ~
            at(18,14), fac(hex(84)), dh$(15), ch(1),                     ~
            at(04,54), fac(hex(84)), dh$(16), ch(1),                     ~
            at(05,54), fac(hex(84)), dh$(17), ch(1),                     ~
            at(06,54), fac(hex(84)), dh$(18), ch(1),                     ~
            at(07,54), fac(hex(84)), dh$(19), ch(1),                     ~
            at(08,54), fac(hex(84)), dh$(20), ch(1),                     ~
            at(09,54), fac(hex(84)), dh$(21), ch(1),                     ~
            at(10,54), fac(hex(84)), dh$(22), ch(1),                     ~
            at(11,54), fac(hex(84)), dh$(23), ch(1),                     ~
            at(12,54), fac(hex(84)), dh$(24), ch(1),                     ~
            at(13,54), fac(hex(84)), dh$(25), ch(1),                     ~
            at(14,54), fac(hex(84)), dh$(26), ch(1),                     ~
            at(15,54), fac(hex(84)), dh$(27), ch(1),                     ~
            at(16,54), fac(hex(84)), dh$(28), ch(1),                     ~
            at(17,54), fac(hex(84)), dh$(29), ch(1),                     ~
            at(18,54), fac(hex(84)), dh$(30), ch(1),                     ~
            at(19,54), fac(hex(84)), dh$(31), ch(1),                     ~
                                                                         ~
            at(04,09), fac(hex(8c)), dd$(01), ch(3),                     ~
            at(05,09), fac(hex(8c)), dd$(02), ch(3),                     ~
            at(06,09), fac(hex(8c)), dd$(03), ch(3),                     ~
            at(07,09), fac(hex(8c)), dd$(04), ch(3),                     ~
            at(08,09), fac(hex(8c)), dd$(05), ch(3),                     ~
            at(09,09), fac(hex(8c)), dd$(06), ch(3),                     ~
            at(10,09), fac(hex(8c)), dd$(07), ch(3),                     ~
            at(11,09), fac(hex(8c)), dd$(08), ch(3),                     ~
            at(12,09), fac(hex(8c)), dd$(09), ch(3),                     ~
            at(13,09), fac(hex(8c)), dd$(10), ch(3),                     ~
            at(14,09), fac(hex(8c)), dd$(11), ch(3),                     ~
            at(15,09), fac(hex(8c)), dd$(12), ch(3),                     ~
            at(16,09), fac(hex(8c)), dd$(13), ch(3),                     ~
            at(17,09), fac(hex(8c)), dd$(14), ch(3),                     ~
            at(18,09), fac(hex(8c)), dd$(15), ch(3),                     ~
            at(04,49), fac(hex(8c)), dd$(16), ch(3),                     ~
            at(05,49), fac(hex(8c)), dd$(17), ch(3),                     ~
            at(06,49), fac(hex(8c)), dd$(18), ch(3),                     ~
            at(07,49), fac(hex(8c)), dd$(19), ch(3),                     ~
            at(08,49), fac(hex(8c)), dd$(20), ch(3),                     ~
            at(09,49), fac(hex(8c)), dd$(21), ch(3),                     ~
            at(10,49), fac(hex(8c)), dd$(22), ch(3),                     ~
            at(11,49), fac(hex(8c)), dd$(23), ch(3),                     ~
            at(12,49), fac(hex(8c)), dd$(24), ch(3),                     ~
            at(13,49), fac(hex(8c)), dd$(25), ch(3),                     ~
            at(14,49), fac(hex(8c)), dd$(26), ch(3),                     ~
            at(15,49), fac(hex(8c)), dd$(27), ch(3),                     ~
            at(16,49), fac(hex(8c)), dd$(28), ch(3),                     ~
            at(17,49), fac(hex(8c)), dd$(29), ch(3),                     ~
            at(18,49), fac(hex(8c)), dd$(30), ch(3),                     ~
            at(19,49), fac(hex(8c)), dd$(31), ch(3),                     ~
                                                                         ~
                keys(hex(0004050c0d0e0f10)), key(keyhit%)                ~

            if keyhit% = 16 then datasave
            if keyhit% = 14 then changewcs
            if keyhit% = 12 then L65000
            if keyhit% =  0 then L41650
            if keyhit% <> 4 then L41510
                if f% = 1 then L40330
                mo% = mm%(f%-1)
                yr% = yy%(f%-1)
                goto L40130
L41510:     if keyhit% <> 5 then L41570
                if l% = 490 then L40330
                mo% = mm%(l%+1)
                yr% = yy%(l%+1)
                goto L40130

L41570:     if keyhit% <> 13 then L41610
                call "MANUAL" ("HOLINPUT")
                goto L40000

L41610:     if keyhit% <> 15 then L40330
                call "PRNTSCRN"
                goto L40330

L41650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                if cursor%(1) < 4 or cursor%(1) > 19 then L40330
                if cursor%(1) > 18 and cursor%(2) < 40 then L40330
               field%  = cursor%(1) - 3
               if cursor%(2) > 40 then field% = field% + 15
               if field% - 1 + f% > 490 then L40330
               if field% - 1 + f% < 1   then L40330
               if dd$(field%) = " "    then L40330
               if dh$(field%) =  " " then  L41770
                dh$(field%), h$(field%-1%+f%) = " "
                goto L40330
L41770:        dh$(field%), h$(field%-1%+f%) = "X"
               goto L40330

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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
