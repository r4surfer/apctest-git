        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  TTTTT    A     SSS   K  K    CCC   TTTTT  L              *~
            *    T     A A   S      K K    C   C    T    L              *~
            *    T    AAAAA   SSS   KK     C        T    L              *~
            *    T    A   A      S  K KK   C   C    T    L              *~
            *    T    A   A   SSS   K  KK   CCC     T    LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TASKCTL  - Brings down/up bckgrnd tasks for this database *~
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
            * 10/15/85 ! ORIGINAL                                 ! HES *~
            * 07/17/86 ! Added BCKUPDATE to the list              ! ERN *~
            * 09/22/87 ! Corrected call to JB2TIF (new args)      ! HES *~
            * 11/01/88 ! Added CDAVSCOM to list of Tasks.         ! LDJ *~
            * 07/12/93 ! No Need for two passes to bring down     ! KAB *~
            *          !    both BCK and CDAVSCOM.                !     *~
            *          ! Regroup - CDA to the back of the bus!!   !     *~
            * 03/24/94 ! Provide 'SET Port ID Only' function      ! KAB *~
            *          ! Remove Port ID's (Unix)                  !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim date$8,                                                      ~
            errormsg$79,                                                 ~
            footer$79,                                                   ~
            floatmsg$79,                                                 ~
            inlib$8,                                                     ~
            header$79,                                                   ~
            what_am_i_supposed_to_do$1,                                  ~
            which$(6)1,                                                  ~
            work_variable$50,                                            ~
            do_it_now_or_when_alls_calm$1

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! VBKBUFFR ! BACKLOG BUFFER FOR PO HEADERS            *~
            * # 2 ! CDANRECV ! Caelus Data Acquisition Network Host Rec *~
            * # 3 ! BCKBUFFR ! Backlog Buffer for SO Headers            *~
            *************************************************************

            select #1,  "VBKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1044,                                  ~
                        keypos = 1, keylen = 10,                         ~
                        alternate key 1, keypos =  4, keylen =  7, dup,  ~
                                  key 3, keypos = 24, keylen = 16

            select #2,   "CDARECVE",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 768,                                   ~
                        keypos = 2,    keylen = 19


            select #3,  "BCKBUFFR",                                      ~
                        varc, indexed, recsize = 1020,                   ~
                        keypos = 1, keylen =   10,                       ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16


        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, open1%, 0%, 100%, " ")
            call "OPENCHCK" (#2, open2%, 0%,   0%, " ")
            call "OPENCHCK" (#3, open3%, 0%, 100%, " ")

        REM *************************************************************~
            * Check Platform                                            *~
            *************************************************************

            call "EXTRACT" addr("S#", str(temp$,,6))
            unix% = 0%
            convert str(temp$,,6) to unix%, data goto L09070
            goto L10000
L09070:         unix% = -1%
                goto L10000

L10000: REM *************************************************************~
            *       G E T   W H A T  T O   D O   T O   W H O ( M )      *~
            * --------------------------------------------------------- *~
            * Get what to do (up/down) and who to do it to (task).      *~
            *************************************************************

*          INPUT "UNIX", UNIX%
*
            date$ = date
            call "DATEFMT" (date$)
            header$ = "ALL Background Posting Tasks Are Affected"
            str(header$,63%) = "TASKCTL: " & cms2v$

            floatmsg$ = "('S' to Set Port ID Only"
            if unix% < 0% then                                           ~
               floatmsg$ = floatmsg$ & ", 'K' to Kill Message Ports)"    ~
                          else                                           ~
               floatmsg$ = floatmsg$ & ")"

L10110:     REM This is it...
L10120:     accept at(01,02), "Manage Background Posting Tasks",         ~
                   at(01,59), "Today's Date:",                           ~
                   at(01,73), fac(hex(8c)), date$               , ch(08),~
                   at(02,02), fac(hex(ac)), header$             , ch(79),~
                   at(03,02), fac(hex(94)), errormsg$           , ch(79),~
                   at(06,10), "Type 'U' To Bring Tasks Up, 'D' To Bring T~
        ~asks Down.",                                                     ~
                   what_am_i_supposed_to_do$                    , ch(01),~
                   at(07,10), fac(hex(8c)), floatmsg$           , ch(71),~
                   at(08,05), "Place a non-blank character next to the ta~
        ~sk(s) that are to be affected:",                                 ~
                   at(09,15), fac(hex(81)), which$(1),                   ~
                   at(10,15), fac(hex(81)), which$(2),                   ~
                   at(11,15), fac(hex(81)), which$(3),                   ~
                   at(12,15), fac(hex(81)), which$(4),                   ~
                   at(13,15), fac(hex(81)), which$(5),                   ~
                   at(14,15), fac(hex(81)), which$(6),                   ~
                   at(09,18), "Issue Components To Jobs      [JBPOST1 ]",~
                   at(10,18), "All other Job Updates         [JBPOST2 ]",~
                   at(11,18), "Purchase Order Updating       [VBKUPDTE]",~
                   at(12,18), "Sales Order Updating          [BCKUPDTE]",~
                   at(13,18), "CDA Transaction Decoder       [CDATOCMS]",~
                   at(14,18), "CDA to VS Communications      [CDAVSCOM]",~
                                                                         ~
                   at(21,02), fac(hex(ac)), footer$             , ch(79),~
                   at(22,65), "(13)Instructions",                        ~
                   at(23,65), "(15)Print Screen",                        ~
                   at(24,65), "(16)Exit Program",                        ~
                   keys(hex(000d0f10)),                                  ~
                   key(keyhit%)

            if keyhit% <> 13 then L10360
                call "MANUAL" ("TASKCTL")
                goto L10120

L10360:     if keyhit% <> 15 then L10400
                call "PRNTSCRN"
                goto L10120

L10400:     if keyhit% = 16 then exit_program

            errormsg$ = " "
            if unix% < 0% then L10424
               if pos("DUS" = what_am_i_supposed_to_do$) <> 0% then L10430
                  goto L10460
L10424:        if pos("DUSK" = what_am_i_supposed_to_do$) <> 0% then L10430
                  goto L10460
L10430:     if str(which$()) <> " " then L10450
                errormsg$ = "Please specify which task(s)." :  goto L10110

L10450:     on pos("DUSK" = what_am_i_supposed_to_do$) goto L10480,       ~
                                                            L13000,       ~
                                                            L14000,       ~
                                                            L15000
L10460:         errormsg$ = "Enter 'U'p or 'D'own."
                errormsg$ = errormsg$ & " ('S' to Set Port Id's"
                if unix% < 0% then                                       ~
                  errormsg$ = errormsg$ & ", 'K' to Kill Message Port)." ~
                              else                                       ~
                  errormsg$ = errormsg$ & ")."
                goto L10110

L10480
*        Bring'em Down; But How Soon????
L10490:     accept at(01,02), "Manage Background Posting Tasks",         ~
                   at(01,59), "Today's Date:",                           ~
                   at(01,73), fac(hex(8c)), date$               , ch(08),~
                   at(02,02), fac(hex(ac)), header$             , ch(79),~
                   at(08,14), "The Task(s) indicated below will be affect~
        ~ed:",                                                            ~
                   at(09,15), fac(hex(84)), which$(1),                   ~
                   at(10,15), fac(hex(84)), which$(2),                   ~
                   at(11,15), fac(hex(84)), which$(3),                   ~
                   at(12,15), fac(hex(84)), which$(4),                   ~
                   at(13,15), fac(hex(84)), which$(5),                   ~
                   at(14,15), fac(hex(84)), which$(6),                   ~
                   at(09,18), "Issue Components To Jobs      [JBPOST1 ]",~
                   at(10,18), "All other Job Updates         [JBPOST2 ]",~
                   at(11,18), "Purchase Order Updating       [VBKUPDTE]",~
                   at(12,18), "Sales Order Updating          [BCKUPDTE]",~
                   at(13,18), "CDA Transaction Decoder       [CDATOCMS]",~
                   at(14,18), "CDA to VS Communications      [CDAVSCOM]",~
                                                                         ~
                   at(16,02), "Type 'I' To End Tasks IMMEDIATELY, 'W' To ~
        ~Bring Down When Tasks Idles", do_it_now_or_when_alls_calm$,ch(1),~
                                                                         ~
                   at(21,02), fac(hex(ac)), footer$             , ch(79),~
                   at(22,02), "(1)Respecify Selections",                 ~
                   at(22,65), "(13)Instrcutions",                        ~
                   at(23,65), "(15)Print Screen",                        ~
                   at(24,65), "(16)Exit Program",                        ~
                   keys(hex(00010d0f10)),                                ~
                   key(keyhit%)

            if keyhit% =  1 then L10110

            if keyhit% <> 13 then L10750
                call "MANUAL" ("TASKCTL")
                goto L10490

L10750:     if keyhit% <> 15 then L10790
                call "PRNTSCRN"
                goto L10490

L10790:     if keyhit% = 16 then exit_program
            on pos("IW" = do_it_now_or_when_alls_calm$) goto L11000, L12000
            goto L10490

L11000: REM *************************************************************~
            *            B R I N G   T A S K S   D O W N                *~
            * --------------------------------------------------------- *~
            * Drop tasks IMMEDIATELY.   (Send CANCEL Message).          *~
            *************************************************************

            call "EXTRACT" addr("IL", inlib$)
            call "SHOSTAT" ("Terminating Background Tasks For Data Base: ~
        ~" & inlib$)

*        Do Shop Floor Control Tasks...
         if which$(1) = " " then L11140
            call "JB2TIF" ("J1", 1%, 0%, 99%, hex(01), " ", " ", " ", 0%,~
                               " ", " ", " ", " ", " ", 0, " ", " ", " ")
L11140:  if which$(2) = " " then L11180
            call "JB2TIF" ("J2", 1%, 0%, 99%, hex(01), " ", " ", " ", 0%,~
                               " ", " ", " ", " ", " ", 0, " ", " ", " ")

L11180
*        Now Purchasing Task...
            if open1% <> 1% or which$(3) = " " then L11250
            work_variable$ = all(hex(01))
            write #1, using L12220," ",work_variable$," "," "," "," "," ",~
                eod goto L11230
L11230:     call "TASKUP" ("PO", 9999%)

L11250
*        Now the Sales Order Update Task....
            if open3% <> 1% or which$(4) = " " then L11340
            work_variable$ = all(hex(01))
            write #3, using L11300," ",work_variable$," "," "," "," "," ",~
                                                           eod goto L11310
L11300:         FMT CH(3), CH(7), 4*CH(250), CH(10)
L11310:     call "TASKUP" ("SO", 9999%)

L11340
*        CDAN Transaction Decoder...
            if open2% <> 1% or which$(5) = " " then L11420
            work_variable$ = all(hex(01))
            write #2, using L11390,"N",work_variable$," ", " ", " ",      ~
                                                           eod goto L11400
L11390:         FMT CH(1), CH(6), CH(249), 2*CH(256)
L11400:     call "TASKUP" ("CD", 9999%)

L11420
*        Now the CDA to VS Communications Task...
            if which$(6) = " " then exit_program
            call "TASKUP" ("TK", 9999%)
            goto exit_program

L12000: REM *************************************************************~
            *            B R I N G   T A S K S   D O W N                *~
            * --------------------------------------------------------- *~
            * Drop tasks When All's Calm (ABORT).                       *~
            *************************************************************

            call "EXTRACT" addr("IL", inlib$)
            call "SHOSTAT" ("Requesting Background Tasks To Terminate For~
        ~ Data Base: " & inlib$)

*        Do Shop Floor Control Tasks...
          if which$(1) = " " then L12140
            call "JB2TIF" ("J1", 1%, 0%, 99%, hex(ff), " ", " ", " ", 0%,~
                               " ", " ", " ", " ", " ", 0, " ", " ", " ")
L12140:   if which$(2) = " " then L12170
            call "JB2TIF" ("J2", 1%, 0%, 99%, hex(ff), " ", " ", " ", 0%,~
                               " ", " ", " ", " ", " ", 0, " ", " ", " ")
L12170
*        Now Purchasing Task...
            if open1% <> 1 or which$(3) = " " then L12250
            work_variable$ = all(hex(ff))
            write #1, using L12220," ",work_variable$," "," "," "," "," ",~
                eod goto L12230
L12220:     FMT CH(3), CH(7), 4*CH(250), CH(34)
L12230:     call "TASKUP" ("PO", 9999%)

L12250
*        Now Sales Orders
            if open3% <> 1% or which$(4) = " " then L12330
            work_variable$ = all(hex(ff))
            write #3, using L12300," ",work_variable$," "," "," "," "," ",~
                                                          eod goto L12310
L12300:         FMT CH(3), CH(7), 4*CH(250), CH(10)
L12310:     call "TASKUP" ("SO", 9999%)

L12330
*        CDAN Transaction Decoder...
            if open2% <> 1 or which$(5) = " " then L12420
            work_variable$ = all(hex(ff))
            write #2, using L12380,"N",work_variable$," ", " ", " ",      ~
                                                           eod goto L12390
L12380:         FMT CH(1), CH(6), CH(249), 2*CH(256)
L12390:     call "TASKUP" ("CD", 9999%)

L12420
*        Now the CDA to VS Communications Task...
            if which$(6) = " " then exit_program
            call "TASKUP" ("TK", 9998%)
            goto exit_program

L13000: REM *************************************************************~
            *            B R I N G   T A S K S   U P                    *~
            * --------------------------------------------------------- *~
            * Submit tasks requested.                                   *~
            *************************************************************

            call "EXTRACT" addr("IL", inlib$)
            call "SHOSTAT" ("Attempting To Bring Up Background Tasks For ~
        ~Data Base: " & inlib$)
*        Do Shop Floor Control Tasks...
            if which$(1) <> " " then call "JB2TIF" ("J1", 2%, 0%)
            if which$(2) <> " " then call "JB2TIF" ("J2", 2%, 0%)
*        Do Purchasing Task...
            if which$(3) <> " " then call "TASKUP" ("PO", 0%)
*        Now Sales Orders
            if which$(4) <> " " then call "TASKUP" ("SO", 0%)
*        Now CDAN Decoder Tasks...
            if which$(5) <> " " then call "TASKUP" ("CD", 0%)
            if which$(6) <> " " then call "TASKUP" ("TK", 0%)

            goto exit_program

L14000: REM *************************************************************~
            *            S E T   P O R T   I D   O N L Y                *~
            * --------------------------------------------------------- *~
            * Submit tasks requested.                                   *~
            *************************************************************

            call "EXTRACT" addr("IL", inlib$)
            call "SHOSTAT"                                               ~
                 ("Attempting To Set Port ID's For Data Base: " & inlib$)

*        Do Shop Floor Control Tasks...
            if which$(1) <> " " then call "TASKUP" ("J1", 8000%)
            if which$(2) <> " " then call "TASKUP" ("J2", 8000%)
*        Do Purchasing Task...
            if which$(3) <> " " then call "TASKUP" ("PO", 8000%)
*        Now Sales Orders
            if which$(4) <> " " then call "TASKUP" ("SO", 8000%)
*        Now CDAN Decoder Tasks...
            if which$(5) <> " " then call "TASKUP" ("CD", 8000%)
            if which$(6) <> " " then call "TASKUP" ("TK", 8000%)

            goto exit_program


L15000: REM *************************************************************~
            *  DESTROY MESSAGE PORT (UNIX ONLY)                         *~
            * --------------------------------------------------------- *~
            * Submit tasks requested.                                   *~
            *************************************************************

            if unix% < 0% then L15060
               goto exit_program

L15060:     call "EXTRACT" addr("IL", inlib$)
            call "SHOSTAT"                                               ~
            ("Attempting To Remove Message Ports for Data Base: "        ~
             & inlib$)

*        Do Shop Floor Control Tasks...
            if which$(1) <> " " then call "TASKUP" ("J1", 8001%)
            if which$(2) <> " " then call "TASKUP" ("J2", 8001%)
*        Do Purchasing Task...
            if which$(3) <> " " then call "TASKUP" ("PO", 8001%)
*        Now Sales Orders
            if which$(4) <> " " then call "TASKUP" ("SO", 8001%)
*        Now CDAN Decoder Tasks...
            if which$(5) <> " " then call "TASKUP" ("CD", 8001%)
            if which$(6) <> " " then call "TASKUP" ("TK", 8001%)

            goto exit_program

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
