        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   JJJJJ BBBB   TTTTT   CCCC   CCCC  DDDD   IIIII  N   N   *~
            *      J  B   B    T    C      C      D   D    I    NN  N   *~
            *      J  BBBB     T    C      C      D   D    I    N N N   *~
            *   J  J  B   B    T    C      C      D   D    I    N  NN   *~
            *    JJ   BBBB     T     CCCC   CCCC  DDDD   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTCCDIN - Allows definition of Time Card Indirect        *~
            *            Activity codes.  Input is done in GENCDSIN     *~
            *            style.                                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/23/84 ! ORIGINAL                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cfac$(15)1,                  /* FACs for Codes             */~
            code$(15)6,                  /* Codes                      */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            date$8,                      /* Date For Screen Display    */~
            descr$(15)30,                /* Code Description           */~
            dfac$(15)1,                  /* Code description FACs      */~
            edtmessage$79,               /* Edit Screen Message        */~
            eof$3,                       /* End-Of-File Flag           */~
            errormsg$79,                 /* Error Message              */~
            filler$(15)63,               /* Rear-end of record         */~
            hdr0$9,                      /* Column Headings            */~
            hdr1$30,                     /*                            */~
            hdr2$4,                      /*                            */~
            i$(24)80,                    /* Screen Image               */~
            line2$79,                    /* 2nd screen line            */~
            pfac$(15)1,                  /* Length Facs                */~
            prl$(15)1,                   /* P/R task?                  */~
            pfdescr$(3,2)79,             /* PF Key Descriptions        */~
            pfkeys$(2)17,                /* PF KEYS (HEX)              */~
            plowkey$50                   /* Plow Key                   */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! JBTCCDES ! Time Card Indirect Activity Codes        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "JBTCCDES",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos =    1,  keylen = 6

        call "SHOSTAT" ("Opening Files, One Moment Please.")
            call "OPENCHCK" (#1, f1%, 0%, 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes Information Necessary For Program.            *~
            *************************************************************

            date$  = date  :  call "DATEFMT" (date$)
            str(line2$,62) = "JBTCCDIN: " & cms2v$

            pfdescr$(1,1) = "(RETURN)Manage code typed or at cursor  " & ~
                            "                       (13)Instructions"
            pfdescr$(2,1) = "(2)FIRST Screen        (10)MOVE Screen T" & ~
                            "o Entry Typed          (15)Print Screen"
            pfdescr$(3,1) = "(5)NEXT Screen         (14)Print Codes  " & ~
                            "                       (16)EXIT        "

            pfdescr$(1,2) = "(RETURN)SAVE DATA                       " & ~
                            "                       (13)Instructions"
            pfdescr$(2,2) = "(1)Start Line Over               (12)Del" & ~
                            "ete Code               (15)Print Screen"
            pfdescr$(3,2) = "                                        " & ~
                            "                                       "

            pfkeys$(1) = hex(ff02ffff05ffffffff0affff0d0e0f1000)
            pfkeys$(2) = hex(01ffffffffffffffffffff0c0dff0fff00)

            hdr0$      = "Task Code"
            hdr1$      = "Task Code Description   "
            hdr2$      = "P/R?"
            gosub load_first


        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *-----------------------------------------------------------*~
            *  Screen controls and branching.                           *~
            *************************************************************

        screen_loop
            errormsg$ = " "
L10080:     gosub'101(0%)      /* Get what is to be done               */
                if keyhit%  =  2 then gosub load_first
                if keyhit%  =  5 then gosub load_next
                if keyhit%  = 10 then gosub load_stated
                if keyhit%  = 14 then gosub report_printing
                if keyhit%  = 14 then gosub load_first
                if keyhit%  = 16 then L65000
                if keyhit% <>  0 then L10080
            errormsg$ = " "
            l% = cursor%(1) - 5%
            if l% <> 1% then L10240
            /* Add -or- Change a specifically mentioned code.          */
                if code$(1) <> " " then L10230
                    errormsg$ = "Code may not be blank"
                    goto L10080
L10230:         gosub load_specific
L10240:     if l% < 1 or l% > last% then L10080

            gosub'051(l%)      /* Set FACs for line entry              */
L10270:     gosub'101(l%)      /* Get field entries.                   */
                if keyhit%  =  1 then gosub startover
                if keyhit%  = 12 then gosub delete_code
                if keyhit% <>  0 then L10270
            gosub'151(l%)      /* Edit fields                          */
                if errormsg$ <> " "  then L10270
                gosub save_data
                goto screen_loop

        REM *************************************************************~
            *        S E T   F A C S   F O R   E N T R Y                *~
            *-----------------------------------------------------------*~
            *  Set FACs on for line modification.                       *~
            *************************************************************

        deffn'051(l%)
            dfac$(l%) = hex(80)
            pfac$(l%) = hex(81)
            return

        REM *************************************************************~
            *             S T A R T   O V E R                           *~
            *-----------------------------------------------------------*~
            *  Abort line modfification. Gives operator a second chance.*~
            *  Note that if code is displayed on screen we must reload  *~
            *  so that it's values revert to what they were before they *~
            *  we modified.                                             *~
            *************************************************************
        startover
L29090:     keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "* ABORT CURRENT ENTRY *",         ~
                            "Press (1) to RETURN to Display", "- OR -",  ~
                            "Press RETURN to ABORT Changes made to code")
            if keyhit1%  = 1% then return
                if keyhit1% <> 0% then L29090

                return clear           /* Enter- Abort Changes         */
                if l% <>  1% then L29210
                    gosub clear_first
                    goto screen_loop
L29210:         code$(1) = code$(2)
                gosub load_stated
                goto screen_loop

        REM *************************************************************~
            *           L O A D   D A T A   R O U T I N E S             *~
            * --------------------------------------------------------- *~
            *   Move a screen's worth of data into display arrays.      *~
            *************************************************************

        load_first        /* Load from top of file                     */
            plowkey$ = xor plowkey$
            goto load_data

        load_stated       /* Load from operator specified code         */
            if len(code$(1)) = 6% then                                   ~
              plowkey$ = str(code$(1),,5) & bin(val(str(code$(1),6,1))-1)~
              else plowkey$ = code$(1) & hex(00)
            gosub clear_first
            goto load_data

        load_next         /* Load from last code listed                */
            if eof$ <> "YES" then L30180
                errormsg$ = "ALREADY AT END OF FILE."
                return
L30180:     if last% = 1% then load_first
            plowkey$ = code$(last%) & hex(00)

        load_data
            errormsg$ = " "
            last% = 1
            eof$  = "NO"
            init (" ") code$(), descr$(), prl$(), filler$()

          plowloop
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%)
            if f1% = 1% then L30300
                eof$ = "YES"
                return
L30300:     last% = last% + 1%  :  l% = last%
            gosub get_record   /* Load record and format for display   */
          if l% < 15% then plowloop else return


        load_specific     /* Get specific code from file               */
                          /* If new, supply defaults.                  */
            call "READ100" (#1, code$(l%), f1%)
            if f1% <> 1% then L30610
                gosub get_record
                return
L30610:   /* Not on file-- setup with defaults               */
            descr$(l%) = " "
            prl$  (l%) = "Y"
         return


        get_record   /* Get record from buffer and format for display  */
            get #1 using L30850, code$(l%), descr$(l%), prl$(l%),         ~
                                filler$(l%)
        return


L30850:         FMT  CH( 6),             /* Code                       */~
                     CH(30),             /* Code Description           */~
                     CH( 1),             /* P/R? Y/N flag              */~
                     CH(63)              /* Filler                     */


        REM *************************************************************~
            *           S A V E   D A T A   O N    F I L E              *~
            *-----------------------------------------------------------*~
            *  Update file with code maintained (L%). After save, if    *~
            *  Code was on line 1 we redo screen with that code on top. *~
            *************************************************************
        save_data
            call "READ101" (#1, code$(l%), f1%)
            put #1 using L30850, code$(l%), descr$(l%), prl$(l%),         ~
                                filler$(l%)
            if f1% = 0% then write #1 else rewrite #1
            if f1% = 0% then call "CDANPOST" (#1, "A")                   ~
                        else call "CDANPOST" (#1, "C")

         /* Now make sure that code added/changed appears on screen.   */
         /* Also accessed by DELETE_CODE routine.                      */
         screen_align
            if l% <> 1% then L31180
            if code$(l%) > code$(last%) and last% < 15% then L31180
            if code$(l%) < code$(2) or                                   ~
                                      code$(l%) > code$(last%) then L31190

L31180:     code$(1) = code$(2)          /* Keeps the screen the same  */
                goto L31200
L31190:     code$(1) = code$(l%)         /* Put modified code on top   */
L31200:     gosub load_stated            /* Start display at CODE$(1)  */
          return

        REM *************************************************************~
            *             D E L E T E   C O D E                         *~
            *-----------------------------------------------------------*~
            *  Delete code from file after getting confirmation.        *~
            *************************************************************
        delete_code
L32070:     errormsg$ = "CODE: " & code$(l%) & " (" & descr$(l%) & ")"
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "**** DELETE CODE ****",           ~
                            errormsg$,                                   ~
                            "Press RETURN to DELETE this code,",         ~
                            "-OR- PF-1 to ABORT deletion.   ")
            errormsg$ = " "
            if keyhit1%  =  1% then return
            if keyhit1% <>  0% then L32070
                call "DELETE" (#1, code$(l%), 6%)
                call "CDANPOST" (#1, "D")
                code$(1) = code$(l%)
                gosub screen_align
                return clear
                goto screen_loop


        REM *************************************************************~
            *        R E P O R T   P R I N T I N G                      *~
            *-----------------------------------------------------------*~
            *  Print report for either ALL logical files -or- just one. *~
            *************************************************************
        report_printing
            page% = 0% :  line% = 857%
            call "SETPRNT" ("JB0008", " ", 0%, 0%)
            call "SHOSTAT" ("Printing Time Card Task Codes")
            select printer(134)
            plowkey$ = xor plowkey$

        report_loop
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%)
            if f1% = 1% then L33770
                call "SETPRNT" ("JB0008", " ", 0%, 1%)
                close printer  :  select ws  :  return
L33770:     get #1 using L33780, code$(1), descr$(1), prl$(1)
L33780:         FMT CH(6), CH(30), CH(1)

            if line% < 55% then L33990
                line% = 5%
                page% = page% + 1%

                print page
                print using L34040, date$, page%
                print
                print using L34070, "Code", "Code Description",           ~
                                   "P/R?"
                print using L34080

L33990:     print using L34070, code$(1), descr$(1), prl$(1)
            line% = line% + 1%
            goto report_loop


L34040: %########   JB0008      Time Card Indirect Task Code Listing     ~
        ~ ####
L34070: %             ########  ##############################  ####
L34080: %             --------  ------------------------------  ----

        REM *************************************************************~
            *        D A T A   E N T R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            *  Combination Display and Manage Screen.                   *~
            *                                                           *~
            *  There are essentially 2 entry points --                  *~
            *     line =  0 - Get from operator what to do. The code    *~
            *                 field on the 'wild line' is the only      *~
            *                 modifiable field.                         *~
            *     line <> 0 - Get entries for the line.                 *~
            *************************************************************
        deffn'101(l%)
            if l% <> 0% then L40260

          /* Case 1. Get what is to be done.                           */
            init (hex(84)) cfac$()  :  cfac$(1) = hex(81) /* Code FAC  */
            init (hex(8c)) dfac$()                        /* Line FAC  */
            init (hex(8c)) pfac$()                        /* P/R? FAC  */
            mode% = 1%
            edtmessage$ = "Enter Code to Add/Change.  Leave Blank" &     ~
                          " & Press RETURN to Find an existing Code"
            goto L40360

L40260:   /* Case 2. Modify line L%.                                   */
            init (hex(8c)) cfac$() : cfac$(l%) = hex(a4)
            edtmessage$ = "Modify fields then press RETURN to save."
            mode% = 2%

L40360: accept                                                           ~
          at(01,02),"Time Card Task Codes Maintenance",                  ~
          at(01,67), "Date:", fac(hex(8c)), date$               , ch(08),~
          at(02,02), fac(hex(ac)), line2$                       , ch(79),~
          at(04,02), fac(hex(94)), errormsg$,                            ~
                                                                         ~
          at(05,12), fac(hex(ac)), hdr0$,                                ~
          at(05,24), fac(hex(ac)), hdr1$,                                ~
          at(05,60), fac(hex(ac)), hdr2$,                                ~
                                                                         ~
          at(06,12), fac(cfac$( 1)),     code$( 1),                      ~
          at(07,12), fac(cfac$( 2)),     code$( 2),                      ~
          at(08,12), fac(cfac$( 3)),     code$( 3),                      ~
          at(09,12), fac(cfac$( 4)),     code$( 4),                      ~
          at(10,12), fac(cfac$( 5)),     code$( 5),                      ~
          at(11,12), fac(cfac$( 6)),     code$( 6),                      ~
          at(12,12), fac(cfac$( 7)),     code$( 7),                      ~
          at(13,12), fac(cfac$( 8)),     code$( 8),                      ~
          at(14,12), fac(cfac$( 9)),     code$( 9),                      ~
          at(15,12), fac(cfac$(10)),     code$(10),                      ~
          at(16,12), fac(cfac$(11)),     code$(11),                      ~
          at(17,12), fac(cfac$(12)),     code$(12),                      ~
          at(18,12), fac(cfac$(13)),     code$(13),                      ~
          at(19,12), fac(cfac$(14)),     code$(14),                      ~
          at(20,12), fac(cfac$(15)),     code$(15),                      ~
                                                                         ~
          at(06,24), fac(dfac$( 1)),             descr$( 1),             ~
          at(07,24), fac(dfac$( 2)),             descr$( 2),             ~
          at(08,24), fac(dfac$( 3)),             descr$( 3),             ~
          at(09,24), fac(dfac$( 4)),             descr$( 4),             ~
          at(10,24), fac(dfac$( 5)),             descr$( 5),             ~
          at(11,24), fac(dfac$( 6)),             descr$( 6),             ~
          at(12,24), fac(dfac$( 7)),             descr$( 7),             ~
          at(13,24), fac(dfac$( 8)),             descr$( 8),             ~
          at(14,24), fac(dfac$( 9)),             descr$( 9),             ~
          at(15,24), fac(dfac$(10)),             descr$(10),             ~
          at(16,24), fac(dfac$(11)),             descr$(11),             ~
          at(17,24), fac(dfac$(12)),             descr$(12),             ~
          at(18,24), fac(dfac$(13)),             descr$(13),             ~
          at(19,24), fac(dfac$(14)),             descr$(14),             ~
          at(20,24), fac(dfac$(15)),             descr$(15),             ~
                                                                         ~
          at(06,61), fac(pfac$( 1)),             prl$( 1),               ~
          at(07,61), fac(pfac$( 2)),             prl$( 2),               ~
          at(08,61), fac(pfac$( 3)),             prl$( 3),               ~
          at(09,61), fac(pfac$( 4)),             prl$( 4),               ~
          at(10,61), fac(pfac$( 5)),             prl$( 5),               ~
          at(11,61), fac(pfac$( 6)),             prl$( 6),               ~
          at(12,61), fac(pfac$( 7)),             prl$( 7),               ~
          at(13,61), fac(pfac$( 8)),             prl$( 8),               ~
          at(14,61), fac(pfac$( 9)),             prl$( 9),               ~
          at(15,61), fac(pfac$(10)),             prl$(10),               ~
          at(16,61), fac(pfac$(11)),             prl$(11),               ~
          at(17,61), fac(pfac$(12)),             prl$(12),               ~
          at(18,61), fac(pfac$(13)),             prl$(13),               ~
          at(19,61), fac(pfac$(14)),             prl$(14),               ~
          at(20,61), fac(pfac$(15)),             prl$(15),               ~
                                                                         ~
          at(21,02), fac(hex(a4)), edtmessage$,                          ~
          at(22,02), fac(hex(8c)), pfdescr$(1,mode%)            , ch(79),~
          at(23,02), fac(hex(8c)), pfdescr$(2,mode%)            , ch(79),~
          at(24,02), fac(hex(8c)), pfdescr$(3,mode%)            , ch(79),~
                                                                         ~
                keys(str(pfkeys$(mode%))),                               ~
                key (keyhit%)

               if keyhit% <> 13% then L41060
                  call "MANUAL" ("JBTCCDIN")
                  goto L40360

L41060:        if keyhit% <> 15% then L41100
                  call "PRNTSCRN"
                  goto L40360

L41100:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())

            if keyhit% <> 0% then return
                if mode% <> 1% then return
                if code$(1)        <> " " then return
                if cursor%(1) - 5% <> 1%  then return
                call "PLOWCODE" (#1,code$(1),descr$(1),0%,0.3,f1%)
                if f1% = 0% then L40360
                     get #1 using L41220, prl$(1)
L41220:                   FMT POS(37), CH(1)
                     return

        REM *************************************************************~
            *           E D I T   D A T A                               *~
            *-----------------------------------------------------------*~
            *  Edit data fields for line entered. If in error, set      *~
            *  message and FACs, else just return.                      *~
            *************************************************************
        deffn'151(l%)
            init (hex(8c)) dfac$(), pfac$()
            errormsg$ = " "

          /* Description                                               */
             if descr$(l%) <> " " then  L50160
                errormsg$ = "Sorry, Description Can't Be Blank"
                dfac$(l%) = hex(80)
                return

L50160:   /* Payroll Activity                                          */
            if pos("YN" = prl$(l%)) > 0% then return
                errormsg$ = "Enter 'Y' or 'N'"
                pfac$(l%) = hex(81)
                return

        REM *************************************************************~
            *        M I S C.  S U B - R O U T I N E S                  *~
            *************************************************************

        clear_first       /* Clear array bucket 1                      */
            code$(1), descr$(1), prl$(1), filler$(1) = " "
        return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
