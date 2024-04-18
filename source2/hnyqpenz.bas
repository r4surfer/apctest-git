        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   QQQ   PPPP   EEEEE  N   N  ZZZZZ   *~
            *  H   H  NN  N  Y   Y  Q   Q  P   P  E      NN  N     Z    *~
            *  HHHHH  N N N   YYY   Q   Q  PPPP   EEEE   N N N    Z     *~
            *  H   H  N  NN    Y    Q Q Q  P      E      N  NN   Z      *~
            *  H   H  N   N    Y     QQQ   P      EEEEE  N   N  ZZZZZ   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYQPENZ - Zeros the pending withdrawl quantity fields in *~
            *            the HNYQUAN file.                              *~
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
            * 03/10/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            axd$4,                       /* The next big disease       */~
            date$8,                      /* Date for screen display    */~
            info$(15)79,                 /* Info & Warning Message     */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            rslt$20                      /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYQUAN  ! Inventory Costs & Quantity Master        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date : call "DATEFMT" (date$)
            str(line2$,62%) = "HNYQPENZ: " & str(cms2v$,,8%)

L09090:     i% = i% + 1%
            read str(info$(i%),10)
            if i% < 15% then L09090

        data                                                             ~
          "      *****  ZERO PENDING INVENTORY WITHDRAWALS  *****      ",~
          "                                                            ",~
          "   This program clears all Pending Inventory Withdrawals in ",~
          "the file HNYQUAN.  It is meant to aid you in data base clean",~
          "up following a system crash due to hardware failure.        ",~
          "                                                            ",~
          "   All existing inventory transactions should be processed  ",~
          "prior to execution.  Otherwise, you may be prevented from   ",~
          "withdrawing inventory that is on hand and/or allowed to     ",~
          "withdraw inventory that doesn't exist.                      ",~
          "                                                            ",~
          "   Since this program will lock the HNYQUAN file, no one    ",~
          "should be logged onto the data base while it is running.    ",~
          "                                                            ",~
          "Press RETURN to return to the menu -or- PF-16 to continue..."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Display warning message and get confirmation to proceed.  *~
            *************************************************************

            gosub confirmation_screen
                if keyhit%  = 16% then process_file
                if keyhit%  =  0% then exit_program
                                  goto confirmation_screen

        REM *************************************************************~
            *                 P R O C E S S   F I L E                   *~
            *-----------------------------------------------------------*~
            * Zero out the quantity pending field.                      *~
            *************************************************************

        process_file

*        First OPEN the HNYQUAN file in IO mode.
L11090:     call "SHOSTAT"  ("Opening & Locking HNYQUAN file")
            f2% = 1%
            call "OPENFILE" (#1, "IO   ", f2%, rslt$, axd$)
            if f2% = 0% then L11220
L11130:         keyhit% = 2%
                call "ASKUSER" (keyhit%, "FILE OPEN ERROR",              ~
                                "Unable to open and/or lock the HNYQUAN",~
                                "file. Please resolve problem and Press",~
                                "RETURN to Retry Open or PF-16 to Exit.")
                if keyhit% = 0% then L11090
                if keyhit% = 16% then exit_program
                goto L11130

L11220
*        Now zap dem little bugers.
            print page
            call "SHOSTAT" ("Zeroing Pending Quantities in HNYQUAN")

         loop
            call "READNXT1" (#1, f1%)
            if f1% = 0% then exit_program
                put #1 using L11300, 0
L11300:              FMT POS(109), PD(14,4)
                rewrite #1
                goto loop


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
            *          C O N F I R M A T I O N   S C R E E N            *~
            *-----------------------------------------------------------*~
            * Warn of possible impending doom and get OK to continue... *~
            *************************************************************

        confirmation_screen

L40170:     accept                                                       ~
               at (01,02), "Zero Pending Inventory Withdrawls",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), info$( 1%)             , ch(79),~
               at (06,02), fac(hex(8c)), info$( 2%)             , ch(79),~
               at (07,02), fac(hex(8c)), info$( 3%)             , ch(79),~
               at (08,02), fac(hex(8c)), info$( 4%)             , ch(79),~
               at (09,02), fac(hex(8c)), info$( 5%)             , ch(79),~
               at (10,02), fac(hex(8c)), info$( 6%)             , ch(79),~
               at (11,02), fac(hex(8c)), info$( 7%)             , ch(79),~
               at (12,02), fac(hex(8c)), info$( 8%)             , ch(79),~
               at (13,02), fac(hex(8c)), info$( 9%)             , ch(79),~
               at (14,02), fac(hex(8c)), info$(10%)             , ch(79),~
               at (15,02), fac(hex(8c)), info$(11%)             , ch(79),~
               at (16,02), fac(hex(8c)), info$(12%)             , ch(79),~
               at (17,02), fac(hex(8c)), info$(13%)             , ch(79),~
               at (18,02), fac(hex(8c)), info$(14%)             , ch(79),~
               at (19,02), fac(hex(8c)), info$(15%)             , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(RETURN)Exit Program",                       ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Zero Pending",                           ~
                     keys(hex(000d0f10)), key (keyhit%)

               if keyhit% <> 13% then L40430 : call "MANUAL" ("HNYQPENZ")
                                              goto L40170

L40430:        if keyhit% <> 15% then return: call "PRNTSCRN"
                                              goto L40170

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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

        exit_program
            call "SHOSTAT" ("Thank You, One Moment Please")
            end
