        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   PPPP   U   U  RRRR    GGG   EEEEE          *~
            *  S      A   A  P   P  U   U  R   R  G      E              *~
            *   SSS   AAAAA  PPPP   U   U  RRRR   G GGG  EEEE           *~
            *      S  A   A  P      U   U  R   R  G   G  E              *~
            *   SSS   A   A  P       UUU   R   R   GGG   EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAPURGE  - Program accepts a year from the operator,      *~
            *            validates it, and upon confirmation, purges    *~
            *            a year of data from the SA Summary files.      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/19/86 ! Original                                 ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            hdr$80,                      /* ASKUSER constant           */~
            i$(24)80,                    /* Screen Image               */~
            inpfac$1,                    /* FAC for INPMESSAGE$        */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            msg$(3)80,                   /* ASKUSER constants          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            purge_cd$6,                  /* Year to purge (unformatted)*/~
            purge_yr$8,                  /* Year to purge (formatted)  */~
            sysfile2_key$20,             /* Key to SYSFILE2            */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "04.18.02 03/30/87 Patch release                   "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                 S E L E C T   F I L E S                   *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SASUMRY0 ! Sales Analysis Summary File              *~
            * #02 ! SASUMRY1 ! Sales Analysis Summary File              *~
            * #03 ! SASUMRY2 ! Sales Analysis Summary File              *~
            * #04 ! SASUMRY3 ! Sales Analysis Summary File              *~
            * #05 ! SASUMRY4 ! Sales Analysis Summary File              *~
            * #06 ! SASUMRY5 ! Sales Analysis Summary File              *~
            * #07 ! SASUMRY6 ! Sales Analysis Summary File              *~
            * #08 ! SASUMRY7 ! Sales Analysis Summary File              *~
            * #09 ! SASUMRY8 ! Sales Analysis Summary File              *~
            * #10 ! SASUMRY9 ! Sales Analysis Summary File              *~
            * #11 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~

            select #01, "SASUMRY0",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #02, "SASUMRY1",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #03, "SASUMRY2",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #04, "SASUMRY3",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #05, "SASUMRY4",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #06, "SASUMRY5",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #07, "SASUMRY6",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #08, "SASUMRY7",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #09, "SASUMRY8",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #10,  "SASUMRY9",                                     ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =  993, keylen =  56,         ~
                            key  2, keypos = 1024, keylen =  25, dup

            select #11,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select pool #01,#02,#03,#04,#05,#06,#07,#08,#09,#10, blocks=2

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program, issues     *~
            * operator messages, and OPENs all available files.         *~
            *************************************************************

        REM See if this User is a Data Base or SA Module Administrator
            call "CMSMACHK" ("SA", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then goto initial_1
L09100:         int% = 0%
                hdr$ = "*** SECURITY CHECK ***"
                msg$(1) = "Sales Analysis PURGE"
                msg$(2) = "You must be a Data Base or S/A Module Admin"  ~
                     & "istrator to run this program."
                msg$(3) = "Press (RETURN) to terminate"
                call "ASKUSER" (int%, hdr$, msg$(1), msg$(2), msg$(3))
                if int% <> 0% then goto L09100
                goto L65000

        initial_1
L09210:     int% = 0%
            hdr$ = "*** SALES ANALYSIS PURGE ADVISORY ***"
            msg$(1) = "It is YOUR responsibility to have adequate" &     ~
                 " BACKUPs prior to PURGING"
            msg$(2) = "Press PF(16) to terminate               -- OR --"
            msg$(3) = "Press (RETURN) to continue"
            call "ASKUSER" (int%, hdr$, msg$(1), msg$(2), msg$(3))
            if int% = 16% then goto L65000
            if int% <> 0% then goto L09210

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#02, fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#03, fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#04, fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#05, fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#06, fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#07, fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#08, fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#09, fs%(9 ), f2%(9 ), 0%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))

            if min(f2%()) = 0% then goto initial_3
L09450:         int% = 0%
                hdr$ = "*** DATA BASE FAULT ***"
                msg$(1) = "Sales Analysis PURGE"
                msg$(2) = "There are no Sales Analysis Summary files " & ~
                     "to PURGE"
                msg$(3) = "-> Press (RETURN) to terminate"
                call "ASKUSER" (int%, hdr$, msg$(1), msg$(2), msg$(3))
                if int% <> 0% then goto L09450
                goto L65000

        initial_3
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))

            if f2%(11) = 0% then goto initial_4
L09590:         int% = 0%
                hdr$ = "*** SYSFILE2 MISSING ***"
                msg$(1) = "Sales Analysis PURGE"
                msg$(2) = "There is no System Control file ('SYSFILE2')"
                msg$(3) = "-> Press (RETURN) to terminate"
                call "ASKUSER" (int%, hdr$, msg$(1), msg$(2), msg$(3))
                if int% <> 0% then goto L09590
                goto L65000

        initial_4
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            sysfile2_key$ = "SA.YEARS.USED."
            str(sysfile2_key$,15) = all(hex(00))
            call "PLOWNEXT" (#11, sysfile2_key$, 14%, f1%(11))
            if f1%(11) <> 0% then goto L10200
L10110:         int% = 0%
                hdr$ = "*** NO MORE S/A YEARS ***"
                msg$(1) = "There are no more S/A 'years' to process"
                msg$(2) = " "
                msg$(3) = "-> Press PF(16) to terminate"
                call "ASKUSER" (int%, hdr$, msg$(1), msg$(2), msg$(3))
                if int% <> 16% then goto L10110
                goto L65000

L10200:     get #11 using L10210, purge_cd$
L10210:         FMT  POS(15), CH(6)
            purge_yr$ = purge_cd$ : call "DATEFMT" (purge_yr$)
            pf16$="(16)Exit Program"

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
L10270:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L10270
            next fieldnr%

        REM *************************************************************~
            *                S E C O N D   C H A N C E                  *~
            *-----------------------------------------------------------*~
            * Operator gets a second chance before actually purging.    *~
            *************************************************************

        second_chance
                int% = 2%
                call "ASKUSER" (int%, "*** CONFIRM PURGE ***",           ~
                     "2nd CHANCE! Press PF(16) to confirm that you want"&~
                     " to PURGE this data", "-- OR --",                  ~
                     "Press PF(1) to Start Over")
                  if int%  =  1% then gosub startover
                  if int% <> 16% then goto second_chance

        REM *************************************************************~
            *            M A I N   P R O G R A M   L O O P              *~
            *-----------------------------------------------------------*~
            * Purges records from file(s) SASUMRY? based on operator    *~
            * input. '?' is 0 thru 9 (file channels 1-10, respectively).*~
            *************************************************************

            int% = 0% /* Indicate 'no data purged' */
            for n% = 1% to 10%
                if f2%(n%) <> 0% then goto bump_to_next_file /*Not open*/
                plowkey$ = purge_cd$
                str(plowkey$,7) = all(hex(00))
                call "PLOWNEXT" (#n%, plowkey$, 6%, f1%(n%))
                if f1%(n%) <> 1% then goto bump_to_next_file /*No data*/
                str(plowkey$,7) = all(hex(00))
        REM Note the question mark below is necessary
                msg$(1) = "Now Purging SASUMRY? of all " & purge_yr$ &   ~
                     " data"
        REM Overlay the question mark above with the SASUMRY file number
                convert n%-1% to str(msg$(1),pos(msg$(1)="?"),1), pic(0)
                call "SHOSTAT" (msg$(1))
                call "DELETE" (#n%, plowkey$, 6%)
                int% = 1% /* Indicate 'some data purged' */
        bump_to_next_file
            next n%
            if f1%(11) = 0% then goto L19280
                call "READ101" (#11, sysfile2_key$, f1%(11))
                delete #11                 /* Delete yr from SYSFILE2 */
L19280:     if int% <> 0% then goto exit_program
L19290:         int% = 0%
                hdr$ = "*** OPERATOR ADVISORY ***"
                msg$(1) = "No " & purge_yr$ & " data was found to purge"
                msg$(2) = " "
                msg$(3) = "-> Press (RETURN) to continue"
                call "ASKUSER" (int%, hdr$, msg$(1), msg$(2), msg$(3))
                if int% <> 0% then goto L19290
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  on fieldnr% gosub L20090          /* Year to PURGE   */
                     return
L20090:     REM Year to PURGE                        PURGE_YR$
            inpfac$ = hex(a4)
            inpmessage$ = "Press (RETURN) to indicate you wish to " &    ~
                "PURGE this Sales Analysis 'year'"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2% /* Window at bottom of screen */
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,63%) = "SAPURGE: " & str(cms2v$,,8%)
                  init(hex(84)) lfac$()

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis: PURGE a 'Year' of Data",              ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (06,02), "S/A 'Year' to PURGE:",                       ~
               at (06,23), fac(lfac$( 1)), purge_yr$            , ch(08),~
                                                                         ~
               at (21,02), fac(inpfac$),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40360
                  call "MANUAL" ("SAPURGE ")
                  goto L40100

L40360:        if keyhit% <> 15 then L40400
                  call "PRNTSCRN"
                  goto L40100

L40400:        if fieldnr% > 0% then return
                  close ws
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
