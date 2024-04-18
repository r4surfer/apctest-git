        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT  BBBB   DDDD   TTTTT  L       *~
            *  A   A  R   R  MM MM    T    B   B  D   D    T    L       *~
            *  AAAAA  RRRR   M M M    T    BBBB   D   D    T    L       *~
            *  A   A  R   R  M   M    T    B   B  D   D    T    L       *~
            *  A   A  R   R  M   M    T    BBBB   DDDD     T    LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTBDTL - Prints Detail Trial Balance (Driver).          *~
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
            * 12/02/86 ! Original                                 ! ERN *~
            * 06/08/88 ! Mod to allow as-of date to = settle date ! DAW *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$(2)12, acctie$1,        /* GL Range                   */~
            asof$8,                      /* As Of Date                 */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            cus_range$(2)9,              /* Bill-to Customer Range     */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_stl$8,                  /* Last Settling Date         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            min_bal$10,                  /* Minimum Balance            */~
            pastdue$4,                   /* Past Due Items Only?       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            readkey$50,                  /* Read Key                   */~
            summary$1,                   /* Summary (S) or Detail (D)  */~
            types$(10)2,                 /* Selected Document Types    */~
            userid$3                     /* Current User Id            */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* File Read Status Flag      */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #1  ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #2  ! SYSFILE2 ! System File                              *~
            * #3  ! CUSTOMER ! Customer Master                          *~
            * #4  ! GLMAIN   ! General Ledger Master File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen = 21

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =   9

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))

            if min(fs%()) < 0% then exit_program

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
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            select printer

            readkey$ = "ARM.LAST.SETTLING"
            last_stl$ = blankdate$
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09180, last_stl$
L09180:         FMT XX(20), CH(6)
            call "DATEFMT" (last_stl$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, asof$, cus_range$(),       ~
                      summary$, types$(), pastdue$, min_bal$,            ~
                      acct$(), acctie$

            for fieldnr% = 1% to 7%
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Report"
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11160:     fieldnr% = cursor%(1) - 5%
            if fieldnr% > 4% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1% or   fieldnr% > 7% then edtpg1
            if fieldnr% = lastfieldnr% then edtpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11240:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11240
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11240
                     lastfieldnr% = fieldnr%
                     goto L11160


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Calls print subroutine.                                   *~
            *************************************************************

        datasave
            if types$( 1) <> " " then types$( 1) = "II"
            if types$( 2) <> " " then types$( 2) = "IC"
            if types$( 3) <> " " then types$( 3) = "IA"
            if types$( 4) <> " " then types$( 4) = "IF"
            if types$( 5) <> " " then types$( 5) = "CU"
            if types$( 6) <> " " then types$( 6) = "CB"
            if pos(str(types$(),,12) = " ") = 0% then str(types$()) = " "
            call "ARMTBDSB" ("ARMTBDTL", "ARM003", asof$,                ~
                             cus_range$(1), cus_range$(2), summary$,     ~
                             str(types$()), pastdue$, min_bal$, #1, #3,  ~
                             acctie$, acct$(1), acct$(2))
            goto exit_program


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            enabled% = 1%
                  on fieldnr% gosub L20160,         /* As Of Date       */~
                                    L20220,         /* Bill-to Range    */~
                                    L20270,         /* Summary Option   */~
                                    L20320,         /* Selected Types   */~
                                    L20400,         /* Past Due Only?   */~
                                    L20460,         /* Minimum Balance  */~
                                    L20500          /* GL Range         */
                     return

L20160
*        As Of Date                            ASOF$
            inpmessage$ = "Transactions posted after this date are"  &   ~
                          " ignored by the report."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
            return

L20220
*        Bill-to Customer Range                CUS_RANGE$()
            inpmessage$ = "Enter range to print (ALL, FIRST, LAST)."
            if str(cus_range$()) = " " then cus_range$(1) = "ALL"
            return

L20270
*        Summary (S) or Detail (D)             SUMMARY$
            inpmessage$ = "Summary prints settlement balances only."
            if summary$ = " " then summary$ = "S"
            return

L20320
*        Selected Document Types               TYPES$()
            inpmessage$ = "Place a non-blank character next to types" &  ~
                          " to print."
            if str(types$()) <> " " then return
                types$(1), types$(2), types$(3), types$(4), types$(5),   ~
                types$(6) = "X"
                return

L20400
*        Past Due Items Only?                  PASTDUE$
            inpmessage$ = "Enter 'N' to list regardless of aging, else" &~
                          " enter number of days."
            if pastdue$ = " " then pastdue$ = "N"
            return

L20460
*        Minimum Balance                       MIN_BAL$
            inpmessage$ = "Enter Minimum Balance to List.  Leave blank" &~
                          " to list all balances."
            return

L20500
*        Selected Account
            inpmessage$ = "Enter G/L Account Range or 'ALL' & Include" & ~
                          "/Exclude selection (via I or E)."
            if acctie$  = " " then acctie$  = "I"
            if acct$(1) <> " " then return
               acct$(1) = "ALL"
               acct$(2) = " "
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
            u3% = 2%
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
                  str(line2$,62%) = "ARMTBDTL: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40210,         /* As Of Date       */~
                                    L40210,         /* Bill-to Range    */~
                                    L40210,         /* Summary Option   */~
                                    L40210,         /* Selected Types   */~
                                    L40210,         /* Past Due Only?   */~
                                    L40240,         /* Minimum Balance  */~
                                    L40210          /* GL Range         */
                  goto L40280

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40210:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40240:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R DETAIL TRIAL BALANCE REPORT",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "As Of Date",                                 ~
               at (06,30), fac(lfac$( 1)), asof$                , ch(08),~
               at (06,49), "Last Settled on ",                           ~
               at (06,65), fac(hex(8c)), last_stl$              , ch(08),~
                                                                         ~
               at (07,02), "Bill-to Customer Range", at(07,41), "to",    ~
               at (07,30), fac(lfac$( 2)), cus_range$(1)        , ch(09),~
               at (07,45), fac(lfac$( 2)), cus_range$(2)        , ch(09),~
                                                                         ~
               at (08,02), "Summary (S) or Detail (D)",                  ~
               at (08,30), fac(lfac$( 3)), summary$             , ch(01),~
                                                                         ~
               at (09,02), "Selected Document Types",                    ~
               at (09,32), "Invoices    Credit Memos    Inv Adjs    FCs",~
               at (10,32), "Unapplied Cash    Balance Forward Payments", ~
               at (09,30), fac(lfac$( 4)), types$( 1)           , ch(01),~
               at (09,42), fac(lfac$( 4)), types$( 2)           , ch(01),~
               at (09,58), fac(lfac$( 4)), types$( 3)           , ch(01),~
               at (09,70), fac(lfac$( 4)), types$( 4)           , ch(01),~
               at (10,30), fac(lfac$( 4)), types$( 5)           , ch(01),~
               at (10,48), fac(lfac$( 4)), types$( 6)           , ch(01),~
                                                                         ~
               at (11,02), "Past Due Items Only?",                       ~
               at (11,30), fac(lfac$( 5)), pastdue$             , ch(04),~
               at (11,37), "('N' -or- Number of Days)",                  ~
                                                                         ~
               at (12,02), "Minimum Balance",                            ~
               at (12,30), fac(lfac$( 6)), min_bal$             , ch(10),~
                                                                         ~
               at (13,02), "Include/Exclude G/L Accts:",                 ~
               at (13,30), fac(lfac$( 7)), acct$(1)             , ch(12),~
               at (13,43), "to",                                         ~
               at (13,46), fac(lfac$( 7)), acct$(2)             , ch(12),~
               at (13,60), "Include/Exclude",                            ~
               at (13,76), fac(lfac$( 7)), acctie$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104050d0f10)), key (keyhit%)


               if keyhit% <> 13 then L40770
                  call "MANUAL" ("ARMTBDTL")
                  goto L40280

L40770:        if keyhit% <> 15 then L40810
                  call "PRNTSCRN"
                  goto L40280

L40810:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* As Of Date       */~
                                    L50270,         /* Bill-to Range    */~
                                    L50320,         /* Summary Option   */~
                                    L50370,         /* Selected Types   */~
                                    L50420,         /* Past Due Only?   */~
                                    L50530,         /* Minimum Balance  */~
                                    L50700          /* GL Range         */
                  return

L50160
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (asof$)
                call "DATUNFMT" (last_stl$)
                if asof$ <  last_stl$ then errormsg$ =                   ~
        "As Of Date cannot be before last settling date."
                call "DATEFMT" (asof$)
                call "DATEFMT" (last_stl$)
                return

L50270
*        Bill-to Customer Range                CUS_RANGE$()
            call "TESTRNGE" (cus_range$(1), cus_range$(2), " ", " ",     ~
                             errormsg$)
            return

L50320
*        Summary (S) or Detail (D)             SUMMARY$
            if summary$ = "D" or summary$ = "S" then return
                errormsg$ = "Enter 'S' or 'D'."
                return

L50370
*        Selected Document Types               TYPES$()
            if str(types$()) <> " " then return
                errormsg$ = "Please select at least one type."
                return

L50420
*        Past Due Items Only?                  PASTDUE$
            if pastdue$ = "N" then return
                if pastdue$ = " " then pastdue$ = "0"
                convert pastdue$ to temp%, data goto L50460 : goto L50490
L50460:              errormsg$ = "Enter 'N' or Number of Days (-999 to" &~
                                 " +999)."
                     return
L50490:         if temp% < -999 or temp% > 999 then L50460
                convert temp% to pastdue$, pic(-###)
                return

L50530
*        Minimum Balance                       MIN_BAL$
            if min_bal$ = " " then return
                convert min_bal$ to temp%, data goto L50560 : goto L50590
L50560:              errormsg$ = "Leave Blank or enter Minimum Balance" &~
                                 " (-99999999 to +999999999)."
                     return
L50590:         if temp% < -9999999 or temp% > 999999999 then L50560
                convert temp% to min_bal$, pic(-#########)
                return


L50700
*        Select G/L Account
            if acct$(1) = "ALL" then L50860
            if (pos("IE" = acctie$) = 0%) then acctie$ = "I"
                call "GLVALID" (acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#4, acct$(1), temp$, 0%, 0, f1%(4))
                if acct$(2) = " " then acct$(2) = acct$(1)
                call "GLVALID" (acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#4, acct$(2), temp$, 0%, 0, f1%(4))
                if acct$(2) < acct$(1) then errormsg$ = "Invalid Range."
                return
L50860:     acct$(2) = " "
            acctie$ = "I"
            return

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
