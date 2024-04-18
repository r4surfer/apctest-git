        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  TTTTT  BBBB    AAA    GGGG  EEEEE   *~
            *  A   A  R   R  MM MM    T    B   B  A   A  G      E       *~
            *  AAAAA  RRRR   M M M    T    BBBB   AAAAA  G GGG  EEE     *~
            *  A   A  R   R  M   M    T    B   B  A   A  G   G  E       *~
            *  A   A  R   R  M   M    T    BBBB   A   A   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMTBAGE - Prints Detail Trial Balance (Driver).          *~
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
            * 04/08/88 ! As of date can be same as settlmnt date  ! DAW *~
            * 06/09/88 ! Added aging by source document date      ! ERN *~
            * 10/29/91 ! PRR 11016.  Added 'I' option to include  ! JDH *~
            *          !  zero balance transactions.              !     *~
            *          ! PRR 11148.  Added account range.         !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            *          !     Credit to JDH for having it mostly   !     *~
            *          !     done.                                !     *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$(2)12, acctie$1,        /* Selected A/R Account       */~
            ageper$1,                    /* Aging Per- Disc,Net,Invce  */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            asof$8, asofu$6,             /* As Of Date                 */~
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
            pf8$26,                      /* PF 5 Screen Literal        */~
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
            * #4  ! GLMAIN   ! G/L Account Master File                  *~
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
            pf4$="(4)Previous Field" : pf8$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, asof$, cus_range$(),       ~
                      summary$, types$(), pastdue$, min_bal$, ageper$,   ~
                      aging$(), asofu$, acct$(), acctie$
            mat aging% = zer
                aging%(1) =  30% : aging%( 2) =  60% : aging%(3) =  90%
                aging%(4) = 120% : aging%(10) =   5%
                aging%(8) = -99999%  :  aging%(5), aging%(9) = 99999%

            for fieldnr% = 1% to 8%
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10290
L10190:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10190
L10290:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%

            gosub aging_parameters

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " " : pf8$ = "(8)Modify Aging Parameters"
            pf16$ = "(16)Print Report"
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then gosub aging_parameters
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11160:     fieldnr% = min(max(1%, cursor%(1) - 5%), 10%)
                if fieldnr% > 4% then fieldnr% = fieldnr% - 1%
                if fieldnr% < 1% or   fieldnr% > 9% then edtpg1
                if fieldnr% = lastfieldnr% then edtpg1
                if fieldnr% <> 9% then L11230
                     gosub aging_parameters  :  goto L11310

L11230:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then edtpg1
                  pf4$, pf8$, pf16$ = " "
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
L11310:              lastfieldnr% = fieldnr%
                     goto L11160


        aging_parameters
            call "ARMDATES" (asofu$, "AGED A/R DETAIL TRIAL BALANCE",    ~
                             aging%(), 1%, 1%, aging$(), u3%)
            if u3% <> 1% then return
                return clear all
                goto   inputmode

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
            call "ARMTBASB" ("ARMTBAGE", "ARM014",                       ~
                             asof$, ageper$, aging%(),                   ~
                             cus_range$(1), cus_range$(2), summary$,     ~
                             str(types$()), pastdue$, min_bal$, acct$(), ~
                             acctie$, #1, #3, #4)
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            enabled% = 1%
                  on fieldnr% gosub L20180,         /* As Of Date       */~
                                    L20240,         /* Bill-to Range    */~
                                    L20290,         /* Summary Option   */~
                                    L20340,         /* Selected Types   */~
                                    L20420,         /* Past Due Only?   */~
                                    L20480,         /* Minimum Balance  */~
                                    L20590,         /* Age Per Date     */~
                                    L20650          /* Selected Account */
                     return

L20180
*        As Of Date                            ASOF$
            inpmessage$ = "Transactions posted after this date are"  &   ~
                          " ignored by the report."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
            return

L20240
*        Bill-to Customer Range                CUS_RANGE$()
            inpmessage$ = "Enter range to print (ALL, FIRST, LAST)."
            if str(cus_range$()) = " " then cus_range$(1) = "ALL"
            return

L20290
*        Summary (S) or Detail (D)             SUMMARY$
            inpmessage$ = "Summary prints settlement balances only."
            if summary$ = " " then summary$ = "S"
            return

L20340
*        Selected Document Types               TYPES$()
            inpmessage$ = "Place a non-blank character next to types" &  ~
                          " to print."
            if str(types$()) <> " " then return
                types$(1), types$(2), types$(3), types$(4), types$(5),   ~
                types$(6) = "X"
                return

L20420
*        Past Due Items Only?                  PASTDUE$
            inpmessage$ = "Enter 'N' to list regardless of aging, else" &~
                          " enter number of days."
            if pastdue$ = " " then pastdue$ = "N"
            return

L20480
*        Minimum Balance                       MIN_BAL$
            inpmessage$ = "Enter Minimum Balance to List; Blank to li" & ~
                          "st all; 'I' to include zero balances."
            return

L20590
*        Age Per Date                          AGEPER$
            inpmessage$ = "Enter 'D' to Age by Cash Disc Date, 'N' by" & ~
                          " Net Due Date, or 'I' by Invoice Date."
            if ageper$  = " " then ageper$ = "N"
            return

L20650
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

            deffn'101(fieldnr%, edit%)
                  str(line2$,62%) = "ARMTBAGE: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  if edit% = 1% then lfac$(9) = hex(9c)
                  on fieldnr% gosub L40240,         /* As Of Date       */~
                                    L40240,         /* Bill-to Range    */~
                                    L40240,         /* Summary Option   */~
                                    L40240,         /* Selected Types   */~
                                    L40240,         /* Past Due Only?   */~
                                    L40240,         /* Minimum Balance  */~
                                    L40240,         /* Age per date     */~
                                    L40240          /* Selected Account */
                  goto L40310

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40240:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40310:     accept                                                       ~
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
               at (13,02), "Age Per Date (Disc/Net/Inv)",                ~
               at (13,30), fac(lfac$( 7)), ageper$              , ch(01),~
                                                                         ~
               at (14,02), "Include/Exclude G/L Accts:",                 ~
               at (14,30), fac(lfac$( 8)), acct$(1)             , ch(12),~
               at (14,43), "to",                                         ~
               at (14,46), fac(lfac$( 8)), acct$(2)             , ch(12),~
               at (14,60), "Include/Exclude",                            ~
               at (14,76), fac(lfac$( 8)), acctie$              , ch(01),~
                                                                         ~
               at (15,02), "Aging Parameters:",                          ~
               at (15,20), "1)", at(16,20), "2)", at(17,20), "3)",       ~
               at (18,20), "4)", at(19,20), "5)", at(15,30), "Days",     ~
               at (15,23), fac(lfac$( 9)), aging%( 1)       ,pic(-#####),~
               at (16,23), fac(lfac$( 9)), aging%( 2)       ,pic(-#####),~
               at (17,23), fac(lfac$( 9)), aging%( 3)       ,pic(-#####),~
               at (18,23), fac(lfac$( 9)), aging%( 4)       ,pic(-#####),~
               at (19,23), fac(lfac$( 9)), aging%( 5)       ,pic(-#####),~
               at (15,37), fac(lfac$( 9)), aging$( 1)           , ch(20),~
               at (16,37), fac(lfac$( 9)), aging$( 2)           , ch(20),~
               at (17,37), fac(lfac$( 9)), aging$( 3)           , ch(20),~
               at (18,37), fac(lfac$( 9)), aging$( 4)           , ch(20),~
               at (19,37), fac(lfac$( 9)), aging$( 5)           , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf8$,                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104080d0f10)), key (keyhit%)

               if keyhit% <> 13 then L41020
                  call "MANUAL" ("ARMTBAGE")
                  goto L40310

L41020:        if keyhit% <> 15 then L41060
                  call "PRNTSCRN"
                  goto L40310

L41060:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* As Of Date       */~
                                    L50330,         /* Bill-to Range    */~
                                    L50380,         /* Summary Option   */~
                                    L50430,         /* Selected Types   */~
                                    L50480,         /* Past Due Only?   */~
                                    L50590,         /* Minimum Balance  */~
                                    L50760,         /* Age Per Date     */~
                                    L50810          /* Selected Account */
                  return

L50180
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (asof$)
                call "DATUNFMT" (last_stl$)
                if asof$ <  last_stl$ then errormsg$ =                   ~
        "As Of Date cannot be before last settling date."
                asofu$ = asof$
                call "DATEFMT" (asof$)
                call "DATEFMT" (last_stl$)
                if edit% = 2% then                                       ~
                     call "ARMDATES" (asofu$, " ", aging%(), 1%, 1%,     ~
                                                           aging$(), 99%)
                return

L50330
*        Bill-to Customer Range                CUS_RANGE$()
            call "TESTRNGE" (cus_range$(1), cus_range$(2), " ", " ",     ~
                             errormsg$)
            return

L50380
*        Summary (S) or Detail (D)             SUMMARY$
            if summary$ = "D" or summary$ = "S" then return
                errormsg$ = "Enter 'S' or 'D'."
                return

L50430
*        Selected Document Types               TYPES$()
            if str(types$()) <> " " then return
                errormsg$ = "Please select at least one type."
                return

L50480
*        Past Due Items Only?                  PASTDUE$
            if pastdue$ = "N" then return
                if pastdue$ = " " then pastdue$ = "0"
                convert pastdue$ to temp%, data goto L50520 : goto L50550
L50520:              errormsg$ = "Enter 'N' or Number of Days (-999 to" &~
                                 " +999)."
                     return
L50550:         if temp% < -999 or temp% > 999 then L50520
                convert temp% to pastdue$, pic(-###)
                return

L50590
*        Minimum Balance                       MIN_BAL$
            if min_bal$ = " " or min_bal$ = "I" then return
                convert min_bal$ to temp%, data goto L50620 : goto L50650
L50620:              errormsg$ = "Enter Blank, 'I', or Minimum Balance" &~
                                 " (-99999999 to +999999999)."
                     return
L50650:         if temp% < -9999999 or temp% > 999999999 then L50620
                convert temp% to min_bal$, pic(-#########)
                return

L50760
*        Age Per                               AGEPER$
            if pos("DNI" = ageper$) > 0% then return
                errormsg$ = "Enter 'D' or 'N'."
                return

L50810
*        Select G/L Account
            if acct$(1) = "ALL" then L50950
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
L50950:     acct$(2) = " "
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
