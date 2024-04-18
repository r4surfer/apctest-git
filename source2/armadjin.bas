        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   AAA   DDDD   JJJJJ  IIIII  N   N   *~
            *  A   A  R   R  MM MM  A   A  D   D    J      I    NN  N   *~
            *  AAAAA  RRRR   M M M  AAAAA  D   D    J      I    N N N   *~
            *  A   A  R   R  M   M  A   A  D   D  J J      I    N  NN   *~
            *  A   A  R   R  M   M  A   A  DDDD    J     IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMADJIN - Program allows modifications to be made to     *~
            *            non-monetary items in the Trial Balance File.  *~
            *            Changes made are written to the report file    *~
            *            ARMADJRF.                                      *~
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
            * 12/17/86 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            billto$9,                    /* Bill-to Number             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Date/Time Stamp            */~
            discdue$(3)8,                /* Discount Due Date          */~
            discpct$(3)6, discpct(3),    /* Cash Discount Percent      */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            grace$(3)2, grace%(3),       /* Grace Days                 */~
            hdrs$(3)16,                  /* Column Headings            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            name$30,                     /* Bill-to Name               */~
            netdue$(3)8,                 /* Net Due Date               */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            po$(3)16,                    /* Customer PO / Comment      */~
            readkey$50,                  /* A Read Key                 */~
            srcedoc$8,                   /* Source Document Number     */~
            stlmnt$12,                   /* Settlement Number          */~
            transamt$10,                 /* Transaction Amount         */~
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
            * # 1 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * # 3 ! CUSTOMER ! Customer Master File                     *~
            * # 9 ! ARMADJRF ! A/R Adjustments Report File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21

            select # 3, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select # 9, "ARMADJRF",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =  1,   keylen = 28

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 100%, rslt$( 9))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            hdrs$(1) = "New Values"
            hdrs$(2) = "Current Values"
            hdrs$(3) = "Orig. Values  "


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16$ = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, billto$, stlmnt$, po$(),   ~
                      discpct$(), discdue$(), grace$(), netdue$(), name$,~
                      srcedoc$, transamt$

            for fieldnr% = 1 to  2
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10190
L10150:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10150
L10190:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10150
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
L12160:     fieldnr% = cursor%(1) - 7%
            fieldnr% = max(3%, min(fieldnr%, 7%))
            if str(stlmnt$,11,2) <> "00" then fieldnr% = 3%
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf16$ = " "
L12220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12220
                  lastfieldnr% = fieldnr%
            goto L12160

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto  inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  inpmessage$ = " "
                  enabled% = 1%
                  on fieldnr% gosub L20170,         /* Bill-to Number   */~
                                    L20210,         /* Settlement Number*/~
                                    L20250,         /* Customer PO      */~
                                    L20290,         /* Cash Discount %  */~
                                    L20330,         /* Discount Due Date*/~
                                    L20370,         /* Grace Days       */~
                                    L20410          /* Net Due Date     */
                     return
L20170
*        Bill-to Number                        BILLTO$
            inpmessage$ = "Enter Bill-to Number."
            return

L20210
*        Settlement Number                     STLMNT$
            inpmessage$ = "Enter Settlement Number."
            return

L20250
*        Customer PO / Comment                 PO$
            inpmessage$ = "Enter Customer PO Number."
            return

L20290
*        Cash Discount Percent                 DISCPCT$
            inpmessage$ = "Enter Cash Discount Perentage."
            return

L20330
*        Discount Due Date                     DISCDUE$
            inpmessage$ = "Enter Discount Due Date."
            return

L20370
*        Grace Days                            GRACE$
            inpmessage$ = "Enter Payment Grace Days."
            return

L20410
*        Net Due Date                          NETDUE$
            inpmessage$ = "Enter Net Due Date."
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
            get #1 using L30110, stlmnt$,                                 ~
                                discpct(1), discdue$(1), grace%(1),      ~
                                netdue$(1),  po$(1), transamt, srcedoc$, ~
                                discpct(3), discdue$(3), grace%(3),      ~
                                netdue$(3),  po$(3)
L30110:         FMT POS(10), CH(12), PD(14,4), CH(6), BI(1), CH(6),      ~
                    CH(16), POS(68), PD(14,4), POS(89), CH(8), POS(140), ~
                    PD(14,4), CH(6), BI(1), CH(6), CH(16)
            discpct(2) = discpct(1)
            call "DATEFMT" (discdue$(1)) : discdue$(2) = discdue$(1)
            call "DATEFMT" (discdue$(3))
            call "DATEFMT" (netdue$ (1)) : netdue$ (2) = netdue$ (1)
            call "DATEFMT" (netdue$ (3))
            convert grace%(1) to grace$(1), pic(#0)
            convert grace%(3) to grace$(3), pic(#0)
                    grace%(2) =  grace%(1)
                    grace$(2) =  grace$(1)
            po$(2) = po$(1)
            call "CONVERT" (discpct(1), 2.2, discpct$(1))
            call "CONVERT" (discpct(3), 2.2, discpct$(3))
                discpct (2) = discpct (1)
                discpct$(2) = discpct$(1)
            call "CONVERT" (transamt, 2.2, transamt$)
            return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUNFMT" (discdue$(1)) : call "DATUNFMT" (discdue$(2))
            call "DATUNFMT" (netdue$ (1)) : call "DATUNFMT" (netdue$ (2))
            readkey$ = str(billto$) & stlmnt$
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then return
            put #1 using L31130, discpct(1), discdue$(1), grace%(1),      ~
                                netdue$(1), po$(1), userid$, date
L31130:         FMT POS(22), PD(14,4), CH(6), BI(1), CH(6), CH(16),      ~
                    CH(3), CH(6)
            rewrite #1

            call "GETDTTM" addr(datetime$)
            write #9 using L31260,                                        ~
                     billto$, stlmnt$, datetime$, name$, date, userid$,  ~
                     discpct$(2), discdue$(2), grace$(2), netdue$(2),    ~
                     po$(2),                                             ~
                     discpct$(1), discdue$(1), grace$(1), netdue$(1),    ~
                     po$(1), " "
L31260:         FMT CH(9), CH(12), CH(7), CH(30), CH(6), CH(3),          ~
                    CH(6), CH(8), CH(2), CH(8), CH(16),                  ~
                    CH(6), CH(8), CH(2), CH(8), CH(16), CH(53)

            call "DATEFMT" (discdue$(1)) : call "DATEFMT" (discdue$(2))
            call "DATEFMT" (netdue$ (1)) : call "DATEFMT" (netdue$ (2))
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
            str(line2$,62%) = "ARMADJIN: " & str(cms2v$,,8%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if str(stlmnt$,11,2) <> "00" then                            ~
                                  init(hex(8c)) str(lfac$(),4)
                  on fieldnr% gosub L40240,         /* Bill-to Number   */~
                                    L40240,         /* Settlement Number*/~
                                    L40210,         /* Customer PO      */~
                                    L40270,         /* Cash Discount %  */~
                                    L40240,         /* Discount Due Date*/~
                                    L40270,         /* Grace Days       */~
                                    L40240          /* Net Due Date     */
                  goto L40310

L40210:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40240:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40270:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40310:     accept                                                       ~
               at (01,02), "A/R Trial Balance Adjustments",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Bill-to Number",                             ~
               at (06,27), fac(lfac$( 1)), billto$              , ch(09),~
               at (06,45), fac(hex(8c)),   name$                , ch(32),~
                                                                         ~
               at (07,02), "Settlement Number",                          ~
               at (07,27), fac(lfac$( 2)), str(stlmnt$, 1,8)    , ch(08),~
               at (07,36), fac(lfac$( 2)), str(stlmnt$, 9,2)    , ch(02),~
               at (07,39), fac(lfac$( 2)), str(stlmnt$,11,2)    , ch(02),~
               at (07,45), "Document = ",                                ~
               at (07,56), fac(hex(8c)),   srcedoc$             , ch(08),~
               at (07,66), fac(hex(8c)),   transamt$            , ch(10),~
                                                                         ~
               at (09,27), fac(hex(ac)), hdrs$(1)               , ch(16),~
               at (09,45), fac(hex(ac)), hdrs$(2)               , ch(16),~
               at (09,63), fac(hex(ac)), hdrs$(3)               , ch(16),~
                                                                         ~
               at (10,02), "Customer PO / Comment",                      ~
               at (10,27), fac(lfac$( 3)), po$(1)               , ch(16),~
               at (10,45), fac(hex(8c)),   po$(2)               , ch(16),~
               at (10,63), fac(hex(8c)),   po$(3)               , ch(16),~
                                                                         ~
               at (11,02), "Cash Discount Percent",                      ~
               at (11,27), fac(lfac$( 4)), discpct$(1)          , ch(06),~
               at (11,45), fac(hex(8c)),   discpct$(2)          , ch(06),~
               at (11,63), fac(hex(8c)),   discpct$(3)          , ch(06),~
                                                                         ~
               at (12,02), "Discount Due Date",                          ~
               at (12,27), fac(lfac$( 5)), discdue$(1)          , ch(08),~
               at (12,45), fac(hex(8c)),   discdue$(2)          , ch(08),~
               at (12,63), fac(hex(8c)),   discdue$(3)          , ch(08),~
                                                                         ~
               at (13,02), "Grace Days",                                 ~
               at (13,27), fac(lfac$( 6)), grace$(1)            , ch(02),~
               at (13,45), fac(hex(8c)),   grace$(2)            , ch(02),~
               at (13,63), fac(hex(8c)),   grace$(3)            , ch(02),~
                                                                         ~
               at (14,02), "Net Due Date",                               ~
               at (14,27), fac(lfac$( 7)), netdue$(1)           , ch(08),~
               at (14,45), fac(hex(8c)),   netdue$(2)           , ch(08),~
               at (14,63), fac(hex(8c)),   netdue$(3)           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                   keys(hex(00010d0f10)),  key(keyhit%)

                if keyhit% <> 13% then L40890 : call "MANUAL" ("ARMADJIN")
                                               goto L40310

L40890:         if keyhit% <> 15% then L40920 : call "PRNTSCRN"
                                               goto L40310

L40920:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50170,         /* Bill-to Number   */~
                                    L50290,         /* Settlement Number*/~
                                    L50400,         /* Customer PO      */~
                                    L50430,         /* Cash Discount %  */~
                                    L50550,         /* Discount Due Date*/~
                                    L50670,         /* Grace Days       */~
                                    L50760          /* Net Due Date     */
                  return

L50170
*        Bill-to Number                        BILLTO$
            name$ = hex(06) & "Select Bill-to Customer"
            call "GETCODE" (#3, billto$, name$, 0%, 0, f1%(3))
            if f1%(3) = 1% then L50230
                errormsg$ = "Bill-to not on file."
                return
L50230:     readkey$ = str(billto$) & hex(00)
            call "PLOWNEXT" (#1, readkey$, 9%, f1%(1))
            if f1%(1) = 1% then return
                errormsg$ = "No transactions exist for Bill-to."
                return

L50290
*        Settlement Number                     STLMNT$
            if stlmnt$ = " " then L50330
                if str(stlmnt$, 9,2) = " " then str(stlmnt$, 9,2) = "00"
                if str(stlmnt$,11,2) = " " then str(stlmnt$,11,2) = "00"
L50330:     readkey$ = str(billto$) & stlmnt$
            call "PLOWCODE" (#1, readkey$, " ", 9%, 0, f1%(1))
            if f1%(1) = 1% then L50370
                errormsg$ = "Settlement not on file."  :  return
L50370:     gosub load_data
            return

L50400
*        Customer PO / Comment                 PO$
            return

L50430
*        Cash Discount Percent                 DISCPCT$
            if discpct$(1) = " " then discpct$(1) = "0"
            convert discpct$(1) to discpct(1), data goto L50470
            goto L50490
L50470:         errormsg$ = "Invalid entry for Cash Discount Percent"
                return
L50490:     if discpct(1) >= -99.99 and discpct(1) <= 100.00 then L50520
                errormsg$ = "Cash Discount % must be -99.99 to +100"
                return
L50520:     call "CONVERT" (discpct(1), 2.2, discpct$(1))
            return

L50550
*        Discount Due Date                     DISCDUE$
            call "DATEOK" (discdue$(1), u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (discdue$(1))
                call "DATUNFMT" (netdue$ (1))
                if netdue$(1) < discdue$(1) then                         ~
                     errormsg$ = "Discount Due Date must be on or"  &    ~
                                 " before Net Due Date"
                call "DATEFMT" (discdue$(1))
                call "DATEFMT" (netdue$ (1))
            return

L50670
*        Grace Days                            GRACE$
            if grace$(1) = " " then grace$(1) = "0"
            convert grace$(1) to grace%(1), data goto L50700 : goto L50710
L50700:         errormsg$ = "Invalid entry for grace days."  :  return
L50710:     if grace%(1) >= 0 and grace%(1) <= 99% then L50730
                errormsg$ = "Grace Days must be 0 to 99."  :  return
L50730:     convert grace%(1) to grace$(1), pic(#0)
            return

L50760
*        Net Due Date                          NETDUE$
            call "DATEOK" (netdue$(1), u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (discdue$(1))
                call "DATUNFMT" (netdue$ (1))
                if netdue$(1) < discdue$(1) then                         ~
                     errormsg$ = "Net Due Date must be on or after" &    ~
                                 " Discount Due Date"
                call "DATEFMT" (discdue$(1))
                call "DATEFMT" (netdue$ (1))
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
