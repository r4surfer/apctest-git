        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   V   V  RRRR   FFFFF  Y   Y          *~
            *  P   P    I    P   P  V   V  R   R  F      Y   Y          *~
            *  PPPP     I    PPPP   V   V  RRRR   FFFF    YYY           *~
            *  P        I    P       V V   R   R  F        Y            *~
            *  P      IIIII  P        V    R   R  F        Y            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPVRFY  - Phase I - Work from PIP files to 'Masters' to  *~
            *                      verify the legitimacy (need for)     *~
            *                      and accuracy of PIP Details.         *~
            *                      This phase will update or delete     *~
            *                      details.                             *~
            *            Phase II - Work from 'Masters' to PIP files to *~
            *                      verify the existence of (or create)  *~
            *                      appropriate details.                 *~
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
            * 01/06/87 ! Original                                 ! KAB *~
            * 03/19/87 ! Expiration Pipouts added                 ! KAB *~
            * 04/20/87 ! Branch Errror @ NEXT_PIPOUT_VALID        ! KAB *~
            * 04/27/88 ! Change PIPOUT rewrite to write, no FS95  ! HES *~
            * 06/01/89 ! prr 10964 corrected edit-mode logic      ! DWT *~
            * 09/19/89 ! Corrected more of the edit-mode logic    ! JDH *~
            * 02/09/94 ! Purchase Jobs Project - note existance   ! KAB *~
            *          !   of RW's and BW's.  General once over   !     *~
            *          !   for sanity.  Make Sure PO and QC are   !     *~
            *          !   consistent with VBK/RCV UPDTE. Honor   !     *~
            *          !   Rework Success flag (SFC).             !     *~
            * 06/11/96 ! PRR 13205,13609. Orders outside planning ! JDH *~
            *          !   calendar now realign properly.         !     *~
            *          ! PRR 13610.  Don't write PIPIN for PJ PO. !     *~
            * 08/12/97 ! Add Reporting of Whats been done.        ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            askmsg$79,                   /* ASKUSER message            */~
            company$40,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8, dateun$8,            /* Date for screen display    */~
            day1$6,                      /* Date Indexing - Base       */~
            day2$6,                      /* Date Indexing - Detail     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80, j$(24)80,          /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inpmessage1$79,              /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            part$25,                     /* Part Code                  */~
            p_count$6,                   /* Record count               */~
            p_date$8,                    /* Date                       */~
            p_file$8,                    /* File acted on              */~
            p_index$8,                   /* PIP Date for printing      */~
            p_part$25,                   /* Part for Printing          */~
            p_tagnr$19,                  /* Tag number for printing    */~
            p_type$7,                    /* Type of PIP fix for print  */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pipinvalid$(10)1,            /* PIPIN Validation           */~
            pipinverify$(10)1,           /* PIPIN Verification         */~
            pipmsg$(2)40,                /* PIP Messages               */~
            pipoutvalid$(10)1,           /* PIPOUT Validation          */~
            pipoutverify$(10)1,          /* PIPOUT Verification        */~
            prev_file$8,                 /* Previous file reported upon*/~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            record$100,                  /* Detail Record              */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            rwkpip$1,                    /* Rework Success Anticiation */~
            store$3,                     /* Store                      */~
            tag$19,                      /* PIP Tag                    */~
            time$8,                      /* Time of report             */~
            type$10,                     /* type from pipmastr         */~
            userid$3,                    /* Current User Id            */~
            wcmsg$(2)40,                 /* WC Messages                */~
            wcoutvalid$(10)1,            /* WCOUT Validation           */~
            wcoutverify$(10)1,           /* WCOUT Verification         */~
            zeroes$(8)256                /* Just a bunch of nothing    */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 3 ! PIPIN    ! Planned inventory additions detail       *~
            * # 4 ! PIPOUT   ! Planned inventory use detail             *~
            * # 5 ! WCOUT    ! Planned work center use detail           *~
            * # 6 ! VBKMASTR ! Purchase Order Header File               *~
            * # 7 ! VBKLINES ! Purchase Order Line Items File           *~
            * # 8 ! PORLSE   ! Purchase Order Requisitions file         *~
            * # 9 ! JBMASTR2 ! Production job master file               *~
            * #10 ! JOBMASTR ! Project Master File                      *~
            * #11 ! BCKMASTR ! Backlog Master File (get store number)   *~
            * #12 ! BCKLINES ! Back Log Line Item File                  *~
            * #13 ! DEMMASTR ! Demand Master File                       *~
            * #14 ! SYSFILE2 ! System Control File                      *~
            * #15 ! HNYQUAN  ! Inventrory Store/Lot File                *~
            * #16 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #17 ! SFMASTR2 ! Sales forecast master file               *~
            * #50 ! WORKFILE ! Print PIP workfile                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 2, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 3, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select # 4, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select # 5, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27           ~

            select # 6, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select # 7, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select # 8, "PORLSE",                                        ~
                        varc,     indexed,  recsize =  492,              ~
                        keypos =    1, keylen =  66,                     ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup,    ~
                            key  5, keypos =  242, keylen =  19, dup     ~

            select # 9, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize =  1300,             ~
                        keypos =    1, keylen =   8                      ~

            select #10, "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =   8                      ~

            select #11, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #13, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #14, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #15, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos=17, keylen = 44,                          ~
                        alternate key 1, keypos =  1, keylen = 44

            select #16, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #17, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    1, keylen =  14

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 100%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 100%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17), 0%, rslt$(17))

*            call "OPENCHCK" (#50, fs%(50%), f2%(50%),   0%, rslt$(50%))
*            call "FILEBGON" (#50)
*            call "OPENCHCK" (#50, fs%(50%), f2%(50%), 100%, rslt$(50%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$, dateun$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Block & Press (RETURN)."

            pip_restore_required% = 0%
            wc_restore_required%  = 0%

*        Get first date of Planning Calendar from SYSFILE2.
            call "READ100" (#14, "MONTHS OPEN", f1%(14))
            if f1%(14) = 0 then L09961
                get #14, using L09820 , day1$
L09820:              FMT XX(32), CH(6)

            day2$ = day1$ : index% = 0% : pip% = -1%
            gosub pip_index
            if index% <> 1% then L09961

            readkey$ = "SWITCHS.SFC"
            call "READ100" (#14, readkey$, f1%(14%))
               if f1%(14%) = 0% then L09940
            get #14 using L09920, rwkpip$
L09920:         FMT POS(70), CH(1)

L09940:     init (hex(00)) str(zeroes$())

            call "COMPNAME" (12%, company$, ret%) : ret% = ret%
            rpttitle$ = "    Pip Verify Processing Result Report"
            rptid$ = "PIP005"

            goto L10000

L09961: /* Oops, Can't Even Start */
            ask% = 0%
            call "ASKUSER" (ask%, "* * * CALENDAR ERROR * * *",          ~
                            "Either the plannning calendar base date" &  ~
                            " was not found or is invalid.", " ",        ~
                            "Press any PF Key to EXIT PROGRAM")
            goto L65000

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            gosub L29000  /* initialize */

            for fieldnr% = 1 to 3
                if fieldnr% > 1 then pf4$ = "(4)Previous Block"
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
L10220:               if keyhit% = 16 then exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
            next fieldnr%

            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            for fieldnr% = 1 to  2
                if fieldnr% > 1 then pf4$ = "(4)Previous Block"
L10300:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10420
L10320:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10400
L10350:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10320
                         if fieldnr% = 1% then L10300
                         goto L10350
L10400:               if keyhit% = 16 then exit_program
                      if keyhit% <>  0 then       L10320
L10420:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10320
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            pf5$  = "(5)Next Screen"
            pf16$ = "(16)Verify Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       editpg2
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
L11170:     fieldnr% = 0
            if cursor%(1) >  5 and cursor%(1) < 14 then fieldnr% = 1
            if cursor%(1) > 14 and cursor%(1) < 21 then fieldnr% = 3
            if fieldnr%   =  1 and cursor%(2) > 39 then fieldnr% = 2
               if fieldnr% = 0 then editpg1
               if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11230:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
                  lastfieldnr% = fieldnr%
            goto L11170

        editpg2
            pf4$  = "(4)Previous Screen"
            pf5$  = " "
            pf16$ = "(16)Verify Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       editpg1
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg2
L11420:     fieldnr% = 0
            if cursor%(2) < 39% then fieldnr% = 1%
            if cursor%(2) > 38% then fieldnr% = 2%
               if fieldnr% = 0 then editpg2
               if fieldnr% = lastfieldnr% then editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg2
                  pf4$, pf5$, pf16$ = " "
L11480:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11480
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11480
                  lastfieldnr% = fieldnr%
            goto L11420

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub validate_files
            gosub verify_files
            if pip_restore_required% = 0% then L19200
               pipmsg$(1) = "PIP Detail FILES have been Changed."
               pipmsg$(2) = "Re-balance DETAILS and MASTERS A.S.A.P."
               askmsg$    = "PIP "

L19200:     if wc_restore_required% = 0% then L19300
               wcmsg$(1) = "Work Center Details have been Deleted"
               wcmsg$(2) = "Re-balance DETAILS and MASTERS A.S.A.P."
               if askmsg$ = " " then askmsg$ = "Work Center "    ~
                        else askmsg$ = askmsg$ & " & Work Center "

L19300:     if pipmsg$(1) = " " and wcmsg$(1) = " " then goto inputmode
            askmsg$ = askmsg$ & "Details have Changed"
            ask% = 0%
            call "ASKUSER" (ask%, " ** Files Have Been Modified ** ",   ~
                            askmsg$,                                    ~
                            "Press F9 to Print Processing Report",      ~
                            " or Any Other Key to Continue")
            if ask% = 9% then gosub print_report

            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* PIPIN              */    ~
                              L20200,         /* PIPOUT             */    ~
                              L20300          /* WCOUT              */    ~

            return

L20100: REM Def/Enable PIPIN
            inpmessage$ = "Place a non-blank to indicate which PROCUREMEN~
        ~T types to VALIDATE."
            return
L20200: REM Def/Enable PIPOUT
            inpmessage$ = "Place a non-blank to indicate which WITHDRAWAL~
        ~ types to VALIDATE."
            return
L20300: REM Def/Enable WCOUT
            inpmessage$ = "Place a non-blank to indicate which WORK CENTE~
        ~R DETAIL types to VALIDATE."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L21100,         /* PIPIN              */    ~
                              L21200          /* PIPOUT             */    ~

            return

L21100: REM Def/Enable PIPIN
            inpmessage$ = "Place a non-blank to indicate which PROCUREMEN~
        ~T types to VERIFY."
            return
L21200: REM Def/Enable PIPOUT
            inpmessage$ = "Place a non-blank to indicate which WITHDRAWAL~
        ~ types to VERIFY."
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$,                            ~
            last_so$, store$,                                            ~
            pipinvalid$(),               /* PIPIN Validation           */~
            pipinverify$(),              /* PIPIN Verification         */~
            pipoutvalid$(),              /* PIPOUT Validation          */~
            pipoutverify$(),             /* PIPOUT Verification        */~
            wcoutvalid$(),               /* WCOUT Validation           */~
            wcoutverify$()               /* WCOUT Verification         */~

            workfile_opened% = 0%
            return

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
            *               V A L I D A T I O N                         *~
            *-----------------------------------------------------------*~
            * Validate from Detail to rest of system. First PIPIN       *~
            *************************************************************
        validate_files

            if str(pipinvalid$()) = " " then pipout_valid
            call "SHOSTAT" ("Validating Procurements File")

            if pipinvalid$(7) = " " then L30100
               gosub'255(3%)  /* Delete Orphans, Find 'Single Parents' */
            if str(pipinvalid$(),1,6) = " " then pipout_valid

L30100:     init (hex(00)) plowkey$

        read_next_pipin
            call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
                if f1%(3) = 0% then pipout_valid

            get #3, using L30130, record$
L30130:         FMT CH(60)

            if str(record$,30,2) = "PO" then po_pipin_valid
            if str(record$,30,2) = "QC" then qc_pipin_valid
            if str(record$,30,2) = "RO" then ro_pipin_valid
            if str(record$,30,2) = "RW" then ro_pipin_valid
            if str(record$,30,2) = "JO" then jo_pipin_valid
            if str(record$,30,2) = "WO" then next_pipin_valid
            if str(record$,30,2) = "BO" then next_pipin_valid
            if str(record$,30,2) = "BW" then next_pipin_valid

        /* Fall thru & Last Gasp Loop Back                              */

          if pipinvalid$(6) = " " then next_pipin_valid
        bo_pipin_valid
            call "READ100" (#12, str(record$,30,19), f1%(12))
                if f1%(12) = 0% then delete_pipin   /* No BCKLINES      */
            get #12, using L30210, readkey$, onord, alloc, preinv, day2$
L30210:         FMT CH(25), POS(109), PD(14,4), POS(125), 2*PD(14,4),    ~
                    POS(212), CH(6)

            onord = max(0, onord - alloc - max(0, preinv - alloc))
            if onord = 0 then delete_pipin          /* Open Qty = 0     */
            call "REDALT0" (#13, str(record$,30,19), 1%, f1%(13))
                if f1%(13) = 0% then delete_pipin   /* No DEMMASTR      */
            get #13, using L30226, temp$
L30226:         FMT CH(1)
            if temp$ > "5" then delete_pipin        /* Planned          */

            if str(last_so$,1,16) = str(readkey$,10,16) then L30242
            call "READ100" (#11, readkey$, f1%(11))
                if f1%(11) = 0% then delete_pipin   /* No BCKMASTR ???  */
            get #11, using L30240, last_so$, store$
L30240:         FMT POS(10), CH(16), POS(803), CH(3)
L30242:     if str(store$,1,1) < "0" or str(store$,1,1) > "9"            ~
               then delete_pipin                    /* Not PIP Store    */

            get str(record$) using L30322, pip%, pipqty
            if pipqty = 0 then delete_pipin         /* <= Open allows   */
            gosub pip_index                         /* for pre-plan     */
            if pipqty > onord then update_pipin     /* Allocation       */
            if pip% =  index% then next_pipin_valid
               /* Note fall thru */

        /* Common 'Finish off' section                                  */
        update_pipin
            call "READ101" (#3, str(record$,30,19), f1%(3))
               if f1%(3) = 0% then next_pipin_valid
            put str(record$) using L30322, index%, onord
L30322:         FMT POS(26), BI(4), POS(49), PD(14,4)
            put #3, using L30330, record$
L30330:         FMT CH(60)
            rewrite #3
            pip_restore_required% = pip_restore_required% + 1%

            /* write to workfile for reporting */
            get str(record$) using L30340, p_part$, p_index%, p_tagnr$
L30340:          FMT CH(25), BI(4), CH(19)
            p_file$ = "PIPIN"
            p_type$ = "Updated"
            gosub print_record_to_workfile

            goto next_pipin_valid

        delete_pipin
            call "DELETE" (#3, str(record$,30,19), 19%)

            /* write to workfile for reporting */
            get str(record$) using L30340, p_part$, p_index%, p_tagnr$
            p_file$ = "PIPIN"
            p_type$ = "Delete"
            gosub print_record_to_workfile

            pip_restore_required% = pip_restore_required% + 1%

        next_pipin_valid
            init (hex(ff)) str(plowkey$,20)
            goto read_next_pipin

        po_pipin_valid
            if pipinvalid$(1) = " " then next_pipin_valid
            readkey$ = str(record$,32,14) & "  "
            call "REDALT0" (#6, readkey$, 1%, f1%(6))
               if f1%(6) = 0% then bo_pipin_valid    /* No VBKMASTR     */
            get #6, using L30424, readkey$
L30424:         FMT CH(25)
            str(readkey$,26,3) = str(record$,46,3)
            call "READ100" (#7, readkey$, f1%(7))
               if f1%(7) = 0% then bo_pipin_valid    /* No VBKLINES     */
            get #7, using L30444, onord, day2$, temp_pj$, temp$
L30444:       FMT POS(109), PD(14,4), POS(142), CH(6),                   ~
                  POS(166), CH(2), POS(174), CH(3)
            if str(temp$,1,1) < "0" or str(temp$,1,1) > "9"              ~
               then delete_pipin                     /* Not PIP Store   */
            if temp_pj$ = "PJ" then delete_pipin     /* PJs handled     */
                                                     /*  from Job side. */
            if onord = 0 then delete_pipin           /* No Open Qty     */
            get str(record$) using L30322, pip%, pipqty
            gosub pip_index
            if pip% <> index% then update_pipin      /* Bad Date In     */
            if onord = pipqty then next_pipin_valid  /* Qty OK, move on */
               goto update_pipin

        qc_pipin_valid
            if pipinvalid$(2) = " " then next_pipin_valid
            readkey$ = str(record$,32,14) & "  "
            call "REDALT0" (#6, readkey$, 1%, f1%(6))
               if f1%(6) = 0% then bo_pipin_valid    /* No VBKMASTR     */
            get #6, using L30524, readkey$
L30524:         FMT CH(25)
            str(readkey$,26,3) = str(record$,46,3)
            call "READ100" (#7, readkey$, f1%(7))
               if f1%(7) = 0% then bo_pipin_valid    /* No VBKLINES     */
            get #7, using L30544, day2$, temp_pj$, temp$, onord, onhold,  ~
                                                                   pipqty
L30544:       FMT POS(142), CH(6), POS(166), CH(2),                      ~
                  POS(174), CH(3), POS(373), 3*PD(14,4)
            if str(temp$,1,1) < "0" or str(temp$,1,1) > "9"              ~
               then delete_pipin                     /* Not PIP Store   */
            if temp_pj$ = "PJ" then delete_pipin     /* PJs handled     */
                                                     /*  from Job side. */
            onord = onord + onhold     /* Total = RCV Hold + QC Pend    */
            onord = onord + pipqty     /* Total = Total    + QC Hold    */
            if onord = 0 then delete_pipin           /* No RCV / QC Qty */
            get str(record$) using L30322, pip%, pipqty
            gosub pip_index
            if pip% <> index% then update_pipin      /* Date In Wrong   */
            if onord = pipqty then next_pipin_valid  /* Qty OK, Move On */
               goto update_pipin

        ro_pipin_valid
            if pipinvalid$(3) = " " then next_pipin_valid
            call "REDALT0" (#8, str(record$,30,19), 5%, f1%(8))
               if f1%(8) = 0% then bo_pipin_valid    /* Not fnd, PORLSE */
            get #8 using L30625, onord, day2$
L30625:         FMT POS(67), PD(14,4), POS(108), CH(6)
            if onord = 0 then delete_pipin           /* No QTY          */
            get str(record$) using L30322, pip%, pipqty
            gosub pip_index
            if pip% <> index% then update_pipin      /* Date In Wrong   */
            if onord = pipqty then next_pipin_valid  /* Qty OK, Move On */
               goto update_pipin

        jo_pipin_valid
            if str(record$,30,10) <> "JOB ORDER:" then pr_pipin_valid
            if pipinvalid$(4) = " " then next_pipin_valid
            call "READ100" (#9, str(record$,41,8), f1%(9))
                if f1%(9) = 0% then bo_pipin_valid   /* No JBMASTR2     */
            get #9, using L30730, onord, onhold, day2$
L30730:         FMT POS(83), 2*PD(14,4), POS(174), CH(6)
            onord = max(0, onord - onhold)
            if str(record$,41,2) <> "RW" then L30740
               if rwkpip$ <> "A" then onord = 0
L30740:     if onord = 0 then delete_pipin           /* None Left to Bld*/
            get str(record$) using L30322, pip%, pipqty
            gosub pip_index
            if pip% <> index% then update_pipin      /* Comp Date Error */
            if onord = pipqty then next_pipin_valid  /* Qty Ok, Move On */
               goto update_pipin

        pr_pipin_valid
            if str(record$,30,8) <> "JOB(PR):" then bo_pipin_valid
            if pipinvalid$(5) = " " then next_pipin_valid
            call "READ100" (#10, str(record$,38,8), f1%(10))
                if f1%(10) = 0% then bo_pipin_valid  /* No PRJMASTER    */
                   goto next_pipin_valid
                   /* Existance of Master is best we can do */

        REM *************************************************************~
            *               V A L I D A T I O N                         *~
            *-----------------------------------------------------------*~
            * Validate from Detail to rest of system. Next PIPOUT.      *~
            *************************************************************
        pipout_valid

            if str(pipoutvalid$()) = " " then wcout_valid
            call "SHOSTAT" ("Validating Withdrawals File")

            if pipoutvalid$(6) = " " then L31140
               gosub'255(4%)  /* Delete Orphans, Find 'Single Parents' */
            if str(pipoutvalid$(),1,5) = " " then wcout_valid

L31140:     init (hex(00)) plowkey$

        read_next_pipout
            call "PLOWNEXT" (#4, plowkey$, 0%, f1%(4))
                if f1%(4) = 0% then wcout_valid

            get #4, using L31200, record$
L31200:         FMT CH(64)

            if str(record$,1,2) = "WO" then wo_pipout_valid
            if str(record$,1,2) = "BW" then wo_pipout_valid
            if str(record$,1,2) = "RW" then wo_pipout_valid
            if str(record$,1,2) = "JO" then jo_pipout_valid
            if str(record$,1,2) = "EX" then ex_pipout_valid

        /* Fall thru & Last Gasp Loop Back                             */

            if pipoutvalid$(5) = " " then next_pipout_valid
        so_pipout_valid
            call "READ100" (#12, str(record$,1,19), f1%(12))
               if f1%(12) = 0% then delete_pipout  /* No BCKLINES      */
            get #12, using L31350, readkey$, onord, preinv, day2$
L31350:         FMT CH(25), POS(109), PD(14,4), POS(133), PD(14,4),      ~
                    POS(212), CH(6)
            onord = max(0, onord - preinv)
            if onord = 0 then delete_pipout
            call "READ100" (#11, readkey$, f1%(11))
                if f1%(11) = 0% then delete_pipout /* No BCKMASTR      */
            get #11, using L31420, temp$
L31420:         FMT POS(803), CH(3)
            if str(temp$,1,1) < "0" or str(temp$,1,1) > "9"              ~
               then delete_pipout                  /* Not PIP Store    */
            get str(record$) using L31550, pip%, pipqty
            if pipqty = 0 then delete_pipout_1
            gosub pip_index
            if pip% < index% then update_pipout
            if pipqty <= onord then read_next_pipout

        update_pipout
            call "READ101" (#4, str(record$,1,56), f1%(4))
               if f1%(4) = 0% then read_next_pipout
            onord = min(pipqty, onord)
            put str(record$) using L31550, index%, onord
L31550:         FMT POS(45), BI(4), POS(57), PD(14,4)
            delete #4 : write #4, using L31570, record$
L31570:         FMT CH(64)

            /* write to workfile for reporting */
            get str(record$) using L31580, p_tagnr$, p_part$, p_index%
L31580:          FMT CH(19), CH(25), BI(4)
            p_file$ = "PIPOUT"
            p_type$ = "Updated"
            gosub print_record_to_workfile

            pip_restore_required% = pip_restore_required% + 1%
            goto read_next_pipout

        delete_pipout_1
            call "READ101" (#4, str(record$,1,56), f1%(4))
               if f1%(4) = 0% then read_next_pipout
            delete #4

            /* write to workfile for reporting */
            get str(record$) using L31590, p_tagnr$
L31590:          FMT CH(19)
            p_file$ = "PIPOUT"
            p_type$ = "Delete"
            p_part$ = "ALL Deleted"
            p_index% = 0%
            gosub print_record_to_workfile

            pip_restore_required% = pip_restore_required% + 1%
            goto read_next_pipout

        delete_pipout
            call "DELETE" (#4, str(record$,1,19), 19%)

            /* write to workfile for reporting */
            get str(record$) using L31580, p_tagnr$, p_part$, p_index%
            p_file$ = "PIPOUT"
            p_type$ = "Deleted"
            gosub print_record_to_workfile

            pip_restore_required% = pip_restore_required% + 1%
            goto read_next_pipout


        next_pipout_valid
            init (hex(ff)) str(plowkey$,20)
            goto read_next_pipout

        jo_pipout_valid
            if str(record$,1,10) <> "JOB ORDER:" then pr_pipout_valid
            if pipoutvalid$(2) = " " then next_pipout_valid
            call "READ100" (#9, str(record$,12,8), f1%(9))
                if f1%(9) = 0% then so_pipout_valid
                   goto next_pipout_valid

        pr_pipout_valid
            if str(record$,1,10) <> "JOB(PROJ):" then so_pipout_valid
            if pipoutvalid$(3) = " " then next_pipout_valid
            call "READ100" (#10, str(record$,12,8), f1%(10))
                if f1%(10) = 0% then so_pipout_valid
                   goto next_pipout_valid

        wo_pipout_valid
            if pipoutvalid$(1) = " " then next_pipout_valid
            call "READ100" (#3, str(record$,1,19), f1%(3))
                if f1%(3) = 0% then so_pipout_valid
                   goto next_pipout_valid

        ex_pipout_valid
            if pipoutvalid$(4) = " " then next_pipout_valid
            readkey$ = str(record$,20,25) & str(record$, 3,17)
            call "READ100" (#15, readkey$, f1%(15))
                if f1%(15) = 0% then so_pipout_valid
            get #15, using L31950, onord, temp$
L31950:         FMT POS(69), PD(14,4), POS(404), CH(6)
            call "HNYEXSUB" (str(record$,6,14), str(record$,3,3),        ~
                             str(record$,20,25), temp$, onord)
            goto next_pipout_valid

        REM *************************************************************~
            *               V A L I D A T I O N                         *~
            *-----------------------------------------------------------*~
            * Validate from Detail to rest of system. Next WCOUT.       *~
            *************************************************************
        wcout_valid

            if str(wcoutvalid$()) = " " then return
            call "SHOSTAT" ("Validating Work Center Details File")
            init (hex(00)) plowkey$

L32110:     call "PLOWNEXT" (#5, plowkey$, 0%, f1%(5))
                if f1%(5) = 0% then return

            get #5, using L32150, record$
L32150:         FMT CH(68)

            if str(record$,9,2) = "WO" then wo_wcout_valid
            if str(record$,9,2) = "BW" then wo_wcout_valid
            if str(record$,9,2) = "RW" then wo_wcout_valid
            if str(record$,9,2) = "JO" then jo_wcout_valid

        delete_wcout
            call "DELETE" (#5, str(record$,9,19), 19%)

            /* write to workfile for reporting */
            get str(record$) using L32300, p_tagnr$, p_index%
L32300:         FMT POS(9), CH(19), BI(2)
            p_part$ = " "
            p_file$ = "WCOUT"
            p_type$ = "Deleted"
            gosub print_record_to_workfile

            wc_restore_required% = wc_restore_required% + 1%
        next_wcout_valid
            init (hex(ff)) str(plowkey$,20)
            goto L32110

        jo_wcout_valid
        if wcoutvalid$(2) = " " then next_wcout_valid
            call "READ100" (#9, str(record$,20,8), f1%(9))
                if f1%(9) = 0% then delete_wcout
                   goto next_wcout_valid

        wo_wcout_valid
        if wcoutvalid$(1) = " " then next_wcout_valid
            call "READ100" (#3, str(record$,9,19), f1%(3))
                if f1%(3) <> 0% then next_wcout_valid
                   if str(record$,9,2) <> "WO" then delete_wcout
            call "REDALT0" (#13, "PM" & str(record$,11,17), 1%, f1%(13))
                if f1%(13) = 0% then delete_wcout
            get #13, using L32400, temp$
L32400:         FMT CH(1)
            if temp$ > "5" then next_wcout_valid
               goto delete_wcout

        REM *************************************************************~
            *               V A L I D A T I O N                         *~
            *-----------------------------------------------------------*~
            * Miscellaneous Common Routines.                            *~
            *************************************************************

        pip_index
*        Calculate the index in the planning calendar.
            index% = 0%
            call "DATE" addr("G-", day1$, day2$, index%, ret%)
            index% = index% + 1
            index% = max(1%, min(index%, 490%))
            if ret% = 0  and  (index% < 1 or index% > 490) then ret% = 2
            if ret% <> 0  then index% = pip%
            return


        deffn'255 (file%)    /* Delete Orphans, Find Single Parents */

            init (hex(00)) plowkey$

L33240:     call "PLOWALTS" (#file%, plowkey$, 1%, 0%, f1%(file%))
                if f1%(file%) = 0% then return     /* No more Details   */
            if file% = 3% then                                            ~
                         get #3 using L33250, p_part$, P_index%, p_tagnr$ ~
                  else   get #4 using L33260, p_tagnr$, p_part$, P_index%
L33250:   /* PIPIN */     FMT CH(25), BI(4), CH(19)
L33260:   /* PIPOUT */    FMT CH(19), CH(25), BI(4)

            call "READ100" (#1, str(plowkey$,1,25), f1%(1)) /* HNYMASTR */
                if f1%(1)  = 0% then L33400
            call "READ100" (#2, str(plowkey$,1,25), f1%(2)) /* PIPMASTR */
                if f1%(2) <> 0% then L33510  /* Accept it as OK          */
                   get #1, using L33310, type$
L33310:                FMT POS(180), CH(10)
                       convert type$ to type%, data goto L33420
                       if type% > 0% and type% < 200% then goto L33420
                   write #2 using L33360, " ", str(plowkey$,1,25),        ~
                                         str(zeroes$(),,1998)
L33360:                  FMT CH(1), CH(25), CH(1998)
                   /* Don't lose or fail to create details, but . . . */
                   pip_restore_required% = pip_restore_required% + 1%

                   /* write to workfile for reporting */
                   p_file$ = "PIPMASTR"
                   p_type$ = "Created"
                   gosub print_record_to_workfile

                   goto L33510
L33400: /* Delete PIPMASTR, No HNYMASTR or wrong Part Type */
           call "DELETE" (#2, str(plowkey$,1,25), 25%)
                   /* write to workfile for reporting */
                   p_file$ = "PIPMASTR"
                   p_type$ = "Deleted"
                   gosub print_record_to_workfile

L33420: /* Delete All Details for this Part */
           call "DELETE" (#16, str(plowkey$,1,25), 25%)
                   /* write to workfile for reporting */
                   p_file$ = "SFCUM2"
                   p_type$ = "Deleted"
                   gosub print_record_to_workfile

           call "DELETE" (#17, str(plowkey$,1,25), 25%)
                   /* write to workfile for reporting */
                   p_file$ = "SFMASTR2"
                   p_type$ = "Deleted"
                   gosub print_record_to_workfile


            init (hex(00)) str(plowkey$,26)
L33470:     call "PLOWAL1" (#file%, plowkey$, 1%, 25%, f1%(file%))
                if f1%(file%) = 0% then L33520
            delete #file%

            /* write to workfile for reporting */
            if file% = 3% then p_file$ = "PIPIN" else p_file$ = "PIPOUT"
            if file% = 3% then                                            ~
                         get #3 using L33250, p_part$, P_index%, p_tagnr$ ~
                  else   get #4 using L33260, p_tagnr$, p_part$, P_index%
            p_type$ = "Deleted"
            gosub print_record_to_workfile

            goto L33470

L33510: /* Move on to next Part             */
L33520:     init (hex(ff)) str(plowkey$,26)
            goto L33240


        REM *************************************************************~
            *                V E R I F I C A T I O N                    *~
            *-----------------------------------------------------------*~
            * Verify from rest of system to Detail                      *~
            *************************************************************

        verify_files

            if str(pipinverify$(),1,4) = " " then so_verify

            if pipinverify$(1) = " " then po_pipin_verify_start
            call "SHOSTAT" ("Verification of Released PO Files.")
            init (hex(00)) plowkey$ : str(plowkey$,1,1) = "R"

        ro_pipin_verify

            call "PLOWALTS" (#8, plowkey$, 5%, 1%, f1%(8))
               if f1%(8) = 0% then po_pipin_verify_start
            get #8 using L35225, part$, onord, day2$, tag$
L35225:         FMT POS(14), CH(25), POS(67), PD(14,4),                  ~
                    POS(108), CH(6),  POS(242), CH(19)

            if onord <= 0 then ro_pipin_verify

            call "READ100" (#2, part$, f1%(2))
               if f1%(2) = 0% then ro_pipin_verify

            call "READ100" (#3, tag$, f1%(3))
               if f1%(3) <> 0% then ro_pipin_verify

            gosub pip_index
              if ret% <> 0% then ro_pipin_verify
            index1% = index%
            convert str(tag$,3,3) to index1%, data goto L35305

L35305:     put str(record$) using L35315, part$, index%, tag$,           ~
                             onord, index1%
L35315:     FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            write #3, using L35330, str(record$,,60),                     ~
                                        eod goto ro_pipin_verify
L35330:           FMT CH(60)
            pip_restore_required% = pip_restore_required% + 1%

            /* write to workfile for reporting */
            p_tagnr$ = tag$
            p_part$  = part$
            p_index% = index%
            p_file$  = "PIPIN"
            p_type$  = "Updated"
            gosub print_record_to_workfile

            goto ro_pipin_verify

        po_pipin_verify_start

            if str(pipinverify$(),2,2) = " " then jo_pipin_verify_start
            call "SHOSTAT" ("Verification of PO/QC Files.")
            init (hex(00)) plowkey$

        po_pipin_verify

            call "PLOWNEXT" (#7, plowkey$, 0%, f1%(7))
               if f1%(7) = 0% then jo_pipin_verify_start

            get #7, using L35465, str(tag$,1,14), str(tag$,15,3), part$,  ~
                                 onord, day2$, temp_pj$, temp$, onhold,  ~
                                 qc, qchold
L35465:       FMT POS(10), CH(14), POS(26), CH(3), POS(32), CH(25),      ~
                  POS(109), PD(14,4), POS(142), CH(6),                   ~
                  POS(166), CH(2), POS(174), CH(3), POS(373), 3*PD(14,4)

            if str(temp$,1,1) < "0" or str(temp$,1,1) > "9" then         ~
                                                   po_pipin_verify
            if temp_pj$ = "PJ" then po_pipin_verify  /* PJs handled   */
                                                     /* from Job side. */
            call "READ100" (#2, part$, f1%(2))
               if f1%(2) = 0% then po_pipin_verify

            if pipinverify$(2) = " " then qc_pipin_verify

            if onord = 0 then qc_pipin_verify
            call "READ100" (#3, "PO" & tag$, f1%(3))
               if f1%(3) <> 0% then qc_pipin_verify

            gosub pip_index
              if ret% <> 0% then po_pipin_verify

            put str(record$) using L35565, part$, index%, "PO", tag$,     ~
                             onord, index%
L35565:     FMT CH(25), BI(4), CH(2), CH(17), PD(14,4), BI(4)
            write #3, using L35580, str(record$,,60),                     ~
                                        eod goto qc_pipin_verify
L35580:           FMT CH(60)

            /* write to workfile for reporting */
            p_tagnr$ = tag$
            p_part$  = part$
            p_index% = index%
            p_file$  = "PIPIN"
            p_type$  = "Updated"
            gosub print_record_to_workfile

            pip_restore_required% = pip_restore_required% + 1%

        qc_pipin_verify

            if pipinverify$(3) = " " then po_pipin_verify
            if onhold + qc + qchold = 0 then po_pipin_verify
            call "READ100" (#3, "QC" & tag$, f1%(3))
               if f1%(3) <> 0% then po_pipin_verify

            gosub pip_index
              if ret% <> 0% then po_pipin_verify

            put str(record$) using L35660, part$, index%, "QC", tag$,     ~
                             onhold + qc + qchold, index%
L35660:     FMT CH(25), BI(4), CH(2), CH(17), PD(14,4), BI(4)
            write #3, using L35675, str(record$,,60),                     ~
                                        eod goto po_pipin_verify
L35675:         FMT CH(60)
            pip_restore_required% = pip_restore_required% + 1%

            /* write to workfile for reporting */
            p_tagnr$ = tag$
            p_part$  = part$
            p_index% = index%
            p_file$  = "PIPIN"
            p_type$  = "Updated"
            gosub print_record_to_workfile

            goto po_pipin_verify

        jo_pipin_verify_start

            if pipinverify$(4) = " " then so_verify
            call "SHOSTAT" ("Verification of SFC Files.")
            init (hex(00)) plowkey$

        jo_pipin_verify
            call "PLOWNEXT" (#9, plowkey$, 0%, f1%(9))
                if f1%(9) = 0% then so_verify

            get #9, using L35750, tag$, part$, onord, onhold, day2$
L35750:     FMT CH(8),POS(58),CH(25),POS(83),2*PD(14,4),POS(174),CH(6)
            onord = max(0, onord - onhold)

            if str(plowkey$,1,2) <> "RW" then L35765
               if rwkpip$ <> "A" then onord = 0

L35765:     if onord = 0 then jo_pipin_verify

            call "READ100" (#2, part$, f1%(2))
               if f1%(2) = 0% then jo_pipin_verify

            call "READ100" (#3, "JOB ORDER: " & tag$, f1%(3))
                if f1%(3) <> 0% then jo_pipin_verify

            gosub pip_index
              if ret% <> 0% then jo_pipin_verify

            put str(record$) using L35830, part$, index%, "JOB ORDER: ",  ~
                             tag$, onord, index%
L35830:     FMT CH(25), BI(4), CH(11), CH(8), PD(14,4), BI(4)
            write #3, using L35840, str(record$,,60),                     ~
                      eod goto jo_pipin_verify
L35840:         FMT CH(60)
            pip_restore_required% = pip_restore_required% + 1%

            /* write to workfile for reporting */
            p_tagnr$ = tag$
            p_part$  = part$
            p_index% = index%
            p_file$  = "PIPIN"
            p_type$  = "Updated"
            gosub print_record_to_workfile

            goto jo_pipin_verify

        so_verify
           if pipoutverify$(1) <> " " then gosub ex_verify
           if pipinverify$(5) = " " and pipoutverify$(2) = " " then return
            call "SHOSTAT" ("Verification of SALES ORDER Files.")
           init (hex(00)) plowkey$

        next_so
            call "PLOWNEXT" (#11, plowkey$, 0%, f1%(11))
               if f1%(11) = 0% then return
            get #11, using L36100, tag$, temp$
L36100:         FMT POS(10), CH(16), POS(803), CH(3)
            if str(temp$,1,1) < "0" or str(temp$,1,1) > "9" then next_so

            init (hex(00)) str(tag$,17)

        next_so_line
            f1%(3), f1%(4) = 1%
            call "PLOWNEXT" (#12, tag$, 16%, f1%(12))
               if f1%(12) = 0% then next_so

            get #12, using L36210, part$, onord, alloc, preinv, day2$
L36210:         FMT POS(32), CH(25), POS(109), PD(14,4),                 ~
                    POS(125), 2*PD(14,4), POS(212), CH(6)

            inqty = max(0, onord - alloc - max(0, preinv - alloc))
            outqty = max(0, onord - preinv)
            if outqty = 0 then next_so_line

            call "READ100" (#2, part$, f1%(2))
               if f1%(2) = 0% then next_so_line

            if pipinverify$(5) = " " then L36410
            if inqty = 0 then L36410

            call "REDALT0" (#13, tag$, 1%, f1%(13))
               if f1%(13) = 0% then L36410
            get #13, using L36370, temp$
L36370:         FMT CH(1)
            if temp$ > "5" then L36410
            call "READ100" (#3, tag$, f1%(3))

L36410:     if pipoutverify$(2) = " " then L36470
            if outqty = 0 then L36470

            init (hex(00)) readkey$:str(readkey$,1,19) = tag$
            call "PLOWNEXT" (#4, readkey$, 19%, f1%(4))

L36470:     if f1%(3) <> 0% and f1%(4) <> 0% then next_so_line

            gosub pip_index
              if ret% <> 0% then next_so_line

            if f1%(3) <> 0% then L36600
            put str(record$) using L36550, part$, index%,                 ~
                             tag$, inqty, index%
L36550:     FMT CH(25), BI(4), CH(19), PD(14,4), BI(4)
            write #3, using L36570, str(record$,,60), eod goto L36600
L36570:         FMT CH(60)
            pip_restore_required% = pip_restore_required% + 1%

            /* write to workfile for reporting */
            p_tagnr$ = tag$
            p_part$  = part$
            p_index% = index%
            p_file$  = "PIPIN"
            p_type$  = "Updated"
            gosub print_record_to_workfile


L36600:     if f1%(4) <> 0% then next_so_line
            put str(record$) using L36630, tag$, part$, index%, time,     ~
                             outqty
L36630:     FMT CH(19), CH(25), BI(4), CH(8), PD(14,4)
            write #4, using L36650, str(record$,,64), eod goto next_so_line
L36650:         FMT CH(64)
            pip_restore_required% = pip_restore_required% + 1%

            /* write to workfile for reporting */
            p_tagnr$ = tag$
            p_part$  = part$
            p_index% = index%
            p_file$  = "PIPOUT"
            p_type$  = "Updated"
            gosub print_record_to_workfile

            goto next_so_line

        ex_verify
            init (hex(00)) plowkey$
            call "PLOWNEXT" (#15, plowkey$, 0%, f1%(15))
               goto L38050
L38040:     call "READNEXT" (#15, f1%(15))
L38050:        if f1%(15) = 0% then return
            get #15, using L38080, str(record$,1,16), str(record$,17,25), ~
                str(record$,42,3), onord, temp$
L38080:         FMT CH(16), CH(25), CH(3), POS(69), PD(14,4),            ~
                    POS(404), CH(6)
            call "HNYEXSUB" (str(record$,1,16), str(record$,42,3),       ~
                             str(record$,17,25), temp$, onord)
            goto L38040

print_record_to_workfile

            if workfile_opened% = 0% then                    ~
                call "WORKOPEN" (#50, "SHARE", 500%, f1%(50%))
            workfile_opened% = 1%

            p_count% = P_count% + 1%
            convert p_count% to p_count$, pic(000000)
            convert p_index% to p_index$, pic(########)

            put #50 using L39100, p_count$, p_file$, p_type$, p_part$,  ~
                                  p_tagnr$, p_index$, " "
L39100:       FMT CH(6), CH(8), CH(7), CH(25), CH(19), CH(8), CH(27)

            write #50, eod goto print_record_to_workfile

            return

         print_report

            select printer(82)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */

            prev_file$ = " "

            if lcntr% > 56% then gosub page_head

            call "SHOSTAT" ("Printing Report Now")

*         ** Start Report Loop **

            plowkey$ = all(hex(00))
            call "READ102" (#50, plowkey$, f1%(50%))
            if f1%(50%) <> 0% then L39250
                print
                print "                          NO RECORDS TO REPORT ON"
                goto end_report

        report_loop
            call "READNEXT" (#50, f1%(50%))
            if f1%(50%) = 0% then goto end_report

L39250:     get #50 using L39100, p_count$, p_file$, p_type$, p_part$,   ~
                                  p_tagnr$, p_index$
            p_date$ = " "
            if p_index$ = " " then L39280
                 p_date$ = "bad date"
                 convert p_index$ to p_index : p_index% = p_index
                 call "DATE" addr("G+", dateun$, p_index%, p_date$, ret%)
                 if ret% <> 0% then goto L39280
            call "DATEFMT" (p_date$)

L39280:     if lcntr% > 54% then gosub page_head

            if prev_file$ <> p_file$ then print
            if prev_file$ <> p_file$ then lcntr% = lcntr% + 1%

            print using L60300, p_file$, p_type$, p_part$, p_tagnr$,    ~
                               p_date$

            lcntr% = lcntr% + 1%
            prev_file$ = p_file$

            goto report_loop

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64990, time$     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            call "FILEBGON" (#50) /* Zap Workfile2 */
            workfile_opened% = 0%

            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "PIPVRFY", rptid$
            print using L60110, rpttitle$, pcntr%
            print

            if pcntr% <> 0% then goto L39490
                gosub print_params
                return

L39490:     print using L60280
            print using L60290
            lcntr% = 5%
            return


        print_params           /* Print Page Zero */
L39510:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L39550
                str(i$(), i%, 1%) = hex(20)
                goto L39510
L39550:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 4% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"

L39610:     j% = pos(str(j$()) > hex(7f))
            if j% = 0% then L39650
                str(j$(), j%, 1%) = hex(20)
                goto L39610
L39650:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 4% to 19% : print tab(26); j$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"

            gosub page_head

            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)

              str(line2$, 4%) = "Compare PIP Details to other files."
              str(line2$,62%) = " PIPVRFY: " & str(cms2v$,,8%)

              inpmessage1$ = "These selections will delete or align"   & ~
                             " details as indicated by supporting data"

              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40140,         /* PIPIN - Valid     */   ~
                                L40140,         /* PIPOUT - Valid    */   ~
                                L40140          /* WCOUT - Valid     */   ~

              goto L40155

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40155:     accept                                                       ~
               at (01,02),                                               ~
                  "Validate Contents of PIP Detail Files",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(84)), inpmessage1$           , ch(79),~
                                                                         ~
               at (06,02), "PIP Procurement Details:",                   ~
                                                                         ~
               at (07,06), "Purchase Order Records",                     ~
               at (07,02), fac(lfac$( 1)), pipinvalid$( 1)      , ch(01),~
                                                                         ~
               at (08,06), "Q. C. Records",                              ~
               at (08,02), fac(lfac$( 1)), pipinvalid$( 2)      , ch(01),~
                                                                         ~
               at (09,06), "Released Buy Advices",                       ~
               at (09,02), fac(lfac$( 1)), pipinvalid$( 3)      , ch(01),~
                                                                         ~
               at (10,06), "SFC - Job Order Records",                    ~
               at (10,02), fac(lfac$( 1)), pipinvalid$( 4)      , ch(01),~
                                                                         ~
               at (11,06), "Project Records",                            ~
               at (11,02), fac(lfac$( 1)), pipinvalid$( 5)      , ch(01),~
                                                                         ~
               at (12,06), "Sales Order Records",                        ~
               at (12,02), fac(lfac$( 1)), pipinvalid$( 6)      , ch(01),~
                                                                         ~
               at (13,06), "Part Master File",                           ~
               at (13,02), fac(lfac$( 1)), pipinvalid$( 7)      , ch(01),~
                                                                         ~
               at (06,40), "PIP Withdrawal Details:",                    ~
                                                                         ~
               at (07,44), "Work Order Advices",                         ~
               at (07,40), fac(lfac$( 2)), pipoutvalid$( 1)     , ch(01),~
                                                                         ~
               at (08,44), "SFC - Job Order Records",                    ~
               at (08,40), fac(lfac$( 2)), pipoutvalid$( 2)     , ch(01),~
                                                                         ~
               at (09,44), "Project Records",                            ~
               at (09,40), fac(lfac$( 2)), pipoutvalid$( 3)     , ch(01),~
                                                                         ~
               at (10,44), "Expiration Records",                         ~
               at (10,40), fac(lfac$( 2)), pipoutvalid$( 4)     , ch(01),~
                                                                         ~
               at (11,44), "Sales Order Records",                        ~
               at (11,40), fac(lfac$( 2)), pipoutvalid$( 5)     , ch(01),~
                                                                         ~
               at (12,44), "Part Master File",                           ~
               at (12,40), fac(lfac$( 2)), pipoutvalid$( 6)     , ch(01),~
                                                                         ~
               at (15,02), "Work Center Details:",                       ~
                                                                         ~
               at (16,06), "Work Order Advices",                         ~
               at (16,02), fac(lfac$( 3)), wcoutvalid$( 1)      , ch(01),~
                                                                         ~
               at (17,06), "SFC - Job Order Records",                    ~
               at (17,02), fac(lfac$( 3)), wcoutvalid$( 2)      , ch(01),~
                                                                         ~
               at (15,40), fac(hex(84)), pipmsg$(1)             , ch(40),~
               at (16,40), fac(hex(84)), pipmsg$(2)             , ch(40),~
                                                                         ~
               at (18,40), fac(hex(84)), wcmsg$(1)              , ch(40),~
               at (19,40), fac(hex(84)), wcmsg$(2)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (24,20), fac(hex(8c)), pf5$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40500
                  call "MANUAL" ("PIPVRFY ")
                  goto L40155

L40500:        if keyhit% <> 15 then L40520
                  call "PRNTSCRN"
                  goto L40155

L40520:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)

              str(line2$, 4%) = "Compare other files to PIP Details."
              str(line2$,62%) = " PIPVRFY: " & str(cms2v$,,8%)

              inpmessage1$ = "These selections will create missing"    & ~
                             " details as indicated by supporting data"

              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41170,         /* PIPIN - Verify    */   ~
                                L41170          /* PIPOUT - Verify   */   ~

              goto L41200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41200:     accept                                                       ~
               at (01,02),                                               ~
                  "Verify Contents of PIP Detail Files",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(84)), inpmessage1$           , ch(79),~
                                                                         ~
               at (06,02), "PIP Procurement Details:",                   ~
                                                                         ~
               at (07,06), "Released Purchase Advices",                  ~
               at (07,02), fac(lfac$( 1)), pipinverify$( 1)     , ch(01),~
                                                                         ~
               at (08,06), "Purchase Order Records",                     ~
               at (08,02), fac(lfac$( 1)), pipinverify$( 2)     , ch(01),~
                                                                         ~
               at (09,06), "Q. C. Records",                              ~
               at (09,02), fac(lfac$( 1)), pipinverify$( 3)     , ch(01),~
                                                                         ~
               at (10,06), "SFC - Job Order Records",                    ~
               at (10,02), fac(lfac$( 1)), pipinverify$( 4)     , ch(01),~
                                                                         ~
               at (11,06), "Sales Order Records",                        ~
               at (11,02), fac(lfac$( 1)), pipinverify$( 5)     , ch(01),~
                                                                         ~
               at (06,40), "PIP Withdrawal Details:",                    ~
                                                                         ~
               at (07,44), "Expiration Records",                         ~
               at (07,40), fac(lfac$( 2)), pipoutverify$( 1)    , ch(01),~
                                                                         ~
               at (08,44), "Sales Order Records",                        ~
               at (08,40), fac(lfac$( 2)), pipoutverify$( 2)    , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (24,20), fac(hex(8c)), pf5$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41880
                  call "MANUAL" ("PIPVRFY ")
                  goto L41200

L41880:        if keyhit% <> 15 then L41920
                  call "PRNTSCRN"
                  goto L41200

L41920:           close ws
                  call "SCREEN" addr ("C", u3%, "I", j$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************~

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50230,         /* PIPIN              */    ~
                              L50270,         /* PIPOUT             */    ~
                              L50310          /* WCOUT              */    ~

            return

L50230: REM PIPIN
            for i% = 1% to 10%
              if pipinvalid$(i%) <> " " then pipinvalid$(i%) = "X"
            next i%
*          IF STR(PIPINVALID$(),1,5) <> " " THEN PIPINVALID$(6) = "X"
            return

L50270: REM PIPOUT
            for i% = 1% to 10%
              if pipoutvalid$(i%) <> " " then pipoutvalid$(i%) = "X"
            next i%
*          IF STR(PIPOUTVALID$(),1,4) <> " " THEN PIPOUTVALID$(5) = "X"
            return

L50310: REM WCOUT
            for i% = 1% to 10%
              if wcoutvalid$(i%) <> " " then wcoutvalid$(i%) = "X"
            next i%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************~

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51220,         /* PIPIN              */    ~
                              L51280          /* PIPOUT             */    ~

            return

L51220: REM PIPIN
            for i% = 1% to 10%
              if pipinverify$(i%) <> " " then pipinverify$(i%) = "X"
            next i%
            return

L51280: REM PIPOUT
            for i% = 1% to 10%
              if pipoutverify$(i%) <> " " then pipoutverify$(i%) = "X"
            next i%
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########  #######################################~
        ~   ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#####   PAGE: ####


*       * Column Header Lines
        %################################################################~
        ~##################

*       * Column header Line

L60280: %FILE      ACTION   PART NUNBER                TAG NUMBER        ~
        ~     DATE
L60290: %--------  -------  -------------------------  ------------------~
        ~---  --------
L60300: %########  #######  #########################  ##################~
        ~###  ##########


        %** Report Title for page 0
        %############################################################

L64990:          %    * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * *

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
