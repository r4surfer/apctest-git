        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  V   V  DDDD   TTTTT   *~
            *  H   H  NN  N  Y   Y  P   P    I    V   V  D   D    T     *~
            *  HHHHH  N N N   YYY   PPPP     I    V   V  D   D    T     *~
            *  H   H  N  NN    Y    P        I     V V   D   D    T     *~
            *  H   H  N   N    Y    P      IIIII    V    DDDD     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIVDT - Allows user to 'VOID' a given range of tickets.*~
            *            Option to void all recounts in the given range *~
            *            or only those tickets with a given recount     *~
            *            number.                                        *~
            *                                                           *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/24/87 ! Original                                 ! LDJ *~
            * 12/20/91 ! CMS/DEC 'MASK' Project/Added 'ALLFREE'   ! SID *~
            *          !   and 'SHOSTAT'                          !     *~
            * 09/13/93 ! PUT correct Recount in Alt field POS 328 ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            from_ticket$12,              /* Start From Ticket          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16,                     /* PF 16 literal              */~
            pf4$17,                      /* PF  4 literal              */~
            plowkey$100,                 /* Miscellaneous Read/Plow Key*/~
            recount$1,                   /* Recount Number to Void or A*/~
            recntr_ticket$12,            /* Recount Ticket Number      */~
            session_nbr$2,               /* Count Session Number       */~
            session_nbrdescr$32,         /* Count Session Number       */~
            to_ticket$12,                /* To Ticket                  */~
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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            * #2  ! HNYPITKT ! Physical Inventory Ticket File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYPISYS",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    7, keylen =   2,                     ~
                        alt key  1, keypos =    1, keylen =   8          ~

            select #2,  "HNYPITKT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  52, dup,    ~
                            key  2, keypos =  313, keylen =  16          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            rslt$(1), rslt$(2) = "REQUIRED"

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))

            if f2%(1) + f2%(2) > 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, to_ticket$, session_nbr$,  ~
                      session_nbrdescr$, str(line2$,,50%), from_ticket$, ~
                      recount$

            call "ALLFREE"

            pf16$ = "(16)Exit Program"

            for fieldnr% = 1% to  4%
L10130:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10270
L10141:               if fieldnr% > 1% then pf4$  = "(4)Previous Field"  ~
                                       else pf4$  = " "
                gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10230
L10180:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10141
                         if fieldnr% = 1% then L10130
                         goto L10180
L10230:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10141
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10141
L10270:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
            pf4$  = " "
            pf16$ = "(16)Void Tickets"
            inpmessage$ = edtmessage$

            gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
            oldfield% = 0%
L11130:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% >  4 then editmode
            if oldfield% = fieldnr% then editmode
            gosub'051(fieldnr%)
                  if enabled% = 0% then       editmode
L11170:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11170
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11170
                  oldfield% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Ticket Voiding Now In Progress")
            if from_ticket$ = "FIRST" then from_ticket$ = " "
            if to_ticket$ = "LAST" then to_ticket$ = hex(ff)
            plowkey$ = str(session_nbr$) & str(from_ticket$) & hex(00)
            gosub void_tickets
            gosub update_accounted_for_flag
            goto inputmode

        void_tickets
            call "PLOWNXT1" (#2, plowkey$, 2%, f1%(2))
            if f1%(2) = 0% then return
            if str(plowkey$,3%,12%) > to_ticket$ then return
            key_rcnt% =  val(str(key(#2),15%,1%),1)
            recount% = 99% - key_rcnt%
            if recount% <> rec% and recount$ <> "A" then void_tickets
            if recount$ <> "A" then                                      ~
               str(plowkey$,15%,1%) = hex(ff) /* Set Up for Next Plow */
            recntr_ticket$ = str(key(#2),3,12)
            put #2 using L19300, userid$, /* Operator User ID           */~
                                date,    /* Date Entered               */~
                                time,    /* Time Entered               */~
                                -1,      /* Qty Counted = -1 = VOIDED  */~
                                -1,0,0,0,0,0,0,0,0,0,                    ~
                                session_nbr$, "Y", recntr_ticket$,       ~
                                key_rcnt%
            rewrite #2
            goto void_tickets

L19300:     FMT POS(92), CH(3),POS(101),CH(6),CH(6),PD(14,4),10*PD(14,4),~
                POS(313), CH(2), CH(1), CH(12), BI(1)

        update_accounted_for_flag          /* Turn flag off  */
            call "READ101" (#1, session_nbr$, f1%(1))
            REM *** no test - if gone we're screwed anyway ***
            put #1 using L19370, " "
L19370:     FMT POS(365), CH(1)
            rewrite #1
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Count Session Num*/~
                                    L20200,         /* Start Ticket     */~
                                    L20300,         /* Ending Ticket    */~
                                    L20400          /* Recount Number   */
                     return
L20100:     REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Number to VOID "  ~
                            & "Tickets In"
                return
L20200:     REM Default/Enable for From Ticket
                inpmessage$ = "Enter the Starting Ticket Number to VOID"
*              IF FROM_TICKET$ = " " THEN FROM_TICKET$ = "FIRST"
                return
L20300:     REM Default/Enable for To Ticket
                inpmessage$ = "Enter the Ending Ticket Number to VOID"
*              IF TO_TICKET$ = " " THEN TO_TICKET$ = "LAST"
                return
L20400:     REM Default/Enable for Recount Number
                inpmessage$ = "Enter the Specific RECOUNT Number to VOID"~
                            & " or 'A' to VOID all recounts"
                if recount$ = " " then recount$ = "A"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "HNYPIVDT: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  on fieldnr% gosub L40160,         /* Count Session Num*/~
                                    L40160,         /* Start Ticket     */~
                                    L40160,         /* End   Ticket     */~
                                    L40160          /* Recount Number   */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40230

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40160:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "Void Physical Inventory Tickets",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Count Session Number       :",                        ~
               at (06,31), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (06,49), fac(hex(8c)),   session_nbrdescr$    , ch(32),~
               at (07,02),                                               ~
                  "Starting Ticket to VOID    :",                        ~
               at (07,31), fac(lfac$( 2)), from_ticket$         , ch(12),~
               at (08,02),                                               ~
                  "Ending Ticket to  VOID     :",                        ~
               at (08,31), fac(lfac$( 3)), to_ticket$           , ch(12),~
               at (09,02),                                               ~
                  "Recount Nbr to VOID or All :",                        ~
               at (09,31), fac(lfac$( 4)), recount$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), fac(hex(8c)), pf4$                   , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40620
                  call "MANUAL" ("HNYPIVDT")
                  goto L40230

L40620:        if keyhit% <> 15 then L40670
                  call "PRNTSCRN"
                  goto L40230

L40670:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Count Session Num*/~
                                    L50200,         /* Start Ticket     */~
                                    L50300,         /* End   Ticket     */~
                                    L50400          /* Recount to Void  */
                     return
L50100:     REM Test Data for Count Session Number
                call "GETCODE" (#1, session_nbr$,session_nbrdescr$, 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then errormsg$ =                          ~
                   "Session Undefined or None on File"
                return
L50200:     REM Test Data for Starting Ticket
                if from_ticket$ = "FIRST" then return
                plowkey$=str(session_nbr$) & str(from_ticket$) & bin(99,1)
                call "READ100" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then return
                errormsg$ = "Ticket Number Not on File!"
                return
L50300:     REM Test Data for Ending Ticket
                if to_ticket$ = "LAST" then return
                plowkey$=str(session_nbr$) & str(to_ticket$) & bin(99,1)
                call "READ100" (#2, plowkey$, f1%(2))
                if f1%(2) = 1% then L50360
                errormsg$ = "Ticket Number Not on File!"
                return
L50360:         if to_ticket$ >= from_ticket$ then return
                if from_ticket$ = "FIRST" then return
                errormsg$ = "TO Ticket CANNOT be less than FROM Ticket!"
                return
L50400:     REM Test Data for Recount Number
                if recount$ = "A" then return
                convert recount$ to rec%, data goto L50440
                return
L50440:         errormsg$ = "Must be A or 0 thru 9"
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
