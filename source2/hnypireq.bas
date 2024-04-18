        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  RRRR   EEEEE   QQQ    *~
            *  H   H  NN  N  Y   Y  P   P    I    R   R  E      Q   Q   *~
            *  HHHHH  N N N   YYY   PPPP     I    RRRR   EEEE   Q   Q   *~
            *  H   H  N  NN    Y    P        I    R   R  E      Q Q Q   *~
            *  H   H  N   N    Y    P      IIIII  R   R  EEEEE   QQQ    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIREQ - Allows user to request 'extra' or additional   *~
            *            tickets to be printed for a specified Physical *~
            *            Inventory session.  Requests number of tickets *~
            *            to generate from user & then 'goes for it'.    *~
            *            HNYPISYS is updated with the last ticket number*~
            *            generated, the number of Extra Tickets field   *~
            *            is incremented, and the Tickets Accounted For  *~
            *            flag is turned off.                            *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/14/86 ! Original                                 ! LDJ *~
            * 06/10/86 ! Corrected bug in Ticket Length. Changed  ! LDJ *~
            *          !   function LGT to use function LOG.      !     *~
            * 03/19/87 ! File layout changes, no significant      ! LDJ *~
            *          !  functionality changes to this program.  !     *~
            * 05/27/87 ! Standard Costing Enhancements            ! MJB *~
            * 11/01/91 ! CMS/DEC 'MASK' Project/Added 'ALLFREE'   ! SID *~
            *          !    and 'SHOTSTAT'                        !     *~
            * 05/26/92 ! Exclude Extras if no 'blanks' were first ! RJH *~
            *          !  generated in HNYPIGEN                   !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            case_cost(10),               /* Case Cost & uom fields     */~
            case_costs$80,               /* For PACKZERO               */~
            check_digit$1,               /* Generate Check Digit Y/N   */~
            cost(13),                    /* Cost Buckets               */~
            costs$104,                   /* For PACKZERO               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            extra$6,                     /* Extra Tickets              */~
            extras$5,                    /* Additional 'Extra' Tickets */~
            glvar$1,                     /* Update G/L variances flag  */~
            hnyvar$1,                    /* Update HNY variances Flag  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastticket$6,                /* Last Ticket # Generated    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16,                     /* PF 16 literal              */~
            pf4$17,                      /* PF  4 literal              */~
            prefix$3,                    /* Optional Ticket # Prefix   */~
            session_nbr$2,               /* Count Session Number       */~
            session_nbrdescr$32,         /* Count Session Number       */~
            start_ticket$6,              /* First Ticket in Session    */~
            ticket$12,                   /* Generated Ticket Number    */~
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
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
                           " to Desired Value and Press RETURN."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            call "ALLFREE"

            pf16$ = "(16)Exit Program"
            pf4$  = " "

            for fieldnr% = 1 to  2
                if fieldnr% <> 1% then pf4$  = "(4)Previous Field"
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10240
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10240:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
            pf4$  = " "
            pf16$ = "(16)GEN TICKETS"
            inpmessage$ = edtmessage$

L11060:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then L11060
            gosub'051(fieldnr%)
                  if enabled% = 0% then       L11060
L11140:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Extra Tickets Generation Now In Progress")
            gosub generate_extra_tickets
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Count Session Num*/~
                                    L20200          /* Extra Tickets    */
                     return
L20100:     REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Number to Generate~
        ~ Extra Tickets For"
                return
L20200:     REM Default/Enable for Additional 'Extra' Tickets
                inpmessage$ = "Enter the number of extra tickets to Gener~
        ~ate"
                return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, extras$, session_nbr$,     ~
                      session_nbrdescr$, str(line2$,,50%)
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1 using L30080, prefix$, start_ticket$, lastticket$,     ~
                                check_digit$, extra$, hnyvar$, glvar$
L30080:     FMT POS(320), CH(3),CH(06), CH(06), CH(01), CH(6), POS(345), ~
                CH(01), CH(01)
            str(line2$,,50%) = "Last Ticket Generated = " & lastticket$
            extra = 0
            convert extra$ to extra, data goto L30110
L30110:     return

        construct_ticket_format
            ticket$ = prefix$
            convert lastticket$ to ticket%
            p% = pos(ticket$ = " ")
            l% = log(ticket% + extras) / log(10) + 1%
            if len(start_ticket$) > l% then l% = len(start_ticket$)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            lastticket$ = str(ticket$,p%,l%)
            call "READ101" (#1, session_nbr$, f1%(1))
            REM *** no test - if gone we're screwed anyway ***
            extra = extra + extras
            convert extra to extra$, pic(000000)
            put #1 using L31090, lastticket$, extra$, " "
L31090:     FMT POS(329), CH(06), POS(336), CH(6), POS(365), CH(1)
            rewrite #1
            return


        generate_extra_tickets
            mat cost = zer  :  mat case_cost = zer
            call "PACKZERO" (cost(), costs$)
            call "PACKZERO" (case_cost(), case_costs$)
            for x% = 1% to extras
               ticket% = ticket% + 1%
               on l% gosub L31560, L31580, L31600, L31620, L31640, L31660,L31671
               if check_digit$ = "Y" then gosub compute_check_digit
               put #2 using L35030,  /* FILE: HNYPITKT                  */~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               99%,         /* A number decremented for each recount   */~
                            /* (99=original)                           */~
               " ",         /* Part Number                             */~
               " ",         /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               " ",         /* Lot Number                              */~
               " ",         /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               "X",         /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               " ",         /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               " ",         /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               "N",         /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               " ",         /* Name of person who counted something    */~
               " ",         /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
               " ",         /* Date something was counted              */~
               " ",         /* Date a transaction was entered          */~
               " ",         /* The System Time when a transaction was  */~
                            /* entered                                 */~
               0,           /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               case_costs$, /* Quantity Counted for the current UOM    */~
                            /* (CASE) & UOM's                          */~
               0,           /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               costs$,      /* 12 cost buckets & total cost            */~
               session_nbr$,/* Number corresponding to a Inventory     */~
               "N",         /* Ticket/Count Sheets Print Flag          */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
               99%,         /* A number decremented for each recount   */~
               " "          /* Filler (Internal, unused space)         */

              write #2
            next x%
            return

L31560:     convert ticket% to str(ticket$,p%), pic(0)
            return
L31580:     convert ticket% to str(ticket$,p%), pic(00)
            return
L31600:     convert ticket% to str(ticket$,p%), pic(000)
            return
L31620:     convert ticket% to str(ticket$,p%), pic(0000)
            return
L31640:     convert ticket% to str(ticket$,p%), pic(00000)
            return
L31660:     convert ticket% to str(ticket$,p%), pic(000000)
            return
L31671:     convert ticket% to str(ticket$,p%), pic(0000000)
            return

        compute_check_digit
            sum% = 0%
            for i% = 1% to l%
                convert str(ticket$,p%-1%+i%,1%) to y%
                sum% = sum% + (l%+2%-i%) * y%
            next i%
            check_digit% = 11% - mod(sum%,11%)
            convert check_digit% to str(ticket$,len(ticket$)+1%,1%),pic(0)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************
L35030: FMT                 /* FILE: HNYPITKT                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99=original                            */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            CH(1),          /* General Purpose Flag or Switch          */~
                            /* Indicator                               */~
                            /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            CH(1),          /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
            CH(6),          /* The System Time when a transaction was  */~
                            /* entered                                 */~
            PD(14,4),       /* Quantity counted of something           */~
                            /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
            CH(80),         /* Quantity Counted for the current UOM    */~
                            /* (CASE) & UOM's                          */~
            PD(14,4),       /* Quantity of Something                   */~
                            /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
            CH(104),        /* 12 cost buckets & total cost            */~
                            /*            User Override fields         */~
            CH(2),          /* Number corresponding to a Inventory     */~
            CH(1),          /* Ticket/Count Sheets Print Flag          */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
            BI(1),          /* A number decremented for each recount   */~
            CH(164)         /* Filler (Internal, unused space)         */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "HNYPIREQ: " & str(cms2v$,,8%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40160,         /* Count Session Num*/~
                                    L40190          /* Extra Tickets    */
                  goto L40230

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40160:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40190:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "Request Additional Tickets",                          ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Count Session Number      :",                         ~
               at (06,30), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (06,49), fac(hex(8c)),   session_nbrdescr$    , ch(32),~
               at (07,02),                                               ~
                  "Additional 'Extra' Tickets:",                         ~
               at (07,30), fac(lfac$( 2)), extras$              , ch(05),~
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
                  call "MANUAL" ("HNYPIREQ")
                  goto L40230

L40620:        if keyhit% <> 15 then L40660
                  call "PRNTSCRN"
                  goto L40230

L40660:        if fieldnr% > 0% then return
                  close ws
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
                                    L50200          /* Extra Tickets    */
                     return
L50100:     REM Test Data for Count Session Number
                session_nbrdescr$ = hex(06) & "SELECT COUNT SESSION"
                call "GETCODE" (#1, session_nbr$,session_nbrdescr$, 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then errormsg$ =                          ~
                   "Session Undefined or None on File"                   ~
                else gosub dataload
                if extra < 1 then gosub no_blank_tickets
                return
L50200:     REM Test Data for Additional 'Extra' Tickets
                call "NUMTEST" (extras$, 1, 99999, errormsg$, 0, extras)
                if errormsg$ = " " then gosub construct_ticket_format
                return

        no_blank_tickets
            keyhit% = 2%
            call "ASKUSER" (keyhit%, "** NO EXTRAS ALLOWED **",          ~
                 "No Extra Tickets were requested in HNYPIGEN therefor", ~
                 "No MORE Extra Tickets can be generated.             ", ~
                 "Press Any Key to Continue"  )
            gosub  initialize_variables
            fieldnr% = 0%
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
