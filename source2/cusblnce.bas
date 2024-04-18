        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U   SSS   BBBB   L      N   N   CCC   EEEEE   *~
            *  C   C  U   U  S      B   B  L      NN  N  C   C  E       *~
            *  C      U   U   SSS   BBBB   L      N N N  C      EEEE    *~
            *  C   C  U   U      S  B   B  L      N  NN  C   C  E       *~
            *   CCC    UUU    SSS   BBBB   LLLLL  N   N   CCC   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSBLNCE - Program resets Open Order and Open A/R fields  *~
            *            in Customer Master File.  High A/R Balance and *~
            *            Date may also be reset to today's balance.     *~
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
            * 11/25/86 ! Original                                 ! ERN *~
            * 11/17/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/17/92 ! Cust Credit- Option to zero Avg Days/Pay.! JIM *~
            * 01/08/93 ! Refs to '# Invoices' chgd to '# Payments'! JIM *~
            * 03/31/93 ! PRR 12844. CCRMASTR must exist.          ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            arkey$30,                    /* ARMTRIAL plow key          */~
            bckkey$30,                   /* BCKMASTR plow key          */~
            billto$9,                    /* Bill-to Customer Code      */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Ship-to Customer Code      */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            high_ar_date$6,              /* Date of High A/R Balance   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16,                     /* PF 16 Screen Literal       */~
            reset_ar$1,                  /* Reset High A/R Balance?    */~
            reset_avg$1                  /* Reset Average Days to Pay? */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            axd$4,                       /* For OPENFILE               */~
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
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! BCKMASTR ! Backlog Master File                      *~
            * #3  ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #4  ! BCKBUFFR ! Sales Order Buffer File                  *~
            * #5  ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 21

            select #4,  "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize =  1020,             ~
                        keypos = 1, keylen =   10,                       ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16

            select #5,  "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

        call "SHOSTAT" ("Reset Customer Balances")
L02390:     u3% = 2%
            call "ASKUSER" (u3%, "PROGRAM NOTE",                         ~
                     "This Program locks the Customer, Sales Order, and",~
                     "A/R Trial Balances files from other processing.",  ~
                     "Press RETURN to Exit  -OR-  PF-16 to Continue.")
            if u3% =   0% then exit_program
            if u3% <> 16% then goto L02390
            call "SHOSTAT" ("Reset Customer Balances: Opening Files")

*        Open CUSTOMER file.  Must be successful or we're through!
            rslt$(1) = "REQUIRED"
            call "OPENFILE" (#1, "IO   ", f2%(1), rslt$(1), axd$)
            if f2%(1) <> 0% then exit_program

*        Open BCKMASTR file.  May continue if missing else bye-bye.
            call "OPENFILE" (#2, "IO   ", f2%(2), rslt$(2), axd$)
            if f2%(2)  =  0% then L02680
            if f2%(2)  = 20% then L02610
                call "ASKUSER" (2%, "FILE OPEN ERROR",                   ~
                                "Unable to open file BCKMASTR.",         ~
                                "Press RETURN to Exit Program.", " ")
                goto exit_program

L02610:         u3% = 2%
                call "ASKUSER" (u3%, "FILE NOT FOUND",                   ~
                       "The Sales Order file BCKMASTR was not found.",   ~
                       "Press RETURN to exit Program -OR-",              ~
                       "Press PF-16 to continue.")
                if u3% <> 16% then exit_program

L02680
*        Open ARMTRIAL file.  May continue if missing else exit program.
            call "OPENFILE" (#3, "IO   ", f2%(3), rslt$(3), axd$)
            if f2%(3)  =  0% then L02840
            if f2%(3)  = 20% then L02770
                call "ASKUSER" (2%, "FILE OPEN ERROR",                   ~
                                "Unable to open file ARMTRIAL.",         ~
                                "Press RETURN to Exit Program.", " ")
                goto exit_program

L02770:         u3% = 2%
                call "ASKUSER" (u3%, "FILE NOT FOUND",                   ~
                       "The Trial Balnce file ARMTRIAL was not found.",  ~
                       "Press RETURN to exit Program -OR-",              ~
                       "Press PF-16 to continue.")
                if u3% <> 16% then exit_program

L02840
*        Now verify that SO Buffer is empty.
            call "OPENFILE" (#4, "IO   ", f2%(4), rslt$(4), axd$)
            if f2%(4)  =  0% then L02930
            if f2%(4)  = 20% then L03020
                call "ASKUSER" (2%, "FILE OPEN ERROR",                   ~
                                "Unable to open file BCKBUFFR.",         ~
                                "Press RETURN to Exit Program.", " ")
                goto exit_program

L02930:         get str(rslt$(4)) using L02940, u3%
L02940:              FMT POS(17), BI(4)
                if u3% = 0% then L03020
                     call "ASKUSER" (2%, "ORDERS PENDING",               ~
                                    "All Sales Orders must be processed",~
                                    "before you can run this program.",  ~
                                    "Press RETURN to Exit.")
                     goto exit_program

L03020
*        Open CCRMASTR file.  May not continue if missing.
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$)
            if f2%(5%) =  0% then L09000
            if f2%(5%) = 20% then L03110
                call "ASKUSER" (2%, "FILE OPEN ERROR",                   ~
                                "Unable to open file CCRMASTR.",         ~
                                "Press RETURN to Exit Program.", " ")
                goto exit_program

L03110:         u3% = 2%
                call "ASKUSER" (u3%, "FILE NOT FOUND",                   ~
                      "The Customer Credit file CCRMASTR was not found.",~
                      "Press RETURN to Exit Program.", " ")
                goto exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, reset_ar$, reset_avg$

            for fieldnr% = 1% to  2%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10430
L10330:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf16$ = "(16)Reset Accts"
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       update_balances
                  if keyhit% <>  0 then       edtpg1
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then goto edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  pf16$ = " "
L11190:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
            goto edtpg1

        REM *************************************************************~
            *           U P D A T E   B A L A N C E S                   *~
            *-----------------------------------------------------------*~
            * Update Customer Master with balances derived from         *~
            * the transaction files.                                    *~
            *************************************************************
        update_balances

*        First pass the Customer file and clear All balances.  This is
*        necessary in order to correctly update Bill-to O/O $s in the
*        next pass.

            call "SHOSTAT" ("Clearing Customer File Balances")

            zero_balance_loop        /* Reset all balances in CUSTOMER */
                call "READNEXT" (#1, f1%(1%))              /* CUSTOMER */
                if f1%(1%) = 0% then goto zero_avg_days_loop
                call "READ101" (#5, key(#1), f1%(5%))      /* CCRMASTR */
                if f1%(5%) = 0% then gosub write_new_ccrmastr
                put #5 using L12190, 0, 0, 0                /* CCRMASTR */
L12190:             FMT POS(114), 3*PD(14,4)
                rewrite #5                                 /* CCRMASTR */
                goto zero_balance_loop

            zero_avg_days_loop
*        Conditionally reset Average Days to Pay & # Payments in CCRMASTR
                if reset_avg$ <> "Y" then goto L12240   /* The condition */
                call "SHOSTAT" ("Zeroing Average Days to Pay")
                primer$ = xor primer$
                call "READ103" (#5, primer$, f1%(5%))      /* CCRMASTR */
                goto L12233
            zero_avg_days_loop_2
                call "READNXT1" (#5, f1%(5%))              /* CCRMASTR */
L12233:         if f1%(5%) = 0% then goto L12240
                put #5 using L12235, 0, 0%                  /* CCRMASTR */
L12235:              FMT POS(74), PD(14,4), BI(2)
                rewrite #5                                 /* CCRMASTR */
                goto zero_avg_days_loop_2

L12240
*        Now reset balances.
            call "SHOSTAT" ("Resetting Customer File Balances")
            cuscode$ = hex(00)

            balance_loop
                call "PLOWNEXT" (#1, cuscode$, 0%, f1%(1%)) /* CUSTOMER */
                if f1%(1%) = 0% then exit_program
                get #1 using L12340, billto$
L12340:              FMT POS(780), CH(9)
                call "READ101" (#5, key(#1), f1%(5%))       /* CCRMASTR */
                get #5 using L12348, high_ar_date$, bill, ship, ar, high_ar
L12348:              FMT POS(108), CH(6), 4*PD(14,4)
                ship, ar = 0      /* For documentation purposes */

                bckkey$ = str(cuscode$) & hex(00) /* Get Open Order $s */
                bck_loop
                     call "PLOWNEXT" (#2, bckkey$, 9%, f1%(2))
                     if f1%(2) = 0% then L12460
                          get #2 using L12420, disc, oo
L12420:                        FMT POS(859), 2*PD(14,4)
                          ship = ship + oo - round(oo * disc * .01, 2)
                          goto bck_loop

L12460:         arkey$ = str(cuscode$) & hex(00)  /* Get Open A/R $s   */
                ar_loop
                     call "PLOWNEXT" (#3, arkey$, 9%, f1%(3))
                     if f1%(3) = 0% then L12550
                          get #3 using L12510, trans
L12510:                        FMT POS(68), PD(14,4)
                          ar = ar + trans
                          goto ar_loop

L12550:         if billto$ = cuscode$ then bill = bill + ship
                if reset_ar$ = "N" then L12580
                     high_ar = ar  :  high_ar_date$ = date
L12580:         put #5 using L12590, high_ar_date$, bill, ship, ar, high_ar
L12590:              FMT POS(108), CH(6), 4*PD(14,4)
                rewrite #5    /* Update this Customer in CCRMASTR */

                if billto$ = cuscode$ then balance_loop
                     call "READ101" (#5, billto$, f1%(5%))
                     get #5 using L12650, bill
L12650:                   FMT POS(114), PD(14,4)
                     bill = bill + ship
                     put #5 using L12650, bill  /* Update open order $s */
                     rewrite #5                /* for bill-to customer */
                     goto balance_loop

        write_new_ccrmastr
*        Dummy up a CCRMASTR record for this customer.
            t$ = "010101"
            write #05 using L12830, key(#1), 0, 0, 0, t$, 0, " ", 0, " ", ~
                                 0, 0%, t$, t$, t$, t$, t$, 0, 0, 0, 0,  ~
                                 " ", t$, " "
            call "READ101" (#5, key(#1), f1%(5%)) /* For operation */
                                                  /* after RETURN  */
            return

L12830:     FMT CH(9),         /*   1/ 9-   Customer Code (Key)        */~
                PD(14,4),      /*  10/ 8-   Total Sales                */~
                PD(14,4),      /*  18/ 8-   Total Credits              */~
                PD(14,4),      /*  26/ 8-   High Credit Limit          */~
                CH(6),         /*  34/ 6-   High Credit Limit Date     */~
                PD(14,4),      /*  40/ 8-   Last Invoice Amount        */~
                CH(8),         /*  48/ 8-   Last Invoice Number        */~
                PD(14,4),      /*  56/ 8-   Last Payment Amount        */~
                CH(10),        /*  64/10-   Last Payment Check #       */~
                PD(14,4),      /*  74/ 8-   Average # Days to Pay      */~
                BI(2),         /*  82/ 2-   # Invoices in Average Days */~
                5*CH(6),       /*  84/30-   'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),    /* 114/32-   'Dynamic' amnts fr CUSTOMER*/~
                CH(3),         /* 146/ 3-   User Last Modified         */~
                CH(6),         /* 149/ 6-   Date Last Modified         */~
                CH(46)         /* 155/46-   Filler                     */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Reset High A/R?  */~
                                    L21005          /* Reset Avg Days?  */
                  return

L20100
*        Reset High A/R Balance?               RESET_AR$
            inpmessage$ = "Enter 'Y' to Reset High A/R Balances to" &    ~
                          " today else enter 'N'."
            if reset_ar$ = " " then reset_ar$ = "N"
            return

L21005
*        Reset Average Days to Pay?            RESET_AVG$
            inpmessage$ = "Enter 'Y' to Reset Average Days to Pay & # o"&~
                "f applied Payments to Zero."
            if reset_avg$ = " " then reset_avg$ = "N"
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
                str(line2$,62%) = "CUSBLNCE: " & str(cms2v$,,8%)
                if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()
                on fieldnr% gosub L40180,           /* Reset High A/R?  */~
                                  L40180            /* Reset Avg Days?  */
                goto L40250

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40180:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "Reset Customer Master Balances",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Reset High A/R Balance?",                    ~
               at (06,33), fac(lfac$( 1)), reset_ar$            , ch(01),~
                                                                         ~
               at (07,02), "Reset Avg Days to Pay to Zero?",             ~
               at (07,33), fac(lfac$( 2)), reset_avg$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
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

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("CUSBLNCE")
                  goto L40250

L40530:        if keyhit% <> 15 then L40570
                  call "PRNTSCRN"
                  goto L40250

L40570:        if fieldnr% > 0% then return
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
                  on fieldnr% gosub L50100,         /* Reset High A/R?  */~
                                    L51005          /* Reset Avg Days?  */
                  return

L50100
*        Reset High A/R Balance?               RESET_AR$
            if reset_ar$ = "Y" or reset_ar$ = "N" then return
                errormsg$ = "Enter 'Y' -or- 'N'."
                return

L51005
*        Reset Average Days to Pay?            RESET_AVG$
            if reset_avg$ = "Y" or reset_avg$ = "N" then return
                errormsg$ = "Enter 'Y' -or- 'N'."
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
