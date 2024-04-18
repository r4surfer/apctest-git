        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD    SSS   PPPP   N   N   OOO    *~
            *  P   P  R   R  L      D   D  S      P   P  NN  N  O   O   *~
            *  PPPP   RRRR   L      D   D   SSS   PPPP   N N N  O   O   *~
            *  P      R   R  L      D   D      S  P      N  NN  O   O   *~
            *  P      R   R  LLLLL  DDDD    SSS   P      N   N   OOO    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDSPNO - Generates prenotifications for ACH tape for    *~
            *            either all employees with direct deposits or   *~
            *            one employee at a time.                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/23/88 ! Original                                 ! RN1 *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        sub "PRLDSPNO" (bank_in$)

        dim                                                              ~
            acct$17,                     /* Bank Acount Number         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bank$4, bank_in$4,           /* Internal Bank Code         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dfi$9,                       /* Receiving Bank Route       */~
            dtstamp$7,                   /* Date-Time Stamp            */~
            edtmessage$79,               /* Edit screen message        */~
            effto$6,                     /* Effective To Date          */~
            emp$12,                      /* Employee Number or 'ALL'   */~
            empdescr$32,                 /* Employee Number or 'ALL'   */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_emp$12,                 /* Last employee read         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            trcode$2,                    /* Transaction Code           */~
            userid$3                     /* Current User Id            */~

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
            * #03 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #04 ! PRLBANKF ! Payroll bank information file            *~
            * #05 ! EMPBANKS ! Supplemental to Employee Master File     *~
            * #09 ! PRLDDTIF ! Accumulating Batch of Direct Deposit Ent *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #03, "PERMASTR",                                      ~
                        varc,     indexed,  recsize =  950,              ~
                        keypos =   39, keylen =  12,                     ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50          ~

            select #04, "PRLBANKF",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   4,                     ~
                        alt key  1, keypos =    5, keylen =  30, dup     ~

            select #05, "EMPBANKS",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  13                      ~

            select #09, "PRLDDTIF",                                      ~
                        varc,     indexed,  recsize =  108,              ~
                        keypos =  102, keylen =  7,                      ~
                        alt key  1, keypos =   95, keylen =  14

            call "OPENCHCK" (#3, fs%(3), f2%(3),   0%, rslt$(3))
            call "OPENCHCK" (#4, fs%(4), f2%(4),   0%, rslt$(4))
            call "OPENCHCK" (#5, fs%(5), f2%(5),   0%, rslt$(5))
            call "OPENCHCK" (#9, fs%(9), f2%(9), 500%, rslt$(9))

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

            str(line2$,62) = "PRLDSPNO: " & str(cms2v$,,8)

            if bank_in$ = " " then L10000
                emp$   = "ALL"
                break% = 0%
                goto datasave


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Generating Prenotifications...")
            gosub dataput
            convert count% to inpmessage$, pic(#####)
            call "STRING" addr("LJ", inpmessage$, 5%)
            inpmessage$ = inpmessage$ & " Records were"
            if count% = 0% then inpmessage$ = "No Records were"
            if count% = 1% then inpmessage$ = "One Record was"
            inpmessage$ = inpmessage$                                    ~
                        & " written to the direct deposits' buffer."
            call "ASKUSER" (2%, "UPDATE RESULTS",                        ~
                     "Processing has been successfully completed:",      ~
                     inpmessage$, "Press RETURN to continue...")
            if break% = 0% then exit_program
                str(line2$,,60) = "Last Employee: " & emp$
                goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* EmployeeSelection      */
            return

L20100: REM Def/Enable Employee Number or 'ALL'    EMP$
            if emp$ = " " then emp$ = "ALL"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Employee Number or 'ALL'.                              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      emp$, empdescr$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput

*        Set up PLOW KEY per value of BREAK%...
            last_emp$ = all(hex(f1))
            plowkey$  = all(hex(00))
            if break% = 12% then str(plowkey$,,12) = emp$
            count%    =  0%

        loop
            call "PLOWNXT1" (#5, plowkey$, break%, f1%(5))
            if f1%(5) = 0% then return
                get #5 using L31170, emp$, bank$, acct$, trcode$, effto$
L31170:              FMT CH(12), XX(1), CH(4), CH(20), CH(2), POS(54),   ~
                         CH(6)
                if effto$   <> " " and effto$ <> blankdate$ and  ~
                     effto$ <= date then loop
                if bank_in$ <> " " and bank$  <> bank_in$ then loop

                if trcode$ = "22" then trcode$ = "23" else trcode$ = "33"
                call "READ100" (#4, bank$, f1%(4))
                if f1%(4) = 0% then loop
                get #4 using L31260, dfi$
L31260:              FMT POS(125), CH(9)
                if emp$ = last_emp$ then L31340
                     last_emp$ = emp$
                     call "READ100" (#3, emp$, f1%(3))
                     if f1%(3) = 0% then loop
                         get #3 using L31320, name_last$, name_first$
L31320:                      FMT XX(1), CH(15), CH(10)
                         empdescr$ = name_last$ & ", " & name_first$
L31340:         put #5 using L31350, date
L31350:              FMT POS(60), CH(6)
                rewrite #5
L31370:         call "GETDTTM" addr(dtstamp$)
                write #9 using L35200, "6", trcode$, dfi$, acct$,         ~
                     "0000000000", emp$, empdescr$, " ", "0", " ", " ",  ~
                     " ", dtstamp$, eod goto L31370
                count% = count% + 1%
                goto loop


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: PRLBANKF                          */~
            CH(4),          /* Direct Deposit Bank Code                */~
            CH(30),         /* bank description                        */~
            3*CH(30),       /* Address                                 */~
            CH(9),          /* direct deposit bank routing code        */~
            CH(123)         /* Unused Space                            */~

        FMT                 /* FILE: EMPBANKS                          */~
            CH(12),         /* Employee Code                           */~
            CH(1),          /* Sequence Number                         */~
            CH(4),          /* Direct Deposit Bank Code                */~
            CH(20),         /* Direct Deposit checking account number  */~
            CH(2),          /* Code used to identify type of a transati*/~
            PD(14,4),       /* Quantity of Something                   */~
            2*CH(6),        /* Effective Date Range                    */~
            CH(6),          /* Last PreNotification Date               */~
            CH(191)         /* Unused Space                            */~

L35200: FMT                 /* FILE: PRLDDTIF                          */~
            CH(1),          /* Record Type Identifier for Multi-Record */~
            CH(2),          /* Code used to identify type of a transati*/~
            CH(9),          /* ACH route/transit number of receiving ba*/~
            CH(17),         /* Employee's checking/savings account numb*/~
            CH(10),         /* Quantity of Something                   */~
            CH(15),         /* Employee Identification Number          */~
            CH(22),         /* Employee Name                           */~
            CH(2),          /* Optional Data                           */~
            CH(1),          /* Addenda Record Indicator                */~
            CH(8),          /* Routing/Transit Number of ACH Destinatio*/~
            CH(7),          /* Sequence Number                         */~
            CH(7),          /* Batch Number                            */~
            CH(7)           /* Sequence Number                         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40075          /* EmployeeSelection */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Generate Prenotifications",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Employee Number or 'ALL'",                   ~
               at (06,30), fac(lfac$( 1)), emp$                 , ch(12),~
               at (06,49), fac(hex(8c)),   empdescr$            , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40205
                  call "MANUAL" ("PRLDSPNO") : goto L40090

L40205:        if keyhit% <> 15 then L40220
                  call "PRNTSCRN" : goto L40090

L40220:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40315     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Routine"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40295
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40295:     if fieldnr% > 2% then L40305
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40305:     return

L40315: if fieldnr% > 0% then L40360  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Gen PreNotes"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40360:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50110          /* EmployeeSelection      */
            return

L50110: REM Test for Employee Number or 'ALL'     EMP$
            if emp$ <> "ALL" then L50160
                break% = 0%
                empdescr$ = "For ALL Employees"
                return
L50160:     call "GETEMPL" (#3, emp$, empdescr$, 0%, f1%(3))
            if f1%(3) = 1% then L50200
                errormsg$ = "Employee code not on file."
                return
L50200:     break% = 12%
            plowkey$ = str(emp$) & hex(00)
            call "PLOWNEXT" (#5, plowkey$, break%, f1%(5))
            if f1%(5) = 1% then return
                errormsg$ = "There are no direct deposit entries for"    ~
                          & " this employee."
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
