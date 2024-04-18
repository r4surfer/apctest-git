        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      RRRR    AAA   N   N   GGG   EEEEE   *~
            *  P   P  R   R  L      R   R  A   A  NN  N  G      E       *~
            *  PPPP   RRRR   L      RRRR   AAAAA  N N N  G GGG  EEEE    *~
            *  P      R   R  L      R   R  A   A  N  NN  G   G  E       *~
            *  P      R   R  LLLLL  R   R  A   A  N   N   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLRANGE - INPUTS RANGE OF DEPARTMENTS, EMPLOYEE CODES,   *~
            *            AND PAY FREQUENCIES TO PROCESS.                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/19/81 ! ORIGINAL                                 ! TEM *~
            * 01/04/89 ! Added effectivity date                   ! ERN *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 08/27/96 ! Changes for the Year 2000.               ! DXL *~
            * 10/09/97 ! Changed SHOWMSG to SHOSTAT (1 call)      ! MLJ *~
            *************************************************************

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* SCREEN VARIABLE            */~
            date$8,                      /* SCREEN DATE                */~
            dept$(2)4,                   /* FIRST-LAST DEPARTMENT      */~
            edtmessage$79,               /* EDIT MESSAGE               */~
            eff_date$8,                  /* Effectivity Date           */~
            empcode$(2)12,               /* EMPLOYEE CODE              */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            hdr$(2)12,                   /* From/To                    */~
            i$(24)80,                    /* SCREEN VARIABLE            */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            lfac$(20)1,                  /* LINEAR FAC'S               */~
            line2$79,                    /* Second screen line         */~
            payfreq$(2)1,                /* PAY FREQUENCIES            */~
            pf16$16                      /* PF-16 Prompt               */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            *************************************************************

            select #1, "SYSFILE2",                                       ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

        call "SHOSTAT" ("Opening files, one moment please")
            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")
            if f2%(1) <> 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "PRLRANGE: " & str(cms2v$,,8)
            edtmessage$ = "To Modify Displayed Values, Position Cursor"  ~
                        & " to Desired Value and Press RETURN."
            hdr$(1) = "  F R O M   "  :  hdr$(2) = "    T O     "
            call "READ100" (#1, "PRL PROCESSING RANGE", f1%(1))


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Input Mode Main Program.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, dept$(), empcode$(),       ~
                      payfreq$(), eff_date$
            pf16$ = "(16)Exit Program"

            for fieldnr% = 1% to 4%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10180
L10120:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then end(1%)
                      if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles Operation Of Edit Mode For Linear Screens.        *~
            *************************************************************

            pf16$ = "(16)Save Data"
L11070:     lastfieldnr% = 0%
            inpmessage$  = edtmessage$
            gosub'201(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L11070
L11120:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then L11070
            if fieldnr% = lastfieldnr% then L11070

            gosub'161(fieldnr%)
                  if enabled% = 0% then L10180
L11180:     gosub'201(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
                  goto L11120


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "DELETE" (#1, "PRL PROCESSING RANGE", 20%)
            gosub L30000
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
            enabled% = 1%
            inpmessage$ = " "
            on fieldnr%   gosub     L20100,         /* Departments      */~
                                    L20300,         /* Employees        */~
                                    L20500,         /* Pay Frequencies  */~
                                    L20700          /* Effectivity Date */
                     return

L20100
*        Default/Enable for DEPARTMENT CODES
            inpmessage$ = "Enter the range of department codes that are" ~
                        & " to be processed."
            if dept$(1) <> " " then return
                if f1%(1) = 0% then dept$(1) = "ALL"                     ~
                               else get #1 using L20130, dept$()
L20130:                            FMT POS(21), 2*CH(4)
                return

L20300
*        Default/Enable for EMPLOYEE CODES
            inpmessage$ = "Enter the range of employee codes that are"   ~
                        & " to be processed."
            if empcode$(1) <> " " then return
                if f1%(1) = 0% then empcode$(1) = "ALL"                  ~
                               else get #1, using L20330, empcode$()
L20330:                            FMT POS(29), 2*CH(12)
                return

L20500
*        Default/Enable for PAY FREQUENCY
            inpmessage$ = "Pay Frequencies vary from 1 to 7"
            if payfreq$(1) <> " " then return
                payfreq$(1) = "1"  :  payfreq$(2) = "7"
                if f1%(1) = 1% then get #1 using L20530, payfreq$()
L20530:                                      FMT POS(53), 2*CH(1)
                return

L20700
*        Default/Enable for EFFECTIVITY DATE
            inpmessage$ = "Enter date to be used to determine deduction" ~
                        & " effectivity."
            if eff_date$ <> " " and eff_date$ <> blankdate$ then return
                if f1%(1) = 0% then eff_date$ = date                     ~
                               else get #1 using L20760, eff_date$
L20760:                                  FMT POS(55), CH(6)
                call "DATEFMT" (eff_date$)
                return



        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                goto inputmode


L30000: REM *************************************************************~
            *   S A V E   R A N G E   O N   D I S K                     *~
            *************************************************************

            call "DATUNFMT" (eff_date$)
            write #1, using L30100, "PRL PROCESSING RANGE",               ~
                                   dept$(), empcode$(), payfreq$(),      ~
                                   eff_date$, " "
            return
L30100:         FMT CH(20), 2*CH(4), 2*CH(12), 2*CH(1), CH(6), CH(440)


        REM *************************************************************~
            *                  S C R E E N   P A G E   1                *~
            * --------------------------------------------------------- *~
            * Input/Edit Mode Page 1.                                   *~
            *************************************************************

            deffn'201(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()                  ~
                             else init(hex(8c)) lfac$()
            on fieldnr% gosub       L40190,         /* Department       */~
                                    L40190,         /* Employees        */~
                                    L40190,         /* Pay Frequencies  */~
                                    L40190          /* Eff Date         */
            goto L40260
                                    lfac$(fieldnr%) = hex(80)  :  return
L40190:                             lfac$(fieldnr%) = hex(81)  :  return
                                    lfac$(fieldnr%) = hex(82)  :  return

L40260:     accept                                                       ~
               at (01,02), "SET PAYROLL PROCESSING RANGE",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,30), fac(hex(ac)), hdr$(1),                        ~
               at (05,44), fac(hex(ac)), hdr$(2),                        ~
                                                                         ~
               at (06,02), "Department Codes",                           ~
               at (06,30), fac(lfac$( 1)), dept$(1)             , ch(04),~
               at (06,44), fac(lfac$( 1)), dept$(2)             , ch(04),~
                                                                         ~
               at (07,02), "Employee Codes",                             ~
               at (07,30), fac(lfac$( 2)), empcode$(1)          , ch(12),~
               at (07,44), fac(lfac$( 2)), empcode$(2)          , ch(12),~
                                                                         ~
               at (08,02), "Pay Frequencies",                            ~
               at (08,30), fac(lfac$( 3)), payfreq$(1)          , ch(01),~
               at (08,44), fac(lfac$( 3)), payfreq$(2)          , ch(01),~
                                                                         ~
               at (09,02), "Effectivity Date",                           ~
               at (09,30), fac(lfac$( 4)), eff_date$            , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                   keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13% then L40670
                  call "MANUAL" ("PRLRANGE")
                  goto L40260

L40670:        if keyhit% <> 15% then L40710
                  call "PRNTSCRN"
                  goto L40260

L40710:        close ws
               call "SCREEN" addr ("C", 3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests Data for the Items On Page 1.                       *~
            * Right, wrong, or indifferent, I left the logic here alone.*~
            *************************************************************

            deffn'151(fieldnr%)
                errormsg$ = " "
                on fieldnr%   gosub L50100,         /* Departments      */~
                                    L50300,         /* Employees        */~
                                    L50500,         /* Pay Frequencies  */~
                                    L50700          /* Effective Date   */
                return

L50100
*        Test Data for DEPARTMENT CODES
            if dept$(1) <> " " then L50210
                errormsg$ = "First Department Code Cannot Be Blank"
                return
L50210:     if dept$(1)  = "ALL" then dept$(2) = " "
            if dept$(2)  = " "   then return
                if dept$(2) >= dept$(1) then return
                     errormsg$ = "Illegal Department Code Range"
                     return

L50300
*        Test Data for EMPLOYEE CODES
            if empcode$(1) <> " " then L50410
                errormsg$ = "First Employee Code Cannot Be Blank"
                return
L50410:     if empcode$(1) = "ALL" then empcode$(2) = " "
            if empcode$(2) = " "   then return
                if empcode$(2) >= empcode$(1) then return
                     errormsg$ = "Illegal Employee Code Range"
                     return

L50500
*        Test Data for PAY FREQUENCIES
            if payfreq$(1) <> " " then L50540
                errormsg$ = "First Pay Frequency Cannot Be Blank"
                return
L50540:     if payfreq$(1) >= "1" and payfreq$(1) <= "7" then L50610
                errormsg$ = "Illegal Entry For First Pay Frequency"
                return

L50610:     if payfreq$(2)  = " " then return
            if payfreq$(2) >= "1" and payfreq$(2) <= "7" then L50640
                errormsg$ = "Illegal Entry For Last Pay Frequency"
                return
L50640:     if payfreq$(2) >= payfreq$(1) then return
                errormsg$ = "FROM must be less than or equal to TO"
                return

L50700
*        Test data for EFFECTIVITY DATE
            call "DATEOK" (eff_date$, u3%, errormsg$)
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
        exit_program
            call "SHOSTAT" ("ONE MOMENT PLEASE")
            end(0%)
