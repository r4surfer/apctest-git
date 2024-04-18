        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD    AAA   TTTTT  EEEEE   SSS    *~
            *  P   P  R   R  L      D   D  A   A    T    E      S       *~
            *  PPPP   RRRR   L      D   D  AAAAA    T    EEEE    SSS    *~
            *  P      R   R  L      D   D  A   A    T    E          S   *~
            *  P      R   R  LLLLL  DDDD   A   A    T    EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDATES - INPUT/EDITS THE PAYROLL PERIOD PERIOD DATES FOR*~
            *            THE SEVEN PAY FREQUENCY TYPES.                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/19/81 ! ORIGINAL                                 ! TEM *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 08/26/96 ! Changes for the year 2000.               ! DXL *~
            * 10/09/97 ! Changed SHOWMSG to SHOSTAT (2 calls)     ! MLJ *~
            *************************************************************

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION            */~
            date$8,                      /* SCREEN FORMATTED DATE      */~
            dateone$10,                  /* FIRST DATE PAY PERIOD      */~
            datetwo$10,                  /* LAST DATE PAY PERIOD       */~
            edtmessage$79,               /* EDIT MESSAGE TEXT          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            first$8,                     /* FIRST DATE LAST PAY PERIOD */~
            i$(24)80,                    /* SCREEN VARIABLE            */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            last$8,                      /* LAST DATE LAST PAY PERIOD  */~
            lfac$(20)1,                  /* FAC'S FOR LINEAR INPUT     */~
            paydescr$(7)20,              /* PAY FREQUENCY DESCRIPTIONS */~
            payfreq$1,                   /* PAY FREQUENCY              */~
            payfreqdescr$30,             /* PAY FREQUENCY DESCRIPTION  */~
            record$(10)50                /* WHOLE 8063 RECORD          */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE (PAY PERIOD DATES)   *~
            *************************************************************

            select #1, "SYSFILE2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1, keylen = 20

*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening files, one moment please")
            call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))

            REM IF FILE NOT THERE CREATE IT
            if f2%(1) = 0 then L09000
               call "OPENFILE" (#1, "OUTPT", f2%(1), rslt$(1), axd$(1))
               close #1
               call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            REM PAY FREQUENCY DESCRIPTION
                paydescr$(1) = "Weekly              "
                paydescr$(2) = "Biweekly            "
                paydescr$(3) = "Semimonthly         "
                paydescr$(4) = "Monthly             "
                paydescr$(5) = "Quarterly           "
                paydescr$(6) = "Semi-annually       "
                paydescr$(7) = "Annually            "

            REM FIND RECORD IF ON FILE
                call "READ100" (#1, "PAY PERIODS", f1%(1))
                     if f1%(1) = 0 then L10000
                     get #1, using L09250, str(record$(), 1)
L09250:                      FMT XX(20), CH(480)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, payfreq$, payfreqdescr$,   ~
                      dateone$, datetwo$, first$, last$

            for fieldnr% = 1 to  3
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

L11060:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then L11060

L11130:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "DATUFMTC" (dateone$)
            call "DATUFMTC" (datetwo$)
            convert payfreq$ to temp%
            str(record$(), 1 + (temp%-1)*13, 1) = payfreq$
            str(record$(), 2 + (temp%-1)*13, 6) = dateone$
            str(record$(), 8 + (temp%-1)*13, 6) = datetwo$
            call "DELETE" (#1, "PAY PERIODS", 20%)
            write #1, using L19160, "PAY PERIODS         ",               ~
                                    str(record$(), 1)
L19160:               FMT CH(20), CH(480)

            go to inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* PAY FREQUENCY    */~
                                    L20200,         /* FIRST DATE       */~
                                    L20300          /* LAST DATE        */
                     return
L20100:     REM DEFAULT/ENABLE FOR PAY FREQUENCY
                enabled% = 1
                inpmessage$ = "1=Weekly 2=Biweekly 3=Semimonthly 4=Monthl~
        ~y 5=Quarterly 6=Semiannual 7=Annual"
                return
L20200:     REM DEFAULT/ENABLE FOR FIRST DATE PAY PERIOD
                enabled% = 1
                inpmessage$ = "Last Pay Period For This Pay Frequency was~
        ~ " & first$ & " to " & last$
                dateone$ = last$
                call "DATUNFMT" (dateone$)
                call "DATFMTC" (dateone$)
                return
L20300:     REM DEFAULT/ENABLE FOR LAST DATE PAY PERIOD
                enabled% = 1
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29918:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29942, L29948, L29952
               return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40160,         /* PAY FREQUENCY    */~
                                    L40160,         /* FIRST DATE       */~
                                    L40160          /* LAST DATE        */
                     goto L40230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "MAINTAIN PAYROLL PERIOD DATES",                       ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PAY FREQUENCY",                                       ~
               at (06,30), fac(lfac$( 1)), payfreq$             , ch(01),~
               at (06,49), fac(hex(8c)),   payfreqdescr$        , ch(32),~
               at (07,02),                                               ~
                  "FIRST DATE PAY PERIOD",                               ~
               at (07,30), fac(lfac$( 2)), dateone$             , ch(10),~
               at (08,02),                                               ~
                  "LAST DATE PAY PERIOD",                                ~
               at (08,30), fac(lfac$( 3)), datetwo$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40560
                  call "MANUAL" ("PRLDATES")
                  goto L40230

L40560:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40230

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41160,         /* PAY FREQUENCY    */~
                                    L41160,         /* FIRST DATE       */~
                                    L41160          /* LAST DATE        */
                     goto L41230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41230:     accept                                                       ~
               at (01,02),                                               ~
                  "MAINTAIN PAYROLL PERIOD DATES",                       ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PAY FREQUENCY",                                       ~
               at (06,30), fac(lfac$( 1)), payfreq$             , ch(01),~
               at (06,49), fac(hex(8c)),   payfreqdescr$        , ch(32),~
               at (07,02),                                               ~
                  "FIRST DATE PAY PERIOD",                               ~
               at (07,30), fac(lfac$( 2)), dateone$             , ch(10),~
               at (08,02),                                               ~
                  "LAST DATE PAY PERIOD",                                ~
               at (08,30), fac(lfac$( 3)), datetwo$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41560
                  call "MANUAL" ("PRLDATES")
                  goto L41230

L41560:        if keyhit% <> 15 then L41600
                  call "PRNTSCRN"
                  goto L41230

L41600:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* PAY FREQUENCY    */~
                                    L50200,         /* FIRST DATE       */~
                                    L50300          /* LAST DATE        */
                     return
L50100:     REM TEST DATA FOR PAY FREQUENCY
                if payfreq$ >= "1" and  payfreq$ <= "7" then L50140
                   errormsg$ = "Invalid Entry For Pay Frequency: " &     ~
                                   payfreq$
                   return
L50140:         convert payfreq$ to temp%
                payfreqdescr$ = paydescr$(temp%)
                first$ = str(record$(), 2 + (temp%-1)*13, 6)
                last$  = str(record$(), 8 + (temp%-1)*13, 6)
                if first$ <> " " then call "DATEFMT" (first$)
                if last$  <> " " then call "DATEFMT" (last$ )
                return
L50200:     REM TEST DATA FOR FIRST DATE PAY PERIOD
                call "DATEOKC" (dateone$, u3%, errormsg$)
                return
L50300:     REM TEST DATA FOR LAST DATE PAY PERIOD
                call "DATEOKC" (datetwo$, u3%, errormsg$)
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
