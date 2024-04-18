        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      PPPP   RRRR   DDDD   IIIII  N   N   *~
            *  P   P  R   R  L      P   P  R   R  D   D    I    NN  N   *~
            *  PPPP   RRRR   L      PPPP   RRRR   D   D    I    N N N   *~
            *  P      R   R  L      P      R   R  D   D    I    N  NN   *~
            *  P      R   R  LLLLL  P      R   R  DDDD   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLPRDIN - SETS THE PAYROLL PERIOD IN SYSFILE2            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/16/81 ! ORIGINAL                                 ! TEM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION            */~
            date$8,                      /* DATE FOR SCREEN            */~
            edtmessage$79,               /* EDIT MESSAGE INSTRUCTIONS  */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            i$(24)80,                    /* SCREEN VARIABLE            */~
            inpmessage$79,               /* INPUT INSTRUCTIONS         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTER  */~
            oldperiod$1,                 /* PAYROLL PERIOD ON FILE     */~
            payprd$1                     /* PAYROLL PERIOD ENTERED     */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other         "
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
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE (PAYROLL PERIOD)     *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOWMSG" ("Opening files, one moment please")

            call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            call "READ100" (#1, "PAYROLL PERIOD", f1%(1))
                 if f1%(1) = 0 then L10000
            get #1, using L09150, oldperiod$
L09150:             FMT XX(20), CH(1)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, payprd$

            for fieldnr% = 1 to  1
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
            if fieldnr% < 1 or fieldnr% >  1 then L11060

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
            call "READ101" (#1, "PAYROLL PERIOD", f1%(1))
            put #1, using L19085, "PAYROLL PERIOD      ", payprd$, " "
L19085:             FMT CH(20), CH(1), CH(479)
            if f1%(1) = 1 then rewrite #1                                ~
                          else write #1
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* PAYROLL PERIOD   */
                     return
L20100:     REM DEFAULT/ENABLE FOR PAYROLL PERIOD
                enabled% = 1
                inpmessage$ = "Valid Entries Are 1, 2, 3, 4, 5, or 6"
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
                  on fieldnr% gosub L40170          /* PAYROLL PERIOD   */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40170:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "SET PAYROLL PERIOD",                                  ~
               at (01,66),                                               ~
                  "LAST PERIOD:",                                        ~
               at (01,79), fac(hex(84)), oldperiod$             , ch(01),~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PAYROLL PERIOD",                                      ~
               at (06,30), fac(lfac$( 1)), payprd$              , ch(01),~
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

            if keyhit% <> 13 then L40500
                call "MANUAL" ("PRLPRDIN")
                goto L40210

L40500:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41170          /* PAYROLL PERIOD   */
                     goto L41210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41170:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "SET PAYROLL PERIOD",                                  ~
               at (01,66),                                               ~
                  "LAST PERIOD: ",                                       ~
               at (01,79), fac(hex(84)), oldperiod$             , ch(01),~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PAYROLL PERIOD",                                      ~
               at (06,30), fac(lfac$( 1)), payprd$              , ch(01),~
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

            if keyhit% <> 13 then L41500
                call "MANUAL" ("PRLPRDIN")
                goto L41210

L41500:        if keyhit% <> 15 then L41540
                  call "PRNTSCRN"
                  goto L41210

L41540:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* PAYROLL PERIOD   */
                     return
L50100:     REM TEST DATA FOR PAYROLL PERIOD
                if payprd$ >= "1" and payprd$ <= "6" then return
                errormsg$ = "Payroll Period Must Be Between 1 and 6: "   ~
                               & payprd$
                return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOWMSG" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
