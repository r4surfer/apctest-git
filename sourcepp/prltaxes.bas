        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   RRRR   L      TTTTT   AAA   X   X  EEEEE   SSS    *~
            *  P   P  R   R  L        T    A   A   X X   E      S       *~
            *  PPPP   RRRR   L        T    AAAAA    X    EEEE    SSS    *~
            *  P      R   R  L        T    A   A   X X   E          S   *~
            *  P      R   R  LLLLL    T    A   A  X   X  EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLTAXES - ALLOWS USER TO ADJUST THE TAX CONSTANTS (RATES)*~
            *            THAT ARE STORED IN THE PRLDDT RECORD.          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/21/83 ! ORIGINAL                                 ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            amount(6),                   /* AMOUNTS FOR DISK I/O       */~
            amount$(6)12,                /* AMOUNTS FOR 4 AMOUNT FIELDS*/~
            title$79,                    /* KEYS ACTIVE                */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* SCREEN DATE FOR DISPLAY    */~
            deduc$(80)6,                 /* METHOD OF DEDUCTION        */~
            descr$(80)25,                /* DESCRIPTION OF DEDUCTION   */~
            cdescr$(6)25,                /* DESCRIPTION OF DEDUCTION   */~
            descr$30,                    /* DEDUCTION DESCRIPTION      */~
            edtmessage$79,               /* EDIT MESSAGE FIELD         */~
            empflag$40,                  /* EMPLOYER/EMPLOYEE FLAG     */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(80)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            flag$(80)1,                  /* INPUT VARIABLE FOR SCREEN 1*/~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INPUT MESSAGE TEXT         */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            method$6,                    /* METHOD OF DEDUCTION CODE   */~
            record$(6)50,                /* FREE SPACE IN DDT RECORD.  */~
            table$1,                     /* DOES THIS DED USE A TAX TBL*/~
            tablemsg$79                  /* MESSAGE FIELD              */~

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
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
            *************************************************************

            select # 1, "PRLDDT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1, keylen = 6


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("LINKING TO DATA BASE TO MANAGE PAYROLL DEDUC~
        ~TION CONSTANTS")

            call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))
            if f2%(1) <> 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZE SCREEN INFORMATION.                            *~
            * ALSO, THIS ROUTINE LOADS PAYROLL SUBROUTINE INFO AND MAKES*~
            * SURE IT'S STILL VALID.  IF NO SUBROUTINE INFO, THEN WE GO *~
            * INTO INPUT MODE.  IF SUBROUTINE INFO, BUT NOT VALID, THEN *~
            * GO INTO EDIT SUBROUTINE INFORMATION MODE.                 *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            gosub fill_array
            title$ = "(4)PREV (5)NEXT  (13)INSTR.  (15)PRNT SCRN (16)EXIT"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM FOR DEDUCTION DEFINITIONS.        *~
            *************************************************************

        inputmode
            init(" ")inpmessage$, errormsg$, empflag$, table$, record$(),~
                     method$, descr$, cdescr$(), amount$(), flag$()
            line% = 0

L10160:         gosub'201(0%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then line% = 0
                      if keyhit%  =  5 and maxlines% > 40 then line% = 40
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10160
                gosub'151(1%)
                      if errormsg$ <> " " then L10160

            for fieldnr% = 1 to 6
                gosub'162(fieldnr%)
                      if enabled% =  0 then L10330
L10280:         gosub'202(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L10280
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10280
L10330:         next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * EDIT MODE MAIN DRIVER ROUTINE FOR DEFINITIONS.            *~
            *************************************************************

        edtpg2
L11230:     gosub'212(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then goto  datasave
                  if keyhit% <>  0 then       L11230
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% > 6 then edtpg2
            if cdescr$(fieldnr%) = " " then edtpg2

L11430:     gosub'212(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11430
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11430
            goto edtpg2

        REM *************************************************************~
            *                    W R I T E   D A T A                    *~
            *                                                           *~
            * WRITES DATA TO THE DEFINITION FILE, REPLACING THAT ALREADY*~
            * OUT THERE.                                                *~
            *************************************************************

        datasave
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   S E C O N D   P A G E  *~
            *                                                           *~
            * DEFAULT/ENABLE FOR THE SECOND PAGE.  THIS ROUTINE HELPS   *~
            * OUT BY WHIZZING BY WHEN THE DESCRIPTION IS BLANKED BEFORE *~
            * THIS ONE SO WE DON'T INPUT 4 AMOUNTS WHEN WE ONLY WANT 2  *~
            *************************************************************

            deffn'162(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20170,         /* CAMOUNT 1        */~
                                    L20170,         /* CAMOUNT 2        */~
                                    L20170,         /* CAMOUNT 3        */~
                                    L20170,         /* CAMOUNT 4        */~
                                    L20170,         /* CAMOUNT 5        */~
                                    L20170          /* CAMOUNT 6        */
                    return
L20170:     REM DEFAULT/ENABLE FOR AMOUNT
                   if cdescr$(fieldnr%) <> " " then enabled% = 1
                   return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29090:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29210, L29240, L29260
               return

L29210:        REM START OVER (ENTER)
                   return clear
                   goto inputmode
L29240:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29260:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29090

L30000: REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *                                                           *~
            * LOADS THE DATA FROM THE FILE.  DESCRIBES ACCOUNTS, ETC.   *~
            *************************************************************

            call "READ101" (#1, method$, f1%(1))
                 if f1%(1) = 0 then return

            get #1, using L30240, record$(1), record$(2), record$(3),     ~
                                 record$(4), cdescr$(), amount(),        ~
                                 descr$, table$, record$(6)

            for temp% = 1 to 6
             if cdescr$(temp%) <> " " or amount(temp%) <> 0 then         ~
                convert amount(temp%) to amount$(temp%), pic(#######.####)
            next temp%

           if str(record$(1),19,1) = "Y" then empflag$ = "This is paid by~
        ~ the employee" else empflag$ = "This is NOT paid by the employee"

           tablemsg$ = " "
           if table$ = "Y" then tablemsg$ = "This Deductions also has tax~
        ~ tables (They are managed through another program)"
            return

L30240:     FMT CH(50),                  /* FIRST                      */~
                CH(50),                  /* PART OF                    */~
                CH(50),                  /* OF THE                     */~
                CH(2),                   /* RECORD                     */~
                6*CH(25),                /* CONSTANT DESCRIPTIONS      */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                CH(30),                  /* extended description       */~
                CH(1),                   /* TAX TABLE REQUIRED?        */~
                CH(19)                   /* FREE SPACE IN RECORD       */~

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITES DATA TO FILE, WRITING OR REWRITING AS NEEDED.      *~
            *************************************************************

            mat amount = zer
            for temp% = 1 to 6
                if amount$(temp%) <> " " then convert amount$(temp%) to  ~
                                                   amount(temp%)
                next temp%

          rewrite #1, using L30240, record$(1), record$(2), record$(3),   ~
                                 record$(4), cdescr$(), amount(),        ~
                                 descr$, table$, record$(6)

            return

        REM *************************************************************~
            *    L O A D   D A T A   F O R   F I R S T   S C R E E N    *~
            *                                                           *~
            * LOADS ALL DEDUCTION FROM THE DDT FILE THAT HAVE AT LEAST  *~
            * ONE NON-BLANK CONSTANTS DESCRIPTION FIELD.                *~
            *************************************************************

        fill_array
            readkey$ = " "
            maxlines% = 0

L32110:     call "PLOWNEXT" (#1, readkey$, 0%, f1%(1))
                if f1%(1) = 0 then L32210

            get #1, using L32260, method$, cdescr$(), descr$
            if cdescr$() = " " then L32110
               maxlines% = maxlines% + 1
               deduc$(maxlines%) = method$
               descr$(maxlines%) = descr$
               goto L32110

L32210:     if maxlines% = 0 then L65000
        return

L32260:     FMT CH(06),                  /* METHOD                     */~
                XX(96),                  /*                            */~
                XX(50),                  /*                            */~
                6*CH(25),                /* CONSTANT DESCRIPTIONS      */~
                XX(48),                  /* CONSTANT AMOUNTS           */~
                CH(30)                   /* extended description       */~
            /*  CH(1)                    /* TAX TABLE REQUIRED?        */~

        REM *************************************************************~
            *           T A B U L A R   M O D E   S C R E E N           *~
            *                                                           *~
            * HANDLES THE INPUT OF FIELDS IN TABLE, IN EITHER INPUT,    *~
            * EDIT, INSERT, OR DELETE STYLE.                            *~
            *************************************************************

            deffn'201(fieldnr%)                    /* EDIT MODE        */

            init (hex(8c)) fac$()
            init (hex(81)) str(fac$(),,maxlines%)

            infomsg$ = "Place a non-blank character in the box to edit th~
        ~at deduction's constants."

L40060:     accept                                                       ~
               at (01,02), "MANAGE DEDUCTION CONSTANTS (RATES, LIMITS, ET~
        ~C.)",                                                            ~
               at (02,02), fac(hex(8c)),     title$             , ch(60),~
               at (03,02), fac(hex(94)),     errormsg$          , ch(60),~
               at (04,02), fac(hex(a4)),     infomsg$           , ch(79),~
                                                                         ~
               at (01,64), "! Today is " ,                               ~
               at (02,64), "!"                                          ,~
               at (03,64), "+---------------"                           ,~
               at (02,66), fac(hex(8c)), date$,                   ch(08),~
                                                                         ~
               at (05,02),  fac(hex(84)),  deduc$(line%+ 1),      ch(06),~
               at (06,02),  fac(hex(84)),  deduc$(line%+ 3),      ch(06),~
               at (07,02),  fac(hex(84)),  deduc$(line%+ 5),      ch(06),~
               at (08,02),  fac(hex(84)),  deduc$(line%+ 7),      ch(06),~
               at (09,02),  fac(hex(84)),  deduc$(line%+ 9),      ch(06),~
               at (10,02),  fac(hex(84)),  deduc$(line%+11),      ch(06),~
               at (11,02),  fac(hex(84)),  deduc$(line%+13),      ch(06),~
               at (12,02),  fac(hex(84)),  deduc$(line%+15),      ch(06),~
               at (13,02),  fac(hex(84)),  deduc$(line%+17),      ch(06),~
               at (14,02),  fac(hex(84)),  deduc$(line%+19),      ch(06),~
               at (15,02),  fac(hex(84)),  deduc$(line%+21),      ch(06),~
               at (16,02),  fac(hex(84)),  deduc$(line%+23),      ch(06),~
               at (17,02),  fac(hex(84)),  deduc$(line%+25),      ch(06),~
               at (18,02),  fac(hex(84)),  deduc$(line%+27),      ch(06),~
               at (19,02),  fac(hex(84)),  deduc$(line%+29),      ch(06),~
               at (20,02),  fac(hex(84)),  deduc$(line%+31),      ch(06),~
               at (21,02),  fac(hex(84)),  deduc$(line%+33),      ch(06),~
               at (22,02),  fac(hex(84)),  deduc$(line%+35),      ch(06),~
               at (23,02),  fac(hex(84)),  deduc$(line%+37),      ch(06),~
               at (24,02),  fac(hex(84)),  deduc$(line%+39),      ch(06),~
                                                                         ~
               at (05,42),  fac(hex(84)),  deduc$(line%+ 2),      ch(06),~
               at (06,42),  fac(hex(84)),  deduc$(line%+ 4),      ch(06),~
               at (07,42),  fac(hex(84)),  deduc$(line%+ 6),      ch(06),~
               at (08,42),  fac(hex(84)),  deduc$(line%+ 8),      ch(06),~
               at (09,42),  fac(hex(84)),  deduc$(line%+10),      ch(06),~
               at (10,42),  fac(hex(84)),  deduc$(line%+12),      ch(06),~
               at (11,42),  fac(hex(84)),  deduc$(line%+14),      ch(06),~
               at (12,42),  fac(hex(84)),  deduc$(line%+16),      ch(06),~
               at (13,42),  fac(hex(84)),  deduc$(line%+18),      ch(06),~
               at (14,42),  fac(hex(84)),  deduc$(line%+20),      ch(06),~
               at (15,42),  fac(hex(84)),  deduc$(line%+22),      ch(06),~
               at (16,42),  fac(hex(84)),  deduc$(line%+24),      ch(06),~
               at (17,42),  fac(hex(84)),  deduc$(line%+26),      ch(06),~
               at (18,42),  fac(hex(84)),  deduc$(line%+28),      ch(06),~
               at (19,42),  fac(hex(84)),  deduc$(line%+30),      ch(06),~
               at (20,42),  fac(hex(84)),  deduc$(line%+32),      ch(06),~
               at (21,42),  fac(hex(84)),  deduc$(line%+34),      ch(06),~
               at (22,42),  fac(hex(84)),  deduc$(line%+36),      ch(06),~
               at (23,42),  fac(hex(84)),  deduc$(line%+38),      ch(06),~
               at (24,42),  fac(hex(84)),  deduc$(line%+40),      ch(06),~
                                                                         ~
               at (05,09),  fac(hex(8c)),  descr$(line%+ 1),      ch(25),~
               at (06,09),  fac(hex(8c)),  descr$(line%+ 3),      ch(25),~
               at (07,09),  fac(hex(8c)),  descr$(line%+ 5),      ch(25),~
               at (08,09),  fac(hex(8c)),  descr$(line%+ 7),      ch(25),~
               at (09,09),  fac(hex(8c)),  descr$(line%+ 9),      ch(25),~
               at (10,09),  fac(hex(8c)),  descr$(line%+11),      ch(25),~
               at (11,09),  fac(hex(8c)),  descr$(line%+13),      ch(25),~
               at (12,09),  fac(hex(8c)),  descr$(line%+15),      ch(25),~
               at (13,09),  fac(hex(8c)),  descr$(line%+17),      ch(25),~
               at (14,09),  fac(hex(8c)),  descr$(line%+19),      ch(25),~
               at (15,09),  fac(hex(8c)),  descr$(line%+21),      ch(25),~
               at (16,09),  fac(hex(8c)),  descr$(line%+23),      ch(25),~
               at (17,09),  fac(hex(8c)),  descr$(line%+25),      ch(25),~
               at (18,09),  fac(hex(8c)),  descr$(line%+27),      ch(25),~
               at (19,09),  fac(hex(8c)),  descr$(line%+29),      ch(25),~
               at (20,09),  fac(hex(8c)),  descr$(line%+31),      ch(25),~
               at (21,09),  fac(hex(8c)),  descr$(line%+33),      ch(25),~
               at (22,09),  fac(hex(8c)),  descr$(line%+35),      ch(25),~
               at (23,09),  fac(hex(8c)),  descr$(line%+37),      ch(25),~
               at (24,09),  fac(hex(8c)),  descr$(line%+39),      ch(25),~
                                                                         ~
               at (05,49),  fac(hex(8c)),  descr$(line%+ 2),      ch(25),~
               at (06,49),  fac(hex(8c)),  descr$(line%+ 4),      ch(25),~
               at (07,49),  fac(hex(8c)),  descr$(line%+ 6),      ch(25),~
               at (08,49),  fac(hex(8c)),  descr$(line%+ 8),      ch(25),~
               at (09,49),  fac(hex(8c)),  descr$(line%+10),      ch(25),~
               at (10,49),  fac(hex(8c)),  descr$(line%+12),      ch(25),~
               at (11,49),  fac(hex(8c)),  descr$(line%+14),      ch(25),~
               at (12,49),  fac(hex(8c)),  descr$(line%+16),      ch(25),~
               at (13,49),  fac(hex(8c)),  descr$(line%+18),      ch(25),~
               at (14,49),  fac(hex(8c)),  descr$(line%+20),      ch(25),~
               at (15,49),  fac(hex(8c)),  descr$(line%+22),      ch(25),~
               at (16,49),  fac(hex(8c)),  descr$(line%+24),      ch(25),~
               at (17,49),  fac(hex(8c)),  descr$(line%+26),      ch(25),~
               at (18,49),  fac(hex(8c)),  descr$(line%+28),      ch(25),~
               at (19,49),  fac(hex(8c)),  descr$(line%+30),      ch(25),~
               at (20,49),  fac(hex(8c)),  descr$(line%+32),      ch(25),~
               at (21,49),  fac(hex(8c)),  descr$(line%+34),      ch(25),~
               at (22,49),  fac(hex(8c)),  descr$(line%+36),      ch(25),~
               at (23,49),  fac(hex(8c)),  descr$(line%+38),      ch(25),~
               at (24,49),  fac(hex(8c)),  descr$(line%+40),      ch(25),~
                                                                         ~
               at (05,35), fac(fac$(line%+ 1)), flag$ (line%+ 1), ch(01),~
               at (06,35), fac(fac$(line%+ 3)), flag$ (line%+ 3), ch(01),~
               at (07,35), fac(fac$(line%+ 5)), flag$ (line%+ 5), ch(01),~
               at (08,35), fac(fac$(line%+ 7)), flag$ (line%+ 7), ch(01),~
               at (09,35), fac(fac$(line%+ 9)), flag$ (line%+ 9), ch(01),~
               at (10,35), fac(fac$(line%+11)), flag$ (line%+11), ch(01),~
               at (11,35), fac(fac$(line%+13)), flag$ (line%+13), ch(01),~
               at (12,35), fac(fac$(line%+15)), flag$ (line%+15), ch(01),~
               at (13,35), fac(fac$(line%+17)), flag$ (line%+17), ch(01),~
               at (14,35), fac(fac$(line%+19)), flag$ (line%+19), ch(01),~
               at (15,35), fac(fac$(line%+21)), flag$ (line%+21), ch(01),~
               at (16,35), fac(fac$(line%+23)), flag$ (line%+23), ch(01),~
               at (17,35), fac(fac$(line%+25)), flag$ (line%+25), ch(01),~
               at (18,35), fac(fac$(line%+27)), flag$ (line%+27), ch(01),~
               at (19,35), fac(fac$(line%+29)), flag$ (line%+29), ch(01),~
               at (20,35), fac(fac$(line%+31)), flag$ (line%+31), ch(01),~
               at (21,35), fac(fac$(line%+33)), flag$ (line%+33), ch(01),~
               at (22,35), fac(fac$(line%+35)), flag$ (line%+35), ch(01),~
               at (23,35), fac(fac$(line%+37)), flag$ (line%+37), ch(01),~
               at (24,35), fac(fac$(line%+39)), flag$ (line%+39), ch(01),~
                                                                         ~
               at (05,75), fac(fac$(line%+ 2)), flag$ (line%+ 2), ch(01),~
               at (06,75), fac(fac$(line%+ 4)), flag$ (line%+ 4), ch(01),~
               at (07,75), fac(fac$(line%+ 6)), flag$ (line%+ 6), ch(01),~
               at (08,75), fac(fac$(line%+ 8)), flag$ (line%+ 8), ch(01),~
               at (09,75), fac(fac$(line%+10)), flag$ (line%+10), ch(01),~
               at (10,75), fac(fac$(line%+12)), flag$ (line%+12), ch(01),~
               at (11,75), fac(fac$(line%+14)), flag$ (line%+14), ch(01),~
               at (12,75), fac(fac$(line%+16)), flag$ (line%+16), ch(01),~
               at (13,75), fac(fac$(line%+18)), flag$ (line%+18), ch(01),~
               at (14,75), fac(fac$(line%+20)), flag$ (line%+20), ch(01),~
               at (15,75), fac(fac$(line%+22)), flag$ (line%+22), ch(01),~
               at (16,75), fac(fac$(line%+24)), flag$ (line%+24), ch(01),~
               at (17,75), fac(fac$(line%+26)), flag$ (line%+26), ch(01),~
               at (18,75), fac(fac$(line%+28)), flag$ (line%+28), ch(01),~
               at (19,75), fac(fac$(line%+30)), flag$ (line%+30), ch(01),~
               at (20,75), fac(fac$(line%+32)), flag$ (line%+32), ch(01),~
               at (21,75), fac(fac$(line%+34)), flag$ (line%+34), ch(01),~
               at (22,75), fac(fac$(line%+36)), flag$ (line%+36), ch(01),~
               at (23,75), fac(fac$(line%+38)), flag$ (line%+38), ch(01),~
               at (24,75), fac(fac$(line%+40)), flag$ (line%+40), ch(01),~
                                                                         ~
               keys(hex(0004050d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40780
                  call "MANUAL" ("PRLTAXES")
                  goto L40060

L40780:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40060

        REM *************************************************************~
            *             I N P U T   S E C O N D   P A G E             *~
            *                                                           *~
            * INPUTS THE SECOND PAGE OF THE HEADER                      *~
            *************************************************************

            deffn'202(fieldnr%)
                  init(hex(84)) fac$()
                  on fieldnr% gosub L41390,         /* CAMOUNT 1        */~
                                    L41390,         /* CAMOUNT 2        */~
                                    L41390,         /* CAMOUNT 3        */~
                                    L41390,         /* CAMOUNT 4        */~
                                    L41390,         /* CAMOUNT 5        */~
                                    L41390          /* CAMOUNT 6        */
                     goto L41430

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L41390:           REM SET FAC'S FOR NUMERIC INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L41430:     accept                                                       ~
               at (01,02),                                               ~
                  "MANAGE PAYROLL TAX CONSTANTS",                        ~
               at (02,02), "THIS METHOD:",                               ~
               at (02,15), fac(hex(84)),  method$               , ch(06),~
               at (02,22), fac(hex(84)),  descr$                , ch(30),~
               at (02,75),                                               ~
                  "PAGE 2",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), empflag$               , ch(40),~
               at (07,02), fac(hex(8c)), cdescr$(1)             , ch(25),~
               at (07,30), fac(fac$(1)), amount$(1)             , ch(12),~
               at (08,02), fac(hex(8c)), cdescr$(2)             , ch(25),~
               at (08,30), fac(fac$(2)), amount$(2)             , ch(12),~
               at (09,02), fac(hex(8c)), cdescr$(3)             , ch(25),~
               at (09,30), fac(fac$(3)), amount$(3)             , ch(12),~
               at (10,02), fac(hex(8c)), cdescr$(4)             , ch(25),~
               at (10,30), fac(fac$(4)), amount$(4)             , ch(12),~
               at (11,02), fac(hex(8c)), cdescr$(5)             , ch(25),~
               at (11,30), fac(fac$(5)), amount$(5)             , ch(12),~
               at (12,02), fac(hex(8c)), cdescr$(6)             , ch(25),~
               at (12,30), fac(fac$(6)), amount$(6)             , ch(12),~
               at (15,02), fac(hex(84)), tablemsg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),  inpmessage$           , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L42100
                  call "MANUAL" ("PRLTAXES")
                  goto L41430

L42100:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41430

        REM *************************************************************~
            *              E D I T   S E C O N D   P A G E              *~
            *                                                           *~
            * EDITS THE SECOND PAGE OF THE DOCUMENT                     *~
            *************************************************************

            deffn'212(fieldnr%)
                  init(hex(84)) fac$()
                  if fieldnr% = 0 then init(hex(86)) fac$()
                  on fieldnr% gosub L43400,         /* CAMOUNT 1        */~
                                    L43400,         /* CAMOUNT 2        */~
                                    L43400,         /* CAMOUNT 3        */~
                                    L43400,         /* CAMOUNT 4        */~
                                    L43400,         /* CAMOUNT 5        */~
                                    L43400          /* CAMOUNT 6        */
                     goto L43440

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L43400:           REM SET FAC'S FOR NUMERIC INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L43440:     accept                                                       ~
               at (01,02),                                               ~
                  "MANAGE PAYROLL TAX CONSTANTS",                        ~
               at (02,02), "THIS METHOD:",                               ~
               at (02,15), fac(hex(84)),  method$               , ch(06),~
               at (02,22), fac(hex(84)),  descr$                , ch(30),~
               at (02,75),                                               ~
                  "PAGE 2",                                              ~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), empflag$               , ch(40),~
               at (07,02), fac(hex(8c)), cdescr$(1)             , ch(25),~
               at (07,30), fac(fac$(1)), amount$(1)             , ch(12),~
               at (08,02), fac(hex(8c)), cdescr$(2)             , ch(25),~
               at (08,30), fac(fac$(2)), amount$(2)             , ch(12),~
               at (09,02), fac(hex(8c)), cdescr$(3)             , ch(25),~
               at (09,30), fac(fac$(3)), amount$(3)             , ch(12),~
               at (10,02), fac(hex(8c)), cdescr$(4)             , ch(25),~
               at (10,30), fac(fac$(4)), amount$(4)             , ch(12),~
               at (11,02), fac(hex(8c)), cdescr$(5)             , ch(25),~
               at (11,30), fac(fac$(5)), amount$(5)             , ch(12),~
               at (12,02), fac(hex(8c)), cdescr$(6)             , ch(25),~
               at (12,30), fac(fac$(6)), amount$(6)             , ch(12),~
               at (15,02), fac(hex(84)), tablemsg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),  edtmessage$           , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(00010f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 15 then L44140
                  call "PRNTSCRN"
                  goto L43440

L44140:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FROM THE FIRST PAGE                            *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* METHOD OF DEDXN  */
                     return
L50100:     REM TEST DATA FOR METHOD OF DEDUCTION
                errormsg$ = hex(00)
                if flag$() = " " then return
                method$ = deduc$(pos(flag$() <> " "))
                if method$ = " " then return
                gosub L30000
                if f1%(1) <> 0 then errormsg$ = " " /* TRICKY, TRICKY */
                return

        REM *************************************************************~
            *          T E S T   S E C O N D   P A G E   D A T A        *~
            *                                                           *~
            * TESTS DATA FROM THE SECOND PAGE.  NOTE THAT WE DO A CHECK *~
            * HERE IN THE DESCRIPTIONS FOR NONBLANKS ENTERED WHERE THERE*~
            * ARE BLANK DESCRIPTION LINES ABOVE.                        *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51650,         /* CAMOUNT 1        */~
                                    L51650,         /* CAMOUNT 2        */~
                                    L51650,         /* CAMOUNT 3        */~
                                    L51650,         /* CAMOUNT 4        */~
                                    L51650,         /* CAMOUNT 5        */~
                                    L51650          /* CAMOUNT 6        */
                    return
L51650:     REM TEST DATA FOR CONSTANT AMOUNTS
                t% = fieldnr%
                if amount$(t%) = " " and cdescr$(t%) = " " then return
                if not(cdescr$(t%) = " " and amount$(t%)<>" ") then L51720
                   errormsg$ = "You Cannot Enter An Amount For A Blank De~
        ~scription!!"
                   return
L51720:         if amount$(t%) = " " then amount$(t%) = "0"
                   convert amount$(t%) to n, data goto L51770
                   convert n to amount$(t%), pic(#######.####)
            if pos(amount$(t%)="#")<>0 then errormsg$="NUMBER IS TOO BIG"
                   return
L51770:               errormsg$ = "Invalid Entry For Amount 1 :"         ~
                                         & amount$(t%)
                      return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("DATA BASE INTEGRITY CHECK IN PROCESS")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
