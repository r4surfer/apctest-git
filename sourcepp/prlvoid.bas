        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      V   V   OOO   IIIII  DDDD           *~
            *  P   P  R   R  L      V   V  O   O    I    D   D          *~
            *  PPPP   RRRR   L      V   V  O   O    I    D   D          *~
            *  P      R R    L       V V   O   O    I    D   D          *~
            *  P      R   R  LLLLL    V     OOO   IIIII  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLVOID - INPUT VOID PAYROLL CHECKS                       *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+---------------WHAT-----------------------+-WHO-*~
            * 03/06/84 ! ORIGINAL                                 ! HES *~
            * 10/13/86 ! Allowing Voiding Direct Deposit Slips    ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            blankline$79,                /* BLANK LINE FOR INPUT SCREEN*/~
            checkdate$8,                 /* CHECK DATE INFORMATION     */~
            checknr$8,                   /* CHECK NUMBER               */~
            date$8,                      /* DATE FOR SCREEN HEADER     */~
            diskkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            lastcheck$8,                 /* LAST CHECK NUMBER WRITTEN  */~
            linefac$(20)1,               /* FIELD ATTRIBUTE CHARACTERS */~
            message$(2)79,               /*                            */~
            reconciled$1,                /* CHECK RECONCILIATION FLAG  */~
            origuserid$3,                /* USERID OF ORIGINAL USER    */~
            seqnr$4,                     /* SEQUENCE NUMBER OF CHECK   */~
            userid$3                     /* USERID OF CURRENT USER     */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 7 ! PRLCHK   ! PAYROLL CHECKS HEADER FILE               *~
            * # 8 ! PRLCHK2  ! PAYROLL CHECKS DETAIL FILE               *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select # 7, "PRLCHK",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 23,                       ~
                        alt key  1, keypos =  13, keylen =  11,          ~
                            key  2, keypos =  42, keylen =   9, dup

            select # 8, "PRLCHK2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 25


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#8, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SET DATES, TRANSLATION TABLES, ETC.                       *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#1, userid$, f1%(1))
                 if f1%(1) = 0 then L65000
                    get #1, using L09170 , date$
L09170:             FMT XX(9), CH(6)
                    call "DATEFMT" (date$)

        REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            *                                                           *~
            * INPUTS HEADER, LOADS OLD CHECKS IF ON FILE, ETC...        *~
            *************************************************************

        inputmode
            init(" ") checknr$, errormsg$, errormsg$, infomsg$, lastpayee$
            origuserid$ = userid$ : checkdate$ = date$

            for fieldnr% = 1 to 1
                gosub'160(fieldnr%)
                      if enabled% =  0 then L10220
L10160:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10160
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10160
L10220:         next fieldnr%

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            *                                                           *~
            * EDITS HEADER INFORMATION.                                 *~
            *************************************************************

        editmode
            init(" ") errormsg$, infomsg$

            gosub'202(0%)                /* SHOW WITH NON-MODIFIABLE   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 12 then       datasave
            goto editmode

        REM *************************************************************~
            *                   S A V E   D A T A                       *~
            *                                                           *~
            * SAVES THE VOID CHECK NUMBER IN THE CHECK FILE.            *~
            *************************************************************

        datasave
            REM NOW DELETE OLD INVOICE FROM BUFFER AND WRITE NEW ONE.
                diskkey$ = "*** VOID ***"
                str(diskkey$, 13) = checknr$
                call "PLOWNXT1" (#7, diskkey$, 20%, f1%(7))
                     if f1%(7) = 0 then L19440
                delete #7
                diskkey$ = key(#7)
                call "DELETE" (#8, diskkey$, 23%)
L19440:         if keyhit% <> 12 then gosub L31000
                lastcheck$ = checknr$
                goto inputmode

        REM *************************************************************~
            * S E T   D E F A U L T S   F O R   L I N E A R   I N P U T *~
            *                                                           *~
            * SETS DEFAULTS FOR LINEAR INPUT INFORMATION.               *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20200          /* CHECK NUMBER     */
                  return
L20200:     REM SET DEFAULTS FOR CHECK NUMBER
                infomsg$ = "Enter the desired check number"
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

L29942:        REM START OVER (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY  1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918

L30000: REM *************************************************************~
            *      L O A D   O L D   C H E C K   F R O M   F I L E      *~
            *                                                           *~
            * FIND CHECK NUMBER IN DISBURSEMENTS FILE. IF NOT THERE,    *~
            * THEN RETURN FOR INPUT.                                    *~
            *************************************************************

            REM INITIALIZE FOR LOADING UP A CHECK.
                oldcheckonfile% = 0
                reconciled$ = " "
                diskkey$ = checknr$

            REM TRY FOR CHECK IN MAIN DISBURSEMENTS FILE.
                call "PLOWALTS" (#7, diskkey$, 1%, 8%, f1%(7))
                     if f1%(7) = 0 then return     /* IF NO THEN INPUT */
                get #7, using L30135, lastpayee$, checkdate$
L30135:                 FMT CH(12), XX(8), XX(3), CH(6)

            REM GET AND FORMAT HEADER INFORMATION.
                oldcheckonfile% = 1
                call "DATEFMT" (checkdate$)

            REM CHECK TO SEE IF MAIN DISBURSEMENTS CHECK HAS BEEN PAID.
                get #7, using L30340, reconciled$
L30340:                 FMT XX(66), CH(1)
                return

L31000: REM *************************************************************~
            * W R I T E   E A R N I N G   R E G I S T E R   R E C O R D *~
            *                                                           *~
            *************************************************************

            seqnr$ = "1000"
            diskkey$ = checknr$
            call "PLOWALTS" (#7, diskkey$, 1%, 8%, f1%(7))
            if f1%(7) <> 0 then get #7, using L31080, seqnr$
L31080:     FMT XX(12), XX(8), CH(3)

            convert seqnr$ to seq%
            convert seq% - 1 to seqnr$, pic(###)
            write #7, using L31180, "*** VOID ***",                       ~
                                 checknr$, seqnr$, date, " ", " ",       ~
                                 " ", 0, 0, "N", " ", " ", " ", " "
            return

L31180:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(6),                   /* DATE OF CHECK              */~
                CH(6),                   /* FIRST DATE THIS PAY PERIOD */~
                CH(6),                   /* LAST DATE THIS PAY PERIOD  */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                PD(14,4),                /* GROSS PAY AMOUNT           */~
                PD(14,4),                /* NET PAY AMOUNT             */~
                CH(1),                   /* RECONCILIATION FLAG        */~
                CH(6),                   /* RECONCILIATION DATE        */~
                CH(6),                   /* POSTING DATE               */~
                CH(4),                   /* EMPLOYEE DEPARTMENT        */~
                CH(37)                   /* EARN. REGISTER FREE SPACE  */~

        REM *************************************************************~
            *     I N P U T   C H E C K   H E A D E R   S C R E E N     *~
            *                                                           *~
            * GETS CHECK HEADERS FROM THE SCREEN.  STANDARD INPUT STYLE *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) linefac$()
                  on fieldnr% gosub  L40200         /* CHECK NUMBER     */
                  gosub L40270
                  return

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linefac$(fieldnr%) = hex(80)
                      return
L40200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linefac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linefac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "VOID PAY ROLL CHECKS: ENTER THE CHECK NUMBER",        ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,59),                                               ~
                  "LAST CHECK:",                                         ~
               at (02,72), fac(hex(8c)), lastcheck$             , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "CHECK NUMBER",                                        ~
               at (06,30), fac(linefac$(1)), checknr$           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(ac)), infomsg$               , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,73),                                               ~
                  "(16)EXIT",                                            ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40740
                  call "MANUAL" ("PRLVOID ")
                  goto L40270

L40740:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *            E D I T   H E A D E R   S C R E E N            *~
            *                                                           *~
            * EDITS HEADER INFORMATION.  JUST LIKE LINE ITEMS, EXCEPT   *~
            * FOR THE INSTRUCTIONS AND PFKEY DEFINITIONS.               *~
            *************************************************************

            deffn'202(fieldnr%)
                  infomsg$ = " "
                  if oldcheckonfile% = 0 then L41080
                  if str(message$(1),,1) = hex(94) then L41080
                  infomsg$ = "(12)UN-VOID THIS CHECK NUMBER"
L41080:           init(hex(84)) linefac$()
                  on fieldnr% gosub  L41200         /* CHECK NUMBER     */
                  goto L41270

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linefac$(fieldnr%) = hex(80)
                      return
L41200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linefac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linefac$(fieldnr%) = hex(82)
                      return

L41270:     accept                                                       ~
               at (01,02),                                               ~
                  "VOID PAY ROLL CHECKS",                                ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,59),                                               ~
                  "LAST CHECK:",                                         ~
               at (02,72), fac(hex(8c)), lastcheck$             , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "CHECK NUMBER",                                        ~
               at (06,30), fac(linefac$(1)), checknr$           , ch(08),~
               at (07,02),                                               ~
                  "DATE VOIDED",                                         ~
               at (07,30), fac(hex(84)), checkdate$             , ch(08),~
               at (08,02),                                               ~
                  "ORIGINALY VOIDED BY USER:",                           ~
               at (08,30), fac(hex(84)), origuserid$            , ch(03),~
               at (12,05), fac(hex(84)), message$(1)            , ch(79),~
               at (13,05), fac(hex(84)), message$(2)            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),     blankline$         , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,02), fac(hex(84)), infomsg$               , ch(30),~
               at (24,67),                                               ~
                  "(16)VOID CHECK",                                      ~
                                                                         ~
               keys(hex(0001020c0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41760
                  call "MANUAL" ("PRLVOID ")
                  goto L41270

L41760:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            *                                                           *~
            * MAKES SURE THE VENDOR IS ON FILE, THAT THE CHECK ISN'T,   *~
            * (LOADS AND DROPS INTO EDIT MODE IF IT IS), AND CHECKS     *~
            * OUT THE OTHER NUMBERS TO MAKE SURE THEY'RE OK.            *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50130          /* CHECK NUMBER     */
                  return

L50130:     REM TEST FOR CHECK ON FILE, LOAD UP IF YES.
                blankline$ = "To void this check number, press PF 16."
                message$() = " "
                if checknr$ <> " " then L50190
                   errormsg$ = "BLANK CHECK NUMBER NOT ALLOWED."
                   return
L50190: REM FORMAT CHECK NUMBER
                if str(checknr$,,1) <> "D" then L50240
                   convert str(checknr$,2) to checknr%, data goto L50440
                   convert checknr% to str(checknr$,2), pic (0000000)
                   goto L50260
L50240:         call "NUMTEST" (checknr$,1,99999999,errormsg$,-0.001,0)
                     if errormsg$ <> " " then return
L50260:         gosub L30000              /* SEARCH FOR CHECK ON FILE.  */
                if reconciled$ <> "Y" then L50310   /* CHECK RECONCILED?*/
                   errormsg$ = "THIS CHECK HAS BEEN RECONCILED...NO EDIT ~
        ~ALLOWED!"
                   return
L50310:         if oldcheckonfile% = 0 then return
                   message$(1) = "This check number has allready been voi~
        ~ded."
                   message$(2) = "If you want to 'unvoid' it, press PF 12"
                   if lastpayee$ = "*** VOID ***" then L50400
                   message$(1) = hex(94) & "WARNING:" & hex(84) & "This c~
        ~heck was written to Employee Number : " & lastpayee$ & "."
                   message$(2) = "If voided, the original check will be s~
        ~aved, and is veiwable when reconciling"
L50400:            blankline$ = " "
                   return clear
                   return clear
                   goto editmode
L50440:         errormsg$="Invalid Deposit Slip Number " & checknr$
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
