        REM *************************************************************~
            *                                                           *~
            *  EEEEE  M   M  PPPP   DDDD   L      EEEEE  TTTTT  EEEEE   *~
            *  E      MM MM  P   P  D   D  L      E        T    E       *~
            *  EEEE   M M M  PPPP   D   D  L      EEEE     T    EEEE    *~
            *  E      M   M  P      D   D  L      E        T    E       *~
            *  EEEEE  M   M  P      DDDD   LLLLL  EEEEE    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EMPDLETE - Deletes Employees from File.  Will only do so  *~
            *            for terminated employees whose earnings and    *~
            *            deduction accumulators are all zero.           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/11/81 ! ORIGINAL                                 ! TEM *~
            * 03/12/86 ! Added Call to CDANPOST.                  ! LDJ *~
            * 08/18/86 ! Customized for FAIRCHILD (delete EMPMAST2! LDJ *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            cursor%(2),                  /* CURSOR POSITION            */~
            d(4),                        /* DEDUCTION ACCUMULATORS     */~
            date$8,                      /* SCREEN FORMATTED DATE      */~
            e(10),                       /* EARNINGS ACCUMULATORS      */~
            edtmessage$79,               /* EDIT MESSAGE TEXT          */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            empname$60,                  /* EMPLOYEE NAME              */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            header$79,                   /* Screen Top Line            */~
            i$(24)80,                    /* SCREEN VARIABLE            */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            lfac$(20)1,                  /* LINEAR FAC'S               */~
            name$(3)20,                  /* EMPLOYEE NAMES             */~
            plowkey$50,                  /* PLOW VARIABLE              */~
            showmsg$79,                  /* DISPLAYED MESSAGE TEXT     */~
            ssnum$10,                    /* SOCIAL SECURITY NUMBER     */~
            status$1                     /* EMPLOYEE STATUS            */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTION FILE                  *~
            * # 6 ! EMPEARN1 ! EMPLOYEE EARNINGS FILE                   *~
            * # 7 ! PERMASTR ! EMPLOYEE PERSONNEL FILE                  *~
            * # 8 ! EMPBANKS ! EMPLOYEE DIRECT DEPOSIT INFORMATION      *~
            * # 9 ! EMPACRLS ! employee sick & vacation accrual ledger  *~
            *************************************************************

            select # 4, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select  #5, "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alt key 1, keypos = 16, keylen = 18, dup

            select  #6, "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alt key 1, keypos = 16, keylen = 28

            select #7,  "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #8,  "EMPBANKS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 13

            select #9,  "EMPACRLS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1,    keylen = 13


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

           call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#8, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "If you do not desire to delete this employee, ~
        ~then START OVER (PF Key 1)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, empcode$, empname$,        ~
                      ssnum$

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
            if fieldnr% <> 1 then L11060

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
            showmsg$ = "Deleting Employee"
            str(showmsg$, 18, 1) = hex(84)
            str(showmsg$, 19) = empname$
            call "SHOSTAT" (showmsg$)
            call "DELETE" (#5, empcode$, 12%)
            call "DELETE" (#6, empcode$, 12%)
            call "DELETE" (#8, empcode$, 12%)
            call "DELETE" (#9, empcode$, 12%)
            call "READ101" (#4, empcode$, f1%(4))
            if f1%(4) = 0% then inputmode
            delete #4
            call "CDANPOST" (#4, "D")
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* EMPLOYEE CODE    */~
                                    L20200,         /* EMPLOYEE NAME    */~
                                    L20300          /* SOCIAL SECURITY  */
                     return
L20100:     REM DEFAULT/ENABLE FOR EMPLOYEE CODE
                enabled% = 1
                return
L20200:     REM DEFAULT/ENABLE FOR EMPLOYEE NAME
                return
L20300:     REM DEFAULT/ENABLE FOR SOCIAL SECURITY NUMBER
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            k% = 2%
            call "STARTOVR" (k%)
            if k% = 1% then return
            if k% <> 0% then startover
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *           C H E C K   EARNING/DEDUCTIONS  R E C O R D S   *~
            *                                                           *~
            * THIS CHECKS ALL THE DEDUCTION AND EARNINGS TYPES FOR NON- *~
            * ZERO ENTRIES.                                             *~
            *************************************************************

            plowkey$ = empcode$
L30080:     call "PLOWNEXT" (#5, plowkey$, 12%, f1%(5))
                  if f1%(5) = 0 then L30200
                  get #5, using L30110, d(1), d(2), d(3), d(4)
L30110:                FMT XX(116), 4*PD(14,4)
                  x% = 1
L30130:           if abs(d(x%)) < .01 then L30160
                     delete% = 1
                     return
L30160:           if x% = 4 then L30080
                     x% = x% + 1
                     go to L30130

L30200:     plowkey$ = empcode$
L30210:     call "PLOWNEXT" (#6, plowkey$, 12%, f1%(6))
                 if f1%(6) = 0 then return
                 get #6, using L30260, e( 1), e( 2), e( 3), e( 4), e( 5), ~
                                      e( 6), e( 7), e( 8), e( 9), e(10)

L30260:          FMT XX(84), 10*PD(14,4)
                 x% = 1
L30310:          if abs(e(x%)) < .01 then L30340
                    delete% = 1
                    return
L30340:          if x% = 10 then L30210
                    x% = x% + 1
                    go to L30310

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  str(header$,62) = "EMPDLETE: " & cms2v$
                  on fieldnr% gosub L40170,         /* EMPLOYEE CODE    */~
                                    L40140,         /* EMPLOYEE NAME    */~
                                    L40170          /* SOCIAL SECURITY  */
                     goto L40240

L40140:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02), "Remove Employees From Payroll Master File",  ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "Employee Code",                                       ~
               at (06,30), fac(lfac$( 1)), empcode$             , ch(12),~
               at (07,02),                                               ~
                  "Employee Name",                                       ~
               at (07,30), fac(lfac$( 2)), empname$             , ch(50),~
               at (08,02),                                               ~
                  "Social Security Number",                              ~
               at (08,30), fac(lfac$( 3)), ssnum$               , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L40540
                call "MANUAL" ("EMPDLETE")
                goto L40240

L40540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) lfac$()
                  str(header$,62) = "EMPDLETE: " & cms2v$
                  on fieldnr% gosub L41170,         /* EMPLOYEE CODE    */~
                                    L41140,         /* EMPLOYEE NAME    */~
                                    L41170          /* SOCIAL SECURITY  */
                     goto L41240

L41140:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02), "Remove Employees From Payroll Master File",  ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "Employee Code",                                       ~
               at (06,30), fac(lfac$( 1)), empcode$             , ch(12),~
               at (07,02),                                               ~
                  "Employee Name",                                       ~
               at (07,30), fac(lfac$( 2)), empname$             , ch(50),~
               at (08,02),                                               ~
                  "Social Security Number",                              ~
               at (08,30), fac(lfac$( 3)), ssnum$               , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Delete Emply",                           ~
                                                                         ~
               keys(hex(010d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L41540
                call "MANUAL" ("EMPDLETE")
                goto L41240

L41540:        if keyhit% <> 15 then L41580
                  call "PRNTSCRN"
                  goto L41240

L41580:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* EMPLOYEE CODE    */~
                                    L50200,         /* EMPLOYEE NAME    */~
                                    L50300          /* SOCIAL SECURITY  */
                     return
L50100:     REM TEST DATA FOR EMPLOYEE CODE
                empname$, ssnum$ = " "
                call "GETEMPL" (#7, empcode$, " ", 0%, f1%(7))
                     if f1%(7) = 1 then L50127
                     errormsg$ = "Employee Code Not On File: " & empcode$
                     return
L50127:      get #7,using L50130,status$,name$(1),name$(2),name$(3),ssnum$
L50130:              FMT CH(1), CH(15), CH(10), CH(1), CH(11)
                empname$ = name$(1) & ", " & name$(2) & ". " & name$(3)
                if status$ = "N" then L50165
                errormsg$="Employee Status Must Be 'Terminated' In Person~
        ~nel To Allow Delete"
                  return
L50165:         delete% = 0
                gosub L30000
                if delete% = 0 then return
                   errormsg$ = "Employee's Accruals Must ALL Be Zero To D~
        ~elete."
                   return
L50200:     REM TEST DATA FOR EMPLOYEE NAME
                return
L50300:     REM TEST DATA FOR SOCIAL SECURITY NUMBER
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
