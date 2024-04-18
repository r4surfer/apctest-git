        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L       SSS   EEEEE  TTTTT  U   U  PPPP    *~
            *  P   P  R   R  L      S      E        T    U   U  P   P   *~
            *  PPPP   RRRR   L       SSS   EEEE     T    U   U  PPPP    *~
            *  P      R   R  L          S  E        T    U   U  P       *~
            *  P      R   R  LLLLL   SSS   EEEEE    T     UUU   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLSETUP - MODIFIES, WITHOUT RECOURSE TO EARNINGS CYCLE,  *~
            *            THE EMPLOYEE MASTER RECORD.  ALSO USED TO SET  *~
            *            UP AN EMPLOYEE WITH EARNINGS IN THE MIDDLE OF  *~
            *            THE YEAR, WHEN IT'S IMPRACTICAL TO RUN BOGUS   *~
            *            CHECKS FOR HIM.                                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/81 ! ORIGINAL                                 ! BCW *~
            * 10/06/81 ! ARRAY SIZE FOR AMOUNTS.  USE MAXLINES%() ! TEM *~
            * 10/14/81 ! SECURITY FOR SALARIED PERSONNELL         ! TEM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 03/17/86 ! ADDED FULL SCREEN EDIT                   ! SGA *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            amount$(100,8)10,            /* AMOUNTS FOR EARNINGS       */~
            amount(12),                  /* AMOUNTS FOR READ IN        */~
            cursor%(2),                  /* CURSOR LOCATIONS FOR EDITS */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dedamt$(100,12)10,           /* DEDUCTION AMOUNTS ALPHA    */~
            deduction$(100)12,           /* DEDUCTION DESCRIPTIONS     */~
            desc1$(100)6,                /* FOR DEDUCTION SCREEN       */~
            desc2$(100,2)12,             /* FOR DEDUCTION SCREEN       */~
            edtmessage$79,               /* MESSAGE FOR EDIT MODE      */~
            edttran$(24)80,              /* CURSOR LOC==>FIELD # TABLE */~
            empcode$12,                  /* EMPLOYEE CODE NUMBER       */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20,12)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* SCREEN RECORD WORK AREA    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MODE MESSAGE TEXT    */~
            lastemp$12,                  /* LAST EMPLOYEE PROCESSED    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            maxlines%(2),                /* MAX # OF LINES FOR TABLES  */~
            name$(3)20,                  /* EMPLOYEE NAME              */~
            pfkeys$(2)17,                /* PF KEYS FOR EDITS          */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            screentitle1$79,             /* Screen Title               */~
            screentitle2$79,             /* Screen Title               */~
            title$(4,2)64,               /* P.F. KEY NAMES FOR MODES   */~
            tran$80,                     /* TRAN, DEDUCTIONS EDIT MODE */~
            total(8),                    /* TOTAL ALL EARNINGS         */~
            type$(100)12,                /* EARNINGS TYPE DESCRIPTIONS */~
            units$(100,3)6               /* UNITS FOR EARNINGS AMOUNTS */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other         "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS FILE.                *~
            * #14 ! PERMASTR ! Personnel Employee Master File           *~
            *************************************************************

            select # 3, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select # 4, "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 28

            select # 5, "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 18, dup

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Linking To Data Base To Manage Employee Earn~
        ~ing and Deduction Balances")

            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            title$(1,1) = "(1)START OVER(2)COL 1(4)LINE ABOVE(15)PRTSCR(1~
        ~6)EDIT MODE"
            title$(2,1) = "(1)START OVER(2)FIRST(3)LAST(4)PREV(5)NEXT(6)D~
        ~OWN(7)UP"
            title$(2,2) = "(9)DEDUCTIONS(10)SCREEN EDIT (15)PRINT SCREEN ~
        ~(16)WRITE DATA"
            title$(3,2) = "(9)EARNINGS  (10)SCREEN EDIT (15)PRINT SCREEN ~
        ~(16)WRITE DATA"

            pfkeys$(1) = hex(000102ff04ffffffffffffffffff0d0f10)
            pfkeys$(2) = hex(0001020304050607ff090affffff0d0f10)

            REM SET TRAN STRING FOR EARNINGS TABLE-2 LINES
                init(hex(00)) edttran$()
                init(hex(01)) str(edttran$(5),  1, 35)
                init(hex(02)) str(edttran$(5), 36, 15)
                init(hex(03)) str(edttran$(5), 51, 15)
                init(hex(04)) str(edttran$(5), 66, 15)
                init(hex(05)) str(edttran$(6),  1, 35)
                init(hex(06)) str(edttran$(6), 36, 15)
                init(hex(07)) str(edttran$(6), 51, 15)
                init(hex(08)) str(edttran$(6), 66, 15)

                for temp% = 1 to 9
                    edttran$(5 + 2*temp%) = edttran$(5)
                    edttran$(6 + 2*temp%) = edttran$(6)
                    next temp%

            REM SET TRAN STRING FOR DEDUCTIONS-ONE LINE
                init(hex(00)) str(tran$,  1, 80)
                init(hex(01)) str(tran$,  1, 32)
                init(hex(02)) str(tran$, 33, 15)
                init(hex(03)) str(tran$, 48, 15)
                init(hex(04)) str(tran$, 63, 18)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, infomsg$,                  ~
                      empcode$, units$(), type$(), amount$(),            ~
                      deduction$(), dedamt$(), desc1$(), desc2$()
            call "STRTRLSE" (#3)
            call "STRTRLSE" (#4)
            call "STRTRLSE" (#5)

            for fieldnr% = 1 to  1
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10230
L10170:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10230:         next fieldnr%

        REM *************************************************************~
            *           E A R N I N G S   I N P U T   M O D E           *~
            *                                                           *~
            * EARNINGS INPUT MODE INPUT ROUTINE.  STANDARD STUFF,       *~
            * ACTUALLY.                                                 *~
            *************************************************************

            line%, screenline% = 0
            errormsg$ = " "

            title$(1,1) = "(1)START OVER(2)COL 1(4)LINE ABOVE(15)PRTSCR(1~
        ~6)DEDUCTIONS"

L11100:     screenline% = screenline% + 1
            if screenline% < 11 then L11140
               line% = line% + 10
               screenline% = 1
L11140:     currentline%, c% = line% + screenline%
            if currentline% > 100 then L12000       /* INPUT DEDUCTIONS */
               if type$(c%) = " " then L12000       /* IF END OF LIST   */

L11180:     for fieldnr% = 1 to 8
L11190:         gosub'163(fieldnr%)
                      if enabled% =  0 then       L11300
L11210:         gosub'203(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L11280
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone1
                      if keyhit%  =  4 then gosub lineabove1
                      if keyhit%  = 16 and fieldnr% = 1 then L12000
                      goto L11190
L11280:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L11210
L11300:         next fieldnr%
                goto L11100

L12000: REM *************************************************************~
            *         D E D U C T I O N S   I N P U T   M O D E         *~
            *                                                           *~
            * THIS ROUTINE DRIVES DEDUCTION INPUT MODE.  SIMILAR TO     *~
            * THE ABOVE.                                                *~
            *************************************************************

        input_deductions
            gosub total_up_earnings
            line%, screenline% = 0
            errormsg$ = " "

            title$(1,1) = "(1)START OVER(2)COL 1(4)LINE ABOVE(15)PRTSCR(1~
        ~6)EDIT MODE"

L12100:     screenline% = screenline% + 1
            if screenline% < 6 then L12140
               line% = line% + 5
               screenline% = 1

L12140:     currentline%, c% = line% + screenline%
            if currentline% > 100 then L13000       /* IF TOO BIG...    */
               if deduction$(c%) = " " then L13000

L12180:     for fieldnr% = 1 to 12
L12190:         gosub'164(fieldnr%)
                      if enabled% =  0 then       L12300
L12210:         gosub'204(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L12280
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone2
                      if keyhit%  =  4 then gosub lineabove2
                      if keyhit%  = 16 and fieldnr% = 1 then L13000
                      goto L12190
L12280:         gosub'154(fieldnr%)
                      if errormsg$ <> " " then L12210
L12300:         next fieldnr%
                goto L12100

        total_up_earnings
            mat total = zer
            for u3% = 1 to 8
                   for temp% = 1 to maxlines%(1)
                       temp1 = 0
                       if amount$(temp%, u3%) <> " " then convert        ~
                         amount$(temp%, u3%) to temp1, data goto L12442
                         total(u3%) = round(total(u3%) + temp1, 2)
L12442:            next temp%
            next u3%
        return

L13000: REM *************************************************************~
            *     E D I T   E A R N I N G S   I N F O R M A T I O N     *~
            *                                                           *~
            * EDITS EARNINGS INFORMATION FOR THE EMPLOYEE.  USES THE    *~
            * STANDARD TRICKS.  FORTUNATELY, NO INSERT AND DELETES.     *~
            *************************************************************

            line%, currentline%, screenline% = 0

L13090:     gosub'213(0%, 0%)
                  if keyhit%  =  0 then       L13240
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%(1)-10)
                  if keyhit%  =  4 then line% = max(0,line%-9)
                  if keyhit%  =  5 then line% = min(line%+9,max(0,       ~
                                                maxlines%(1)-10))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%(1)-10))
                  if keyhit%  =  9 then       L14000
                  if keyhit%  = 10 then L13231
                  if keyhit%  = 16 then       datasave
                  goto L13090

L13231:     fieldnr% = 99%
            goto L13310
L13240:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                fieldnr% = val(str(edttran$(cursor%(1)),cursor%(2)))
                if fieldnr% = 0 then L13090
                screenline% = (cursor%(1)-5)/2+1
                currentline%, c% = line% + screenline%
                if currentline% > maxlines%(1) then L13090

L13310:         gosub'213(screenline%, fieldnr%)
                      if keyhit%  = 1 then gosub startover
                      if keyhit% <> 0 then L13310
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L13310
                goto L13090

L14000: REM *************************************************************~
            *   E D I T   D E D U C T I O N S   I N F O R M A T I O N   *~
            *                                                           *~
            * EDIT DEDUCTIONS INFORMATION, IN THE SAME MANNER AS ABOVE. *~
            *************************************************************

            line%, currentline%, screenline% = 0

L14080:     gosub'214(0%,0%)
                  if keyhit%  =  0 then       L14230
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0, maxlines%(2) - 4)
                  if keyhit%  =  4 then line% = max(0, line% - 4)
                  if keyhit%  =  5 then line% = min(line% + 4,           ~
                                               max(0, maxlines%(2) - 5))
                  if keyhit%  =  6 then line% = max(0, line% - 1)
                  if keyhit%  =  7 then line% = min(line% + 1,           ~
                                                max(0, maxlines%(2) - 5))
                  if keyhit%  =  9 then       L13000
                  if keyhit%  = 10 then L14221
                  if keyhit%  = 16 then       datasave
                  goto L14080

L14221:     fieldnr% = 99%
            goto L14340
L14230:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                screenline% = max(0%, cursor%(1) - 4%)
                if screenline%  =  0 or screenline% = 24 then L14080
                fieldnr% = val(str(edttran$(5),cursor%(2)))
                if fieldnr% = 0 then L14080
                if mod(screenline% - 1, 4) = 0 then L14301
                  fieldnr% = fieldnr% + 4
                  if mod(screenline% - 2, 4) = 0 then L14301
                    fieldnr% = fieldnr% + 4
L14301:         screenline% = int((screenline% + 3)/4)
                currentline%, c% = screenline% + line%
                if currentline% > maxlines%(2) then L14080

L14340:         gosub'214(screenline%, fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then L14340
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14340
                goto L14080

        REM *************************************************************~
            *  C O L U M N   O N E ,   L I N E   A B O V E   L O G I C  *~
            *                                                           *~
            * HANDLES COLUMN ONE AND LINE ABOVE FUNCTIONS FOR THE TWO   *~
            * INPUT MODES.                                              *~
            *************************************************************

        columnone1
            for temp% = 1 to 8
                amount$(c%, temp%) = "          "
                next temp%
            return clear all
            if fieldnr% <> 1 then L11180
               if currentline% = 1 then L11180
                  screenline% = screenline% - 1
                  if screenline% > 0 then L15180
                     line% = line% - 10
                     screenline% = 1
L15180:           currentline%, c% = line% + screenline%
                  goto L11180

        columnone2
            for temp% = 1 to 4
                dedamt$(c%, temp%) = "          "
                next temp%
            return clear all
            if fieldnr% <> 1 then L12180
               if currentline% = 1 then L12180
                  screenline% = screenline% - 1
                  if screenline% > 0 then L15320
                     line% = line% - 20
                     screenline% = 1
L15320:           currentline%, c% = line% + screenline%
                  goto L12180

        lineabove1
            if currentline% = 1 then return
            on fieldnr% gosub L15460,               /* CURRENT UNITS    */~
                              L15470,               /* MTD     UNITS    */~
                              L15480,               /* QTD     UNITS    */~
                              L15490,               /* YTD     UNITS    */~
                              L15500,               /* CURRENT AMOUNT   */~
                              L15510,               /* MTD     AMOUNT   */~
                              L15520,               /* QTD     AMOUNT   */~
                              L15530                /* YTD     AMOUNT   */
               return
L15460:     amount$(c%, 1) = amount$(c%-1, 1): return
L15470:     amount$(c%, 2) = amount$(c%-1, 2): return
L15480:     amount$(c%, 3) = amount$(c%-1, 3): return
L15490:     amount$(c%, 4) = amount$(c%-1, 4): return
L15500:     amount$(c%, 5) = amount$(c%-1, 5): return
L15510:     amount$(c%, 6) = amount$(c%-1, 6): return
L15520:     amount$(c%, 7) = amount$(c%-1, 7): return
L15530:     amount$(c%, 8) = amount$(c%-1, 8): return

        lineabove2
            if currentline% = 1 then return
               on fieldnr% gosub L15620,            /* CURRENT AMOUNT   */~
                                 L15630,            /* MTD     AMOUNT   */~
                                 L15640,            /* QTD     AMOUNT   */~
                                 L15650             /* YTD     AMOUNT   */
                  return
L15620:     dedamt$(c%, 1) = dedamt$(c%-1, 1): return
L15630:     dedamt$(c%, 2) = dedamt$(c%-1, 2): return
L15640:     dedamt$(c%, 3) = dedamt$(c%-1, 3): return
L15650:     dedamt$(c%, 4) = dedamt$(c%-1, 4): return
            return

        REM *************************************************************~
            *                    W R I T E   D A T A                    *~
            *                                                           *~
            * WRITES DATA TO THE FILE, USING THE PRESCRIBED PAYROLL     *~
            * ORDER OF EMPLOYEE FILE, THEN EARNINGS FILE, THEN          *~
            * DEDUCTIONS FILE.                                          *~
            *************************************************************

        datasave
            gosub L31000                  /* WRITE EARNINGS RECORDS     */
            gosub L32000                  /* WRITE DEDUCTIONS RECORDS   */
            lastemp$ = empcode$          /* SAVE LAST EMPLOYEE CODE    */
            goto L10000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* EMPLOYEE CODE    */
                     return
L20100:     REM DEFAULT/ENABLE FOR EMPLOYEE CODE
                inpmessage$ = "Enter blank or partial code to search file"
                enabled% = 1
                return

        REM *************************************************************~
            *   D E F A U L T / E N A B L E   F O R   E A R N I N G S   *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR EARNINGS INPUT.      *~
            * THIS IS SIMPLE, SINCE ALL THE FIELDS ARE ENABLED AT ONCE. *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L21100,         /* CURRENT UNITS    */~
                                    L21200,         /* QTD     UNITS    */~
                                    L21300,         /* QTD     UNITS    */~
                                    L21400,         /* YTD     UNITS    */~
                                    L21500,         /* CURRENT AMOUNT   */~
                                    L21600,         /* MTD     AMOUNT   */~
                                    L21700,         /* QTD     AMOUNT   */~
                                    L21800          /* YTD     AMOUNT   */
                     return
L21100:     REM DEFAULT/ENABLE FOR CURRENT UNITS
                enabled% = 1
                return
L21200:     REM DEFAULT/ENABLE FOR MTD     UNITS
                enabled% = 1
                return
L21300:     REM DEFAULT/ENABLE FOR QTD     UNITS
                enabled% = 1
                return
L21400:     REM DEFAULT/ENABLE FOR YTD     UNITS
                enabled% = 1
                return
L21500:     REM DEFAULT/ENABLE FOR CURRENT AMOUNT
                enabled% = 1
                return
L21600:     REM DEFAULT/ENABLE FOR MTD     AMOUNT
                enabled% = 1
                return
L21700:     REM DEFAULT/ENABLE FOR QTD     AMOUNT
                enabled% = 1
                return
L21800:     REM DEFAULT/ENABLE FOR YTD     AMOUNT
                enabled% = 1
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   D E D U C T I O N S     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR DEDUCTIONS AMOUNTS   *~
            * PLAYING AROUND.  THIS IS SIMPLE SINCE EVERY FIELD IS      *~
            * ENABLED.                                                  *~
            *************************************************************

            deffn'164(fieldnr%)
                  enabled% = 1
                  inpmessage$ = " "

            REM DEFAULT/ENABLE FOR ALL DEDUCTION FIELDS
                if fieldnr% < 5 then return
                infomsg$ = "The Default Is Total Earnings"
                convert total(fieldnr%-4) to dedamt$(c%, fieldnr%),      ~
                          pic(#######.##)
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 0%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *     L O A D   E M P L O Y E E   I N F O R M A T I O N     *~
            *                                                           *~
            * LOADS EMPLOYEE NAME AND COMPUTES EARNINGS AND DEDUCTIONS  *~
            * INFORMATION.                                              *~
            *************************************************************

            call "READ101" (#3, empcode$, f1%(3))
                 if f1%(3) = 0 then return

            REM LOAD EMPLOYEE MASTER AND SAY WHAT WE'RE UP TO.
                get   #14, using L30150,                                  ~
                                 name$(3), name$(1), name$(2), empcode$
L30150:               FMT XX(1), CH(15), CH(10), CH(1), XX(11), CH(12)
                infomsg$ = "Loading Employee" & hex(84) & empcode$       ~
                            & " (" & name$(1) & " " & name$(2) & " "     ~
                            & name$(3) & ")"
                call "SHOSTAT" (infomsg$)

            REM LOAD THE EARNINGS RECORDS.
                noearnings% = 1
                maxlines%(1), c% = 0
                readkey$ = empcode$

L30260:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then L30440
                maxlines%(1), c% = c% + 1
                get   # 4, using L30690,                                  ~
                           type$(c%), units$(c%,1), amount(1), amount(5),~
                           amount(2), amount(6), amount(3), amount(7),   ~
                           amount(4), amount(8)

                REM NOW FORMAT THEM AND SET FLAG FOR NON-ZERO RECORD.
                    units$(c%, 2) = "AMOUNT"
                    for temp% = 1 to 8
                        if amount(temp%) = 0 then L30410
                           noearnings% = 0
                           call "CONVERT" (amount(temp%), 2.2,           ~
                                                   amount$(c%, temp%))
L30410:                 next temp%
                goto L30260

L30440:     REM LOAD THE DEDUCTIONS RECORDS.
                maxlines%(2), c% = 0
                nodeductions% = 1
                readkey$ = empcode$

L30480:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                     if f1%(5) = 0 then L30660
                maxlines%(2), c% = c% + 1
                get   # 5, using L30760,                                  ~
                           deduction$(c%), amount(1), amount(2),         ~
                           amount(3), amount(4), amount(5), amount(9),   ~
                           amount(6), amount(10), amount(7), amount(11), ~
                           amount(8), amount(12)
                desc1$(c%) = "AMOUNT"
                desc2$(c%,1) = "  UNITS SUBJ"
                desc2$(c%,2) = "DOLLARS SUBJ"
                REM NOW FORMAT THIS WHOLE WAD.
                    for temp% = 1 to 12
                        if amount(temp%) = 0 then L30630
                           nodeductions% = 0
                           call "CONVERT" (amount(temp%), 2.2,           ~
                                                   dedamt$(c%, temp%))
L30630:                 next temp%
                goto L30480

L30660: return

L30690:     FMT XX(27),                  /* RECORD KEYS                */~
                CH(12),                  /* EARNINGS TYPE              */~
                XX(6),                   /* DEPARTMENT CODE, FLAGS     */~
                CH(6),                   /* UNITS OF MEASURE           */~
                XX(49),                  /* MIDDLE OF RECORD           */~
                8*PD(14,4)               /* CURR, MTD, QTD, YTD AMTS   */~

L30760:     FMT XX(39),                  /* FIRST PART OF RECORD       */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                XX(65),                  /* MIDDLE OF RECORD           */~
                4*PD(14,4),              /* RECORD ACCUMULATORS        */~
                XX(8),                                                   ~
                8*PD(14,4)               /* RECORD ACCUMULATORS        */~

L31000: REM *************************************************************~
            *       W R I T E S   E A R N I N G S   R E C O R D S       *~
            *                                                           *~
            * WRITES THE EARNINGS RECORDS AS FAST AS WE CAN.  NEED TO   *~
            * CHECK THAT THE EARNINGS TYPES MATCH SO WE DON'T           *~
            * ACCIDENTALLY CLOBBER THE REST OF HIS RECORDS, OR GO ON TO *~
            * CLOBBER ANYONE ELSE'S.                                    *~
            *************************************************************

            readkey$ = empcode$
            call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                 if f1%(4) = 0 then return
            call "READ101" (#4, readkey$, f1%(4))

            for temp% = 1 to maxlines%(1)
                get #4, using L31160, str(record$(), 1)
L31160:                 FMT CH(200)
                if str(record$(), 1, 12) <> empcode$ then return
                   if str(record$(), 28, 12) <> type$(temp%) then return
                      for u3% = 1 to 8
                          if amount$(temp%, u3%) = " "                   ~
                             then amount(u3%) = 0                        ~
                             else convert amount$(temp%, u3%) to         ~
                                                   amount(u3%)
                          next u3%
                put str(record$(), 101, 64), using L31280,                ~
                    amount(1), amount(5), amount(2), amount(6),          ~
                    amount(3), amount(7), amount(4), amount(8)
L31280:                                      FMT 8*PD(14,4)
                rewrite #4, using L31160, str(record$(), 1)
                call "READNXT1" (#4, f1%(4))
                     if f1%(4) = 0 then return
                next temp%
            return

L32000: REM *************************************************************~
            *      W R I T E   D E D U C T I O N S   R E C O R D S      *~
            *                                                           *~
            * WRITES THE DEDUCTIONS RECORDS TO THE FILE USING MUCH THE  *~
            * SAME TECHNIQUE OF THAT ABOVE.                             *~
            *************************************************************

            readkey$ = empcode$
            call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                 if f1%(5) = 0 then return
            call "READ101" (#5, readkey$, f1%(5))

            for temp% = 1 to maxlines%(2)
                get #5, using L32140, str(record$(), 1)
L32140:                 FMT CH(300)
                if str(record$(), 1, 12) <> empcode$ then return
                   if str(record$(), 40, 12) <> deduction$(temp%)        ~
                           then return
                   for u3% = 1 to 12
                       if dedamt$(temp%, u3%) = " "                      ~
                          then amount(u3%) = 0                           ~
                          else convert dedamt$(temp%, u3%) to            ~
                                                amount(u3%)
                       next u3%
                put str(record$(), 117, 32), using L32260,                ~
                    amount(1), amount(2), amount(3), amount(4)
L32260:                    FMT 4*PD(14,4)
                put str(record$(), 157, 64), using L32269,                ~
                    amount(5), amount(9), amount(6), amount(10),         ~
                    amount(7), amount(11), amount(8), amount(12)
L32269:                    FMT 8*PD(14,4)
                rewrite #5, using L32140, str(record$(), 1)
                call "READNXT1" (#5, f1%(5))
                     if f1%(5) = 0 then return
                next temp%
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)

                  screentitle1$="Enter/Adjust Employee's Earning/Deductio~
        ~n Balances"
                  str(screentitle1$,65,6) = "Today:"
                  str(screentitle1$,72,8) = date$
                  screentitle2$ = "Last Employee:"
                  str(screentitle2$,16,12) = lastemp$
                  str(screentitle2$,62,10) = "PRLSETUP:"
                  str(screentitle2$,72,8) = str(cms2v$,1,8)

                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40190          /* EMPLOYEE CODE    */
                  goto L40260

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40260:     accept                                                       ~
               at (01,02), fac(hex(8c)),     screentitle1$      , ch(79),~
               at (02,02), fac(hex(ac)),     screentitle2$      , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYEE CODE",                                       ~
               at (06,30), fac(lfac$( 1)), empcode$             , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 15 then L40600
                  call "PRNTSCRN"
L40580:           goto L40260

L40600:        if keyhit% <> 13 then return
                  call "MANUAL" ("PRLSETUP")
                  goto L40580

        REM *************************************************************~
            *        S E T   U P   E A R N I N G S   S C R E E N        *~
            *                                                           *~
            * SET UP AND ADJUST EARNINGS SCREEN.  THIS WORKS WITH THE   *~
            * STANDARD TECHNIQUES FOR THE STUFF.                        *~
            *************************************************************

            deffn'203(screenline%, fieldnr%)
                  screen% = 1
                  goto L41085

            deffn'213(screenline%, fieldnr%)
                  screen% = 2
                  if fieldnr% = 99 then L41076
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L41090
                  goto L41085
L41076:           init(hex(86)) fac$()
                  for k% = 1% to min(maxlines%(1) - line%, 10%)
                  for m% = 1% to 8%
                  fac$(k%, m%) = hex(82)
                  next m%
                  next k%
                  goto L41190
L41085:           init(hex(84)) fac$()
L41090:           on fieldnr% gosub L41170,         /* CURRENT UNITS    */~
                                    L41170,         /* MTD     UNITS    */~
                                    L41170,         /* QTD     UNITS    */~
                                    L41170,         /* YTD     UNITS    */~
                                    L41170,         /* CURRENT AMOUNT   */~
                                    L41170,         /* MTD     AMOUNT   */~
                                    L41170,         /* QTD     AMOUNT   */~
                                    L41170          /* YTD     AMOUNT   */
                  goto L41190

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L41170:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L41190:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$(screen%,1)      , ch(64),~
               at (02,02), fac(hex(8c)), title$(screen%,2)      , ch(64),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(63),~
               at (04,02), fac(hex(a4)), infomsg$               , ch(79),~
                                                                         ~
               at (01,67), "!FOR EMPLOYEE:"                             ,~
               at (02,67), "!             "                             ,~
               at (03,67), "+-------------"                             ,~
               at (02,69), fac(hex(84)), empcode$               , ch(12),~
                                                                         ~
               at (05,02), fac(hex(84)),   type$  (line%+ 1)    , ch(12),~
               at (07,02), fac(hex(84)),   type$  (line%+ 2)    , ch(12),~
               at (09,02), fac(hex(84)),   type$  (line%+ 3)    , ch(12),~
               at (11,02), fac(hex(84)),   type$  (line%+ 4)    , ch(12),~
               at (13,02), fac(hex(84)),   type$  (line%+ 5)    , ch(12),~
               at (15,02), fac(hex(84)),   type$  (line%+ 6)    , ch(12),~
               at (17,02), fac(hex(84)),   type$  (line%+ 7)    , ch(12),~
               at (19,02), fac(hex(84)),   type$  (line%+ 8)    , ch(12),~
               at (21,02), fac(hex(84)),   type$  (line%+ 9)    , ch(12),~
               at (23,02), fac(hex(84)),   type$  (line%+10)    , ch(12),~
                                                                         ~
               at (05,15), fac(hex(8c)),   units$ (line%+ 1,1)  , ch(06),~
               at (07,15), fac(hex(8c)),   units$ (line%+ 2,1)  , ch(06),~
               at (09,15), fac(hex(8c)),   units$ (line%+ 3,1)  , ch(06),~
               at (11,15), fac(hex(8c)),   units$ (line%+ 4,1)  , ch(06),~
               at (13,15), fac(hex(8c)),   units$ (line%+ 5,1)  , ch(06),~
               at (15,15), fac(hex(8c)),   units$ (line%+ 6,1)  , ch(06),~
               at (17,15), fac(hex(8c)),   units$ (line%+ 7,1)  , ch(06),~
               at (19,15), fac(hex(8c)),   units$ (line%+ 8,1)  , ch(06),~
               at (21,15), fac(hex(8c)),   units$ (line%+ 9,1)  , ch(06),~
               at (23,15), fac(hex(8c)),   units$ (line%+10,1)  , ch(06),~
                                                                         ~
               at (05,22), "CUR",                                        ~
                                                                         ~
               at (05,26), fac(fac$( 1,1)), amount$(line%+ 1,1) , ch(10),~
               at (07,26), fac(fac$( 2,1)), amount$(line%+ 2,1) , ch(10),~
               at (09,26), fac(fac$( 3,1)), amount$(line%+ 3,1) , ch(10),~
               at (11,26), fac(fac$( 4,1)), amount$(line%+ 4,1) , ch(10),~
               at (13,26), fac(fac$( 5,1)), amount$(line%+ 5,1) , ch(10),~
               at (15,26), fac(fac$( 6,1)), amount$(line%+ 6,1) , ch(10),~
               at (17,26), fac(fac$( 7,1)), amount$(line%+ 7,1) , ch(10),~
               at (19,26), fac(fac$( 8,1)), amount$(line%+ 8,1) , ch(10),~
               at (21,26), fac(fac$( 9,1)), amount$(line%+ 9,1) , ch(10),~
               at (23,26), fac(fac$(10,1)), amount$(line%+10,1) , ch(10),~
                                                                         ~
               at (05,37), "MTD",                                        ~
                                                                         ~
               at (05,41), fac(fac$( 1,2)), amount$(line%+ 1,2) , ch(10),~
               at (07,41), fac(fac$( 2,2)), amount$(line%+ 2,2) , ch(10),~
               at (09,41), fac(fac$( 3,2)), amount$(line%+ 3,2) , ch(10),~
               at (11,41), fac(fac$( 4,2)), amount$(line%+ 4,2) , ch(10),~
               at (13,41), fac(fac$( 5,2)), amount$(line%+ 5,2) , ch(10),~
               at (15,41), fac(fac$( 6,2)), amount$(line%+ 6,2) , ch(10),~
               at (17,41), fac(fac$( 7,2)), amount$(line%+ 7,2) , ch(10),~
               at (19,41), fac(fac$( 8,2)), amount$(line%+ 8,2) , ch(10),~
               at (21,41), fac(fac$( 9,2)), amount$(line%+ 9,2) , ch(10),~
               at (23,41), fac(fac$(10,2)), amount$(line%+10,2) , ch(10),~
                                                                         ~
               at (05,52), "QTD",                                        ~
                                                                         ~
               at (05,56), fac(fac$( 1,3)), amount$(line%+ 1,3) , ch(10),~
               at (07,56), fac(fac$( 2,3)), amount$(line%+ 2,3) , ch(10),~
               at (09,56), fac(fac$( 3,3)), amount$(line%+ 3,3) , ch(10),~
               at (11,56), fac(fac$( 4,3)), amount$(line%+ 4,3) , ch(10),~
               at (13,56), fac(fac$( 5,3)), amount$(line%+ 5,3) , ch(10),~
               at (15,56), fac(fac$( 6,3)), amount$(line%+ 6,3) , ch(10),~
               at (17,56), fac(fac$( 7,3)), amount$(line%+ 7,3) , ch(10),~
               at (19,56), fac(fac$( 8,3)), amount$(line%+ 8,3) , ch(10),~
               at (21,56), fac(fac$( 9,3)), amount$(line%+ 9,3) , ch(10),~
               at (23,56), fac(fac$(10,3)), amount$(line%+10,3) , ch(10),~
                                                                         ~
               at (05,67), "YTD",                                        ~
                                                                         ~
               at (05,71), fac(fac$( 1,4)), amount$(line%+ 1,4) , ch(10),~
               at (07,71), fac(fac$( 2,4)), amount$(line%+ 2,4) , ch(10),~
               at (09,71), fac(fac$( 3,4)), amount$(line%+ 3,4) , ch(10),~
               at (11,71), fac(fac$( 4,4)), amount$(line%+ 4,4) , ch(10),~
               at (13,71), fac(fac$( 5,4)), amount$(line%+ 5,4) , ch(10),~
               at (15,71), fac(fac$( 6,4)), amount$(line%+ 6,4) , ch(10),~
               at (17,71), fac(fac$( 7,4)), amount$(line%+ 7,4) , ch(10),~
               at (19,71), fac(fac$( 8,4)), amount$(line%+ 8,4) , ch(10),~
               at (21,71), fac(fac$( 9,4)), amount$(line%+ 9,4) , ch(10),~
               at (23,71), fac(fac$(10,4)), amount$(line%+10,4) , ch(10),~
                                                                         ~
               at (06,15), fac(hex(8c)),    units$ (line%+ 1,2) , ch(06),~
               at (08,15), fac(hex(8c)),    units$ (line%+ 2,2) , ch(06),~
               at (10,15), fac(hex(8c)),    units$ (line%+ 3,2) , ch(06),~
               at (12,15), fac(hex(8c)),    units$ (line%+ 4,2) , ch(06),~
               at (14,15), fac(hex(8c)),    units$ (line%+ 5,2) , ch(06),~
               at (16,15), fac(hex(8c)),    units$ (line%+ 6,2) , ch(06),~
               at (18,15), fac(hex(8c)),    units$ (line%+ 7,2) , ch(06),~
               at (20,15), fac(hex(8c)),    units$ (line%+ 8,2) , ch(06),~
               at (22,15), fac(hex(8c)),    units$ (line%+ 9,2) , ch(06),~
               at (24,15), fac(hex(8c)),    units$ (line%+10,2) , ch(06),~
                                                                         ~
               at (06,26), fac(fac$( 1,5)), amount$(line%+ 1,5) , ch(10),~
               at (08,26), fac(fac$( 2,5)), amount$(line%+ 2,5) , ch(10),~
               at (10,26), fac(fac$( 3,5)), amount$(line%+ 3,5) , ch(10),~
               at (12,26), fac(fac$( 4,5)), amount$(line%+ 4,5) , ch(10),~
               at (14,26), fac(fac$( 5,5)), amount$(line%+ 5,5) , ch(10),~
               at (16,26), fac(fac$( 6,5)), amount$(line%+ 6,5) , ch(10),~
               at (18,26), fac(fac$( 7,5)), amount$(line%+ 7,5) , ch(10),~
               at (20,26), fac(fac$( 8,5)), amount$(line%+ 8,5) , ch(10),~
               at (22,26), fac(fac$( 9,5)), amount$(line%+ 9,5) , ch(10),~
               at (24,26), fac(fac$(10,5)), amount$(line%+10,5) , ch(10),~
                                                                         ~
               at (06,41), fac(fac$( 1,6)), amount$(line%+ 1,6) , ch(10),~
               at (08,41), fac(fac$( 2,6)), amount$(line%+ 2,6) , ch(10),~
               at (10,41), fac(fac$( 3,6)), amount$(line%+ 3,6) , ch(10),~
               at (12,41), fac(fac$( 4,6)), amount$(line%+ 4,6) , ch(10),~
               at (14,41), fac(fac$( 5,6)), amount$(line%+ 5,6) , ch(10),~
               at (16,41), fac(fac$( 6,6)), amount$(line%+ 6,6) , ch(10),~
               at (18,41), fac(fac$( 7,6)), amount$(line%+ 7,6) , ch(10),~
               at (20,41), fac(fac$( 8,6)), amount$(line%+ 8,6) , ch(10),~
               at (22,41), fac(fac$( 9,6)), amount$(line%+ 9,6) , ch(10),~
               at (24,41), fac(fac$(10,6)), amount$(line%+10,6) , ch(10),~
                                                                         ~
               at (06,56), fac(fac$( 1,7)), amount$(line%+ 1,7) , ch(10),~
               at (08,56), fac(fac$( 2,7)), amount$(line%+ 2,7) , ch(10),~
               at (10,56), fac(fac$( 3,7)), amount$(line%+ 3,7) , ch(10),~
               at (12,56), fac(fac$( 4,7)), amount$(line%+ 4,7) , ch(10),~
               at (14,56), fac(fac$( 5,7)), amount$(line%+ 5,7) , ch(10),~
               at (16,56), fac(fac$( 6,7)), amount$(line%+ 6,7) , ch(10),~
               at (18,56), fac(fac$( 7,7)), amount$(line%+ 7,7) , ch(10),~
               at (20,56), fac(fac$( 8,7)), amount$(line%+ 8,7) , ch(10),~
               at (22,56), fac(fac$( 9,7)), amount$(line%+ 9,7) , ch(10),~
               at (24,56), fac(fac$(10,7)), amount$(line%+10,7) , ch(10),~
                                                                         ~
               at (06,71), fac(fac$( 1,8)), amount$(line%+ 1,8) , ch(10),~
               at (08,71), fac(fac$( 2,8)), amount$(line%+ 2,8) , ch(10),~
               at (10,71), fac(fac$( 3,8)), amount$(line%+ 3,8) , ch(10),~
               at (12,71), fac(fac$( 4,8)), amount$(line%+ 4,8) , ch(10),~
               at (14,71), fac(fac$( 5,8)), amount$(line%+ 5,8) , ch(10),~
               at (16,71), fac(fac$( 6,8)), amount$(line%+ 6,8) , ch(10),~
               at (18,71), fac(fac$( 7,8)), amount$(line%+ 7,8) , ch(10),~
               at (20,71), fac(fac$( 8,8)), amount$(line%+ 8,8) , ch(10),~
               at (22,71), fac(fac$( 9,8)), amount$(line%+ 9,8) , ch(10),~
               at (24,71), fac(fac$(10,8)), amount$(line%+10,8) , ch(10),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13 then L41930
                  call "MANUAL" ("PRLSETUP")
                  goto L41190

L41930:        if keyhit% <> 15 then L41950
                  call "PRNTSCRN"
                  goto L41190

L41950:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *      A D J U S T   D E D U C T I O N S   S C R E E N      *~
            *                                                           *~
            * ADJUST DEDUCTIONS SCREEN HANDLES THE CURRENT, QTD, MTD,   *~
            * YTD DEDUCTIONS AMOUNTS.                                   *~
            *************************************************************

            deffn'204(screenline%, fieldnr%)
                  screen% = 0
                  goto L42085

            deffn'214(screenline%, fieldnr%)
                  screen% = 2
                  if fieldnr% = 99 then L42076
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L42090
                  goto L42085
L42076:           init(hex(86)) fac$()
                  for k% = 1% to min(maxlines%(2) - line%, 5%)
                  for m% = 1% to 12%
                  fac$(k%, m%) = hex(82)
                  next m%
                  next k%
                  goto L42155
L42085:           init(hex(84)) fac$()
L42090:           gosub L42135
                  goto L42155

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L42135:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L42155:     accept                                                       ~
               at (01,02), fac(hex(8c)),title$(max(screen%,1),1), ch(64),~
               at (02,02), fac(hex(8c)), title$(screen%+1,2)    , ch(64),~
                                                                         ~
               at (03,02), fac(hex(94)), errormsg$              , ch(63),~
               at (04,02), fac(hex(a4)), infomsg$               , ch(79),~
                                                                         ~
               at (01,67), "! DEDUCTIONS: "                             ,~
               at (02,67), "!             "                             ,~
               at (03,67), "+-------------"                             ,~
               at (02,69), fac(hex(84)), empcode$               , ch(12),~
                                                                         ~
               at (05,02), fac(hex(84)),    deduction$(line%+ 1), ch(12),~
               at (09,02), fac(hex(84)),    deduction$(line%+ 2), ch(12),~
               at (13,02), fac(hex(84)),    deduction$(line%+ 3), ch(12),~
               at (17,02), fac(hex(84)),    deduction$(line%+ 4), ch(12),~
               at (21,02), fac(hex(84)),    deduction$(line%+ 5), ch(12),~
                                                                         ~
               at (05,15), fac(hex(8c)), desc1$(line%+ 1),        ch(06),~
               at (06,09), fac(hex(8c)), desc2$(line%+ 1, 1),     ch(12),~
               at (07,09), fac(hex(8c)), desc2$(line%+ 1, 2),     ch(12),~
               at (09,15), fac(hex(8c)), desc1$(line%+ 2),        ch(06),~
               at (10,09), fac(hex(8c)), desc2$(line%+ 2, 1),     ch(12),~
               at (11,09), fac(hex(8c)), desc2$(line%+ 2, 2),     ch(12),~
               at (13,15), fac(hex(8c)), desc1$(line%+ 3),        ch(06),~
               at (14,09), fac(hex(8c)), desc2$(line%+ 3, 1),     ch(12),~
               at (15,09), fac(hex(8c)), desc2$(line%+ 3, 2),     ch(12),~
               at (17,15), fac(hex(8c)), desc1$(line%+ 4),        ch(06),~
               at (18,09), fac(hex(8c)), desc2$(line%+ 4, 1),     ch(12),~
               at (19,09), fac(hex(8c)), desc2$(line%+ 4, 2),     ch(12),~
               at (21,15), fac(hex(8c)), desc1$(line%+ 5),        ch(06),~
               at (22,09), fac(hex(8c)), desc2$(line%+ 5, 1),     ch(12),~
               at (23,09), fac(hex(8c)), desc2$(line%+ 5, 2),     ch(12),~
                                                                         ~
               at (05,22), "CUR",                                        ~
                                                                         ~
               at (05,26), fac(fac$( 1,1)), dedamt$(line%+ 1, 1), ch(10),~
               at (06,26), fac(fac$( 1,5)), dedamt$(line%+ 1, 5), ch(10),~
               at (07,26), fac(fac$( 1,9)), dedamt$(line%+ 1, 9), ch(10),~
               at (09,26), fac(fac$( 2,1)), dedamt$(line%+ 2, 1), ch(10),~
               at (10,26), fac(fac$( 2,5)), dedamt$(line%+ 2, 5), ch(10),~
               at (11,26), fac(fac$( 2,9)), dedamt$(line%+ 2, 9), ch(10),~
               at (13,26), fac(fac$( 3,1)), dedamt$(line%+ 3, 1), ch(10),~
               at (14,26), fac(fac$( 3,5)), dedamt$(line%+ 3, 5), ch(10),~
               at (15,26), fac(fac$( 3,9)), dedamt$(line%+ 3, 9), ch(10),~
               at (17,26), fac(fac$( 4,1)), dedamt$(line%+ 4, 1), ch(10),~
               at (18,26), fac(fac$( 4,5)), dedamt$(line%+ 4, 5), ch(10),~
               at (19,26), fac(fac$( 4,9)), dedamt$(line%+ 4, 9), ch(10),~
               at (21,26), fac(fac$( 5,1)), dedamt$(line%+ 5, 1), ch(10),~
               at (22,26), fac(fac$( 5,5)), dedamt$(line%+ 5, 5), ch(10),~
               at (23,26), fac(fac$( 5,9)), dedamt$(line%+ 5, 9), ch(10),~
                                                                         ~
               at (05,37), "MTD",                                        ~
                                                                         ~
               at (05,41), fac(fac$( 1, 2)), dedamt$(line%+1, 2), ch(10),~
               at (06,41), fac(fac$( 1, 6)), dedamt$(line%+1, 6), ch(10),~
               at (07,41), fac(fac$( 1,10)), dedamt$(line%+1,10), ch(10),~
               at (09,41), fac(fac$( 2, 2)), dedamt$(line%+2, 2), ch(10),~
               at (10,41), fac(fac$( 2, 6)), dedamt$(line%+2, 6), ch(10),~
               at (11,41), fac(fac$( 2,10)), dedamt$(line%+2,10), ch(10),~
               at (13,41), fac(fac$( 3, 2)), dedamt$(line%+3, 2), ch(10),~
               at (14,41), fac(fac$( 3, 6)), dedamt$(line%+3, 6), ch(10),~
               at (15,41), fac(fac$( 3,10)), dedamt$(line%+3,10), ch(10),~
               at (17,41), fac(fac$( 4, 2)), dedamt$(line%+4, 2), ch(10),~
               at (18,41), fac(fac$( 4, 6)), dedamt$(line%+4, 6), ch(10),~
               at (19,41), fac(fac$( 4,10)), dedamt$(line%+4,10), ch(10),~
               at (21,41), fac(fac$( 5, 2)), dedamt$(line%+5, 2), ch(10),~
               at (22,41), fac(fac$( 5, 6)), dedamt$(line%+5, 6), ch(10),~
               at (23,41), fac(fac$( 5,10)), dedamt$(line%+5,10), ch(10),~
                                                                         ~
               at (05,52), "QTD",                                        ~
                                                                         ~
               at (05,56), fac(fac$(1, 3)), dedamt$(line%+1, 3),  ch(10),~
               at (06,56), fac(fac$(1, 7)), dedamt$(line%+1, 7),  ch(10),~
               at (07,56), fac(fac$(1,11)), dedamt$(line%+1,11),  ch(10),~
               at (09,56), fac(fac$(2, 3)), dedamt$(line%+2, 3),  ch(10),~
               at (10,56), fac(fac$(2, 7)), dedamt$(line%+2, 7),  ch(10),~
               at (11,56), fac(fac$(2,11)), dedamt$(line%+2,11),  ch(10),~
               at (13,56), fac(fac$(3, 3)), dedamt$(line%+3, 3),  ch(10),~
               at (14,56), fac(fac$(3, 7)), dedamt$(line%+3, 7),  ch(10),~
               at (15,56), fac(fac$(3,11)), dedamt$(line%+3,11),  ch(10),~
               at (17,56), fac(fac$(4, 3)), dedamt$(line%+4, 3),  ch(10),~
               at (18,56), fac(fac$(4, 7)), dedamt$(line%+4, 7),  ch(10),~
               at (19,56), fac(fac$(4,11)), dedamt$(line%+4,11),  ch(10),~
               at (21,56), fac(fac$(5, 3)), dedamt$(line%+5, 3),  ch(10),~
               at (22,56), fac(fac$(5, 7)), dedamt$(line%+5, 7),  ch(10),~
               at (23,56), fac(fac$(5,11)), dedamt$(line%+5,11),  ch(10),~
                                                                         ~
               at (05,67), "YTD",                                        ~
                                                                         ~
               at (05,71), fac(fac$(1, 4)), dedamt$(line%+1, 4),  ch(10),~
               at (06,71), fac(fac$(1, 8)), dedamt$(line%+1, 8),  ch(10),~
               at (07,71), fac(fac$(1,12)), dedamt$(line%+1,12),  ch(10),~
               at (09,71), fac(fac$(2, 4)), dedamt$(line%+2, 4),  ch(10),~
               at (10,71), fac(fac$(2, 8)), dedamt$(line%+2, 8),  ch(10),~
               at (11,71), fac(fac$(2,12)), dedamt$(line%+2,12),  ch(10),~
               at (13,71), fac(fac$(3, 4)), dedamt$(line%+3, 4),  ch(10),~
               at (14,71), fac(fac$(3, 8)), dedamt$(line%+3, 8),  ch(10),~
               at (15,71), fac(fac$(3,12)), dedamt$(line%+3,12),  ch(10),~
               at (17,71), fac(fac$(4, 4)), dedamt$(line%+4, 4),  ch(10),~
               at (18,71), fac(fac$(4, 8)), dedamt$(line%+4, 8),  ch(10),~
               at (19,71), fac(fac$(4,12)), dedamt$(line%+4,12),  ch(10),~
               at (21,71), fac(fac$(5, 4)), dedamt$(line%+5, 4),  ch(10),~
               at (22,71), fac(fac$(5, 8)), dedamt$(line%+5, 8),  ch(10),~
               at (23,71), fac(fac$(5,12)), dedamt$(line%+5,12),  ch(10),~
                                                                         ~
               keys (pfkeys$(max(screen%,1))),                           ~
               key  (keyhit%)

               if keyhit% <> 13 then L46320
                  call "MANUAL" ("PRLSETUP")
                  goto L42155

L46320:        if keyhit% <> 15 then L46440
                  call "PRNTSCRN"
                  goto L42155

L46440:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* EMPLOYEE CODE    */
                     return
L50100:     REM TEST DATA FOR EMPLOYEE CODE
                call "GETEMPL" (#14, empcode$, " ", 0%, f1%(14))
                    if f1%(14) <> 0 then L50160
                    errormsg$="Employee Code Not In Personnel Master File"
                    return

L50160:         gosub L30000
                if f1%(3) <> 0 then L50200
                     errormsg$ = "Employee Not In Payroll Master File"
                     return
L50200:
                infomsg$ = "EMPLOYEE: " & name$(1) & " " & name$(2) &    ~
                                                          " " & name$(3)
                if noearnings% = 1 then return
                return clear all
                if nodeductions% = 1 then input_deductions
                goto L13000      /* EDIT MODE    */

        REM *************************************************************~
            *           T E S T    E A R N I N G S   D A T A            *~
            *                                                           *~
            * TESTS EARNINGS DATA FOR VALIDITY.                         *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  if fieldnr% = 99 then L51860
                  on fieldnr% gosub L51100,         /* CURRENT UNITS    */~
                                    L51200,         /* MTD     UNITS    */~
                                    L51300,         /* QTD     UNITS    */~
                                    L51400,         /* YTD     UNITS    */~
                                    L51500,         /* CURRENT AMOUNT   */~
                                    L51600,         /* MTD     AMOUNT   */~
                                    L51700,         /* QTD     AMOUNT   */~
                                    L51800          /* YTD     AMOUNT   */
                     return
L51100:     REM TEST DATA FOR CURRENT UNITS
                call "NUMTEST" (amount$(c%,1), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51200:     REM TEST DATA FOR MTD     UNITS
                call "NUMTEST" (amount$(c%,2), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51300:     REM TEST DATA FOR QTD     UNITS
                call "NUMTEST" (amount$(c%,3), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51400:     REM TEST DATA FOR YTD     UNITS
                call "NUMTEST" (amount$(c%,4), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51500:     REM TEST DATA FOR CURRENT AMOUNT
                call "NUMTEST" (amount$(c%,5), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51600:     REM TEST DATA FOR MTD     AMOUNT
                call "NUMTEST" (amount$(c%,6), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51700:     REM TEST DATA FOR QTD     AMOUNT
                call "NUMTEST" (amount$(c%,7), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return
L51800:     REM TEST DATA FOR YTD     AMOUNT
                call "NUMTEST" (amount$(c%,8), 0, 9e7, errormsg$,        ~
                                  -2.2, amount)
                return

L51860: REM TEST FOR FULL SCREEN EDIT
            for i% = line% + 1% to min(line% + 10%, maxlines%(1))
            for j% = 1% to 8%
                call "NUMTEST" (amount$(i%,j%), 0, 9e7, errormsg$,       ~
                                 -2.2, amount)
                    if errormsg$ <> " " then return
                next j%
                next i%
                return

        REM *************************************************************~
            *          T E S T   D E D U C T I O N S   D A T A          *~
            *                                                           *~
            * TESTS DATA FOR DEDUCTIONS AMOUNTS.  THIS IS A LOT OF      *~
            * REPETITIVE TESTING.                                       *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  if fieldnr% = 99 then L52170
                  gosub L52100          /* ALL AMOUNTS  */

                     return
L52100:     REM TEST DATA FOR CURRENT AMOUNT
                call "NUMTEST" (dedamt$(c%,fieldnr%), 0, 9e7, errormsg$, ~
                                 -2.2, amount)
                return
L52170: REM TEST FOR FULL SCREEN EDIT
              for i% = line% + 1% to min(line% + 5%, maxlines%(2))
              for j% = 1% to 12%
                call "NUMTEST" (dedamt$(i%,j%), 0, 9e7, errormsg$,       ~
                                 -2.2, amount)
                if errormsg$ <> " " then return
              next j%
              next i%
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
