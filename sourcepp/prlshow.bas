        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L       SSS   H   H   OOO   W   W          *~
            *  P   P  R   R  L      S      H   H  O   O  W   W          *~
            *  PPPP   RRRR   L       SSS   HHHHH  O   O  W   W          *~
            *  P      R   R  L          S  H   H  O   O  W W W          *~
            *  P      R   R  LLLLL   SSS   H   H   OOO    W W           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLSHOW  - SHOWS THE AMOUNTS ("$") OF PAYROLL, BOTH UNITS *~
            *            AND DOLLARS, THAT THE DESIGNATED EMPLOYEE      *~
            *            EARNED THIS YEAR.                              *~
            *            THIS PROGRAM IS A CHEAP RIPOFF FROM "PRLSETUP".*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/26/81 ! ORIGINAL                                 ! BCW *~
            * 10/06/81 ! USE OF MAXLINES%(), DIM ON AMOUNT$()     ! TEM *~
            * 10/14/81 ! SECURITY ACCESS PRIVILIGES               ! TEM *~
            * 05/18/92 ! Renamed PRLSHOW from PRLSHOW$. Plus      ! JDH *~
            *          !   minor cleanup.                         !     *~
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
            title$(4,2)64,               /* P.F. KEY NAMES FOR MODES   */~
            tran$80,                     /* TRAN, DEDUCTIONS EDIT MODE */~
            type$(100)12,                /* EARNINGS TYPE DESCRIPTIONS */~
            units$(100,3)6               /* UNITS FOR EARNINGS AMOUNTS */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS FILE.                *~
            * # 6 ! USERINFO ! USER INFORMATION FILE                    *~
            * #10 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
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

            select # 6, "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

            select #10, "PRLDDT",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 6

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

            call "SHOSTAT" ("LINKING TO DATA BASE TO REVIEW EMPLOYEE ACCR~
        ~UALS")

            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))

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
            title$(2,2) = "(9)DEDUCTIONS(15)PRINT SCREEN                 ~
        ~(16)NEXT EMPLY"
            title$(3,2) = "(9)EARNINGS  (15)PRINT SCREEN                 ~
        ~(16)NEXT EMPLY"

            pfkeys$(1) = hex(000102ff04ffffffffffffffffff0d0f10)
            pfkeys$(2) = hex(0001020304050607ff09ffffffff0d0f10)

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
            call "ALLFREE"

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

            line%, maxlines%(1), screenline% = 0
            infomsg$, errormsg$ = " "

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
                maxlines%(1) = maxlines%(1) + 1
                goto L11100

L12000: REM *************************************************************~
            *         D E D U C T I O N S   I N P U T   M O D E         *~
            *                                                           *~
            * THIS ROUTINE DRIVES DEDUCTION INPUT MODE.  SIMILAR TO     *~
            * THE ABOVE.                                                *~
            *************************************************************

            line%, maxlines%(2), screenline% = 0
            infomsg$, errormsg$ = " "

L12100:     screenline% = screenline% + 1
            if screenline% < 6 then L12140
               line% = line% + 4
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
                maxlines%(2) = maxlines%(2) + 1
                goto L12100

L13000: REM *************************************************************~
            *     E D I T   E A R N I N G S   I N F O R M A T I O N     *~
            *                                                           *~
            * EDITS EARNINGS INFORMATION FOR THE EMPLOYEE.  USES THE    *~
            * STANDARD TRICKS.  FORTUNATELY, NO INSERT AND DELETES.     *~
            *************************************************************

            line%, currentline%, screenline% = 0

L13090:     gosub'213(0%, 0%)
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
                  if keyhit%  =  0 then       L14000
                  if keyhit%  = 16 then       datasave
                  goto L13090

            REM NOW FIGURE OUT WHICH FIELD HE HIT.
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
                  if keyhit%  =  0 then       L13000
                  if keyhit%  = 16 then       datasave
                  goto L14080

            REM NOW FIGURE OUT WHICH FIELD HE HIT.
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
                inpmessage$="Enter blank or partial to search for code"
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
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L22100,         /* CURRENT AMOUNT   */~
                                    L22200,         /* MTD AMOUNT       */~
                                    L22300,         /* QTD AMOUNT       */~
                                    L22400          /* YTD AMOUNT       */
                     return
L22100:     REM DEFAULT/ENABLE FOR CURRENT AMOUNT
                enabled% = 1
                return
L22200:     REM DEFAULT/ENABLE FOR MONTH TO DATE AMOUNT
                enabled% = 1
                return
L22300:     REM DEFAULT/ENABLE FOR QUARTER TO DATE AMOUNT
                enabled% = 1
                return
L22400:     REM DEFAULT/ENABLE FOR YEAR TO DATE AMOUNT
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

L30000: REM *************************************************************~
            *     L O A D   E M P L O Y E E   I N F O R M A T I O N     *~
            *                                                           *~
            * LOADS EMPLOYEE NAME AND COMPUTES EARNINGS AND DEDUCTIONS  *~
            * INFORMATION.                                              *~
            *************************************************************

            call "READ100" (#3, empcode$, f1%(3))
                 if f1%(3) = 0 then return

            REM LOAD EMPLOYEE MASTER AND SAY WHAT WE'RE UP TO.
                get   #14, using L30150,                                  ~
                                 name$(3), name$(1), name$(2), empcode$
L30150:               FMT XX(1), CH(15), CH(10), CH(1), XX(11), CH(12)
                infomsg$ = "LOADING EMPLOYEE" & hex(84) & empcode$       ~
                            & " (" & name$(1) & " " & name$(2) & " "     ~
                            & name$(3) & ")"
                call "SHOSTAT" (infomsg$)

            REM LOAD THE EARNINGS RECORDS.
                flag% = 0
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
                   REM  IF AMOUNT(TEMP%) = 0 THEN 30410
                           flag% = 1
                           call "NUMSMASH" (amount(temp%), 2,            ~
                                                   amount$(c%, temp%))
                        next temp%
                goto L30260

L30440:     REM LOAD THE DEDUCTIONS RECORDS.
                maxlines%(2), c% = 0
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
                   REM  IF AMOUNT(TEMP%) = 0 THEN 30630
                           call "NUMSMASH" (amount(temp%), 2,            ~
                                                   dedamt$(c%, temp%))
                        next temp%
                goto L30480

L30660: return

L30690:     FMT XX(27),                  /* RECORD KEYS                */~
                CH(12),                  /* EARNINGS TYPE              */~
                XX(6),                   /* DEPARTMENT CODE, FLAGS     */~
                CH(6),                   /* UNITS OF MEASURE           */~
                XX(49),                  /* MIDDLE OF RECORD           */~
                8 * PD(14,4)             /* CURR, MTD, QTD, YTD AMTS   */~

L30760:     FMT XX(39),                  /* FIRST PART OF RECORD       */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                XX(65),                  /* MIDDLE OF RECORD           */~
                4*PD(14,4),              /* RECORD ACCUMULATORS        */~
                XX(8),                                                   ~
                8*PD(14,4)               /* RECORD ACCUMULATORS        */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40150          /* EMPLOYEE CODE    */
                  goto L40540

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40540:     accept                                                       ~
               at (01,02),                                               ~
                  "REVIEW EMPLOYEE ACCRUALS",                            ~
               at (01,54),                                               ~
                  "LAST EMPLOYEE:",                                      ~
               at (01,69), fac(hex(84)), lastemp$               , ch(12),~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYEE CODE",                                       ~
               at (06,30), fac(lfac$( 1)), empcode$             , ch(12),~
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

               if keyhit% <> 15 then L40890
                  call "PRNTSCRN"
L40860:           goto L40540

L40890:        if keyhit% <> 13 then return
                  call "MANUAL" ("PRLSHOW ")
                  goto L40860

        REM *************************************************************~
            *        S E T   U P   E A R N I N G S   S C R E E N        *~
            *                                                           *~
            * SET UP AND ADJUST EARNINGS SCREEN.  THIS WORKS WITH THE   *~
            * STANDARD TECHNIQUES FOR THE STUFF.                        *~
            *************************************************************

            deffn'203(screenline%, fieldnr%)
                  screen% = 1
                  goto L41075

            deffn'213(screenline%, fieldnr%)
                  screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L41080
                  goto L41075

L41075:           init(hex(84)) fac$()
L41080:           on fieldnr% gosub L41160,         /* CURRENT UNITS    */~
                                    L41160,         /* MTD     UNITS    */~
                                    L41160,         /* QTD     UNITS    */~
                                    L41160,         /* YTD     UNITS    */~
                                    L41160,         /* CURRENT AMOUNT   */~
                                    L41160,         /* MTD     AMOUNT   */~
                                    L41160,         /* QTD     AMOUNT   */~
                                    L41160          /* YTD     AMOUNT   */
                  goto L41180

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L41160:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L41180:     accept                                                       ~
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

               if keyhit% <> 13 then L41900
                  call "MANUAL" ("PRLSHOW ")
                  goto L41180

L41900:        if keyhit% <> 15 then L41920
                  call "PRNTSCRN"
                  goto L41180

L41920:        if screen% <> 2 then return
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
                  screen% = 1
                  goto L42085

            deffn'214(screenline%, fieldnr%)
                  screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L42090
                  goto L42085

L42085:           init(hex(84)) fac$()
L42090:           gosub L42150
                  goto L42170

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L42150:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L42170:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$(screen%, 1)     , ch(64),~
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
               keys (pfkeys$(screen%)),                                  ~
               key  (keyhit%)

               if keyhit% <> 13 then L42880
                  call "MANUAL" ("PRLSHOW ")
                  goto L42170

L42880:        if keyhit% <> 15 then L42900
                  call "PRNTSCRN"
                  goto L42170

L42900:        if screen% <> 2 then return
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
                      if f1%(14) <> 0 then L50120
                      errormsg$ = "Employee Not Set Up In Personnel File"
                      return
L50120:         gosub L30000
                if f1%(3) <> 0 then L50142
                         errormsg$ = "Employee Not In Payroll Master File"
                         return
L50142:         infomsg$ = "EMPLOYEE: " & name$(1) & " " & name$(2)  &   ~
                                                          " " & name$(3)
                         return clear all
                         goto L13000                /* EDIT EARNINGS    */

        REM *************************************************************~
            *           T E S T    E A R N I N G S   D A T A            *~
            *                                                           *~
            * TESTS EARNINGS DATA FOR VALIDITY.                         *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51100,         /* CURRENT UNITS    */~
                                    L51200,         /* QTD     UNITS    */~
                                    L51300,         /* QTD     UNITS    */~
                                    L51400,         /* YTD     UNITS    */~
                                    L51500,         /* CURRENT AMOUNT   */~
                                    L51600,         /* MTD     AMOUNT   */~
                                    L51700,         /* QTD     AMOUNT   */~
                                    L51800          /* YTD     AMOUNT   */
                     return
L51100:     REM TEST DATA FOR CURRENT UNITS
                if amount$(c%, 1) = " " then amount$(c%, 1) = "0"
                   call "NUMVALID" (amount$(c%, 1), err%, 2)
                   if err% = 0 then L51135
                      errormsg$ = "Invalid Entry For Current Units: " &  ~
                                         amount$(c%, 1)
                      return
L51135:            convert amount$(c%, 1) to amount
                   if amount >= 0 then return
                      errormsg$ = "Current Units Must Be Nonnegative: "  ~
                                         & amount$(c%, 1)
                      return
L51200:     REM TEST DATA FOR MTD     UNITS
                if amount$(c%, 2) = " " then amount$(c%, 2) = "0"
                   call "NUMVALID" (amount$(c%, 2), err%, 2)
                   if err% = 0 then L51235
                      errormsg$ = "Invalid Entry For MTD Units: " &      ~
                                         amount$(c%, 2)
                      return
L51235:            convert amount$(c%, 2) to amount
                   if amount >= 0 then return
                      errormsg$ = "MTD Units Must Be Nonnegative: "      ~
                                         & amount$(c%, 2)
                      return
L51300:     REM TEST DATA FOR QTD     UNITS
                if amount$(c%, 3) = " " then amount$(c%, 3) = "0"
                   call "NUMVALID" (amount$(c%, 3), err%, 2)
                   if err% = 0 then L51335
                      errormsg$ = "Invalid Entry For QTD Units: " &      ~
                                         amount$(c%, 3)
                      return
L51335:            convert amount$(c%, 3) to amount
                   if amount >= 0 then return
                      errormsg$ = "QTD Units Must Be Nonnegative: "      ~
                                         & amount$(c%, 3)
                      return
L51400:     REM TEST DATA FOR YTD     UNITS
                if amount$(c%, 4) = " " then amount$(c%, 4) = "0"
                   call "NUMVALID" (amount$(c%, 4), err%, 2)
                   if err% = 0 then L51435
                      errormsg$ = "Invalid Entry For YTD Units: " &      ~
                                         amount$(c%, 4)
                      return
L51435:            convert amount$(c%, 4) to amount
                   if amount >= 0 then return
                      errormsg$ = "YTD Units Must Be Nonnegative: "      ~
                                         & amount$(c%, 4)
                      return
L51500:     REM TEST DATA FOR CURRENT AMOUNT
                if amount$(c%, 5) = " " then amount$(c%, 5) = "0"
                   call "NUMVALID" (amount$(c%, 5), err%, 2)
                   if err% = 0 then L51535
                      errormsg$ = "Invalid Entry For Current Amount: " & ~
                                         amount$(c%, 5)
                      return
L51535:            convert amount$(c%, 5) to amount
                   if amount >= 0 then return
                      errormsg$ = "Current Amount Must Be Nonnegative: " ~
                                         & amount$(c%, 5)
                      return
L51600:     REM TEST DATA FOR MTD     AMOUNT
                if amount$(c%, 6) = " " then amount$(c%, 6) = "0"
                   call "NUMVALID" (amount$(c%, 6), err%, 2)
                   if err% = 0 then L51635
                      errormsg$ = "Invalid Entry For MTD Amount: " &     ~
                                         amount$(c%, 6)
                      return
L51635:            convert amount$(c%, 6) to amount
                   if amount >= 0 then return
                      errormsg$ = "MTD Amount Must Be Nonnegative: "     ~
                                         & amount$(c%, 6)
                      return
L51700:     REM TEST DATA FOR QTD     AMOUNT
                if amount$(c%, 7) = " " then amount$(c%, 7) = "0"
                   call "NUMVALID" (amount$(c%, 7), err%, 2)
                   if err% = 0 then L51735
                      errormsg$ = "Invalid Entry For QTD Amount: " &     ~
                                         amount$(c%, 7)
                      return
L51735:            convert amount$(c%, 7) to amount
                   if amount >= 0 then return
                      errormsg$ = "QTD Amount Must Be Nonnegative: "     ~
                                         & amount$(c%, 7)
                      return
L51800:     REM TEST DATA FOR YTD     AMOUNT
                if amount$(c%, 8) = " " then amount$(c%, 8) = "0"
                   call "NUMVALID" (amount$(c%, 8), err%, 2)
                   if err% = 0 then L51835
                      errormsg$ = "Invalid Entry For YTD Amount: " &     ~
                                         amount$(c%, 8)
                      return
L51835:            convert amount$(c%, 8) to amount
                   if amount >= 0 then return
                      errormsg$ = "YTD Amount Must Be Nonnegative: "     ~
                                         & amount$(c%, 8)
                      return

        REM *************************************************************~
            *          T E S T   D E D U C T I O N S   D A T A          *~
            *                                                           *~
            * TESTS DATA FOR DEDUCTIONS AMOUNTS.  THIS IS A LOT OF      *~
            * REPETITIVE TESTING.                                       *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52100,         /* CURRENT AMOUNT   */~
                                    L52200,         /* MTD AMOUNT       */~
                                    L52300,         /* QTD AMOUNT       */~
                                    L52400          /* YTD AMOUNT       */
                     return
L52100:     REM TEST DATA FOR CURRENT AMOUNT
                if dedamt$(c%, 1) = " " then dedamt$(c%, 1) = "0"
                   call "NUMVALID" (dedamt$(c%, 1), err%, 2)
                   if err% = 0 then L52135
                      errormsg$ = "Invalid Entry For Current Amount: " & ~
                                         dedamt$(c%, 1)
                      return
L52135:            convert dedamt$(c%, 1) to amount
                   if amount >= 0 then return
                      errormsg$ = "Current Amount Must Be Nonnegative: " ~
                                         & dedamt$(c%, 1)
                      return
L52200:     REM TEST DATA FOR MTD     AMOUNT
                if dedamt$(c%, 2) = " " then dedamt$(c%, 2) = "0"
                   call "NUMVALID" (dedamt$(c%, 2), err%, 2)
                   if err% = 0 then L52235
                      errormsg$ = "Invalid Entry For MTD Amount: " &     ~
                                         dedamt$(c%, 2)
                      return
L52235:            convert dedamt$(c%, 2) to amount
                   if amount >= 0 then return
                      errormsg$ = "MTD Amount Must Be Nonnegative: "     ~
                                         & dedamt$(c%, 2)
                      return
L52300:     REM TEST DATA FOR QTD     AMOUNT
                if dedamt$(c%, 3) = " " then dedamt$(c%, 3) = "0"
                   call "NUMVALID" (dedamt$(c%, 3), err%, 2)
                   if err% = 0 then L52335
                      errormsg$ = "Invalid Entry For QTD Amount: " &     ~
                                         dedamt$(c%, 3)
                      return
L52335:            convert dedamt$(c%, 3) to amount
                   if amount >= 0 then return
                      errormsg$ = "QTD Amount Must Be Nonnegative: "     ~
                                         & dedamt$(c%, 3)
                      return
L52400:     REM TEST DATA FOR YTD     AMOUNT
                if dedamt$(c%, 4) = " " then dedamt$(c%, 4) = "0"
                   call "NUMVALID" (dedamt$(c%, 4), err%, 2)
                   if err% = 0 then L52435
                      errormsg$ = "Invalid Entry For YTD Amount: " &     ~
                                         dedamt$(c%, 4)
                      return
L52435:            convert dedamt$(c%, 4) to amount
                   if amount >= 0 then return
                      errormsg$ = "YTD Amount Must Be Nonnegative: "     ~
                                         & dedamt$(c%, 4)
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
