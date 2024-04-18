        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD   DDDD   U   U   CCC   PPPP    *~
            *  P   P  R   R  L      D   D  D   D  U   U  C   C  P   P   *~
            *  PPPP   RRRR   L      D   D  D   D  U   U  C      PPPP    *~
            *  P      R   R  L      D   D  D   D  U   U  C   C  P       *~
            *  P      R   R  LLLLL  DDDD   DDDD    UUU    CCC   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDDUCP - PRINTS A STANDARD FOR REPORT FOR ANY DEDUCTION *~
            *            DEFINED AS 'LIKEFICA' IN PRLDDT.               *~
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
            * 04/28/86 ! MODIFIED REPORT AND CONFORMED TO STDS.   ! SGA *~
            * 05/31/88 ! EXPANDED TOTALS BY 1 DIGIT               ! RJM *~
            * 03/21/89 ! NOW INCLUDES EMP'S W/ QTD EARNINGS = 0   ! RJM *~
            *          !   BUT EXCLUDES IF QTD = 0 AND YTD = 0    !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            title$79,                    /* KEYS ACTIVE                */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* SCREEN DATE FOR DISPLAY    */~
            deduc$(80)6,                 /* METHOD OF DEDUCTION        */~
            descr$(80)25,                /* DESCRIPTION OF DEDUCTION   */~
            descr$30,                    /* DESCRIPTION OF DEDUCTION   */~
            depart$4,                    /* DEPARTMENT                 */~
            depart$(2)4,                 /* DEPARTMENT PRINT RANGE     */~
            edtmessage$79,               /* EDIT MESSAGE FIELD         */~
            emp$(2)12,                   /* EMPLOYEE PRINT RANGE       */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(80)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            flag$(80)1,                  /* INPUT VARIABLE FOR SCREEN 1*/~
            fname$10,                    /* DUDS FIRST NAME            */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INPUT MESSAGE TEXT         */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            printmsg$87,                 /* Text for Print             */~
            heading$45,                  /* DATE & TIME FOR REPORT     */~
            limitmsg$30,                 /* DEDUCTION CUTOFF           */~
            lname$15,                    /* LAST NAME                  */~
            method$6,                    /* METHOD OF DEDUCTION CODE   */~
            mname$1,                     /* MIDDLE NAME                */~
            name$30,                     /* FORMATED NAME              */~
            plowkey$50,                  /* FOR PLOWS                  */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            rate$10,                     /* TAX RATE (FORMATTED)       */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            screentitle1$79,             /* SCREEN TITLE               */~
            screentitle2$79,             /* SCREEN TITLE               */~
            ssn$11,                      /* SOCIAL SECURITY NUMBER     */~
            total(6)                     /* PRINT TOTALS               */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************
            mat f2% = con



        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! EMPMASTR ! Employee master file                     *~
            * #02 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #03 ! EMPEARN1 ! Employee earnings file                   *~
            * #04 ! EMPDEDXN ! Employee deduction file                  *~
            * #05 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
            *************************************************************

            select #01, "EMPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  136,                                  ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   70, keylen =   1, dup     ~

            select #02, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  950,                                  ~
                        keypos =   39, keylen =  12,                     ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50          ~

            select #03, "EMPEARN1",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  200,                                  ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  28          ~

            select #04, "EMPDEDXN",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  18, dup     ~

            select #05, "PRLDDT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1, keylen = 6


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Linking To Data Base To Print Qtrly Reports ~
        ~For FICA Type Deductions")

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, " ")

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

            prgm$ = "PRLDDUCP"
            rptid$ = "PRL007"
            call "COMPNAME" (20%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,61,10) = "PRLDDUCP: "
            str(prgmid$,71,8) = str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM FOR DEDUCTION DEFINITIONS.        *~
            *************************************************************

        inputmode
            init(" ")inpmessage$, errormsg$, emp$(), depart$(), method$, ~
                     descr$, flag$()
            line% = 0

L10160:         gosub'201(0%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then line% = 0
                      if keyhit%  =  5 and maxlines% > 32 then line% = 32
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10160
                gosub'151(1%)
                      if errormsg$ <> " " then L10160

            for fieldnr% = 1 to 4
                gosub'162(fieldnr%)
                      if enabled% =  0 then L10330
L10280:         gosub'202(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then inputmode
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
                  if keyhit%  = 16 then goto  print_it
                  if keyhit% <>  0 then       L11230
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 4 then edtpg2

L11430:     gosub'212(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11430
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11430
            goto edtpg2

        REM *************************************************************~
            *                    P R I N T   D A T A                    *~
            *                                                           *~
            * PRINTS THE REPORT...                                      *~
            *************************************************************

        print_it
            if emp$(1) <> "ALL" then L15110
                emp$(1) = all(hex(00))
                emp$(2) = all(hex(ff))
                goto L15130
L15110:     if emp$(2) = " " then emp$(2) = emp$(1)

L15130:     if depart$(1) <> "ALL" then L15170
                depart$(1) = all(hex(00))
                depart$(2) = all(hex(ff))
                goto L15180
L15170:     if depart$(2) = " " then depart$(2) = depart$(1)
L15180:     readkey$ = emp$(1)
            page%, count% = 0
            line% = 99999
            mat total = zer
            inpmessage$ = "QUARTERLY " & descr$ & " REPORT"
            call "STRING" addr("CT", inpmessage$, 79%)
            limitmsg$ = " "
            if limit > 9e8 then L15270
                put limitmsg$, using L15260, limit
L15260:         %MAXIMUM WAGE LIMIT: $#####.##
            call "STRING" addr("CT", limitmsg$, 30%)
L15270:     call "SHOSTAT" ("Printing Requested Report...")
            select printer(132)

L15300:     gosub next_employee
                gosub get_his_gross
        REM     IF ABS(QTDGROSS) < .0001 THEN 15300 /* No QTD Then Skip*/
                if abs(qtdgross) < .0001 and abs(gross) < .0001          ~
                     then L15300         /* No QTD and No YTD Then Skip */
                gosub get_deduction
                if found% = 0 then L15300 /* Employee Doesn't Take This */
                excess = round((qtdgross - qtdsub), 2)
                gosub print_line
                count% = count% + 1
                total(1) = round(total(1) + gross , 2)
                total(2) = round(total(2) + ytdsub, 2)
                total(3) = round(total(3) + qtdgross, 2)
                total(4) = round(total(4) + qtdsub, 2)
                total(5) = round(total(5) + excess, 2)
               total(6)=round(total(6)+amount,2) /*SAVE THIS TO COMPARE*/
            goto L15300                         /* TO CALCULATED TOTAL  */
                                               /* TAX. (AMOUNT IS THE  */
        print_line                             /* $ ACTUALLY DEDUCTED) */
            gosub form_control
            print using L15510, ssn$, name$, empcode$, gross, ytdsub,     ~
                                             qtdgross, qtdsub, excess
            return

L15510:   %!########### !############################## !############ !-#~
        ~###,###.## !-####,###.## !-####,###.## !-####,###.## !-####,###.#~
        ~# !
        form_control
            line% = line% + 1
            if line% < 62 then return
            if page% <> 0 then print using L15760
            print page
            page% = page% + 1
            call "DATE" addr ("HD", heading$)
            print using L15700, date$, cmpname$, page%
            print using L15730, prgm$, inpmessage$, rptid$
            print using L15750, limitmsg$
            print using L15760
            print using L15790
            print using L15820
            print using L15760
            line% = 7
        return

L15700: %########                          ##############################~
        ~##############################                        PAGE: #####~
        ~#
L15730: %########                 #######################################~
        ~########################################            REPORT: #####~
        ~#
L15750: %                                                 ###############~
        ~################

L15760: %+------------+-------------------------------+-------------+----~
        ~---------+-------------+-------------+-------------+-------------~
        ~+

L15790: %!            !                               !EMPLOYEE     !    ~
        ~YTD      !    YTD      !    QTD      !    QTD      !   EXCESS    ~
        ~!

L15820: %!SS NUMBER   !EMPLOYEE NAME                  !CODE         ! GRO~
        ~SS WAGE  !  SUBJECT    ! GROSS WAGE  !  SUBJECT    !THIS QUARTER ~
        ~!

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   S E C O N D   P A G E  *~
            *                                                           *~
            * DEFAULT/ENABLE FOR THE SECOND PAGE.  THIS ROUTINE HELPS   *~
            *************************************************************

            deffn'162(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20170,         /* STARTING EMPLOYEE*/~
                                    L20210,         /* ENDING EMPLOYEE  */~
                                    L20260,         /* STARTING DEPARTME*/~
                                    L20310          /* ENDING DEPARTMENT*/
                    return
L20170:     REM DEFAULT/ENABLE FOR START EMPLOYEE
                   emp$(1) = "ALL"
                   enabled% = 1
                   return

L20210:     REM DEFAULT/ENABLE FOR END EMPLOYEE
                   enabled% = 1
                   return

L20260:     REM DEFAULT/ENABLE FOR START DEPARTMENT
                   depart$(1) = "ALL"
                   enabled% = 1
                   return

L20310:     REM DEFAULT/ENABLE FOR END DEPARTMENT
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

            keyhit1% = 0%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

        REM *************************************************************~
            *        G E T   T H E   N E X T   E M P L O Y E E          *~
            *                                                           *~
            * PLOWS EMPMASTR & PERMASTR, CHECKS RANGES TO SEE WHO GOES. *~
            *************************************************************

        next_employee
L30070:     call "PLOWNEXT" (#1, readkey$, 0%, f1%(1))
                if f1%(1) = 0 then done
            call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0 then L30070
            if str(readkey$,,12) > emp$(2) then done
            get #1, using L30130, depart$
L30130:     FMT XX(91), CH(04)
            if depart$ > depart$(2) or depart$ < depart$(1) then L30070
            get #2, using L30160, lname$, fname$, mname$, ssn$, empcode$
L30160:     FMT XX(1), CH(15), CH(10), CH(1), CH(11), CH(12)
            name$ = lname$ & ", " & fname$ & " " & mname$
            if mname$ <> " " then name$ = name$ & "."
        return

        done
            return clear all
            if page% = 0 then L30390
            convert rate to rate$, pic(#####.####)
            line% = line% + 1
            print using L15760
            gosub form_control
            print using L30420, total(1), total(2), total(3), total(4),   ~
                                                             total(5)
            line% = line% + 1
            print using L30480
            printmsg$ = "CALCULATED QUARTER TO DATE " & descr$ &         ~
                        " USING A RATE OF " & rate$ & "%"
            call "STRING" addr("RJ", printmsg$, 87%)
            gosub form_control
            print using L30450, count%, printmsg$,                        ~
                                            round((rate/100)*total(4), 2)
            print using L30510  /* PRINT TAGLINE */
            if round(total(6),0) <> round((rate/100)*total(4) ,0) then   ~
                print using L30540, total(6)
L30390:     close printer
            goto inputmode

L30420:   %!             TOTALS ===>                                  !##~
        ~#######.## !#########.## !#########.## !#########.## !#########.#~
        ~# !

L30450:   %!EMPLOYEE COUNT:#####  #######################################~
        ~################################################      !$###,###.#~
        ~# !

L30480: %+----------------------------------------------------------+----~
        ~---------+-------------+-------------+-------------+-------------~
        ~+

L30510: %+---------------------------------------------------------------~
        ~----------------------------------------------------+------------~
        ~+

L30540: %WARNING --- THE CALCULATED AMOUNT ABOVE DOESN'T EQUAL THE TOTAL ~
        ~AMOUNT PAID BY/FOR THE EMPLOYEES ###,###.## (FROM ACCRUALS)

        REM *************************************************************~
            *           C A L C U L A T E   G R O S S   P A Y           *~
            *                                                           *~
            * PLOWS EARNINGS AND ADDS UP TO ARIVE AT GROSS.             *~
            *************************************************************

        get_his_gross
            plowkey$ = empcode$
            gross, qtdgross = 0

L31100:     call "PLOWNEXT" (#3, plowkey$, 12%, f1%(3))
                if f1%(3) = 0 then return
            get #3, using L31130, qtd, ytd
L31130:     FMT XX(140),PD(14,4), XX(8), PD(14,4)
            gross = round(gross + ytd, 2)
            qtdgross = round(qtdgross + qtd, 2)
            goto L31100

        REM *************************************************************~
            *         G E T   D E D U C T I O N   A M O U N T S         *~
            *                                                           *~
            * GETS THE YTD$ SUBJECT, QTD$ SUBJECT AND QTD DEDUCTION $   *~
            *************************************************************

        get_deduction
            plowkey$ = empcode$
            found%, ytdsub, qtdsub, amount = 0

L32100:     call "PLOWNEXT" (#4, plowkey$, 12%, f1%(4))
                if f1%(4) = 0 then return
            get #4, using L32130, temp$
L32130:     FMT XX(33), CH(6)
            if temp$ <> method$ then L32100

            found% = 1
            get #4, using L32180, amount, qtdsub, ytdsub
L32180:     FMT XX(132), PD(14,4), XX(56), PD(14,4), XX(8), PD(14,4)
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

L35110:     call "PLOWNEXT" (#5, readkey$, 0%, f1%(5))
                if f1%(5) = 0 then L35210

            get #5, using L35240, method$, routine$, descr$
            if routine$ = "LIKEFICA" then L35160
            if routine$ = "FICA" then L35160
            if routine$ = "FICAEXMP" then L35160
            goto L35110
L35160:        maxlines% = maxlines% + 1
               deduc$(maxlines%) = method$
               descr$(maxlines%) = descr$
               goto L35110

L35210:     if maxlines% = 0 then L65000
        return

L35240:     FMT CH(06),                  /* METHOD                     */~
                XX(96),                  /*                            */~
                XX(41),                  /*                            */~
                CH(8),                   /* ROUTINE NAME               */~
                XX(151),                 /* CONSTANT DESCRIPTIONS      */~
                XX(48),                  /* CONSTANT AMOUNTS           */~
                CH(30)                   /* extended description       */~

        REM *************************************************************~
            *    L O A D   D A T A   F O R   F I R S T   S C R E E N    *~
            *                                                           *~
            * LOADS ALL DEDUCTION FROM THE DDT FILE THAT HAVE AT LEAST  *~
            * ONE NON-BLANK CONSTANTS DESCRIPTION FIELD.                *~
            *************************************************************

        get_rate_and_limit

            call "READ100" (#5, method$, f1%(5))
                if f1%(5) = 0 then done
            get #5, using L36240, rate, limit, descr$
            if abs(limit) < .0001 then limit = 9e9
        return

L36240:     FMT XX(06),                  /* METHOD                     */~
                XX(96),                  /*                            */~
                XX(41),                  /*                            */~
                XX(9),                   /* ROUTINE NAME               */~
                XX(150),                 /* CONSTANT DESCRIPTIONS      */~
                2*PD(14,4),              /* RATE FOLLOWED BE LIMIT     */~
                XX(32),                                                  ~
                CH(30)

        REM *************************************************************~
            *           T A B U L A R   M O D E   S C R E E N           *~
            *                                                           *~
            * HANDLES THE INPUT OF FIELDS IN TABLE, IN EITHER INPUT,    *~
            * EDIT, INSERT, OR DELETE STYLE.                            *~
            *************************************************************

            deffn'201(fieldnr%)                    /* EDIT MODE        */

                  screentitle1$="Deduction Report Selection"
                  str(screentitle1$,64,6) = "Today:"
                  str(screentitle1$,71,8) = date$
                  str(screentitle2$,61,18) = str(prgmid$,61,18)

            init (hex(8c)) fac$()
            init (hex(81)) str(fac$(),,maxlines%)

            infomsg$ = "Place a nonblank character in the box to print th~
        ~e deduction report."

L40075:     accept                                                       ~
               at (01,02), fac(hex(8c)),     screentitle1$      , ch(79),~
               at (02,02), fac(hex(ac)),     screentitle2$      , ch(79),~
               at (04,02), fac(hex(94)),     errormsg$          , ch(60),~
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
                                                                         ~
               at (21,02), fac(hex(a4)),     infomsg$           , ch(79),~
               at (22,20),                                               ~
                  "(4)Previous",                                         ~
               at (22,32),                                               ~
                  "(5)Next",                                             ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0004050d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40800
                  call "MANUAL" ("PRLDDUCP")
                  goto L40075

L40800:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40075

        REM *************************************************************~
            *             I N P U T   S E C O N D   P A G E             *~
            *                                                           *~
            * INPUTS THE SECOND PAGE OF THE HEADER                      *~
            *************************************************************

            deffn'202(fieldnr%)
                  screentitle1$="Print Quarterly and Year to Date Deducti~
        ~on Report"
                  str(screentitle1$,64,6) = "Today:"
                  str(screentitle1$,71,8) = date$
                  screentitle2$ = "For Deduction:"
                  str(screentitle2$,16,30) = descr$
                  str(screentitle2$,61,18) = str(prgmid$,61,18)

                  init(hex(84)) fac$()
                  on fieldnr% gosub L41140,         /* STARTING EMPLOYEE*/~
                                    L41140,         /* ENDING EMPLOYEE  */~
                                    L41140,         /* STARTING DEPARTME*/~
                                    L41140          /* ENDING DEPARTMENT*/
                     goto L41240

L41140:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02), fac(hex(8c)), screentitle1$          , ch(79),~
               at (02,02), fac(hex(ac)), screentitle2$          , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "STARTING EMPLOYEE ",                         ~
               at (06,25), fac(fac$(1)), emp$(1)                , ch(12),~
               at (07,02), "ENDING EMPLOYEE ",                           ~
               at (07,25), fac(fac$(2)), emp$(2)                , ch(12),~
                                                                         ~
               at (08,02), "STARTING DEPARTMENT",                        ~
               at (08,25), fac(fac$(3)), depart$(1)             , ch(04),~
               at (09,02), "ENDING DEPARTMENT",                          ~
               at (09,25), fac(fac$(4)), depart$(2)             , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),  inpmessage$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Return",                                          ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41600
                  call "MANUAL" ("PRLDDUCP")
                  goto L41240

L41600:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41240

        REM *************************************************************~
            *             E D I T    S E C O N D   P A G E              *~
            *                                                           *~
            * INPUTS THE SECOND PAGE OF THE HEADER                      *~
            *************************************************************

            deffn'212(fieldnr%)
                  init(hex(84)) fac$()
                  on fieldnr% gosub L42140,         /* STARTING EMPLOYEE*/~
                                    L42140,         /* ENDING EMPLOYEE  */~
                                    L42140,         /* STARTING DEPARTME*/~
                                    L42140          /* ENDING DEPARTMENT*/
                     goto L42240

L42140:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L42240:     accept                                                       ~
               at (01,02), fac(hex(8c)), screentitle1$          , ch(79),~
               at (02,02), fac(hex(ac)), screentitle2$          , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "STARTING EMPLOYEE ",                         ~
               at (06,25), fac(fac$(1)), emp$(1)                , ch(12),~
               at (07,02), "ENDING EMPLOYEE ",                           ~
               at (07,25), fac(fac$(2)), emp$(2)                , ch(12),~
                                                                         ~
               at (08,02), "STARTING DEPARTMENT",                        ~
               at (08,25), fac(fac$(3)), depart$(1)             , ch(04),~
               at (09,02), "ENDING DEPARTMENT",                          ~
               at (09,25), fac(fac$(4)), depart$(2)             , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),  edtmessage$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Print Report",                                    ~
                                                                         ~
               keys(hex(00010f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 15 then L42590
                  call "PRNTSCRN"
                  goto L42240

L42590:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
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
                gosub get_rate_and_limit
                if f1%(5) <> 0 then errormsg$ = " " /* TRICKY, TRICKY */
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
                  on fieldnr% gosub L51150,         /* START EMPLOYEE   */~
                                    L51190,         /* END EMPLOYEE     */~
                                    L51240,         /* START DEPARTMENT */~
                                    L51280          /* END DEPARTMENT   */
                    return
L51150:     REM TEST DATA FOR START EMPLOYEE
                if emp$(1) = "ALL" then fieldnr% = fieldnr% + 1
                return

L51190:     REM TEST DATA FOR END EMPLOYEE
                if emp$(2) >= emp$(1) or emp$(2) = " " then return
                errormsg$ = "MUST BE GREATER THEN THE STARTING EMPLOYEE #"
                return

L51240:     REM TEST DATA FOR START DEPARTMENT
                if depart$(1) = "ALL" then fieldnr% = fieldnr% + 1
                return

L51280:     REM TEST DATA FOR END DEPARTMENT
                if depart$(2) >= depart$(1) or depart$(2)=" " then return
                errormsg$ = "MUST BE GREATER THEN THE STARTING DEPARTMENT"
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
