        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  EEEEE  M   M  PPPP    AAA    CCC   IIIII  N   N  PPPP    *~
            *  E      MM MM  P   P  A   A  C   C    I    NN  N  P   P   *~
            *  EEEE   M M M  PPPP   AAAAA  C        I    N N N  PPPP    *~
            *  E      M   M  P      A   A  C   C    I    N  NN  P       *~
            *  EEEEE  M   M  P      A   A   CCC   IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EMPACINP - DIRECTLY MANAGE EMPLOYEE SICK AND VACATION     *~
            *            ACCRUAL RECORDS.                               *~
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
            * 04/09/84 ! ORIGINAL                                 ! HES *~
            * 03/21/86 ! Added Report For Ledger Balances and     !     *~
            *          ! Conformed Screen and Report to Standards ! SGA *~
            * 02/17/92 ! PRR 12226  Fixed remarks for EMPACRLS.   ! JDH *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.  Removed old extra  !     *~
            *          !  defination of CMS2V$ @ 1934.            !     *~
            * 08/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            accrued$10,                  /* TOTAL UNITS ACCRUED        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* BLANK LINE FOR SCREEN      */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            curbal$10,                   /* CURRENT UNITS BALANCE      */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            curvalue$10,                 /* CURRENT ESTIMATED VALUE ($)*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            emp$12,                      /* EMPLOYEE CODE              */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            firstcode$12,                /* FIRST EMPLOYEE CODE        */~
            lastadded$10,                /* LAST UNITS ADDED BY SYSTEM */~
            lastcode$12,                 /* LAST EMPLOYEE CODE         */~
            lasttaken$10,                /* LAST UNITS TAKEN BY USER   */~
            lastuserid$3,                /* LAST USER TO MODIFY DIRCTLY*/~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            method$36,                   /* EMPLOYEE'S METHOD OF ACCRL */~
            name$(3)15,                  /* EMPLOYEE'S NAME            */~
            name$30,                     /* EMPLOYEE'S NAME            */~
            nonvested$10,                /* NON-VESTED UNITS ACCRUED   */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            sorv$1,                      /* 'S' = SICK, 'V' = VACATION */~
            sysadded$8,                  /* LAST DATE SYS ADDED UNITS  */~
            systaken$8,                  /* LAST DATE EMPL USED UNITS  */~
            taken$10,                    /* TOTAL UNITS USED           */~
            tcurbal$10,                  /* TOTAL CURRENT BALANCE      */~
            tdate$8, udate$8,            /* Temporary Date Variables   */~
            title$36,                    /* REPORT TITLE               */~
            testval$10,                  /* TOTAL ESTIMATED VALUE      */~
            tunitsadd$10,                /* TOTAL UNITS ADDED          */~
            tunitstaken$10,              /* TOTAL UNITS TAKEN          */~
            type$2,                      /* METHOD TYPE                */~
            userid$3,                    /* CURRENT USER               */~
            usermoddate$8,               /* LAST DATE MODIFIED DIRECTLY*/~
            vestdate$8,                  /* NEXT ROLL DATE FOR VESTING */~
            vestflag$1                   /* N=VEST AS ACCRUED,Y=ANNUALY*/

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #02 ! EMPMASTR ! Employee master file                     *~
            * #03 ! EMPACRLS ! employee sick & vacation accrual ledger  *~
            * #04 ! PRLACRLS ! Accrual methods (control records)        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  950,                                  ~
                        keypos =   39, keylen =  12,                     ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50          ~

            select #02, "EMPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  136,                                  ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   70, keylen =   1, dup     ~

            select #03, "EMPACRLS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1,    keylen = 13

            select #04, "PRLACRLS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 2


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 1),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 1), 100%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 1),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            prgm$ = "EMPACINP"
            rptid$ = "PRL001"
            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,62,9) = "EMPACINP:"
            str(prgmid$,72,8) = str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, emp$, name$(),name$, sorv$,~
                      curbal$, curvalue$, accrued$, taken$, nonvested$,  ~
                      lastadded$, lasttaken$, sysadded$, systaken$,      ~
                      usermoddate$, lastuserid$, method$, vestdate$
            modified% = 0

            for fieldnr% = 1 to 7
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10220
L10160:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then printinput
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
L10220:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
L11070:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 3 or fieldnr% > 7 then L11070

L11140:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
            modified% = 1
            goto L11070


        REM *************************************************************~
            *        P R I N T   M O D E   E D I T   R A N G E          *~
            *                                                           *~
            * MAIN PROGRAM FOR INPUTING RANGE OF EMPLOYEES TO PRINT.    *~
            *************************************************************
        printinput
            init(" ") errormsg$, firstcode$, lastcode$, thiscode$,       ~
                      blankline$
            firstcode$ = "ALL"

            for fieldnr% = 1 to 3
L12140:         gosub'202(fieldnr%)
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L12140
                gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12140
                next fieldnr%

        REM *************************************************************~
            *        P R I N T   M O D E   E D I T    R A N G E         *~
            *                                                           *~
            * MAIN PROGRAM FOR INPUTING RANGE OF EMPLOYEES TO PRINT.    *~
            *************************************************************
        printedit

L13130:         gosub'212(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L14000
                  if keyhit% <>  0 then       L13130
                fieldnr% = cursor%(1) - 5
                if fieldnr% <= 0 or fieldnr% > 3 then L13130

L13164:         gosub'212(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  0 then       L13164
                gosub'152(fieldnr%)
                  if errormsg$ <> " " then L13130
                goto printedit

L14000: REM *************************************************************~
            *        P R I N T   S E L E C T I O N   R O U T I N E      *~
            *                                                           *~
            *  ROUTINE FOR THE SELECTION AND PRINTING OF INFORMATION    *~
            *************************************************************

            if firstcode$ <> "ALL" then L14059
               init(hex(00)) firstcode$
               init(hex(ff)) lastcode$

            if lastcode$ <> " " then L14059
               lastcode$ = firstcode$

L14059:     firstcode$ = add(hex(ff))
            call "SHOSTAT" ("PRINTING VACATION AND SICK LEAVE METHODS")
            if sorv$ = "V" then title$ = "VACATION LEAVE LEDGER BALANCE R~
        ~EPORT"
            if sorv$ = "S" then title$ = "SICK LEAVE LEDGER BALANCE REPOR~
        ~T"
            gosub L21000

             REM RETURN FROM ROUTINE
                  close printer
                   goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            a,b,c,d,e,f,g = 0

            convert curbal$    to a, data goto L19100
L19100:     convert curvalue$  to b, data goto L19110
L19110:     convert accrued$   to c, data goto L19120
L19120:     convert taken$     to d, data goto L19130
L19130:     convert nonvested$ to e, data goto L19140
L19140:     convert lastadded$ to f, data goto L19150
L19150:     convert lasttaken$ to g, data goto L19170

L19170:     if usermoddate$ = "NEVER" then usermoddate$ = " "
            call "DATUNFMT" (usermoddate$)
            call "DATUNFMT" (sysadded$)
            call "DATUNFMT" (systaken$)
            if vestflag$ <> "Y" then vestdate$ = " "
            call "DATUNFMT" (vestdate$)

            if modified% = 0 then L19280
                usermoddate$ = date
                lastuserid$ = userid$

L19280:     call "READ101" (#3, str(emp$) & sorv$, f1%(3))
                if f1%(3) <> 0 then delete #3

            write #3,using L30380,emp$,sorv$, a, b, c, d, e, usermoddate$,~
                                 lastuserid$, sysadded$, f, systaken$, g,~
                                 vestdate$, " "

            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  inpmessage$ = " "
                  on fieldnr% gosub L20170,         /* EMPLOYEE CODE    */~
                                    L20220,         /* S OR V           */~
                                    L20250,         /* CURRENT BALANCE  */~
                                    L20300,         /* CURRENT EST VALUE*/~
                                    L20350,         /* NON-VESTED UNITS */~
                                    L20410,         /* TOTAL TO THE GOOD*/~
                                    L20440          /* TOTAL UNITS USED */
                     return
L20170:     REM DEFAULT/ENABLE FOR EMPLOYEE CODE
            inpmessage$ = "Enter blank or partial code to search the mast~
        ~er file."
                return

L20220:     REM DEFAULT/ENABLE FOR 'S' = SICK, 'V' = VACATION
                return

L20250:     REM DEFAULT/ENABLE FOR CURRENT UNITS BALANCE
            inpmessage$ = "This is the current accrued balance of unit av~
        ~ailable for the employee to use"
                return

L20300:     REM DEFAULT/ENABLE FOR CURRENT ESTIMATED VALUE ($)
            inpmessage$ = "This is the estimated dollar value of the unit~
        ~s available."
                return

L20350:     REM DEFAULT/ENABLE FOR NON-VESTED UNITS
            inpmessage$ = "These units become available on the empoyee's ~
        ~next anniversary (hire) date."
            if vestflag$ = "N" then enabled% = 0
                return

L20410:     REM DEFAULT/ENABLE FOR TOTAL UNITS ACCRUED
                return

L20440:     REM DEFAULT/ENABLE FOR TOTAL UNITS USED
                return

L21000: REM *************************************************************~
            *        P R I N T   A   R A N G E   O F   C O D E S        *~
            *                                                           *~
            * PRINT A RANGE OF DEPARTMENT CODES.                        *~
            *************************************************************

            thiscode$ = firstcode$
            page%, total%,tcurbal,testval,tunitsadd,tunitstaken = 0
            line% = 1000

L21100:     init(" ") fname$, mname$, lname$, accrued$, taken$, curbal$, ~
                      curvalue$, type$, emp$, sysadded$, systaken$
            a, b, c, d = 0

L21160:     call "PLOWNEXT" (#2, thiscode$, 0%, f1%(2))
                 if f1%(2) = 0 then L21300
                 if thiscode$ > lastcode$ then L21300
                 if sorv$ = "S" then L21200
                 get #2, using L21190, type$
L21190:          FMT XX(82), CH(2)
                 if type$ = " "  then L21160
                 goto L21225
L21200:          if sorv$ <> "S" then L21160
                 get #2, using L21220, type$
L21220:          FMT XX(84), CH(2)
L21225:     call "READ100" (#1, thiscode$, f1%(1))
                 if f1%(1) = 0 then L21100

            gosub L32000                            /* GET TITLE RECORD */
            tcurbal = tcurbal + a
            testval = testval + b
            tunitsadd = tunitsadd + c
            tunitstaken = tunitstaken + d
            gosub L21360                            /* PAGE HEADER CHECK*/
            print using L21630, emp$, fname$, mname$, lname$, sorv$,      ~
                               type$,accrued$,taken$, curbal$, curvalue$,~
                               sysadded$, systaken$
            total% = total% + 1
            call "CONVERT" (tcurbal, 2.2, tcurbal$)
            call "CONVERT" (testval, 2.2, testval$)
            call "CONVERT" (tunitsadd, 2.2, tunitsadd$)
            call "CONVERT" (tunitstaken, 2.2, tunitstaken$)
            goto L21100

L21300:     REM END ROUTINE--PRINT STATS, AND RETURN TO CALLER.
                if total% = 0 then return
                   print using L21655
                   print using L21660, total% , tunitsadd$, tunitstaken$, ~
                                      tcurbal$, testval$
                   print using L21658
                   return

L21360:     select printer(132)
            line% = line%+1
            if line% < 60 then return
               print page
               page% = page% + 1
               print using L21510, date$, cmpname$, page%
               print using L21520, prgm$, title$, rptid$
               print
               print using L21540
               print using L21570
               print using L21600
               print using L21540
               line% = 5
               return

L21510: %########                          ##############################~
        ~##############################                        PAGE: #####~
        ~#
L21520: %########                                      ##################~
        ~##################                                  REPORT: #####~
        ~#
L21540: %+------------+----------------------------+----+------+---------~
        ~-+----------+----------+----------+--------+--------+            ~

L21570: %!  EMPLOYEE  !                            !    !METHOD!   UNITS ~
        ~ !  UNITS   !  CURRENT !ESTIMATED !  DATE  !  DATE  !

L21600: %!    CODE    !            NAME            !TYPE! CODE !   ADDED ~
        ~ !  TAKEN   !  BALANCE !  VALUE   ! ADDED  ! TAKEN  !

L21630: % ############ ########## # ###############   #    ##   #########~
        ~# ########## ########## ########## ######## ########

L21655: %                                                       ---------~
        ~- ---------- ---------- ----------

L21658: %                                                       =========~
        ~= ========== ========== ==========
L21660: %  **** TOTAL FOR ##### EMPLOYEES LISTED *****          #########~
        ~# ########## ########## ##########

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode


L30000: REM *************************************************************~
            *           L O A D   T H E   R E C O R D                   *~
            *                                                           *~
            * LOADS AND FORMATS THE DATA.                               *~
            *************************************************************

            lastuserid$ = userid$
            usermoddate$ = date$
            lastadded$, lasttaken$ = "      0.00"

            call "DATEFMT" ( date,  0%, udate$)
            vestdate$ = hiredate$           /* FORMAT VESTING DATE INCASE */
            call "DATEFMT" (tdate$, 0%, vestdate$)
            str(vestdate$,1%,4%) = udate$   /* THIS IS A NEW ENTRY AND    */
            call "DATECONV" (vestdate$, v%)
            if vestdate$ > date then L30170 /* VESTING IS ANNUAL          */
            convert v% + 10000 to vestdate$, pic(########)
            call "DATEFMT" (vestdate$)

L30170:     call "READ100" (#3, str(emp$) & sorv$, f1%(3))
                if f1%(3) = 0 then return

            get #3,using L30380, emp$, sorv$, a, b, c, d, e, usermoddate$,~
                                lastuserid$, sysadded$, f, systaken$, g, ~
                                vestdate$

            call "CONVERT" (a, -2.4, curbal$)
            call "CONVERT" (b, -2.4, curvalue$)
            call "CONVERT" (c, -2.4, accrued$)
            call "CONVERT" (d, -2.4, taken$)
            call "CONVERT" (e, -2.4, nonvested$)
            call "CONVERT" (f, 2.4, lastadded$)
            call "CONVERT" (g, 2.4, lasttaken$)
            call "DATEFMT" (vestdate$)
            call "DATEFMT" (usermoddate$)
            call "DATEFMT" (sysadded$)
            call "DATEFMT" (systaken$)
            if usermoddate$ = " " or usermoddate$ = blankdate$ ~
                                then usermoddate$ = "NEVER"
        return

L30380: FMT                      /* FILE: EMPACRLS                     */~
            CH(12),              /* employee code                      */~
            CH(1),               /* 'S' FOR SICK, 'V' FOR VACATION     */~
            PD(14,4),            /* CURRENT BALANCE OF UNITS           */~
            PD(14,4),            /* ESTIMATED VALUE OF CURRENT UNITS   */~
            PD(14,4),            /* TOTAL UNITS ADDED TO DATE          */~
            PD(14,4),            /* TOTAL UNITS TAKEN TO DATE          */~
            PD(14,4),            /* PORTION OF UNITS BALANCE NON-VESTED*/~
            CH(6),               /* LAST DATE MODIFIED DIRECTLY        */~
            CH(3),               /* LAST USER TO DIRECTLY MODIFY RECRD */~
            CH(6),               /* LAST DATE SYSTEM ADDED UNITS       */~
            PD(14,4),            /* NUMBER OF UNITS LAST ADDED BY SYSTM*/~
            CH(6),               /* LAST DATE EMPLOYEE USED UNITS      */~
            PD(14,4),            /* NUMBER OF UNITS LAST USED BY EMPLYE*/~
            CH(6),               /* NEXT VESTING ROLL DATE             */~
            CH(104)              /* FILLER FOR REST OF RECORD          */

L32000: REM *************************************************************~
            *   G E T   A   R E C O R D   F O R   P R I N T   M O D E   *~
            *                                                           *~
            * GETS A RECORD FROM THE DEPARTMENT CODE FILE FOR PRINTING. *~
            *************************************************************

            get #1, using L32090, lname$, fname$, mname$, emp$
            goto L32200

L32090:     FMT XX( 1),                  /* STATUS INDICATOR           */~
                CH(15),                  /* LAST NAME                  */~
                CH(10),                  /* FIRST NAME                 */~
                CH( 1),                  /* MIDDLE INITIAL             */~
                XX(11),                  /* SKIP SOCIAL SECURITY NO.   */~
                CH(12)                   /* EMPLOYEE CODE              */

L32200:     call "READ100" (#3, str(emp$) & sorv$, f1%(3))
                if f1%(3) = 0 then return

            get #3,using L30380, emp$, sorv$, a, b, c, d, e, usermoddate$,~
                                lastuserid$, sysadded$, f, systaken$, g, ~
                                vestdate$

            call "CONVERT" (a, 2.2, curbal$)
            call "CONVERT" (b, 2.2, curvalue$)
            call "CONVERT" (c, 2.2, accrued$)
            call "CONVERT" (d, 2.2, taken$)
            call "CONVERT" (e, 2.2, nonvested$)
            call "CONVERT" (f, 2.2, lastadded$)
            call "CONVERT" (g, 2.2, lasttaken$)
            call "DATEFMT" (vestdate$)
            call "DATEFMT" (usermoddate$)
            call "DATEFMT" (sysadded$)
            call "DATEFMT" (systaken$)
            if usermoddate$ = " " or usermoddate$ = blankdate$ ~
                                then usermoddate$ = "NEVER"
        return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40200,         /* EMPLOYEE CODE    */~
                                    L40200,         /* S OR V           */~
                                    L40230,         /* CURRENT BALANCE  */~
                                    L40230,         /* CURRENT EST VALUE*/~
                                    L40230,         /* NON-VESTED UNITS */~
                                    L40230,         /* TOTAL ALL ACCRUED*/~
                                    L40230          /* TOTAL ALL USED   */
                     goto L40270

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40230:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Employee Sick and Vacation Accrual Ledger",    ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYEE CODE",                                       ~
               at (06,30), fac(lfac$( 1)), emp$                 , ch(12),~
               at (06,45), fac(hex(8c)),   name$                , ch(30),~
               at (07,02),                                               ~
                  "'S' = SICK, 'V' = VACATION",                          ~
               at (07,30), fac(lfac$( 2)), sorv$                , ch(01),~
               at (07,44), fac(hex(8c)),   method$              , ch(36),~
                                                                         ~
               at (09,02),                                               ~
                  "UNITS CURRENTLY AVAILABLE",                           ~
               at (09,30), fac(lfac$( 3)), curbal$              , ch(10),~
               at (10,02),                                               ~
                  "     LAST ESTIMATED VALUE",                           ~
               at (10,30), fac(lfac$( 4)), curvalue$            , ch(10),~
               at (11,02),                                               ~
                  "UNITS ACCRUED BUT NOT VESTED",                        ~
               at (11,31), fac(lfac$( 5)), nonvested$           , ch(10),~
               at (12,02),                                               ~
                  "TO DATE UNITS ACCRUED",                               ~
               at (12,30), fac(lfac$( 6)), accrued$             , ch(10),~
               at (13,02),                                               ~
                  "TO DATE UNITS USED",                                  ~
               at (13,30), fac(lfac$( 7)), taken$               , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,20),                                               ~
                  "(3)Print Methods",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0001030d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40800
                  call "MANUAL" ("EMPACINP")
                  goto L40270

L40800:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40270

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41100,         /* EMPLOYEE CODE    */~
                                    L41100,         /* S OR V           */~
                                    L41115,         /* CURRENT BALANCE  */~
                                    L41115,         /* CURRENT EST VALUE*/~
                                    L41115,         /* NON-VESTED UNITS */~
                                    L41115,         /* TOTAL ALL ACCRUED*/~
                                    L41115          /* TOTAL ALL USED   */
                     goto L41135

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41100:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41115:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41135:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Employee Sick and Vacation Accrual Ledger",    ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYEE CODE",                                       ~
               at (06,30), fac(lfac$( 1)), emp$                 , ch(12),~
               at (06,45), fac(hex(8c)),   name$                , ch(30),~
               at (07,02),                                               ~
                  "'S' = SICK, 'V' = VACATION",                          ~
               at (07,30), fac(lfac$( 2)), sorv$                , ch(01),~
               at (07,44), fac(hex(8c)),   method$              , ch(36),~
                                                                         ~
               at (09,02),                                               ~
                  "UNITS CURRENTLY AVAILABLE",                           ~
               at (09,30), fac(lfac$( 3)), curbal$              , ch(10),~
               at (10,02),                                               ~
                  "     LAST ESTIMATED VALUE",                           ~
               at (10,30), fac(lfac$( 4)), curvalue$            , ch(10),~
               at (11,02),                                               ~
                  "UNITS ACCRUED BUT NON-VESTED",                        ~
               at (11,31), fac(lfac$( 5)), nonvested$           , ch(10),~
               at (12,02),                                               ~
                  "TO DATE UNITS ACCRUED",                               ~
               at (12,30), fac(lfac$( 6)), accrued$             , ch(10),~
               at (13,02),                                               ~
                  "TO DATE UNITS USED",                                  ~
               at (13,30), fac(lfac$( 7)), taken$               , ch(10),~
                                                                         ~
               at (09,44), "--------- MODIFICATIONS LOG ---------",      ~
               at (10,44), "System last added units on :",               ~
               at (10,73), fac(hex(8c)), sysadded$              , ch(08),~
               at (11,64),             "units were added",               ~
               at (11,53), fac(hex(8c)), lastadded$             , ch(10),~
               at (12,44), "Employee last took units on:",               ~
               at (12,73), fac(hex(8c)), systaken$              , ch(08),~
               at (13,64),             "units were taken",               ~
               at (13,53), fac(hex(8c)), lasttaken$             , ch(10),~
                                                                         ~
               at (15,44), "Last DIRECTLY modified on :",                ~
               at (15,73), fac(hex(8c)), usermoddate$           , ch(08),~
               at (16,44), "By user :",                                  ~
               at (16,54), fac(hex(8c)), lastuserid$            , ch(03),~
               at (17,44), "Next vesting roll will be on",               ~
               at (17,73), fac(hex(8c)), vestdate$              , ch(08),~
               at (18,44), "------- LOG CAN NOT BE EDITED -------",      ~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41490
                  call "MANUAL" ("EMPACINP")
                  goto L41135

L41490:        if keyhit% <> 15 then L41510
                  call "PRNTSCRN"
                  goto L41135

L41510:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return



        REM *************************************************************~
            *            I N P U T   R A N G E   S C R E E N            *~
            *                                                           *~
            * INPUT VALUES FOR THE RANGE OF EMPLOYEE CODES TO PRINT.    *~
            *************************************************************

            deffn'202(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L42160,         /* FIRST EMPLOYEE   */~
                                    L42160,         /* LAST EMPLOYEE    */~
                                    L42160          /* SORT CODE        */
                     goto L42220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Vacation and Sick Leave Ledger Balances",       ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST EMPLOYEE CODE",                                 ~
               at (06,30), fac(lfac$(01)), firstcode$           , ch(12),~
               at (07,02),                                               ~
                  "LAST EMPLOYEE CODE",                                  ~
               at (07,30), fac(lfac$(02)), lastcode$            , ch(12),~
               at (08,02),                                               ~
                  "'V'ACATION OR 'S'ICK LEAVE?",                         ~
               at (08,30), fac(lfac$(03)), sorv$                , ch(01),~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "(1)Input Mode",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L42570
                call "MANUAL" ("EMPACINP")
                goto L42220

L42570:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *            E D I T   R A N G E   S C R E E N              *~
            *                                                           *~
            * INPUT VALUES FOR THE RANGE OF EMPLOYEE CODES TO PRINT.    *~
            *************************************************************

            deffn'212(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43160,         /* FIRST EMPLOYEE   */~
                                    L43160,         /* LAST EMPLOYEE    */~
                                    L43160          /* SORT CODE        */
                     goto L43230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43230:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Vacation and Sick Leave Ledger Balances",       ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST EMPLOYEE CODE",                                 ~
               at (06,30), fac(lfac$(01)), firstcode$           , ch(12),~
               at (07,02),                                               ~
                  "LAST EMPLOYEE CODE",                                  ~
               at (07,30), fac(lfac$(02)), lastcode$            , ch(12),~
               at (08,02),                                               ~
                  "'V'ACATION OR 'S'ICK LEAVE?",                         ~
               at (08,30), fac(lfac$(03)), sorv$                , ch(01),~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Print Report",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L43580
                call "MANUAL" ("EMPACINP")
                goto L43230

L43580:        if keyhit% <> 15 then L43620
                  call "PRNTSCRN"
                  goto L43230

L43620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* EMPLOYEE CODE    */~
                                    L50310,         /* S OR V           */~
                                    L50550,         /* CURRENT BALANCE  */~
                                    L50600,         /* CURRENT EST VALUE*/~
                                    L50650,         /* NON-VESTED UNITS */~
                                    L50700,         /* TOTAL ALL ACCRUED*/~
                                    L50750          /* TOTAL ALL USED   */
                     return
L50160:     REM TEST DATA FOR EMPLOYEE CODE
                call "GETEMPL" (#1, emp$, name$, 0%, f1%(1))
                     if f1%(1) <> 0 then L50220
                errormsg$="Employee Code Not In Personnel File: "&emp$
                return

L50220:         call "READ100" (#2, emp$, f1%(2))
                     if f1%(2) <> 0 then L50260
                errormsg$="Employee Not In Payroll Master File: "&emp$
                return
L50260:         get #1, using L50270, name$(3),name$(1),name$(2),hiredate$
L50270:                 FMT XX(1), CH(15), CH(10), CH(1), XX(362), CH(6)
                name$ = name$(1) & " " & name$(2) & ". " & name$(3)
                return

L50310:     REM TEST DATA FOR 'S' = SICK, 'V' = VACATION
            if pos("SV"=sorv$)= 0 then errormsg$="PLEASE ENTER 'S' OR 'V'"
            if errormsg$ <> " " then return
            get #2, using L50350, type$
L50350:     FMT XX(82), CH(2)
            if sorv$ <> "S" then L50400
            get #2, using L50380, type$
L50380:     FMT XX(84), CH(2)

L50400:     call "GETCODE" (#4, type$, str(method$,2), 0%, 0, f1%(4))
            if f1%(4) <> 0 then L50460
                errormsg$ = "EMPLOYEE ACCRUAL METHOD IN PAYROLL MASTER" &~
                                            " IS INVALID. EXIT AND FIX!!"
                return

L50460:     get #4, using L50470, vestflag$
L50470:     FMT XX(57), CH(1)
            gosub L30000
            if vestflag$ <> "Y" then vestdate$ = " * N/A *"
            if f1%(3) = 0 then return
                return clear
                return clear
                goto editmode

L50550:     REM TEST DATA FOR CURRENT UNITS BALANCE
            call "NUMTEST" (curbal$, -9e7, 9e7, errormsg$, 0.4, v)
                return

L50600:     REM TEST DATA FOR CURRENT ESTIMATED VALUE ($)
            call "NUMTEST" (curvalue$, -9e7, 9e7, errormsg$, 2.4, v)
                return

L50650:     REM TEST DATA FOR NON-VESTED UNITS
            call "NUMTEST" (nonvested$, 0, 9e7, errormsg$, 0.4, v)
                return

L50700:     REM TEST DATA FOR TOTAL UNITS ACCRUED
            call "NUMTEST" (accrued$, 0, 9e7, errormsg$, 0.4, v)
                return

L50750:     REM TEST DATA FOR TOTAL UNITS USED
            call "NUMTEST" (taken$, 0, 9e7, errormsg$, 0.4, v)
                return

        REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51070,         /* FIRST EMPLOYEE   */~
                                    L51170,         /* LAST EMPLOYEE    */~
                                    L51250          /* SORT VALUE       */
                     return

L51070:      REM HANDLES CASE FOR "ALL" EMPLOYEE CODES
                 if firstcode$ <> "ALL" then L51120
                    fieldnr% = 2
                    return
L51120:      REM HANDLES CASE FOR SINGLE CODE
                call "GETEMPL" (#1, firstcode$, name$, 0%, f1%(1))
                  if f1%(1) <> 0 then return
                  errormsg$ = "EMPLOYEE NOT ON FILE: " & firstcode$
                  return

L51170:      REM TESTS DATA FOR LAST EMPLOYEE SELECTED
                 if lastcode$ = firstcode$ then return
                 call "GETEMPL"(#1, lastcode$, name$, 0%,  f1%(1))
                   if f1%(1) <> 0 then L51240
                  errormsg$ = "EMPLOYEE NOT ON FILE: " & lastcode$
                  return

L51240:      REM HANDLES CASE FOR A RANGE OF CODES
                 if lastcode$ >= firstcode$ then return
                 errormsg$ = "ILLEGAL RANGE!  PLEASE RESPECIFY."
                  return

L51250:      REM TEST DATA FOR 'S' = SICK, 'V' = VACATION
            if pos("SV"=sorv$)= 0 then errormsg$="PLEASE ENTER 'S' OR 'V'"
                  return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("Releasing Files, One Moment Please")

            end
