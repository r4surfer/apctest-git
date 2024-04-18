        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  JJJJJ  BBBB   EEEEE  RRRR   N   N  DDDD   FFFFF  TTTTT   *~
            *    J    B   B  E      R   R  NN  N  D   D  F        T     *~
            *    J    BBBB   EEEE   RRRR   N N N  D   D  FFFF     T     *~
            *  J J    B   B  E      R   R  N  NN  D   D  F        T     *~
            *   J     BBBB   EEEEE  R   R  N   N  DDDD   F        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBERNDFT - SHOP FLOOR EARNINGS DEFAULT FILE SETUP AND     *~
            *            PRINT PROGRAM.  SETS UP EARNINGS CODE WHEN     *~
            *            PAYROLL NOT INSTALLED BUT SFC TIME CARD OPTION *~
            *            IS USED                                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/13/92 ! ORIGINAL (Largely Cloned from PRLEARND)  ! JBK *~
            * 12/21/93 ! Minor mods for UNIX printers.            ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        dim                                                              ~
            acct$16,                     /* ACCOUNT NUMBER FOR PRINT   */~
            acct$(100)16,                /* ACCOUNT NUMBER FOR INPUT   */~
            acctdescr$(100)32,           /* Account Descriptions       */~
            blankline$79,                /* BLANK LINE FOR PRINT SCREEN*/~
            catkey$7,                    /* Plow Key for Category      */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            catagory$4,                  /* CATAGORY   CODE THIS ENTRY */~
            catg$4,                      /* CATAGORY   FOR PRINT MODE  */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(15,5)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            firstcode$4,                 /* FIRST CATAGORY   TO PRINT  */~
            hdr$(5)30,                   /* Summary Screen Headings    */~
            i$(24)80,                    /* SCREEN IMAGE--NOT USED     */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            keydescr$78,                 /* Earn Type for Plow         */~
            lastcode$4,                  /* LAST CODE IN PRINT RANGE   */~
            lastcatg$4,                  /* LAST CATAGORY   INPUT      */~
            linenumber%(3),              /* LINE POINTER FOR PRINT MODE*/~
            linfac$(20)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            prtacct$16,                  /* ACCOUNT NUMBER TO PRINT    */~
            prtacctdescr$30,             /* DESCRIPTION OF ACCOUNT     */~
            prtcatg$4,                   /* CATAGORY   CODE FOR PRINTS */~
            prtfac$1,                    /* Print Screen FAC's         */~
            prtrate$10,                  /* RATE TO PRINT              */~
            prttype$12,                  /* DESCRIPTION OF EARNING     */~
            prtunits$6,                  /* UNITS TO PRINT (HRS,PCS...)*/~
            rate$(100)10,                /* RATES FOR EARNINGS         */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            rpttitle$80,                 /* REPORT TITLE               */~
            thiscode$50,                 /* CODE THIS CATG IN PRINTING */~
            time$8,                      /* System Time                */~
            tran$80,                     /* CURSOR==>FIELD TRAN FOR EDT*/~
            type$12,                     /* DESCRIPTION OF EARN FOR PRT*/~
            type$(100)12,                /* TYPE CODES FOR INPUT MODE  */~
            units$12,                    /* UNITS PER RATE FOR PRINT   */~
            units$(100)6,                /* UNITS PER RATE             */~
            userid$3                     /* USERID OF CURRENT USER.    */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! PRLEDFLT ! PAYROLL EARNINGS DEFAULT TABLE           *~
            *************************************************************

            select #2, "GLMAIN",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 9

            select #3, "PRLEDFLT",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 100,                                    ~
                       keypos = 1, keylen = 7

            call "PRLEXTSB" ("SFC",ret%)         /* Check for Payroll */
                if ret% = 99% then L65000         /* Its here, get out */

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3), 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZE VARIABLES NEEDED FOR SCREEN I/O, EDIT MODE,    *~
            * AND SYSTEM DATES.                                         *~
            *************************************************************

            call "EXTRACT" addr ("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)


            REM SET STRINGS FOR TABLE EDIT COMPUTATION.
            init(hex(00)) tran$
            init(hex(01)) str(tran$, 2, 12)
            init(hex(02)) str(tran$,15,  6)
            init(hex(03)) str(tran$,25, 10)
            init(hex(04)) str(tran$,36, 12)

            prgm$ = "JBERNDFT"
            rptid$ = "JB0011"
            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,62,9) = "JBERNDFT:"
            str(prgmid$,72,8) = str(cms2v$,1,8)
            rpttitle$ = "SHOP FLOOR CONTROL EARNINGS CATEGORIES"

            hdr$(1) = "Type"
            hdr$(2) = "Unit Desc"
            hdr$(3) = "Rate"
            hdr$(4) = "Expense Account"
            hdr$(5) = "Account Description"

L10000: REM *************************************************************~
            *         I N P U T   D E P A R T M E N T   C O D E         *~
            *                                                           *~
            * INPUT EARNINGS DEFAULT CATAGORY   CODE                    *~
            *************************************************************

        inputmode
            editmode% = 0
            init(" ") errormsg$, infomsg$, inpmessage$, blankline$,      ~
                      catagory$, rate$(), units$(), acct$(), type$(),    ~
                      acctdescr$()

            call "ALLFREE"

            for fieldnr% = 1 to 1
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10220
L10150:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then printmode
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10220:         next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUT LINE ITEMS MAIN PROGRAM.                            *~
            *************************************************************

            line%, maxlines%, screenline% = 0
            infomsg$, errormsg$ = " "

L11090:     screenline% = screenline% + 1
            if screenline% < 16 then L11130
               line% = line% + 15
               screenline% = 1
L11130:     c%, currentline% = line% + screenline%
            if currentline% > 100 then editmode

L11170:     for fieldnr% = 1 to 4
L11180:         gosub'163(fieldnr%)      /* ENABLE FIELDS FOR INPUT.   */
                      if enabled% =  0 then       L11300
L11200:         gosub'203(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L11280
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  4 then gosub lineabove
                      if keyhit%  = 16 and fieldnr% = 1 then editmode
                      goto L11180
L11280:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L11200
L11300:         next fieldnr%
                maxlines% = maxlines% + 1
                goto L11090

        REM *************************************************************~
            *      E D I T   H E A D E R   M A I N   P R O G R A M      *~
            *                                                           *~
            * EDITS HEADER OF THE CATAGORY   ENTRY.                     *~
            *************************************************************

        editmode
            editmode% = 1
            errormsg$, infomsg$, inpmessage$ = " "

L12100:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L13000
                  if keyhit%  = 12 then gosub delete_file
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L12100
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 1 then L12100

L12180:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L12180
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L12180
            goto L12100

L13000: REM *************************************************************~
            *  E D I T   L I N E   I T E M S   M A I N   P R O G R A M  *~
            *                                                           *~
            * EDITS LINE ITEMS.                                         *~
            * WE HAVE A SPECIAL PATCH FOR EDITING SINGLE FIELDS--WHEN   *~
            * THE FIELD IS EITHER 7, 8, 9, OR 10, THEN THE THING TESTS  *~
            * TO SEE IF THE DESCRIPTION IS BLANK.  IF DESCRIPTION BLANK,*~
            * THEN IT WON'T LET US EDIT THAT FIELD.                     *~
            *************************************************************

            line%, currentline%, screenline% = 0
L13105:     edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

L13130:     gosub'213(0%, 0%)
                  if keyhit%  =  0 then       L13320
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%-15)
                  if keyhit%  =  4 then line% = max(0,line%-14)
                  if keyhit%  =  5 then line% = min(line%+14,max(0,      ~
                                                maxlines%-15))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%-15))
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 16 then       datasave
                  goto L13105

L13320:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                fieldnr% = val(str(tran$,cursor%(2),1))
                if fieldnr% = 0% then L13130
                screenline% = cursor%(1)-5
                if screenline% < 1% or screenline% > 20% then L13130
                c%, currentline% = line% + screenline%
                if currentline% > maxlines% then L13130

L13390:         gosub'213(screenline%, fieldnr%)
                      if keyhit%  = 1 then gosub startover
                      if keyhit% <> 0 then L13390
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L13390
                goto L13130

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *                                                           *~
            * COLUMN ONE AND LINE ABOVE FUNCTION KEYS LOGIC IS          *~
            * IMPLEMENTED HERE.                                         *~
            *************************************************************

        columnone
            if fieldnr% = 1 then return
            init(" ") type$(c%), units$(c%), rate$(c%), acct$(c%),       ~
                      acctdescr$(c%), errormsg$, infomsg$
            return clear all
            goto L11170

        lineabove
            if currentline% = 1 then return
            on fieldnr% gosub L14260,               /* EARNINGS TYPE    */~
                              L14290,               /* UNITS DESCRIPTION*/~
                              L14300,               /* RATE PER UNIT    */~
                              L14310                /* ACCOUNT NUMBER   */
                    return
L14260:     type$     (c%) = type$     (c%-1): return
L14290:     units$    (c%) = units$    (c%-1): return
L14300:     rate$     (c%) = rate$     (c%-1): return
L14310:     acct$     (c%) = acct$     (c%-1): return
            acctdescr$(c%) = acctdescr$(c%-1): return

        REM *************************************************************~
            *                   I N S E R T   M O D E                   *~
            *                                                           *~
            * INSERT LOGIC HERE.                                        *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return         /* ARRAY FULL, CAN'T*/
            REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
L15090:         screenline% = cursor%(1)-5
                if screenline% < 1% or screenline% > 15% then return
                if line% + screenline% < maxlines% then L15120
                   screenline% = maxlines% - line% /* TO INS AT END    */
L15120:         if screenline% <> 15 then L15190    /* BOTTOM OF PAGE   */
                   line% = line% + 1
                   screenline% = screenline% - 1
                   goto L15190

L15190:         currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L15340
                for temp% = maxlines% to c% step -1
                    type$     (temp%+1) = type$     (temp%)
                    units$    (temp%+1) = units$    (temp%)
                    rate$     (temp%+1) = rate$     (temp%)
                    acct$     (temp%+1) = acct$     (temp%)
                    acctdescr$(temp%+1) = acctdescr$(temp%)
                    next temp%

L15340:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") type$(c%), units$(c%), rate$(c%), acct$(c%),   ~
                          acctdescr$(c%), infomsg$, errormsg$

            REM NOW INPUT THE LINE, MAKE SO WE CAN CANCEL OUT IF NECC
                infomsg$ = " "
                edtmessage$ = "Supply Requested Item and Press (ENTER) or~
        ~ PF(1) to Exit Insert Mode"
                for fieldnr% = 1 to 4
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L15520
L15460:             gosub'223(screenline%, fieldnr%)
                          if keyhit%  =  1 then L15580
                          if keyhit% <>  0 then L15460
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L15460
L15520:             next fieldnr%

                maxlines% = maxlines% + 1
                cursor%(1) = min(cursor%(1)+1, 20)
                goto L15090

L15580:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                gosub L15710              /* ACTUALLY DELETE @C%        */

                temp% = maxlines% + 1
                init(" ") type$(temp%), units$(temp%), rate$(temp%),     ~
                          acct$(temp%), acctdescr$(temp%), infomsg$,     ~
                          errormsg$

            if currentline% >= maxlines% and screenline% = 15            ~
               then line% = max(0%, line% - 1%)
            return

L15710:     for temp% = currentline% to maxlines%
                type$     (temp%) = type$     (temp%+1)
                units$    (temp%) = units$    (temp%+1)
                rate$     (temp%) = rate$     (temp%+1)
                acct$     (temp%) = acct$     (temp%+1)
                acctdescr$(temp%) = acctdescr$(temp%+1)
                next temp%

               if maxlines% = 0 then return
            return

        REM *************************************************************~
            *                  D E L E T E   L O G I C                  *~
            *                                                           *~
            * DELETE A LINE FROM THE TABLE.                             *~
            *************************************************************

        deletemode
            if maxlines% = 0 then return
            screenline% = cursor%(1)-5
            if screenline% < 1 then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return

            edtmessage$ = "Press (ENTER) to DELETE Flashing Line or PF(1)~
        ~ to Exit Delete"
L16130:     gosub'233(screenline%)
                  if keyhit%  =  1 then       return
                  if keyhit% <>  0 then       L16130

            c% = currentline%
            if currentline% < maxlines% then gosub L15710
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") type$(temp%), infomsg$, units$(temp%),             ~
                      rate$(temp%), acct$(temp%), acctdescr$(temp%),     ~
                      errormsg$

            maxlines% = maxlines% - 1
            if currentline% >= maxlines% and screenline% = 15            ~
               then line% = max(0%, line% - 1%)
               return

        REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * GETS RANGE OF ENTRIES TO PRINT FOR PRINT MODE, AND PLOWS  *~
            * THROUGH THE FILE, CALLING THE PRINT SUBROUTINE.           *~
            *************************************************************

        printmode
            init(" ") errormsg$, firstcode$, lastcode$, thiscode$,       ~
                      blankline$
            firstcode$ = "ALL"

L17120:     edtmessage$ = "Enter Category Code to Print or Range."
            gosub L42100
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L65000
                  if keyhit%  <> 0 then       L17120
            gosub L53000
                  if errormsg$ <> " " then L17120
L17190:     edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~Desired Value And Press (ENTER)."
            gosub L42205
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L17270
                  if keyhit%  <> 0 then       L17190
                  goto L17120

L17270:     REM PLOW ROUTINE FOR PRINTING EARNINGS CATAGORY   DEFAULT LIST
                select printer(134)
                page% = -1
                line% = 1000
                time$ = " "  :  call "TIME" (time$)
                call "SETPRNT" ("JB0011", " ", 0%, 0%)
                tagprinted% = 1
                if firstcode$ <> "ALL" then L17370
                     init (hex(00))  firstcode$
                     init (hex(ff))  lastcode$
L17370:         if firstcode$ <> hex(00000000) then firstcode$ =         ~
                                 firstcode$ addc hex(ffffffff)
                thiscode$ = firstcode$
                call "SHOSTAT" ("Printing Earnings Categories")

L17420:         call "PLOWNEXT" (#3, thiscode$, 0%, f1%(3))
                     if f1%(3) = 0 then L17500
                     if str(thiscode$,1,4) > lastcode$ then L17500
                catg$ = str(thiscode$, 1, 4)
                gosub L60000              /* LOAD AND PRINT THAT CATG.  */
                init(hex(ff)) str(thiscode$, 5)
                goto L17420

L17500:         REM RETURN FROM ROUTINE
                    if line% = 1000% then gosub L63000
                    print skip(2)
                    time$ = " "  :  call "TIME" (time$)
                    print using L64490, time$
                    close printer
                    call "SETPRNT" ("JB0011", " ", 0%, 1%)
                    goto inputmode

        REM *************************************************************~
            *                     S A V E   D A T A                     *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE, DELETING PREVIOUS VERSION.   *~
            *************************************************************

        datasave
            lastcatg$ = catagory$
            REM DELETE OLD BEFORE WRITING NEW.
                readkey$ = catagory$
                call "DELETE" (#3, readkey$, 4%)
            gosub L31000
            goto inputmode

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   F I R S T   P A G E *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE FIRST PAGE.  AT  *~
            * PRESENT, THIS ROUTINE DOES EXACTLY NOTHING.               *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* CATAGORY         */
                     return
L20100:     REM DEFAULT/ENABLE FOR EARNINGS TYPE
                enabled% = 1
                inpmessage$ = "Enter Category Code or leave blank to see ~
        ~codes on file."
                return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   L I N E   I T E M S *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR THE LINE ITEM FIELDS. *~
            * MOST OF THE FIELDS ARE DISABLED, BUT THIS CAN BE EASILY   *~
            * CHANGED.                                                  *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L21100,         /* EARNINGS TYPE    */~
                                    L21400,         /* UNITS            */~
                                    L21500,         /* RATE PER UNIT    */~
                                    L21600          /* ACCOUNT NUMBER   */
                    return
L21100:     REM DEFAULT/ENABLE FOR EARNINGS TYPE
                enabled% = 1
                edtmessage$ = "Enter Earnings Type."
                return
L21400:     REM DEFAULT/ENABLE FOR UNITS
                enabled% = 1
                edtmessage$ = "Enter Units Description."
                units$(c%) = "HOURS"
                return
L21500:     REM DEFAULT/ENABLE FOR RATE PER UNIT
                enabled% = 1
                edtmessage$ = "Enter the Rate per Unit or Leave Blank."
                return
L21600:     REM DEFAULT/ENABLE FOR ACCOUNT NUMBER
                enabled% = 1
                edtmessage$ = "Enter the General Ledger Account Number" &~
                              " or Leave Blank."
                return

        REM *************************************************************~
            *                 D E L E T E   R O U T I N E               *~
            *                                                           *~
            * DELETES THE RECORD FROM THE FILE. THIS IS A SIMPLE JOB OF *~
            * WORK FOR US.                                              *~
            *************************************************************

        delete_file
L23080:     askkey% = 2%
            call "ASKUSER" (askkey%, "DELETE VERIFICATION", "Press RETURN~
        ~ to delete category " & "'" & catagory$ & "'", "- OR -", "Press P~
        ~F1 to RETURN to Edit Mode")
            if askkey% = 1% then return
            if askkey% <> 0% then L23080
            delkey$ = catagory$
            call "DELETE" (#3, delkey$, 4%)

            goto L10000

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *       L O A D   D A T A   F R O M   T H E   F I L E       *~
            *                                                           *~
            * LOADS THE DATA FROM THE FILE.                             *~
            *************************************************************

            readkey$ = catagory$
            if editmode% = 0 then maxlines% = 0

L30090:     call "PLOWNEXT" (#3, readkey$, 4%, f1%(3))
                 if f1%(3) = 0 then return
            maxlines%, temp% = maxlines% + 1

            get   #3, using L30280,   type$(temp%), units$(temp%), rate,  ~
                                     acct$(temp%)

            call "DESCRIBE" (#2, acct$(temp%), acctdescr$(temp%), 0%,    ~
                             f1%(2))
            call "GLFMT" (acct$(temp%))

            if rate   <> 0                                               ~
               then convert rate to rate$(temp%), pic(-####.####)

            goto L30090

L30280:     FMT XX(4),                   /* CATAGORY   CODE            */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(12),                  /* EARNINGS TYPE              */~
                XX(1),                   /* PAID IN CASH?              */~
                XX(1),                   /* FREE BYTE                  */~
                CH(6),                   /* UNITS                      */~
                PD(14,4),                /* RATE PER UNIT              */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                XX(8),                   /* USUAL UNITS                */~
                XX(8),                   /* USUAL DOLLARS              */~
                XX(2),                   /* SICK ACCRUAL FLAG          */~
                XX(2),                   /* VACATION ACCRUAL FLAG      */~
                XX(35)                   /* FILLER                     */~

L31000: REM *************************************************************~
            *   W R I T E S   T H E   D A T A   T O   T H E   F I L E   *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE FROM THE ARRAY.  IT MUST HAVE *~
            * BEEN DELETED IF ALREADY PRESENT BY THE "DATA SAVE" ROUTINE*~
            *************************************************************

            if maxlines% = 0 then return

            for temp% = 1 to maxlines%
                rate = 0
                if rate$(temp%) <> " " then convert rate$(temp%) to rate
                call "GLUNFMT" (acct$(temp%))
                write #3, using L31230, catagory$, temp%,                 ~
                          type$(temp%), " ", " ", units$(temp%),         ~
                          rate, acct$(temp%), " ", " ", " ", " ", " "
                next temp%
            return

L31230:     FMT CH(4),                   /* CATAGORY   CODE            */~
                PIC(###),                /* SEQUENCE NUMBER            */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(1),                   /* PAID IN CASH?              */~
                CH(1),                   /* FREE BYTE                  */~
                CH(6),                   /* UNITS                      */~
                PD(14,4),                /* RATE PER UNIT              */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                CH(8),                   /* USUAL UNITS                */~
                CH(8),                   /* USUAL DOLLARS              */~
                CH(2),                   /* SICK ACCRUAL FLAG          */~
                CH(2),                   /* VACATION ACCRUAL FLAG      */~
                CH(35)                   /* FILLER                     */

L32000: REM *************************************************************~
            *   G E T   A   R E C O R D   F O R   P R I N T   M O D E   *~
            *                                                           *~
            * GETS A RECORD FOR PRINT MODE.  IT MUST HAVE ALREADY BEEN  *~
            * FOUND BY PLOW NEXT.                                       *~
            *************************************************************

            get   #3, using L32110,   catg$, type$, units$, rate, acct$

            call "GLFMT" (acct$)
            return

L32110:     FMT CH(4),                   /* CATAGORY   CODE            */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(12),                  /* EARNINGS TYPE              */~
                XX(1),                   /* PAID IN CASH?              */~
                XX(1),                   /* FREE BYTE                  */~
                CH(6),                   /* UNITS                      */~
                PD(14,4),                /* RATE PER UNIT              */~
                CH(9)                    /* ACCOUNT NUMBER             */

        REM *************************************************************~
            *         I N P U T   D E P A R T M E N T   C O D E         *~
            *                                                           *~
            * INPUT CATAGORY   CODE TO PROCESS.                         *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) linfac$()
                  str(prgmid$,1,14) = "Last Category:"
                  str(prgmid$,16,6) = lastcatg$
                  on fieldnr% gosub L40140          /* CATAGORY   CODE  */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      linfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "SFC Earnings Code Management",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),     errormsg$          , ch(79),~
               at (06,02),                                               ~
                  "CATEGORY CODE",                                       ~
               at (06,30), fac(linfac$( 1)), catagory$          , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),     inpmessage$        , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,20),                                               ~
                  "(3)Print Categories",                                 ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
               keys(hex(0001030d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40510
                  call "MANUAL" ("JBERNDFT")
                  goto L40210

L40510:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *          E D I T   D E P A R T M E N T   C O D E          *~
            *                                                           *~
            * EDITS THE CATAGORY   CODE.                                *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) linfac$()
                  str(prgmid$,1,14) = "Last Category:"
                  str(prgmid$,16,6) = lastcatg$
                  on fieldnr% gosub L41140          /* CATAGORY   CODE  */
                     goto L41210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linfac$(fieldnr%) = hex(80)
                      return
L41140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      linfac$(fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "SFC Earnings Code Management",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),     errormsg$          , ch(79),~
               at (06,02),                                               ~
                  "CATEGORY CODE",                                       ~
               at (06,30), fac(linfac$( 1)), catagory$          , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),     inpmessage$        , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,20),                                               ~
                  "(2)Line Items",                                       ~
               at (22,40),                                               ~
                  "(12)Delete",                                          ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(0001020c0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41510
                  call "MANUAL" ("JBERNDFT")
                  goto L41210

L41510:        if keyhit% <> 15 then L41550
                  call "PRNTSCRN"
                  goto L41210

L41550:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *   P R I N T   M O D E   G E T   R A N G E   S C R E E N   *~
            *                                                           *~
            * PRINT MODE GET RANGE SCREEN--GETS RANGE OF CATEGORIES TO  *~
            * PRINT.                                                    *~
            *************************************************************

L42100: REM INPUT MODE
            screen% = 1%
            pf$(1) = "(1)Input Mode                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            prtfac$ = hex(81)
            goto L42250

        REM EDIT MODE
L42205:     screen% = 2%
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            prtfac$ = hex(86)

L42250:     accept                                                       ~
               at (01,02),                                               ~
                  "Print SFC Earnings Categories",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST CATEGORY  ",                                    ~
               at (06,30), fac(prtfac$), firstcode$             , ch(04),~
               at (07,02),                                               ~
                  "LAST CATEGORY  ",                                     ~
               at (07,30), fac(prtfac$), lastcode$              , ch(04),~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keyhit%)

               if keyhit% <> 13 then L42375
                  call "MANUAL" ("JBERNDFT")
                  goto L42250

L42375:        if keyhit% <> 15 then L42395
                  call "PRNTSCRN"
                  goto L42250

L42395:         if screen% = 1% then return
                     close ws
                     call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                     return

        REM *************************************************************~
            *       I N P U T   L I N E   I T E M S   S C R E E N       *~
            *                                                           *~
            * INPUTS LINE ITEMS.  USES MULTI-LINE "BCKINPUT" FORMAT.    *~
            *************************************************************

            deffn'203(screenline%, fieldnr%): screen% = 1
                gosub setpf203
                goto L44200
            deffn'213(screenline%, fieldnr%): screen% = 2
                 gosub setpf213
                 init(hex(86)) fac$()
                 if fieldnr% = 0 then L44210
                 init(hex(8c)) fac$()
                 init(hex(84)) fac$(screenline%, fieldnr%)
                 goto L44210
            deffn'223(screenline%, fieldnr%): screen% = 3
                 gosub setpf223
                 goto L44200

            deffn'233(screenline%)
                  gosub setpf233
                  screen% = 4
                  init(hex(8c)) fac$()
                  for temp% = 1 to 5
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L44420

L44200:           init(hex(8c)) fac$()
L44210:           on fieldnr% gosub L44350,         /* EARNINGS TYPE    */~
                                    L44350,         /* UNITS            */~
                                    L44380,         /* RATE PER UNIT    */~
                                    L44350          /* Account Number   */
                  goto L44420

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L44350:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L44380:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L44420:     str(prgmid$,1,20) = "Category:"
            str(prgmid$,11,4) = catagory$

            accept                                                       ~
                                                                         ~
               at (01,02), "SFC Earnings Code Management",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr$(1)                , ch(12),~
               at (05,15), fac(hex(ac)), hdr$(2)                , ch(09),~
               at (05,25), fac(hex(ac)), hdr$(3)                , ch(10),~
               at (05,36), fac(hex(ac)), hdr$(4)                , ch(16),~
               at (05,53), fac(hex(ac)), hdr$(5)                , ch(28),~
                                                                         ~
               at (06,02), fac(fac$(1, 1)), type$     (line%+ 1), ch(12),~
               at (07,02), fac(fac$(2, 1)), type$     (line%+ 2), ch(12),~
               at (08,02), fac(fac$(3, 1)), type$     (line%+ 3), ch(12),~
               at (09,02), fac(fac$(4, 1)), type$     (line%+ 4), ch(12),~
               at (10,02), fac(fac$(5, 1)), type$     (line%+ 5), ch(12),~
               at (11,02), fac(fac$(6, 1)), type$     (line%+ 6), ch(12),~
               at (12,02), fac(fac$(7, 1)), type$     (line%+ 7), ch(12),~
               at (13,02), fac(fac$(8, 1)), type$     (line%+ 8), ch(12),~
               at (14,02), fac(fac$(9, 1)), type$     (line%+ 9), ch(12),~
               at (15,02), fac(fac$(10,1)), type$     (line%+10), ch(12),~
               at (16,02), fac(fac$(11,1)), type$     (line%+11), ch(12),~
               at (17,02), fac(fac$(12,1)), type$     (line%+12), ch(12),~
               at (18,02), fac(fac$(13,1)), type$     (line%+13), ch(12),~
               at (19,02), fac(fac$(14,1)), type$     (line%+14), ch(12),~
               at (20,02), fac(fac$(15,1)), type$     (line%+15), ch(12),~
                                                                         ~
               at (06,15), fac(fac$(1, 2)), units$    (line%+ 1), ch(06),~
               at (07,15), fac(fac$(2, 2)), units$    (line%+ 2), ch(06),~
               at (08,15), fac(fac$(3, 2)), units$    (line%+ 3), ch(06),~
               at (09,15), fac(fac$(4, 2)), units$    (line%+ 4), ch(06),~
               at (10,15), fac(fac$(5, 2)), units$    (line%+ 5), ch(06),~
               at (11,15), fac(fac$(6, 2)), units$    (line%+ 6), ch(06),~
               at (12,15), fac(fac$(7, 2)), units$    (line%+ 7), ch(06),~
               at (13,15), fac(fac$(8, 2)), units$    (line%+ 8), ch(06),~
               at (14,15), fac(fac$(9, 2)), units$    (line%+ 9), ch(06),~
               at (15,15), fac(fac$(10,2)), units$    (line%+10), ch(06),~
               at (16,15), fac(fac$(11,2)), units$    (line%+11), ch(06),~
               at (17,15), fac(fac$(12,2)), units$    (line%+12), ch(06),~
               at (18,15), fac(fac$(13,2)), units$    (line%+13), ch(06),~
               at (19,15), fac(fac$(14,2)), units$    (line%+14), ch(06),~
               at (20,15), fac(fac$(15,2)), units$    (line%+15), ch(06),~
                                                                         ~
               at (06,25), fac(fac$(1, 3)), rate$     (line%+ 1), ch(10),~
               at (07,25), fac(fac$(2, 3)), rate$     (line%+ 2), ch(10),~
               at (08,25), fac(fac$(3, 3)), rate$     (line%+ 3), ch(10),~
               at (09,25), fac(fac$(4, 3)), rate$     (line%+ 4), ch(10),~
               at (10,25), fac(fac$(5, 3)), rate$     (line%+ 5), ch(10),~
               at (11,25), fac(fac$(6, 3)), rate$     (line%+ 6), ch(10),~
               at (12,25), fac(fac$(7, 3)), rate$     (line%+ 7), ch(10),~
               at (13,25), fac(fac$(8, 3)), rate$     (line%+ 8), ch(10),~
               at (14,25), fac(fac$(9, 3)), rate$     (line%+ 9), ch(10),~
               at (15,25), fac(fac$(10,3)), rate$     (line%+10), ch(10),~
               at (16,25), fac(fac$(11,3)), rate$     (line%+11), ch(10),~
               at (17,25), fac(fac$(12,3)), rate$     (line%+12), ch(10),~
               at (18,25), fac(fac$(13,3)), rate$     (line%+13), ch(10),~
               at (19,25), fac(fac$(14,3)), rate$     (line%+14), ch(10),~
               at (20,25), fac(fac$(15,3)), rate$     (line%+15), ch(10),~
                                                                         ~
               at (06,36), fac(fac$(1, 4)), acct$     (line%+ 1), ch(12),~
               at (07,36), fac(fac$(2, 4)), acct$     (line%+ 2), ch(12),~
               at (08,36), fac(fac$(3, 4)), acct$     (line%+ 3), ch(12),~
               at (09,36), fac(fac$(4, 4)), acct$     (line%+ 4), ch(12),~
               at (10,36), fac(fac$(5, 4)), acct$     (line%+ 5), ch(12),~
               at (11,36), fac(fac$(6, 4)), acct$     (line%+ 6), ch(12),~
               at (12,36), fac(fac$(7, 4)), acct$     (line%+ 7), ch(12),~
               at (13,36), fac(fac$(8, 4)), acct$     (line%+ 8), ch(12),~
               at (14,36), fac(fac$(9, 4)), acct$     (line%+ 9), ch(12),~
               at (15,36), fac(fac$(10,4)), acct$     (line%+10), ch(12),~
               at (16,36), fac(fac$(11,4)), acct$     (line%+11), ch(12),~
               at (17,36), fac(fac$(12,4)), acct$     (line%+12), ch(12),~
               at (18,36), fac(fac$(13,4)), acct$     (line%+13), ch(12),~
               at (19,36), fac(fac$(14,4)), acct$     (line%+14), ch(12),~
               at (20,36), fac(fac$(15,4)), acct$     (line%+15), ch(12),~
                                                                         ~
               at (06,53), fac(fac$(1, 5)), acctdescr$(line%+ 1), ch(28),~
               at (07,53), fac(fac$(2, 5)), acctdescr$(line%+ 2), ch(28),~
               at (08,53), fac(fac$(3, 5)), acctdescr$(line%+ 3), ch(28),~
               at (09,53), fac(fac$(4, 5)), acctdescr$(line%+ 4), ch(28),~
               at (10,53), fac(fac$(5, 5)), acctdescr$(line%+ 5), ch(28),~
               at (11,53), fac(fac$(6, 5)), acctdescr$(line%+ 6), ch(28),~
               at (12,53), fac(fac$(7, 5)), acctdescr$(line%+ 7), ch(28),~
               at (13,53), fac(fac$(8, 5)), acctdescr$(line%+ 8), ch(28),~
               at (14,53), fac(fac$(9, 5)), acctdescr$(line%+ 9), ch(28),~
               at (15,53), fac(fac$(10,5)), acctdescr$(line%+10), ch(28),~
               at (16,53), fac(fac$(11,5)), acctdescr$(line%+11), ch(28),~
               at (17,53), fac(fac$(12,5)), acctdescr$(line%+12), ch(28),~
               at (18,53), fac(fac$(13,5)), acctdescr$(line%+13), ch(28),~
               at (19,53), fac(fac$(14,5)), acctdescr$(line%+14), ch(28),~
               at (20,53), fac(fac$(15,5)), acctdescr$(line%+15), ch(28),~
                                                                         ~
               at (21,02), fac(hex(a4)),    edtmessage$         , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keyhit%)

               if keyhit% <> 13 then L45520
                  call "MANUAL" ("JBERNDFT")
                  goto L44420

L45520:        if keyhit% <> 15 then L45560
                  call "PRNTSCRN"
                  goto L44420

L45560:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        setpf203
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Start Line Over     (4)Line Above    " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Edit Mode   "
            pfkeys$ = hex(0102ff04ffffffffffffffff0dff0f1000)
            return

        setpf213
            pf$(1) = "(1)Start Over                           " &        ~
                     "        (9)Header      (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "       (11)Insert      (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "       (12)Delete      (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ff0b0c0dff0f1000)

            if line% = 0% then L45865
                str(pf$(2), 1,15) = "(2)First"
                str(pfkeys$,2,1)  = hex(02)
                str(pf$(2),16,15) = "(4)Previous"
                str(pfkeys$,4,1)  = hex(04)
                str(pf$(2),32,15) = "(6)Down"
                str(pfkeys$,6,1)  = hex(06)

L45865:         if maxlines% <= line% + 15% then L45890
                str(pf$(3), 1,15) = "(3)Last"
                str(pfkeys$,3,1)  = hex(03)
                str(pf$(3),16,15) = "(5)Next"
                str(pfkeys$,5,1)  = hex(05)
                str(pf$(3),32,15) = "(7)Up"
                str(pfkeys$,7,1)  = hex(07)

L45890:     return

         setpf223
            pf$(1) = "(1)Exit Insert Mode                     " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        setpf233
            pf$(1) = "(1)Cancel Delete                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return




        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TEST DATA FOR THE CATAGORY   CODE.  BASICALLY, THIS JUST  *~
            * LOADS THE CATAGORY   FROM THE FILE IF NEEDED.             *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110          /* CATAGORY   CODE  */
                     return
L50110:     REM TEST DATA FOR CATAGORY   CODE
                init(hex(00)) catkey$
                str(catkey$,1,4) = catagory$
                keydescr$ = hex(06) & "Select CATEGORY CODE to review"
                call "PLOWCODE" (#3, catkey$, keydescr$, -4%, -0.12,     ~
                                 f1%(3))
                if f1%(3) <> 0 then L50160
                if catagory$ = " " then L50210
L50160:         catagory$ = str(catkey$,1,4)
                gosub L30000              /* LOAD CATAGORY   CODE       */
                if maxlines% = 0 then return
                   return clear all
                   goto editmode
L50210:         if catagory$ = " " then                                  ~
                   errormsg$ = "CATEGORY CODE May Not Be Blank"
                return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TEST DATA FOR LINE ITEMS.                                 *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L51100,         /* EARNINGS TYPE    */~
                                    L51400,         /* UNITS DESCRIPTION*/~
                                    L51500,         /* RATE PER UNIT    */~
                                    L51600          /* ACCOUNT NUMBER   */
                    return
L51100:     REM TEST DATA FOR EARNINGS TYPE
                if type$(c%) <> " " then return
                   errormsg$ = "Earnings Type Cannot Be Blank!!"
                   return
L51400:     REM TEST DATA FOR UNITS DESCRIPTION
                if units$(c%) <> " " then return
                   errormsg$ = "Units Description Cannot Be Blank!!"
                   return
L51500:     REM TEST DATA FOR UNIT RATE
                if rate$(c%) = " " then return
                   convert rate$(c%) to n, data goto L51520
                   convert n to rate$(c%), pic(-####.####)
                   convert rate$(c%) to n, data goto L51520
                   if n > 0 then return
                   if n = 0 then L51550
                   goto L51545
L51520:               errormsg$ = "Illegal Entry For Rate :" & rate$(c%)
                      return
L51545:               infomsg$ = "WARNING: Rate Is Less Than 0!" : return
L51550:               errormsg$ = "Rate Cannot Be Zero!!" : return
                   return
L51600:     REM TEST DATA FOR ACCOUNT NUMBER
                acctdescr$(c%) = " "
                if acct$(c%) = " " then return
                   call "GETCODE" (#2, acct$(c%), infomsg$, 0%, 0, f1%(2))
                   acctdescr$(c%) = infomsg$
                   if f1%(2) = 0 then errormsg$ = "Account not On File"
                   return

L53000: REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

            errormsg$ = " "


        REM HANDLES CASE FOR "ALL" CATAGORIES
            if firstcode$ <> "ALL" then L53160
                return

L53160: REM HANDLES CASE FOR "?"
            if firstcode$ <> "?" then L53240
                firstcode$ = " "
                gosub L53490
                if errormsg$ <> " " then return
                     lastcode$ = firstcode$
                     return

L53240: REM "HANDLES CASE FOR blank (" ")
            if firstcode$ <> " " then L53300
            gosub L53490
            if errormsg$ <> " " then return
            goto L53340

L53300: REM "HANDLES CASE FOR non-blank CODE
            gosub L53490
            if errormsg$ <> " " then return

L53340: REM "CHECK LAST CODE
            init (hex(00))  catkey$
            str(catkey$,1,4) = lastcode$
            keydescr$ = hex(06) & "Select LAST CATEGORY CODE to " &      ~
                        "print or PF(16) to RETURN"
            call "PLOWCODE" (#3, catkey$, keydescr$, -4%, -0.12, f1%(3))
                if f1%(3) <> 1% then L53420
                     lastcode$ = str(catkey$,1,4)
L53420:     if lastcode$ <> " " then L53450
                lastcode$ = firstcode$
                return
L53450:     if lastcode$ >= firstcode$ then return
                errormsg$ = "Illegal Range!  Please Re-enter."
                return

L53490: REM CHECK OUT FIRST CODE
            init(hex(00)) catkey$
            str(catkey$,1,4) = firstcode$
            keydescr$ = hex(06) & "Select FIRST CATEGORY CODE to print"
            if firstcode$ <> " " then keydescr$ = keydescr$ & " or "  &  ~
                                   "PF(16) to RETURN"
            call "PLOWCODE" (#3, catkey$, keydescr$, -4%, -0.12, f1%(3))
                if f1%(3) <> 1% then L53580
                     firstcode$ = str(catkey$,1,4)
L53580:         if firstcode$ <> " " then return
                     errormsg$ = "First Category Code CANNOT be Blank."
                     return

L60000: REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * PRINT MODE MAIN PROGRAM DRIVES PRINT MODE FOR THE TABLE.  *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L60100:         for column% = 1 to 2
                    on column% gosub L61000, L62000
                    next column%
                if colsdone% < 2 then L60200        /* IF NOT YET DONE  */
                   gosub L63000
                   REM HANDLE TAG LINE ("+-----+---+...")
                       if tagprinted% = 1 then return
                          print using L64410        /* SEPARATOR LINE   */
                          tagprinted% = 1
                          return                   /* NEXT PART        */
L60200:         gosub L63000              /* PAGE HEADING, IF NECCESSARY*/
                print using L64450, prtcatg$, prttype$, prtunits$,        ~
                            prtrate$, prtacct$, prtacctdescr$

                tagprinted% = 0
                goto L60100

L61000:     REM HANDLES FIRST  COLUMN--METHOD, DESCRIPTION, GOAL, ETC.
                on linenumber%(1) gosub L61100, L61200
                   return
L61100:         REM HANDLES FIRST  CASE--PRINT CATAGORY   CODE
                    prtcatg$ = catg$
                    linenumber%(1) = 2
                    return
L61200:         REM HANDLES SECOND CASE--ZAP VARIABLES
                    prtcatg$ = " "
                    linenumber%(1) = 3
                    colsdone% = colsdone% + 1
                    return

L62000:     REM HANDLES SECOND COLUMN--ACCOUNT NUMBERS
                on linenumber%(2) gosub L62100, L62200, L62300
                   return
L62100:         REM HANDLES FIRST  CASE--INITIALIZE PLOW ROUTINE FOR PRINT
                    readkey$ = str(thiscode$, 1, 4)
                    linenumber%(2) = 2
L62200:         REM HANDLES SECOND CASE--PLOW AND PRINT-FORMAT ENTRIES.
                    call "PLOWNEXT" (#3, readkey$, 4%, f1%(3))
                         if f1%(3) = 0 then L62300
                    gosub L32000                    /* LOAD THIS RECORD */
                    if type$ = " " then L62200
                    prtunits$ = units$
                    prttype$  = type$
                    prtacct$ = acct$
                    if rate = 0 then prtrate$ = " "                      ~
                       else call "CONVERT" (rate, 2.4, prtrate$)
                    if acct$ = " " then prtacct$, prtacctdescr$ = " "    ~
                                   else call "GETCODE" (#2, acct$,       ~
                                           prtacctdescr$, 0%, 99, f1%(2))
                    return
L62300:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    prttype$, prtunits$, prtrate$, prtacct$,             ~
                    prtacctdescr$ = " "
                    linenumber%(2) = 4
                    colsdone% = colsdone% + 1
                    return


L63000:     REM PAGE TITLE SUBROUTINE.
                if page% < 0% then gosub print_params
                line% = line% + 1
                if line% < 60 then return
                   if page% = 0 then L63090
                      if tagprinted% = 1 then L63090
                         print using L64410
                         tagprinted% = 1
L63090:            print page
                   page% = page% + 1
                   print using L64210, date$, time$, cmpname$, "JBERNDFT"
                   print using L64220, rpttitle$, page%
                   print
                   print using L64250
                   print using L64290
                   print using L64330
                   print using L64370
                   print using L64410
                   line% = 9              /* SET STARTING LINE ON PAGE  */
                   return


        print_params           /* Print Page Zero */
            print page
            page% = page% + 1%
            print using L64210, date$, time$, cmpname$, "JBERNDFT"
            print using L64220, rpttitle$, page%
L63518:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L63540
                str(i$(), i%, 1%) = hex(20)
                goto L63518
L63540:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            page% = page% + 1%
            return

L64210: %RUN ########   ########              ###########################~
        ~#################################                ########: JB0011

L64220: %                                               #################~
        ~#####################                                PAGE:   ####

L64250: %+----+------------+------+----------+---------------------------~
        ~----------------+

L64290: %!CAT.!  EARNINGS  ! UNIT ! RATE PER !  A C C O U N T  I N F O R ~
        ~M A T I O N     !

L64330: %!    !            !  OF  !          +------------+--------------~
        ~----------------+

L64370: %!CODE!    TYPE    ! RATE !   UNIT   !  ACCOUNT   !     D E S C R~
        ~ I P T I O N    !

L64410: %+----+------------+------+----------+------------+--------------~
        ~----------------+

L64450: %!####!############!######!##########!############!##############~
        ~################!

*       * Report Title for page 0
        %############################################################

L64490: %                        * * * * * * * * * *   E N D   O F   R E ~
        ~P O R T   A T   ########   * * * * * * * * * *

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
