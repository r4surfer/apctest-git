        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD   EEEEE  DDDD   X   X  N   N   *~
            *  P   P  R   R  L      D   D  E      D   D   X X   NN  N   *~
            *  PPPP   RRRR   L      D   D  EEEE   D   D    X    N N N   *~
            *  P      R   R  L      D   D  E      D   D   X X   N  NN   *~
            *  P      R   R  LLLLL  DDDD   EEEEE  DDDD   X   X  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDEDXN - INPUT OR PRINT PAYROLL DEDUCTION DEFAULTS BY   *~
            *            DEDUCTION CATEGORY INTO THE DEFAULT DEDUCTION  *~
            *            CATEGTORY FILE.                                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/28/80 ! ORIGINAL                                 ! BCW *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 03/28/86 ! Conformed Screens and Report To Standards! SGA *~
            * 09/03/86 ! Minor change related to direct deposits  ! HES *~
            * 03/21/89 ! Fixed Delete Record Logic                ! RJM *~
            * 06/13/91 ! Eliminated unused FNX.  Added ALLFREE.   ! JDH *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            amount(4),                   /* AMOUNTS FOR DISK I/O       */~
            amt$(4,100)12,               /* ALPHA FORMAT 4 AMOUNTS     */~
            amtdescr$(4)15,              /* DESCR. OF 4 AMTS FROM DDT. */~
            applies$(100)6,              /* APPLIES (123456) ALPHA     */~
            applies$6,                   /* WHICH PAY PERIODS/MO TO DO */~
            blankline$79,                /* BLANK LINE FOR PRINT SCREEN*/~
            category$6,                  /* CATEGORY CODE THIS DEDXN   */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            creditacct$(100)16,          /* CREDIT ACCOUNT NUMBER      */~
            creditacct$16,               /* CREDIT ACCOUNT FOR DEDXN   */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            debitacct$(100)16,           /* DEBIT ACCOUNT NUMBER       */~
            debitacct$16,                /* DEBIT ACCOUNT THIS DEDXN   */~
            desc$(4,100)15,              /* DESCRIPTION OF 4 AMOUNTS   */~
            descr$(100)12,               /* DESCRIPTION OF DEDUCTION   */~
            descr$12,                    /* DEDUCTION DESCRIPTION      */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            empflag$(100)1,              /* EMPLOYEE FLAG              */~
            empflag$3,                   /* EMPLOYER/EMPLOYEE FLAG     */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(5,11)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            firstcode$6,                 /* FIRST CODE IN PRINT RANGE  */~
            goal$(100)10,                /* GOAL AMOUNT THIS DEDXN     */~
            hdrdate$45,                  /* FORMATTED DATE/TIME INFO   */~
            head$40,                     /* Heading for ASKUSER        */~
            hi$80,                       /* Top Line for ASKUSER       */~
            mid$80,                      /* Mid Line for ASKUSER       */~
            lo$80,                       /* Low Line for ASKUSER       */~
            i$(24)80,                    /* SCREEN IMAGE--NOT USED     */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            lastcat$6,                   /* LAST DEDUCTION CAT INPUT   */~
            lastcode$6,                  /* LAST CODE IN PRINT RANGE   */~
            linenumber%(3),              /* LINE POINTER FOR PRINT MODE*/~
            linfac$(20)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            method$(100)6,               /* METHOD OF DEDUCTION        */~
            method$6,                    /* METHOD OF DEDUCTION CODE   */~
            pfkeys$(4)17,                /* FUNCTION KEYS ENABLED      */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            prtacctdescr$30,             /* ACCT DESCRIPTION FOR PRINT */~
            prtamt$10,                   /* AMOUNT FOR PRINT MODE      */~
            prtamtdescr$15,              /* DESCRIPTIONS FOR AMOUNTS   */~
            prtapplies$6,                /* HOW OFTEN IT APPLIES TO YOU*/~
            prtcreditacct$16,            /* CREDIT ACCOUNT TO PRINT    */~
            prtdebitacct$16,             /* DEBIT ACCOUNT NUMBER       */~
            prtdescr$12,                 /* DESCRIPTION OF DEDXN METH  */~
            prtempflag$3,                /* EMPLOYEE PAYS FLAG         */~
            prtgoal$10,                  /* GOAL FOR PRINT MODE        */~
            prtmethod$6,                 /* METHOD CODE FOR PRINTING   */~
            prtroutine$3,                /* ROUTINE NUMBER FOR COMPUTE */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            rpttitle$60,                 /* REPORT TITLE               */~
            separator$(5)79,             /* SEPARATOR LINES ("---LINE")*/~
            seqnr$(100)3,                /* SEQUENCE NUMBERS           */~
            thiscode$50,                 /* CURRENT CODE IN PRINT RANGE*/~
            title$(4,2)64,               /* P.F. KEY TITLES            */~
            tran$(24)80,                 /* CURSOR==>FIELD TRAN FOR EDT*/~
            userid$3                     /* USERID OF CURRENT USER.    */~

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
            * # 1 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! PRLDDEFL ! PAYROLL DEDUCTION DEFAULT TABLE          *~
            *************************************************************

            select #1, "PRLDDT",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 400,                                    ~
                       keypos = 1, keylen = 6

            select #2, "GLMAIN",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 9

            select #3, "PRLDDEFL",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 9

*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, " ")
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

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            REM SET STRINGS FOR TABLE EDIT COMPUTATION.
            init(hex(00)) tran$()
            init(hex(01)) str(tran$(6),  1, 16)
            init(hex(02)) str(tran$(6), 16)
            init(hex(03)) str(tran$(6), 33)
            init(hex(04)) str(tran$(6), 42)
            init(hex(05)) str(tran$(6), 59)
            init(hex(06)) str(tran$(6), 75)
            init(hex(07)) str(tran$(7),  1, 16)
            init(hex(08)) str(tran$(7), 17, 16)
            init(hex(09)) str(tran$(7), 33, 16)
            init(hex(0a)) str(tran$(7), 49, 16)
            init(hex(0b)) str(tran$(7), 65, 16)
            init(hex(07)) str(tran$(8),  1, 16)
            init(hex(08)) str(tran$(8), 17, 16)
            init(hex(09)) str(tran$(8), 33, 16)
            init(hex(0a)) str(tran$(8), 49, 16)
            init(hex(0b)) str(tran$(8), 65, 16)
            copy str(tran$(), 321, 1280) to str(tran$(), 641, 1280)

            title$(1,1) = "(1)START OVER(2)COL 1(4)LINE ABOVE(13)INSTRU(1~
        ~6)EDIT MODE"
            title$(2,1) = "(1)START OVER(2)FIRST(3)LAST(4)PREV(5)NEXT(6)D~
        ~OWN(7)UP"
            title$(2,2) = "(9)HEADER(11)INS(12)DEL(13)INSTRU(15)PRT SCRN ~
        ~(16)WRITE DATA"
            title$(3,1) = "SUPPLY REQUESTED ITEMS AND (ENTER) OR (1) TO E~
        ~XIT INSERT MODE"
            title$(3,2) = "TO GET A LIST OF THE ABBREVIATIONS, PRESS (13)~
        ~."
            title$(4,1) = "PRESS (ENTER) TO DELETE FLASHING LINE OR (1) T~
        ~O EXIT DELETE."

            head$ = "DELETE VERIFICATION"
            pfkeys$(1) = hex(000102040a0d0f10ffffffffffffffffff)
            pfkeys$(2) = hex(0001020304050607090a0b0c0d0f10ffff)
            pfkeys$(3) = hex(000d010fffffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010fffffffffffffffffffffffffffff)

            prgm$ = "PRLDEDXN"
            rptid$ = "PRL009"
            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,62,9) = "PRLDEDXN:"
            str(prgmid$,72,8) = str(cms2v$,1,8)
            rpttitle$ = "P A Y R O L L    D E D U C T I O N    C A T E G ~
        ~O R I E S"
L10000: REM *************************************************************~
            *      I N P U T   D E D U C T I O N   C A T E G O R Y      *~
            *                                                           *~
            * INPUT DEDUCTION CATEGORY CODE.                            *~
            *************************************************************

        inputmode
            editmode% = 0
            init(" ") errormsg$, infomsg$, inpmessage$,                  ~
                      category$, method$(), descr$(), applies$(),        ~
                      creditacct$(), debitacct$(), empflag$(), desc$(),  ~
                      amt$(), goal$()
            call "ALLFREE"

            for fieldnr% = 1 to 1
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10230
L10160:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then printmode
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
L10230:         next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUT LINE ITEMS MAIN PROGRAM.                            *~
            *************************************************************

            line%, maxlines%, screenline% = 0
            infomsg$, errormsg$ = " "

L11090:     screenline% = screenline% + 1
            if screenline% < 6 then L11130
               line% = line% + 5
               screenline% = 1
L11130:     c%, currentline% = line% + screenline%
            if currentline% > 100 then editmode
            call "SETSEP" (separator$(), line%, screenline%)

L11170:     for fieldnr% = 1 to 11
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
                convert maxlines% to seqnr$(maxlines%), pic(###)
                goto L11090

        REM *************************************************************~
            *      E D I T   H E A D E R   M A I N   P R O G R A M      *~
            *                                                           *~
            * EDITS HEADER OF THE CATEGORY ENTRY.                       *~
            *************************************************************

        editmode
            editmode% = 1
            errormsg$, infomsg$, inpmessage$ = " "

L12100:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L13000
                  if keyhit%  = 12 then gosub delete_record
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
            call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))

L13130:     gosub'213(0%, 0%)
                  if keyhit%  =  0 then       L13320
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%-5)
                  if keyhit%  =  4 then line% = max(0,line%-4)
                  if keyhit%  =  5 then line% = min(line%+4,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 16 then       datasave
                  call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
                  goto L13130

L13320:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                fieldnr% = val(str(tran$(cursor%(1)),cursor%(2)))
                if fieldnr% = 0 then L13130
                screenline% = (cursor%(1)-5)/4+1
                c%, currentline% = line% + screenline%
                if currentline% > maxlines% then L13130
                   if fieldnr% < 7 or fieldnr% > 10 then L13440
                      if fieldnr% =  7 and desc$(1,c%) = " " then L13130
                      if fieldnr% =  8 and desc$(2,c%) = " " then L13130
                      if fieldnr% =  9 and desc$(3,c%) = " " then L13130
                      if fieldnr% = 10 and desc$(4,c%) = " " then L13130

L13440:         gosub'213(screenline%, fieldnr%)
                      if keyhit%  = 1 then gosub startover
                      if keyhit% <> 0 then L13440
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L13440
                goto L13130

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *                                                           *~
            * COLUMN ONE AND LINE ABOVE FUNCTION KEYS LOGIC IS          *~
            * IMPLEMENTED HERE.                                         *~
            *************************************************************

        columnone
            if fieldnr% = 1 then return
            init(" ") seqnr$(c%), method$(c%), descr$(c%), applies$(c%), ~
                      creditacct$(c%), debitacct$(c%), empflag$(c%),     ~
                      desc$(1,c%), desc$(2,c%), desc$(3,c%), desc$(4,c%),~
                      amt$(1,c%), amt$(2,c%), amt$(3,c%), amt$(4,c%),    ~
                      goal$(c%), errormsg$, infomsg$
            return clear all
            goto L11170

        lineabove
            if currentline% = 1 then return
            on fieldnr% gosub L14310,               /* METHOD OF DEDXN  */~
                              L14320,               /* DEDUCTION DESCR  */~
                              L14330,               /* APPLIES FIELD    */~
                              L14340,               /* CREDIT ACCOUNT   */~
                              L14350,               /* DEBIT ACCOUNT    */~
                              L14360,               /* EMPLOYEE FLAG    */~
                              L14370,               /* AMOUNT 1         */~
                              L14380,               /* AMOUNT 2         */~
                              L14390,               /* AMOUNT 3         */~
                              L14400,               /* AMOUNT 4         */~
                              L14410                /* GOAL             */
                    return
L14310:     method$    (c%) = method$    (c%-1): return
L14320:     descr$     (c%) = descr$     (c%-1): return
L14330:     applies$   (c%) = applies$   (c%-1): return
L14340:     creditacct$(c%) = creditacct$(c%-1): return
L14350:     debitacct$ (c%) = debitacct$ (c%-1): return
L14360:     empflag$   (c%) = empflag$   (c%-1): return
L14370:     amt$    (1, c%) = amt$    (1, c%-1): return
L14380:     amt$    (2, c%) = amt$    (2, c%-1): return
L14390:     amt$    (3, c%) = amt$    (3, c%-1): return
L14400:     amt$    (4, c%) = amt$    (4, c%-1): return
L14410:     goal$      (c%) = goal$      (c%-1): return

        REM *************************************************************~
            *                   I N S E R T   M O D E                   *~
            *                                                           *~
            * INSERT LOGIC HERE.                                        *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return         /* ARRAY FULL, CAN'T*/
            REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
L15045:         screenline% = int((cursor%(1)-1)/4)
                if line% + screenline% < maxlines% then L15060
                   screenline% = maxlines% - line% /* TO INS AT END    */
L15060:         if screenline% <> 5 then L15085     /* BOTTOM OF PAGE   */
                   line% = line% + 1
                   screenline% = screenline% - 1
                   goto L15085

L15085:         call "SETSEP" (separator$(),line%,                       ~
                                      min(5%, maxlines%-line%+1%))
                currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L15210
                for temp% = maxlines% to c% step -1
                    method$    (temp%+1) = method$    (temp%)
                    descr$     (temp%+1) = descr$     (temp%)
                    applies$   (temp%+1) = applies$   (temp%)
                    creditacct$(temp%+1) = creditacct$(temp%)
                    debitacct$ (temp%+1) = debitacct$ (temp%)
                    empflag$   (temp%+1) = empflag$   (temp%)
                    desc$   (1, temp%+1) = desc$   (1, temp%)
                    desc$   (2, temp%+1) = desc$   (2, temp%)
                    desc$   (3, temp%+1) = desc$   (3, temp%)
                    desc$   (4, temp%+1) = desc$   (4, temp%)
                    amt$    (1, temp%+1) = amt$    (1, temp%)
                    amt$    (2, temp%+1) = amt$    (2, temp%)
                    amt$    (3, temp%+1) = amt$    (3, temp%)
                    amt$    (4, temp%+1) = amt$    (4, temp%)
                    goal$      (temp%+1) = goal$      (temp%)
                    seqnr$     (temp%+1) = seqnr$     (temp%)
                    next temp%

L15210:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") seqnr$(c%), method$(c%), descr$(c%),           ~
                          applies$(c%), creditacct$(c%), debitacct$(c%), ~
                          empflag$(c%), desc$(1,c%), desc$(2,c%),        ~
                          desc$(3,c%), desc$(4,c%), amt$(1,c%),          ~
                          amt$(2,c%), amt$(3,c%), amt$(4,c%),            ~
                          goal$(c%), errormsg$, infomsg$

            REM NOW INPUT THE LINE, MAKE SO WE CAN CANCEL OUT IF NECC
                infomsg$ = " "
                for fieldnr% = 1 to 11
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L15315
L15285:             gosub'223(screenline%, fieldnr%)
                          if keyhit%  =  1 then L15360
                          if keyhit% <>  0 then L15285
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L15285
L15315:             next fieldnr%

                maxlines% = maxlines% + 1
                for temp% = 1 to max(maxlines%, currentline% + 1)
                    convert temp% to seqnr$(temp%), pic(###)
                    next temp%
                cursor%(1) = min(cursor%(1)+4, 24)
                goto L15045

L15360:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                gosub L15435              /* ACTUALLY DELETE @C%        */

                temp% = maxlines% + 1
                init(" ") seqnr$(temp%), method$(temp%), descr$(temp%),  ~
                          applies$(temp%), creditacct$(temp%),           ~
                          debitacct$(temp%), empflag$(temp%),            ~
                          desc$(1,temp%), desc$(2,temp%), desc$(3,temp%),~
                          desc$(4,temp%), amt$(1,temp%), amt$(2,temp%),  ~
                          amt$(3,temp%), amt$(4,temp%), goal$(temp%),    ~
                          errormsg$, infomsg$
            if currentline% >= maxlines% and screenline% = 5             ~
               then line% = max(0%, line% - 1%)
            return

L15435:     for temp% = currentline% to maxlines%
                seqnr$     (temp%) = seqnr$     (temp%+1)
                method$    (temp%) = method$    (temp%+1)
                descr$     (temp%) = descr$     (temp%+1)
                applies$   (temp%) = applies$   (temp%+1)
                creditacct$(temp%) = creditacct$(temp%+1)
                debitacct$ (temp%) = debitacct$ (temp%+1)
                empflag$   (temp%) = empflag$   (temp%+1)
                desc$   (1, temp%) = desc$   (1, temp%+1)
                desc$   (2, temp%) = desc$   (2, temp%+1)
                desc$   (3, temp%) = desc$   (3, temp%+1)
                desc$   (4, temp%) = desc$   (4, temp%+1)
                amt$    (1, temp%) = amt$    (1, temp%+1)
                amt$    (2, temp%) = amt$    (2, temp%+1)
                amt$    (3, temp%) = amt$    (3, temp%+1)
                amt$    (4, temp%) = amt$    (4, temp%+1)
                goal$      (temp%) = goal$      (temp%+1)
                next temp%

            init(" ") seqnr$()
               if maxlines% = 0 then return
               for temp% = 1 to min(maxlines%, currentline%)
                   convert temp% to seqnr$(temp%), pic(###)
                   next temp%
            return

        REM *************************************************************~
            *                  D E L E T E   L O G I C                  *~
            *                                                           *~
            * DELETE LOGIC.                                             *~
            *************************************************************

        deletemode
            if maxlines% = 0 then return
            screenline% = int((cursor%(1)-1)/4)
            if screenline% < 1 then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return

L16130:     gosub'233(screenline%)
                  if keyhit%  =  1 then       return
                  if keyhit% <>  0 then       L16130

            c% = currentline%
            if currentline% < maxlines% then gosub L15435
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") method$(temp%), descr$(temp%), applies$(temp%),    ~
                      creditacct$(temp%), debitacct$(temp%),             ~
                      empflag$(temp%), desc$(1,temp%), desc$(2,temp%),   ~
                      desc$(3,temp%), desc$(4,temp%), amt$(1,temp%),     ~
                      amt$(2,temp%), amt$(3,temp%), amt$(4,temp%),       ~
                      goal$(temp%), errormsg$, infomsg$

            maxlines% = maxlines% - 1
            if currentline% >= maxlines% and screenline% = 5             ~
               then line% = max(0%, line% - 1%)
            init(" ") seqnr$()
            if maxlines% = 0 then return
               for temp% = 1 to maxlines%
                   convert temp% to seqnr$(temp%), pic(###)
                   next temp%
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

L17120:     gosub L42000
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L65000
            gosub L53000
                  if errormsg$ <> " " then L17120

            REM PLOW ROUTINE FOR PRINTING DEDUCTION CATEGORY LISTING.
                call "SHOSTAT" ("PRINTING DEDUCTION CATEGORY LISTING")
                tagprinted% = 1
                line% = 1000
                page% = 0

L17240:         call "PLOWNEXT" (#3, thiscode$, 0%, f1%(3))
                     if f1%(3) = 0 then L17520
                     prtcategory$ = str(thiscode$, 1, 6)
                     if prtcategory$ > lastcode$ then L17520
                     str(thiscode$, 7) = " "

                     REM PLOW ALL ENTRIES OF THIS CATEGORY AND PRINT 'EM
L17310:                  call "PLOWNEXT" (#3, thiscode$, 6%, f1%(3))
                              if f1%(3) = 0 then L17450
                         gosub L32000
                         if tagprinted% <> 0 then L17420
L17350:                     gosub L64000
                            if line% <= 58 then L17390
                               line% = 1000
                               goto L17350
L17390:                     if tagprinted% <> 0 then L17420
                               print using L64430
                               tagprinted% = 2
L17420:                  gosub L60000     /* PRINT ENTRY                */
                         goto L17310

L17450:             if tagprinted% <> 0 then L17240
                       gosub L64000
                       if tagprinted% <> 0 then L17240
                          print using L64370
                          tagprinted% = 1
                          goto L17240

L17520:             if tagprinted% = 1 then L17590
                       gosub L64000
                       if tagprinted% = 1 then L17590
                          print using L64370
                          tagprinted% = 1
                          return

L17590:         REM RETURN FROM ROUTINE
                    close printer
                    goto inputmode

        REM *************************************************************~
            *                     S A V E   D A T A                     *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE, DELETING THE OTHER DEDUCTION *~
            * CATEGORIES.                                               *~
            *************************************************************

        datasave
            lastcat$ = category$
            REM DELETE OLD BEFORE WRITING NEW.
                readkey$ = category$
                call "DELETE" (#3, readkey$, 6%)
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
                  on fieldnr% gosub L20100          /* CATEGORY CODE    */
                     return
L20100:     REM DEFAULT/ENABLE FOR CATEGORY CODE
                enabled% = 1
                inpmessage$ = "To Print A Listing Of The Available Deduct~
        ~ion Categories, Press (3)."
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
                  on fieldnr% gosub L21100,         /* METHOD OF DEDXN  */~
                                    L21200,         /* DEDUCTION DESCR  */~
                                    L21300,         /* APPLIES (123456) */~
                                    L21400,         /* CREDIT ACCOUNT   */~
                                    L21500,         /* DEBIT ACCOUNT    */~
                                    L21600,         /* EMPLOYEE FLAG    */~
                                    L21700,         /* AMOUNT 1         */~
                                    L21800,         /* AMOUNT 2         */~
                                    L21900,         /* AMOUNT 3         */~
                                    L22000,         /* AMOUNT 4         */~
                                    L22100          /* GOAL             */
                    return
L21100:     REM DEFAULT/ENABLE FOR METHOD OF DEDUCTION
                enabled% = 1
                return
L21200:     REM DEFAULT/ENABLE FOR DEDUCTION DESCRIPTION
                get #1, using L21220, descr$(c%)
L21220:                 FMT XX(6), CH(12)
                if descr$(c%) <> " " then return
                   enabled% = 1
                   return
L21300:     REM DEFAULT/ENABLE FOR APPLIES FIELD
                get #1, using L21320, applies$(c%)
L21320:                 FMT XX(37), CH(6)
                if applies$(c%) <> " " then return
                   enabled% = 1
                   return
L21400:     REM DEFAULT/ENABLE FOR CREDIT ACCOUNT
                get #1, using L21420, creditacct$(c%)
L21420:                 FMT XX(19), CH(9)
                call "GLFMT" (creditacct$(c%))
                if creditacct$(c%) <> " " then return
                   enabled% = 1
                   return
L21500:     REM DEFAULT/ENABLE FOR DEBIT ACCOUNT
                get #1, using L21520, debitacct$(c%)
L21520:                 FMT XX(28), CH(9)
                call "GLFMT" (debitacct$(c%))
                if debitacct$(c%) <> " " then return
                   enabled% = 1
                   return
L21600:     REM DEFAULT/ENABLE FOR EMPLOYEE FLAG
                get #1, using L21620, empflag$(c%)
L21620:                 FMT XX(18), CH(1)
                if empflag$(c%) <> " " then return
                   enabled% = 1
                   return
L21700:     REM DEFAULT/ENABLE FOR AMOUNT 1
                infomsg$ = " "
                get #1, using L21720, desc$(1,c%)
L21720:                 FMT XX(43), CH(15)
                if desc$(1,c%) = " " then return
                   get #1, using L21750, amount
L21750:                    FMT XX(103), PD(14,4)
                   if amount = 0                                         ~
                      then enabled% = 1                                  ~
                      else convert amount to amt$(1,c%), pic(#######.####)
                   return
L21800:     REM DEFAULT/ENABLE FOR AMOUNT 2
                get #1, using L21820, desc$(2,c%)
L21820:                 FMT XX(58), CH(15)
                if desc$(2,c%) = " " then return
                   get #1, using L21850, amount
L21850:                    FMT XX(111), PD(14,4)
                   if amount = 0                                         ~
                      then enabled% = 1                                  ~
                      else convert amount to amt$(2,c%), pic(#######.####)
                   return
L21900:     REM DEFAULT/ENABLE FOR AMOUNT 3
                get #1, using L21920, desc$(3,c%)
L21920:                 FMT XX(73), CH(15)
                if desc$(3,c%) = " " then return
                   get #1, using L21950, amount
L21950:                    FMT XX(119), PD(14,4)
                   if amount = 0                                         ~
                      then enabled% = 1                                  ~
                      else convert amount to amt$(3,c%), pic(#######.####)
                   return
L22000:     REM DEFAULT/ENABLE FOR AMOUNT 4
                get #1, using L22020, desc$(4,c%)
L22020:                 FMT XX(88), CH(15)
                if desc$(4,c%) = " " then return
                   get #1, using L22050, amount
L22050:                    FMT XX(127), PD(14,4)
                   if amount = 0                                         ~
                      then enabled% = 1                                  ~
                      else convert amount to amt$(4,c%), pic(#######.####)
                   return
L22100:     REM DEFAULT/ENABLE FOR GOAL
                get #1, using L22120, goal
L22120:                 FMT XX(135), PD(14,4)
                if goal = 0                                              ~
                   then enabled% = 1                                     ~
                   else call "CONVERT" (goal, -2.4, goal$(c%))
                return

        REM *************************************************************~
            *                 D E L E T E   R O U T I N E               *~
            *                                                           *~
            * DELETES THE RECORD FROM THE FILE. THIS IS A SIMPLE JOB OF *~
            * WORK FOR US.                                              *~
            *************************************************************

        delete_record
            hi$ = "Press (RETURN) to delete this Category,"
            mid$ = "OR you may"
            lo$ = "Press PF1 to return to the edit mode screen."
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, head$, hi$, mid$, lo$)
            if keyhit1% <> 0% then return
*          CALL "READ101" (#3, CATEGORY$, F1%(3))
*               IF F1%(3) <> 0 THEN DELETE #3     /*DELETE THE RECORD*/

            readkey$ = category$
            call "DELETE" (#3, readkey$, 6%)

            goto L10000

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
            *       L O A D   D A T A   F R O M   T H E   F I L E       *~
            *                                                           *~
            * LOADS THE DATA FROM THE FILE.                             *~
            *************************************************************

            readkey$ = category$
            if editmode% = 0 then maxlines% = 0

L30090:     call "PLOWNEXT" (#3, readkey$, 6%, f1%(3))
                 if f1%(3) = 0 then return
            maxlines%, temp% = maxlines% + 1

            get   #3, using L30300,   method$(temp%),                     ~
                      descr$(temp%), applies$(temp%), creditacct$(temp%),~
                      debitacct$(temp%), empflag$(temp%), amount(1),     ~
                      amount(2), amount(3), amount(4), goal
            call "GLFMT" (creditacct$(temp%))
            call "GLFMT" (debitacct$(temp%))
            call "READ100" (#1, method$(temp%), f1%(1))
                 if f1%(1) = 0 then L30090
            get #1, using L30210, desc$(1, temp%), desc$(2, temp%),       ~
                                 desc$(3, temp%), desc$(4, temp%)
L30210:             FMT XX(43), 4*CH(15)
            for u3% = 1 to 4
                if desc$(u3%, temp%) <> " " then                         ~
               convert amount(u3%) to amt$(u3%, temp%), pic(#######.####)
                next u3%
            convert goal to goal$(temp%), pic(-######.##)
            convert temp% to seqnr$(temp%), pic(###)
            goto L30090

L30300:     FMT XX(6),                   /* DEDUCTION CATEGORY         */~
                XX(3),                   /* SEQUENCE NUMBER (PIC(###)) */~
                CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(6),                   /* APPLIES (123456)           */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(1),                   /* EMPLOYER/EMPLOYEE FLAG     */~
                PD(14,4),                /* AMOUNT 1                   */~
                PD(14,4),                /* AMOUNT 2                   */~
                PD(14,4),                /* AMOUNT 3                   */~
                PD(14,4),                /* AMOUNT 4                   */~
                PD(14,4)                 /* GOAL OF DEDUCTION          */~

L31000: REM *************************************************************~
            *   W R I T E S   T H E   D A T A   T O   T H E   F I L E   *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE FROM THE ARRAY.  IT MUST HAVE *~
            * BEEN DELETED IF ALREADY PRESENT BY THE "DATA SAVE" ROUTINE*~
            *************************************************************

            if maxlines% = 0 then return
            for temp% = 1 to maxlines%
                mat amount = zer
                for u3% = 1 to 4
                    if amt$(u3%, temp%) <> " "                           ~
                       then convert amt$(u3%, temp%) to amount(u3%)
                    next u3%
                if goal$(temp%) <> " " then convert goal$(temp%) to goal
                call "GLUNFMT" (debitacct$(temp%))
                call "GLUNFMT" (creditacct$(temp%))

                write #3, using L31240, category$, temp%, method$(temp%), ~
                          descr$(temp%), applies$(temp%),                ~
                          creditacct$(temp%), debitacct$(temp%),         ~
                          empflag$(temp%), amount(1), amount(2),         ~
                          amount(3), amount(4), goal, " "
                next temp%
            return

L31240:     FMT CH(6),                   /* DEDUCTION CATEGORY         */~
                PIC(###),                /* SEQUENCE NUMBER            */~
                CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(6),                   /* APPLIES (123456)           */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(1),                   /* EMPLOYER/EMPLOYEE FLAG     */~
                PD(14,4),                /* AMOUNT 1                   */~
                PD(14,4),                /* AMOUNT 2                   */~
                PD(14,4),                /* AMOUNT 3                   */~
                PD(14,4),                /* AMOUNT 4                   */~
                PD(14,4),                /* GOAL OF DEDUCTION          */~
                CH(108)                  /* FILLER                     */

L32000: REM *************************************************************~
            *   G E T   A   R E C O R D   F O R   P R I N T   M O D E   *~
            *                                                           *~
            * GETS A RECORD FOR PRINT MODE.  IT MUST HAVE ALREADY BEEN  *~
            * FOUND BY PLOW NEXT.                                       *~
            *************************************************************

            init(" ") amtdescr$()

            get   #3, using L32200,   method$, descr$, applies$,          ~
                                     creditacct$, debitacct$, empflag$,  ~
                                     amount(1), amount(2), amount(3),    ~
                                     amount(4), goal
            call "GLFMT" (creditacct$)
            call "GLFMT" (debitacct$)
            call "READ100" (#1, method$, f1%(1))
                 if f1%(1) = 0 then return
            get #1, using L32170, amtdescr$(1), amtdescr$(2),             ~
                                 amtdescr$(3), amtdescr$(4)
L32170:             FMT XX(43), 4*CH(15)
            return

L32200:     FMT XX(6),                   /* DEDUCTION CATEGORY         */~
                XX(3),                   /* SEQUENCE NUMBER (PIC(###)) */~
                CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(6),                   /* APPLIES (123456)           */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(1),                   /* EMPLOYER/EMPLOYEE FLAG     */~
                PD(14,4),                /* AMOUNT 1                   */~
                PD(14,4),                /* AMOUNT 2                   */~
                PD(14,4),                /* AMOUNT 3                   */~
                PD(14,4),                /* AMOUNT 4                   */~
                PD(14,4)                 /* GOAL OF DEDUCTION          */~


        REM *************************************************************~
            *           I N P U T   C A T E G O R Y   C O D E           *~
            *                                                           *~
            * INPUT CATEGORY CODE OF THE CATEGORY TO PROCESS.           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) linfac$()
                  str(prgmid$,1,14) = "Last Category:"
                  str(prgmid$,16,6) = lastcat$
                  on fieldnr% gosub L40140          /* CATEGORY CODE    */
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
                  "Enter Deduction Category",                            ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),     errormsg$          , ch(79),~
               at (06,02),                                               ~
                  "DEDUCTION CATEGORY CODE",                             ~
               at (06,30), fac(linfac$( 1)), category$          , ch(06),~
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
                                                                         ~
               keys(hex(0001030d0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L40510
                call "MANUAL" ("PRLDEDXN")
                goto L40210

L40510:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *            E D I T   C A T E G O R Y   C O D E            *~
            *                                                           *~
            * EDITS THE CATEGORY CODE.                                  *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) linfac$()
                  str(prgmid$,1,14) = "Last Category:"
                  str(prgmid$,16,6) = lastcat$
                  on fieldnr% gosub L41140          /* CATEGORY CODE    */
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
                  "Edit Deduction Category",                             ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),     errormsg$          , ch(79),~
               at (06,02),                                               ~
                  "DEDUCTION CATEGORY CODE",                             ~
               at (06,30), fac(linfac$( 1)), category$          , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),     edtmessage$        , ch(79),~
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
                call "MANUAL" ("PRLDEDXN")
                goto L41210

L41510:        if keyhit% <> 15 then L41550
                  call "PRNTSCRN"
                  goto L41210

L41550:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L42000: REM *************************************************************~
            *   P R I N T   M O D E   G E T   R A N G E   S C R E E N   *~
            *                                                           *~
            * PRINT MODE GET RANGE SCREEN--GETS RANGE OF CATEGORIES TO  *~
            * PRINT.                                                    *~
            *************************************************************

L42070:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Deduction Categories",                          ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST CATEGORY CODE",                                 ~
               at (06,30), fac(hex(81)), firstcode$             , ch(06),~
               at (07,02),                                               ~
                  "LAST CATEGORY CODE",                                  ~
               at (07,30), fac(hex(81)), lastcode$              , ch(06),~
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

            if keyhit% <> 13 then L42350
                call "MANUAL" ("PRLDEDXN")
                goto L42070

L42350:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *       I N P U T   L I N E   I T E M S   S C R E E N       *~
            *                                                           *~
            * INPUTS LINE ITEMS.  USES MULTI-LINE "BCKINPUT" FORMAT.    *~
            *************************************************************

            deffn'203(screenline%, fieldnr%): screen% = 1: goto L44072
            deffn'213(screenline%, fieldnr%): screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L44076             : goto L44072
            deffn'223(screenline%, fieldnr%): screen% = 3: goto L44072

            deffn'233(screenline%)
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 11
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L44168

L44072:           init(hex(84)) fac$()
L44076:           on fieldnr% gosub L44140,         /* DEDUCTION METHOD */~
                                    L44140,         /* DESCRIPTION      */~
                                    L44152,         /* APPLIES          */~
                                    L44140,         /* CREDIT ACCOUNT   */~
                                    L44140,         /* DEBIT ACCOUNT    */~
                                    L44140,         /* EMPLOYEE FLAG    */~
                                    L44152,         /* AMOUNT 1         */~
                                    L44152,         /* AMOUNT 2         */~
                                    L44152,         /* AMOUNT 3         */~
                                    L44152,         /* AMOUNT 4         */~
                                    L44152          /* GOAL             */
                  goto L44168

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L44140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L44152:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L44168:     accept                                                       ~
                                                                         ~
               at (01,02), fac(hex(8c)), title$(screen%,1)      , ch(64),~
               at (02,02), fac(hex(8c)), title$(screen%,2)      , ch(64),~
                                                                         ~
               at (01,68),                                               ~
                  "!CAT:",                                               ~
               at (01,75), fac(hex(84)),   category$            , ch(06),~
               at (02,68),                                               ~
                  "+------------",                                       ~
               at (03,02), fac(hex(84)),   infomsg$             , ch(63),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),   separator$(1)        , ch(79),~
               at (09,02), fac(hex(84)),   separator$(2)        , ch(79),~
               at (13,02), fac(hex(84)),   separator$(3)        , ch(79),~
               at (17,02), fac(hex(84)),   separator$(4)        , ch(79),~
               at (21,02), fac(hex(84)),   separator$(5)        , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)),   seqnr$      (line%+1), ch(03),~
               at (10,02), fac(hex(84)),   seqnr$      (line%+2), ch(03),~
               at (14,02), fac(hex(84)),   seqnr$      (line%+3), ch(03),~
               at (18,02), fac(hex(84)),   seqnr$      (line%+4), ch(03),~
               at (22,02), fac(hex(84)),   seqnr$      (line%+5), ch(03),~
                                                                         ~
               at (06,06), "MTH"                                        ,~
               at (10,06), "MTH"                                        ,~
               at (14,06), "MTH"                                        ,~
               at (18,06), "MTH"                                        ,~
               at (22,06), "MTH"                                        ,~
                                                                         ~
               at (06,10), fac(fac$(1, 1)), method$    (line%+1), ch(06),~
               at (10,10), fac(fac$(2, 1)), method$    (line%+2), ch(06),~
               at (14,10), fac(fac$(3, 1)), method$    (line%+3), ch(06),~
               at (18,10), fac(fac$(4, 1)), method$    (line%+4), ch(06),~
               at (22,10), fac(fac$(5, 1)), method$    (line%+5), ch(06),~
                                                                         ~
               at (06,17), "DES"                                        ,~
               at (10,17), "DES"                                        ,~
               at (14,17), "DES"                                        ,~
               at (18,17), "DES"                                        ,~
               at (22,17), "DES"                                        ,~
                                                                         ~
               at (06,21), fac(fac$(1, 2)), descr$     (line%+1), ch(12),~
               at (10,21), fac(fac$(2, 2)), descr$     (line%+2), ch(12),~
               at (14,21), fac(fac$(3, 2)), descr$     (line%+3), ch(12),~
               at (18,21), fac(fac$(4, 2)), descr$     (line%+4), ch(12),~
               at (22,21), fac(fac$(5, 2)), descr$     (line%+5), ch(12),~
                                                                         ~
               at (06,34), "AP"                                         ,~
               at (10,34), "AP"                                         ,~
               at (14,34), "AP"                                         ,~
               at (18,34), "AP"                                         ,~
               at (22,34), "AP"                                         ,~
                                                                         ~
               at (06,37), fac(fac$(1, 3)), applies$   (line%+1), ch(06),~
               at (10,37), fac(fac$(2, 3)), applies$   (line%+2), ch(06),~
               at (14,37), fac(fac$(3, 3)), applies$   (line%+3), ch(06),~
               at (18,37), fac(fac$(4, 3)), applies$   (line%+4), ch(06),~
               at (22,37), fac(fac$(5, 3)), applies$   (line%+5), ch(06),~
                                                                         ~
               at (06,44), "CR"                                         ,~
               at (10,44), "CR"                                         ,~
               at (14,44), "CR"                                         ,~
               at (18,44), "CR"                                         ,~
               at (22,44), "CR"                                         ,~
                                                                         ~
               at (06,47), fac(fac$(1, 4)), creditacct$(line%+1), ch(12),~
               at (10,47), fac(fac$(2, 4)), creditacct$(line%+2), ch(12),~
               at (14,47), fac(fac$(3, 4)), creditacct$(line%+3), ch(12),~
               at (18,47), fac(fac$(4, 4)), creditacct$(line%+4), ch(12),~
               at (22,47), fac(fac$(5, 4)), creditacct$(line%+5), ch(12),~
                                                                         ~
               at (06,60), "DB"                                         ,~
               at (10,60), "DB"                                         ,~
               at (14,60), "DB"                                         ,~
               at (18,60), "DB"                                         ,~
               at (22,60), "DB"                                         ,~
                                                                         ~
               at (06,63), fac(fac$(1, 5)), debitacct$ (line%+1), ch(12),~
               at (10,63), fac(fac$(2, 5)), debitacct$ (line%+2), ch(12),~
               at (14,63), fac(fac$(3, 5)), debitacct$ (line%+3), ch(12),~
               at (18,63), fac(fac$(4, 5)), debitacct$ (line%+4), ch(12),~
               at (22,63), fac(fac$(5, 5)), debitacct$ (line%+5), ch(12),~
                                                                         ~
               at (06,76), "EMP"                                        ,~
               at (10,76), "EMP"                                        ,~
               at (14,76), "EMP"                                        ,~
               at (18,76), "EMP"                                        ,~
               at (22,76), "EMP"                                        ,~
                                                                         ~
               at (06,80), fac(fac$(1, 6)), empflag$   (line%+1), ch(01),~
               at (10,80), fac(fac$(2, 6)), empflag$   (line%+2), ch(01),~
               at (14,80), fac(fac$(3, 6)), empflag$   (line%+3), ch(01),~
               at (18,80), fac(fac$(4, 6)), empflag$   (line%+4), ch(01),~
               at (22,80), fac(fac$(5, 6)), empflag$   (line%+5), ch(01),~
                                                                         ~
               at (07,02), fac(hex(8c)),   desc$    (1, line%+1), ch(15),~
               at (11,02), fac(hex(8c)),   desc$    (1, line%+2), ch(15),~
               at (15,02), fac(hex(8c)),   desc$    (1, line%+3), ch(15),~
               at (19,02), fac(hex(8c)),   desc$    (1, line%+4), ch(15),~
               at (23,02), fac(hex(8c)),   desc$    (1, line%+5), ch(15),~
                                                                         ~
               at (07,18), fac(hex(8c)),   desc$    (2, line%+1), ch(15),~
               at (11,18), fac(hex(8c)),   desc$    (2, line%+2), ch(15),~
               at (15,18), fac(hex(8c)),   desc$    (2, line%+3), ch(15),~
               at (19,18), fac(hex(8c)),   desc$    (2, line%+4), ch(15),~
               at (23,18), fac(hex(8c)),   desc$    (2, line%+5), ch(15),~
                                                                         ~
               at (07,34), fac(hex(8c)),   desc$    (3, line%+1), ch(15),~
               at (11,34), fac(hex(8c)),   desc$    (3, line%+2), ch(15),~
               at (15,34), fac(hex(8c)),   desc$    (3, line%+3), ch(15),~
               at (19,34), fac(hex(8c)),   desc$    (3, line%+4), ch(15),~
               at (23,34), fac(hex(8c)),   desc$    (3, line%+5), ch(15),~
                                                                         ~
               at (07,50), fac(hex(8c)),   desc$    (4, line%+1), ch(15),~
               at (11,50), fac(hex(8c)),   desc$    (4, line%+2), ch(15),~
               at (15,50), fac(hex(8c)),   desc$    (4, line%+3), ch(15),~
               at (19,50), fac(hex(8c)),   desc$    (4, line%+4), ch(15),~
               at (23,50), fac(hex(8c)),   desc$    (4, line%+5), ch(15),~
                                                                         ~
               at (07,69), "GOAL"                                       ,~
               at (11,69), "GOAL"                                       ,~
               at (15,69), "GOAL"                                       ,~
               at (19,69), "GOAL"                                       ,~
               at (23,69), "GOAL"                                       ,~
                                                                         ~
               at (08,04), fac(fac$(1, 7)), amt$    (1, line%+1), ch(12),~
               at (12,04), fac(fac$(2, 7)), amt$    (1, line%+2), ch(12),~
               at (16,04), fac(fac$(3, 7)), amt$    (1, line%+3), ch(12),~
               at (20,04), fac(fac$(4, 7)), amt$    (1, line%+4), ch(12),~
               at (24,04), fac(fac$(5, 7)), amt$    (1, line%+5), ch(12),~
                                                                         ~
               at (08,20), fac(fac$(1, 8)), amt$    (2, line%+1), ch(12),~
               at (12,20), fac(fac$(2, 8)), amt$    (2, line%+2), ch(12),~
               at (16,20), fac(fac$(3, 8)), amt$    (2, line%+3), ch(12),~
               at (20,20), fac(fac$(4, 8)), amt$    (2, line%+4), ch(12),~
               at (24,20), fac(fac$(5, 8)), amt$    (2, line%+5), ch(12),~
                                                                         ~
               at (08,36), fac(fac$(1, 9)), amt$    (3, line%+1), ch(12),~
               at (12,36), fac(fac$(2, 9)), amt$    (3, line%+2), ch(12),~
               at (16,36), fac(fac$(3, 9)), amt$    (3, line%+3), ch(12),~
               at (20,36), fac(fac$(4, 9)), amt$    (3, line%+4), ch(12),~
               at (24,36), fac(fac$(5, 9)), amt$    (3, line%+5), ch(12),~
                                                                         ~
               at (08,52), fac(fac$(1,10)), amt$    (4, line%+1), ch(12),~
               at (12,52), fac(fac$(2,10)), amt$    (4, line%+2), ch(12),~
               at (16,52), fac(fac$(3,10)), amt$    (4, line%+3), ch(12),~
               at (20,52), fac(fac$(4,10)), amt$    (4, line%+4), ch(12),~
               at (24,52), fac(fac$(5,10)), amt$    (4, line%+5), ch(12),~
                                                                         ~
               at (08,66), fac(fac$(1,11)), goal$      (line%+1), ch(10),~
               at (12,66), fac(fac$(2,11)), goal$      (line%+2), ch(10),~
               at (16,66), fac(fac$(3,11)), goal$      (line%+3), ch(10),~
               at (20,66), fac(fac$(4,11)), goal$      (line%+4), ch(10),~
               at (24,66), fac(fac$(5,11)), goal$      (line%+5), ch(10),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key(keyhit%)

            if keyhit% <> 13 then L44808
                call "MANUAL" ("PRLDEDXN")
                goto L44168

L44808:        if keyhit% <> 15 then L44824
                  call "PRNTSCRN"
                  goto L44168

L44824:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TEST DATA FOR THE CATEGORY CODE.  BASICALLY, THIS JUST    *~
            * LOADS THE CATEGORY FROM THE FILE IF NEEDED.               *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110          /* CATEGORY CODE    */
                     return
L50110:     REM TEST DATA FOR CATEGORY CODE
                if category$ = "SYSTEM" then L50240
                if category$ <> " " then L50180
                readkey$ = category$
                call "PLOWCODE" (#3, readkey$, " ", -6%, -0.001, f1%(3))
                     if f1%(3) = 0% then L50220
                category$ = str(readkey$,,6)
L50180:         gosub L30000              /* LOAD CATEGORY CODE         */
                if maxlines% = 0 then return
                   return clear all
                   goto editmode
L50220:         errormsg$ = hex(00)
                return
L50240:         errormsg$ = "Deduction Category May Not Be 'SYSTEM'"
                   return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TEST DATA FOR LINE ITEMS.                                 *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L51100,         /* METHOD OF DEDXN  */~
                                    L51200,         /* DEDUCTION DESCR  */~
                                    L51300,         /* APPLIES FIELD    */~
                                    L51400,         /* CREDIT ACCOUNT   */~
                                    L51500,         /* DEBIT ACCOUNT    */~
                                    L51600,         /* EMPLOYEE FLAG    */~
                                    L51700,         /* AMOUNT 1         */~
                                    L51800,         /* AMOUNT 2         */~
                                    L51900,         /* AMOUNT 3         */~
                                    L52000,         /* AMOUNT 4         */~
                                    L52100          /* GOAL             */
                    return
L51100:     REM TEST DATA FOR METHOD OF DEDUCTION
                call "GETCODE" (#1, method$(c%), infomsg$, 1%, 0, f1%(1))
                     if f1%(1) = 0 then L51140
                return
L51140:         errormsg$ = "Unknown Method Of Deduction :" & method$(c%)
                return
L51200:     REM TEST DATA FOR DEDUCTION DESCRIPTION
                if descr$(c%) <> " " then return
                   errormsg$ = "Deduction Description May Not Be Blank!"
                   return
L51300:     REM TEST DATA FOR APPLIES FIELD
               if applies$(c%) = " " then return
               call "NUMTEST" (applies$(c%), 0, 9e7, errormsg$, 0.0, 0)
               if errormsg$ <> " " then return
                  for temp% = 1 to 6
                      if str(applies$(c%), temp%, 1) = " " then L51340
                      if str(applies$(c%), temp%, 1) < "1" then L51350
                      if str(applies$(c%), temp%, 1) > "6" then L51350
L51340:               next temp%
                  return
L51350:         errormsg$="Illegal Entry For 'APPLIES' Field :" &        ~
                                         applies$(c%)
                return
L51400:     REM TEST DATA FOR CREDIT ACCOUNT
                if creditacct$(c%) = " " then return
                   call "GETCODE" (#2, creditacct$(c%), infomsg$,        ~
                                         1%, 0, f1%(2))
                   return
L51500:     REM TEST DATA FOR DEBIT ACCOUNT
                if debitacct$(c%) = " " then return
                   call "GETCODE" (#2, debitacct$(c%), infomsg$,         ~
                                         1%, 0, f1%(2))
                   return
L51600:     REM TEST DATA FOR EMPLOYEE FLAG
                if empflag$(c%) = "Y" then return                        ~
                                      else empflag$(c%) = "N"
                return
L51700:     REM TEST DATA FOR AMOUNT 1
                if amt$(1,c%) = " " and desc$(1,c%) = " " then return
                if desc$(1,c%) <> " " or amt$(1,c%) = " " then L51725
                   errormsg$ = "This Amount Must Be Left Blank!!"
                   return
L51725:         if amt$(1,c%) = " " then amt$(1,c%) = "0"
                convert amt$(1,c%) to n, data goto L51740
                convert n to amt$(1,c%), pic(#######.####)
                convert amt$(1,c%) to n, data goto L51740
                return
L51740:            errormsg$ = "Invalid Entry For"
                   str(errormsg$, len(errormsg$)+2) = desc$(1,c%) & " :" ~
                                                          & amt$(1,c%)
                   return
L51800:     REM TEST DATA FOR AMOUNT 2
                if amt$(2,c%) = " " and desc$(2,c%) = " " then return
                if desc$(2,c%) <> " " or amt$(2,c%) = " " then L51825
                   errormsg$ = "This Amount Must Be Left Blank!!"
                   return
L51825:         if amt$(2,c%) = " " then amt$(2,c%) = "0"
                convert amt$(2,c%) to n, data goto L51840
                convert n to amt$(2,c%), pic(#######.####)
                convert amt$(2,c%) to n, data goto L51840
                return
L51840:            errormsg$ = "Invalid Entry For"
                   str(errormsg$, len(errormsg$)+2) = desc$(2,c%) & " :" ~
                                                          & amt$(2,c%)
                   return
L51900:     REM TEST DATA FOR AMOUNT 3
                if amt$(3,c%) = " " and desc$(3,c%) = " " then return
                if desc$(3,c%) <> " " or amt$(3,c%) = " " then L51925
                   errormsg$ = "This Amount Must Be Left Blank!!"
                   return
L51925:         if amt$(3,c%) = " " then amt$(3,c%) = "0"
                convert amt$(3,c%) to n, data goto L51940
                convert n to amt$(3,c%), pic(#######.####)
                convert amt$(3,c%) to n, data goto L51940
                return
L51940:            errormsg$ = "Invalid Entry For"
                   str(errormsg$, len(errormsg$)+2) = desc$(3,c%) & " :" ~
                                                          & amt$(3,c%)
                   return
L52000:     REM TEST DATA FOR AMOUNT 4
                if amt$(4,c%) = " " and desc$(4,c%) = " " then return
                if desc$(4,c%) <> " " or amt$(4,c%) = " " then L52030
                   errormsg$ = "This Amount Must Be Left Blank!!"
                   return
L52030:         if amt$(4,c%) = " " then amt$(4,c%) = "0"
                convert amt$(4,c%) to n, data goto L52045
                convert n to amt$(4,c%), pic(#######.####)
                convert amt$(4,c%) to n, data goto L52045
                return
L52045:            errormsg$ = "Invalid Entry For"
                   str(errormsg$, len(errormsg$)+2) = desc$(4,c%)        ~
                                                   & " :" & amt$(4,c%)
                   return
L52100:     REM TEST DATA FOR GOAL
                if goal$(c%) = " " then goal$(c%) = "0"
                convert goal$(c%) to n, data goto L52140
                convert n to goal$(c%), pic(-######.##)
                convert goal$(c%) to n, data goto L52140
                return
L52140:            errormsg$ = "Illegal Entry For Goal :" & goal$(c%)
                   return

L53000: REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

            errormsg$ = " "
            REM HANDLES CASE FOR "ALL" DEDUCTION DEFINITIONS
                if firstcode$ <> "ALL" then L53120
                   init(hex(00)) firstcode$
                   init(hex(ff)) lastcode$
                   return
L53120:     REM HANDLES CASE FOR SINGLE CODE
                if lastcode$ <> " " then L53160
                   lastcode$ = firstcode$
                   return
L53160:     REM HANDLES CASE FOR A RANGE OF CODES
                if lastcode$ < firstcode$ then L53200
                   firstcode$ = firstcode$ addc hex(fffffffffffe)
                   return
L53200:     REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                errormsg$ = "ILLEGAL RANGE!  Please Respecify."
                return

L60000: REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * PRINT MODE MAIN PROGRAM DRIVES PRINT MODE FOR THE DDT.    *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L60100:         for column% = 1 to 3
                    on column% gosub L61000, L62000, L63000
                    next column%
                if colsdone% >= 3 then return      /* IF FINISHED      */
                gosub L64000              /* PAGE HEADING, IF NECCESSARY*/
                print using L64400, prtcategory$, prtmethod$, prtdescr$,  ~
                            prtcreditacct$, prtdebitacct$, prtacctdescr$,~
                            prtamtdescr$, prtamt$, prtapplies$,          ~
                            prtempflag$
                prtcategory$ = " "
                tagprinted% = 0
                goto L60100

L61000:     REM HANDLES FIRST  COLUMN--METHOD, DESCRIPTION, GOAL, ETC.
                on linenumber%(1) gosub L61100, L61200
                   return
L61100:         REM HANDLES FIRST  CASE--PRINT INFORMATION.
                    prtmethod$ = method$
                    prtdescr$  = descr$
                    if empflag$ = "Y" then prtempflag$ = "YES"           ~
                                      else prtempflag$ = "NO"
                    prtapplies$ = applies$
                    linenumber%(1) = 2
                    return
L61200:         REM HANDLES SECOND CASE--ZAP VARIABLES
                    prtmethod$, prtdescr$, prtempflag$, prtgoal$,        ~
                    prtroutine$, prtapplies$ = " "
                    linenumber%(1) = 3
                    colsdone% = colsdone% + 1
                    return

L62000:     REM HANDLES SECOND COLUMN--ACCOUNT NUMBERS
                on linenumber%(2) gosub L62100, L62200, L62300
                   return
L62100:         REM HANDLES FIRST  CASE--NONBLANK CREDIT ACCOUNT
                    if creditacct$ = " " then L62200
                    prtcreditacct$ = creditacct$
                    prtdebitacct$  = " "
                    call "GETCODE" (#2, creditacct$, prtacctdescr$,      ~
                                                   0%, 99, f1%(2))
                    if f1%(2) = 0 then prtacctdescr$="Account Not On File"
                    linenumber%(2) = 2
                    return
L62200:         REM HANDLES SECOND CASE--NONBLANK DEBIT  ACCOUNT
                    if debitacct$  = " " then L62300
                    prtdebitacct$  = debitacct$
                    prtcreditacct$ = " "
                    call "GETCODE" (#2, debitacct$, prtacctdescr$,       ~
                                                   0%, 99, f1%(2))
                    if f1%(2) = 0 then prtacctdescr$="Account Not On File"
                    linenumber%(2) = 3
                    return
L62300:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    prtcreditacct$, prtdebitacct$, prtacctdescr$ = " "
                    linenumber%(2) = 4
                    colsdone% = colsdone% + 1
                    return

L63000:     REM HANDLES THIRD  COLUMN--AMOUNT/DESCRIPTION FIELDS
                on linenumber%(3) gosub L63100, L63200, L63300, L63400,      ~
                                        L63500, L63600
                   return
L63100:         REM HANDLES FIRST  CASE--NONBLANK AMOUNT/DESCRIPTION 1
                    if amtdescr$(1) = " " then L63200
                    prtamtdescr$ = amtdescr$(1)
                    call "CONVERT" (amount(1), 2.4, prtamt$)
                    linenumber%(3) = 2
                    return
L63200:         REM HANDLES SECOND CASE--NONBLANK AMOUNT/DESCRIPTION 2
                    if amtdescr$(2) = " " then L63300
                    prtamtdescr$ = amtdescr$(2)
                    call "CONVERT" (amount(2), 2.4, prtamt$)
                    linenumber%(3) = 3
                    return
L63300:         REM HANDLES THIRD  CASE--NONBLANK AMOUNT/DESCRIPTION 3
                    if amtdescr$(3) = " " then L63400
                    prtamtdescr$ = amtdescr$(3)
                    call "CONVERT" (amount(3), 2.4, prtamt$)
                    linenumber%(3) = 4
                    return
L63400:         REM HANDLES FOURTH CASE--NONBLANK AMOUNT/DESCRIPTION 4
                    if amtdescr$(4) = " " then L63500
                    prtamtdescr$ = amtdescr$(4)
                    call "CONVERT" (amount(4), 2.4, prtamt$)
                    linenumber%(3) = 5
                    return
L63500:         REM HANDLES FIFTH  CASE--GOAL
                    if goal = 0 then L63600
                    prtamtdescr$ = "GOAL"
                    call "CONVERT" (goal, 2.4, prtamt$)
                    linenumber%(3) = 6
                    return
L63600:         REM HANDLES SIXTH  CASE--ZAP VARIABLES
                    prtamt$, prtamtdescr$ = " "
                    linenumber%(3) = 7
                    colsdone% = colsdone% + 1
                    return

L64000:     REM PAGE TITLE SUBROUTINE.
                select printer(134)
                line% = line% + 1
                if line% < 60 then return
                   call "DATE" addr ("HD", hdrdate$)
                   if page% = 0 then L64090
                      if tagprinted% <> 0 then L64090
                         print using L64370
                         tagprinted% = 1
L64090:            print page
                   page% = page% + 1
                   print using L64210, date$, cmpname$, page%
                   print using L64220, prgm$, rpttitle$, rptid$
                   print
                   print using L64250
                   print using L64280
                   print using L64310
                   print using L64340
                   print using L64370
                   line% = 6              /* SET STARTING LINE ON PAGE  */
                   return

L64210: %########                          ##############################~
        ~##############################                        PAGE: #####~
        ~#
L64220: %########                          ##############################~
        ~##############################                      REPORT: #####~
        ~#

L64250: %+------+------+------------+------------------------------------~
        ~--------------------+--------------------------+------+--------+

L64280: %!CATE- !      !            !          A C C O U N T  I N F O R M~
        ~ A T I O N          !    AMOUNT INFORMATION    !      !EMPLOYEE!

L64310: %! GORY !METHOD! DESCRIPTION+---------+---------+----------------~
        ~--------------------+---------------+----------+APPLYS!  PAYS  !

L64340: %! CODE !      !            !  CREDIT    !   DEBIT    !     D E S~
        ~ C R I P T I O N    !  DESCRIPTION  !  AMOUNT  !      !  THIS? !

L64370: %+------+------+------------+------------+------------+----------~
        ~--------------------+---------------+----------+------+--------+

L64400: %!######!######!############!############!############!##########~
        ~####################!###############!##########!######!########!

L64430: %!      +------+------------+------------+------------+----------~
        ~--------------------+---------------+----------+------+--------+

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
