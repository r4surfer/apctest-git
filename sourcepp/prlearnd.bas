        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE   AAA   RRRR   N   N  DDDD    *~
            *  P   P  R   R  L      E      A   A  R   R  NN  N  D   D   *~
            *  PPPP   RRRR   L      EEEE   AAAAA  RRRR   N N N  D   D   *~
            *  P      R   R  L      E      A   A  R   R  N  NN  D   D   *~
            *  P      R   R  LLLLL  EEEEE  A   A  R   R  N   N  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLEARND - PAYROLL EARNINGS DEFAULT FILE SETUP AND PRINT  *~
            *            PROGRAM.                                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/07/80 ! ORIGINAL                                 ! BCW *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 03/27/86 ! CONFORMED SCREENS AND REPORTS TO STANDARD! SGA *~
            * 06/13/91 ! Eliminated unused function FNX.          ! JDH *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            acct$16,                     /* ACCOUNT NUMBER FOR PRINT   */~
            acct$(100)16,                /* ACCOUNT NUMBER FOR INPUT   */~
            blankline$79,                /* BLANK LINE FOR PRINT SCREEN*/~
            cash$3,                      /* PAYS IN CASH? FOR PRINT    */~
            cash$(100)3,                 /* THIS PAYABLE IN CASH?      */~
            catkey$7,                    /* Plow Key for Category      */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            catagory$4,                  /* CATAGORY   CODE THIS ENTRY */~
            catg$4,                      /* CATAGORY   FOR PRINT MODE  */~
            clmhdr$(2)30,                /* Alt column header PLOWCODE */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(5, 9)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            filler$(100)35,              /* EARNINGS LINE FILLER       */~
            firstcode$4,                 /* FIRST CATAGORY   TO PRINT  */~
            hdrdate$45,                  /* FORMATTED DATE/TIME INFO   */~
            i$(24)80,                    /* SCREEN IMAGE--NOT USED     */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            junk$(100)1,                 /* FREE BYTE IN MIDDLE OF LINE*/~
            keydescr$32,                 /* Earn Type for Plow         */~
            lastcode$4,                  /* LAST CODE IN PRINT RANGE   */~
            lastcatg$4,                  /* LAST CATAGORY   INPUT      */~
            linenumber%(3),              /* LINE POINTER FOR PRINT MODE*/~
            linfac$(20)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            pfkeys$(4)17,                /* FUNCTION KEYS ENABLED      */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            prtacct$16,                  /* ACCOUNT NUMBER TO PRINT    */~
            prtacctdescr$30,             /* DESCRIPTION OF ACCOUNT     */~
            prtcash$3,                   /* PRINT FOR "PAY IN CASH?"   */~
            prtcatg$4,                   /* CATAGORY   CODE FOR PRINTS */~
            prtrate$10,                  /* RATE TO PRINT              */~
            prttype$12,                  /* DESCRIPTION OF EARNING     */~
            prtunits$6,                  /* UNITS TO PRINT (HRS,PCS...)*/~
            prtusbucks$10,               /* PRINT USUAL AMOUNT         */~
            prtusunits$10,               /* PRINT FOR USUAL # OF UNITS */~
            rate$(100)10,                /* RATES FOR EARNINGS         */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            rpttitle$60,                 /* REPORT TITLE               */~
            separator$(5)79,             /* SEPARATOR LINES ("---LINE")*/~
            sicka$(100)2,                /* EARNINGS SICK ACCRUAL FLAG */~
            thiscode$50,                 /* CODE THIS CATG IN PRINTING */~
            title$(4,2)64,               /* P.F. KEY TITLES            */~
            tran$(24)80,                 /* CURSOR==>FIELD TRAN FOR EDT*/~
            type$12,                     /* DESCRIPTION OF EARN FOR PRT*/~
            type$(100)12,                /* TYPE CODES FOR INPUT MODE  */~
            units$12,                    /* UNITS PER RATE FOR PRINT   */~
            units$(100)6,                /* UNITS PER RATE             */~
            usbucks$(100)10,             /* USUAL DOLLARS FIELD        */~
            userid$3,                    /* USERID OF CURRENT USER.    */~
            usunits$(100)10,             /* USUAL UNITS FOR INPUT      */~
            vacat$(100)2                 /* EARNINGS VACATION ACRL FLAG*/~

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

*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)


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

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            REM SET STRINGS FOR TABLE EDIT COMPUTATION.
            init(hex(00)) tran$()
            init(hex(01)) str(tran$(6),  1)
            init(hex(02)) str(tran$(6), 19)
            init(hex(03)) str(tran$(6), 27)
            init(hex(04)) str(tran$(6), 46)
            init(hex(05)) str(tran$(6), 62)
            init(hex(06)) str(tran$(7),  1)
            init(hex(07)) str(tran$(7), 23)
            init(hex(08)) str(tran$(8),  1)
            init(hex(09)) str(tran$(8), 22)
            copy str(tran$(), 321, 1280) to str(tran$(), 641, 1280)

            title$(1,1) = "(1)START OVER(2)COL 1(4)LINE ABOVE(13)INSTRS(1~
        ~6)EDIT MODE"
            title$(2,1) = "(1)START OVER(2)FIRST(3)LAST(4)PREV(5)NEXT(6)D~
        ~OWN(7)UP"
            title$(2,2) = "(9)HEADER(11)INS(12)DEL(13)INSTRS(15)PRT SCRN ~
        ~(16)WRITE DATA"
            title$(3,1) = "SUPPLY REQUESTED ITEMS AND (ENTER) OR (1) TO E~
        ~XIT INSERT MODE"
            title$(4,1) = "PRESS (ENTER) TO DELETE FLASHING LINE OR (1) T~
        ~O EXIT DELETE."

            pfkeys$(1) = hex(000102040d0f10ffffffffffffffffffff)
            pfkeys$(2) = hex(0001020304050607090b0c0d0f10ffffff)
            pfkeys$(3) = hex(00010d0fffffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010d0fffffffffffffffffffffffffff)

            prgm$ = "PRLEARND"
            rptid$ = "PRL008"
            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,62,9) = "PRLEARND:"
            str(prgmid$,72,8) = str(cms2v$,1,8)
            rpttitle$ = "P A Y R O L L    E A R N I N G S    C A T E G O ~
        ~R I E S"
            clmhdr$(2) = "CATEGORY     1st Type on File"
            clmhdr$(1) = "  Seq #      Description"

L10000: REM *************************************************************~
            *         I N P U T   D E P A R T M E N T   C O D E         *~
            *                                                           *~
            * INPUT EARNINGS DEFAULT CATAGORY   CODE                    *~
            *************************************************************

        inputmode
            editmode% = 0
            init(" ") errormsg$, infomsg$, inpmessage$, blankline$,      ~
                      catagory$, rate$(), cash$(), junk$(), units$(),    ~
                      acct$(), type$(), usunits$(), usbucks$(), sicka$(),~
                      vacat$(), filler$()
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
            if screenline% < 6 then L11130
               line% = line% + 5
               screenline% = 1
L11130:     c%, currentline% = line% + screenline%
            if currentline% > 100 then editmode
            call "SETSEP" (separator$(), line%, screenline%)

L11170:     for fieldnr% = 1 to 9
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
            init(" ") type$(c%), cash$(c%), junk$(c%), units$(c%),       ~
                      rate$(c%), acct$(c%), usunits$(c%), usbucks$(c%),  ~
                      sicka$(c%), vacat$(c%), filler$(c%), errormsg$,    ~
                      infomsg$
            return clear all
            goto L11170

        lineabove
            if currentline% = 1 then return
            on fieldnr% gosub L14260,               /* EARNINGS TYPE    */~
                              L14270,               /* PAYABLE IN CASH? */~
                              L14280,               /* TAXABLE?         */~
                              L14290,               /* UNITS DESCRIPTION*/~
                              L14300,               /* RATE PER UNIT    */~
                              L14310,               /* ACCOUNT NUMBER   */~
                              L14320,               /* USUAL UNITS      */~
                              L14330                /* USUAL BUCKAROOS  */
                    return
L14260:     type$   (c%) = type$      (c%-1): return
L14270:     cash$   (c%) = cash$      (c%-1): return
L14280:     junk$   (c%) = junk$      (c%-1): return
L14290:     units$  (c%) = units$     (c%-1): return
L14300:     rate$   (c%) = rate$      (c%-1): return
L14310:     acct$   (c%) = acct$      (c%-1): return
L14320:     usunits$(c%) = usunits$   (c%-1): return
L14330:     usbucks$(c%) = usbucks$   (c%-1): return
            sicka$  (c%) = sicka$     (c%-1): return
            vacat$  (c%) = vacat$     (c%-1): return
            filler$ (c%) = filler$    (c%-1): return

        REM *************************************************************~
            *                   I N S E R T   M O D E                   *~
            *                                                           *~
            * INSERT LOGIC HERE.                                        *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return         /* ARRAY FULL, CAN'T*/
            REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
L15090:         screenline% = int((cursor%(1)-1)/4)
                if line% + screenline% < maxlines% then L15120
                   screenline% = maxlines% - line% /* TO INS AT END    */
L15120:         if screenline% <> 5 then L15170     /* BOTTOM OF PAGE   */
                   line% = line% + 1
                   screenline% = screenline% - 1
                   goto L15170

L15170:         call "SETSEP" (separator$(),line%,                       ~
                                      min(5%, maxlines%-line%+1%))
                currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L15340
                for temp% = maxlines% to c% step -1
                    type$   (temp%+1) = type$   (temp%)
                    cash$   (temp%+1) = cash$   (temp%)
                    junk$   (temp%+1) = junk$   (temp%)
                    units$  (temp%+1) = units$  (temp%)
                    rate$   (temp%+1) = rate$   (temp%)
                    acct$   (temp%+1) = acct$   (temp%)
                    usunits$(temp%+1) = usunits$(temp%)
                    usbucks$(temp%+1) = usbucks$(temp%)
                    sicka$  (temp%+1) = sicka$  (temp%)
                    vacat$  (temp%+1) = vacat$  (temp%)
                    filler$ (temp%+1) = filler$ (temp%)
                    next temp%

L15340:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") type$(c%), cash$(c%), junk$(c%), units$(c%),   ~
                          rate$(c%), acct$(c%), usunits$(c%), sicka$(c%),~
                          usbucks$(c%), vacat$(c%), filler$(c%),         ~
                          infomsg$, errormsg$

            REM NOW INPUT THE LINE, MAKE SO WE CAN CANCEL OUT IF NECC
                infomsg$ = " "
                for fieldnr% = 1 to 9
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L15520
L15460:             gosub'223(screenline%, fieldnr%)
                          if keyhit%  =  1 then L15580
                          if keyhit% <>  0 then L15460
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L15460
L15520:             next fieldnr%

                maxlines% = maxlines% + 1
                cursor%(1) = min(cursor%(1)+4, 24)
                goto L15090

L15580:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                gosub L15710              /* ACTUALLY DELETE @C%        */

                temp% = maxlines% + 1
                init(" ") type$(temp%), cash$(temp%), junk$(temp%),      ~
                          units$(temp%), rate$(temp%), acct$(temp%),     ~
                          usunits$(temp%), usbucks$(temp%), infomsg$,    ~
                          errormsg$, sicka$(temp%), vacat$(temp%),       ~
                          filler$(temp%)

            if currentline% >= maxlines% and screenline% = 5             ~
               then line% = max(0%, line% - 1%)
            return

L15710:     for temp% = currentline% to maxlines%
                type$   (temp%) = type$   (temp%+1)
                cash$   (temp%) = cash$   (temp%+1)
                units$  (temp%) = units$  (temp%+1)
                rate$   (temp%) = rate$   (temp%+1)
                junk$   (temp%) = junk$   (temp%+1)
                acct$   (temp%) = acct$   (temp%+1)
                usunits$(temp%) = usunits$(temp%+1)
                usbucks$(temp%) = usbucks$(temp%+1)
                sicka$  (temp%) = sicka$  (temp%+1)
                vacat$  (temp%) = vacat$  (temp%+1)
                filler$ (temp%) = filler$ (temp%+1)
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
            screenline% = int((cursor%(1)-1)/4)
            if screenline% < 1 then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return

L16130:     gosub'233(screenline%)
                  if keyhit%  =  1 then       return
                  if keyhit% <>  0 then       L16130

            c% = currentline%
            if currentline% < maxlines% then gosub L15710
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") type$(temp%), cash$(temp%), junk$(temp%), infomsg$,~
                      units$(temp%), rate$(temp%), acct$(temp%),         ~
                      usunits$(temp%), usbucks$(temp%), errormsg$,       ~
                      sicka$(temp%), vacat$(temp%), filler$(temp%)

            maxlines% = maxlines% - 1
            if currentline% >= maxlines% and screenline% = 5             ~
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

L17120:     gosub L42000
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L65000
                  if keyhit%  <> 0 then       L17120
            gosub L53000
                  if errormsg$ <> " " then L17120

            REM PLOW ROUTINE FOR PRINTING EARNINGS CATAGORY   DEFAULT LIST
                page% = 0
                line% = 1000
                tagprinted% = 1
                thiscode$ = firstcode$
                call "SHOSTAT" ("Printing Earnings Categories")

L17250:         call "PLOWNEXT" (#3, thiscode$, 0%, f1%(3))
                     if f1%(3) = 0 then L17320
                     if thiscode$ > lastcode$ then L17320
                catg$ = str(thiscode$, 1, 4)
                gosub L60000              /* LOAD AND PRINT THAT CATG.  */
                init(hex(ff)) str(thiscode$, 5)
                goto L17250

L17320:         REM RETURN FROM ROUTINE
                    close printer
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
                inpmessage$ = "To Print A Listing Of The Available Earnin~
        ~gs Types, Press (3)."
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
                                    L21200,         /* PAYABLE IN CASH? */~
                                    L21400,         /* UNITS            */~
                                    L21500,         /* RATE PER UNIT    */~
                                    L21600,         /* ACCOUNT NUMBER   */~
                                    L21700,         /* USUAL UNITS      */~
                                    L21800,         /* USUAL DOLLARS    */~
                                    L21880,         /* SICK ACCRUAL FLAG*/~
                                    L21950          /* VACA ACCRUAL FLAG*/
                    return
L21100:     REM DEFAULT/ENABLE FOR EARNINGS TYPE
                enabled% = 1
                return
L21200:     REM DEFAULT/ENABLE FOR PAID IN CASH?
                cash$(c%) = "Y"
                enabled% = 1
                return
L21400:     REM DEFAULT/ENABLE FOR UNITS
                enabled% = 1
                units$(c%) = "HOURS"
                return
L21500:     REM DEFAULT/ENABLE FOR RATE PER UNIT
                enabled% = 1
                return
L21600:     REM DEFAULT/ENABLE FOR ACCOUNT NUMBER
                enabled% = 1
                return
L21700:     REM DEFAULT/ENABLE FOR USUAL UNITS
                enabled% = 1
                return
L21800:     REM DEFAULT/ENABLE FOR USUAL DOLLARS
                if usunits$(c%) = " " then return
                   if rate$(c%) = " " then L21860
                      convert usunits$(c%) to usunits
                      convert rate$(c%) to rate
                      call "CONVERT" (rate * usunits,-2.2, usbucks$(c%))
L21860:            enabled% = 1
                   return
L21880:     REM DEFAULT/ENABLE FOR SICK ACCRUAL FLAG
        REM     RETURN
                sicka$(c%) = "1"
                if str(type$(c%),,4)= "SICK" then sicka$(c%) = "-1"
                   infomsg$ = "Enter 1 if this accrues Sick Pay, -1 if it~
        ~ takes Sick Pay, or 0 if neither."
                enabled% = 1
                return
L21950:     REM DEFAULT/ENABLE FOR VACATION ACCRUAL FLAG
        REM     RETURN
                vacat$(c%) = "1"
                if str(type$(c%),,4)= "VACA" then vacat$(c%) = "-1"
                   infomsg$ = "Enter 1 if this accrues Vacation Pay, -1 i~
        ~f it takes Vctn Pay, or 0 if neither."
                enabled% = 1
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

            get   #3, using L30280,   type$(temp%), cash$(temp%),         ~
               junk$(temp%),  units$(temp%), rate, acct$(temp%), usunits,~
               usbucks, sicka$(temp%), vacat$(temp%), filler$(temp%)
            call "GLFMT" (acct$(temp%))

            if rate   <> 0                                               ~
               then convert rate to rate$(temp%), pic(-####.####)
            if usunits <> 0                                              ~
               then convert usunits to usunits$(temp%), pic(-####.####)
            if usbucks <> 0                                              ~
               then convert usbucks to usbucks$(temp%), pic(-######.##)
            if cash$(temp%) = "Y" then cash$(temp%) = "YES"              ~
                                  else cash$(temp%) = "NO"
            goto L30090

L30280:     FMT XX(4),                   /* CATAGORY   CODE            */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(1),                   /* PAID IN CASH?              */~
                CH(1),                   /* FREE BYTE                  */~
                CH(6),                   /* UNITS                      */~
                PD(14,4),                /* RATE PER UNIT              */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                PD(14,4),                /* USUAL UNITS                */~
                PD(14,4),                /* USUAL DOLLARS              */~
                CH(2),                   /* SICK ACCRUAL FLAG          */~
                CH(2),                   /* VACATION ACCRUAL FLAG      */~
                CH(35)                   /* FILLER                     */~

L31000: REM *************************************************************~
            *   W R I T E S   T H E   D A T A   T O   T H E   F I L E   *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE FROM THE ARRAY.  IT MUST HAVE *~
            * BEEN DELETED IF ALREADY PRESENT BY THE "DATA SAVE" ROUTINE*~
            *************************************************************

            if maxlines% = 0 then return

            for temp% = 1 to maxlines%
                rate, usunits, usbucks = 0
                if rate$(temp%) <> " " then convert rate$(temp%) to rate
                if usunits$(temp%) <> " " then convert usunits$(temp%)   ~
                                                    to usunits
                if usbucks$(temp%) <> " " then convert usbucks$(temp%)   ~
                                                    to usbucks
                call "GLUNFMT" (acct$(temp%))
                write #3, using L31230, catagory$, temp%,                 ~
                          type$(temp%), cash$(temp%),                    ~
                          junk$(temp%), units$(temp%), rate,acct$(temp%),~
                          usunits, usbucks, sicka$(temp%), vacat$(temp%),~
                          filler$(temp%)
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
                PD(14,4),                /* USUAL UNITS                */~
                PD(14,4),                /* USUAL DOLLARS              */~
                CH(2),                   /* SICK ACCRUAL FLAG          */~
                CH(2),                   /* VACATION ACCRUAL FLAG      */~
                CH(35)                   /* FILLER                     */

L32000: REM *************************************************************~
            *   G E T   A   R E C O R D   F O R   P R I N T   M O D E   *~
            *                                                           *~
            * GETS A RECORD FOR PRINT MODE.  IT MUST HAVE ALREADY BEEN  *~
            * FOUND BY PLOW NEXT.                                       *~
            *************************************************************

            get   #3, using L32110,   catg$, type$, cash$, units$,        ~
                                     rate, acct$, usunits, usbucks
            call "GLFMT" (acct$)
            return

L32110:     FMT CH(4),                   /* CATAGORY   CODE            */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(1),                   /* PAID IN CASH?              */~
                XX(1),                   /* FREE BYTE                  */~
                CH(6),                   /* UNITS                      */~
                PD(14,4),                /* RATE PER UNIT              */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                PD(14,4),                /* USUAL UNITS                */~
                PD(14,4)                 /* USUAL DOLLARS              */~

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
                  "Enter Earnings Category",                             ~
               at (01,67), "Date:",                                      ~
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
                  call "MANUAL" ("PRLEARND")
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
                  "Edit Earnings Category",                              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),     errormsg$          , ch(79),~
               at (06,02),                                               ~
                  "CATEGORY CODE",                                       ~
               at (06,30), fac(linfac$( 1)), catagory$          , ch(04),~
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
                  call "MANUAL" ("PRLEARND")
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
                  "Print Earnings Categories",                           ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST CATEGORY  ",                                    ~
               at (06,30), fac(hex(81)), firstcode$             , ch(04),~
               at (07,02),                                               ~
                  "LAST CATEGORY  ",                                     ~
               at (07,30), fac(hex(81)), lastcode$              , ch(04),~
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
                  call "MANUAL" ("PRLEARND")
                  goto L42070

L42350:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *       I N P U T   L I N E   I T E M S   S C R E E N       *~
            *                                                           *~
            * INPUTS LINE ITEMS.  USES MULTI-LINE "BCKINPUT" FORMAT.    *~
            *************************************************************

            deffn'203(screenline%, fieldnr%): screen% = 1: goto L44200
            deffn'213(screenline%, fieldnr%): screen% = 2
                 init(hex(86)) fac$()
                 if fieldnr% = 0 then L44210              : goto L44200
            deffn'223(screenline%, fieldnr%): screen% = 3: goto L44200

            deffn'233(screenline%)
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 8
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L44420

L44200:           init(hex(84)) fac$()
L44210:           on fieldnr% gosub L44350,         /* EARNINGS TYPE    */~
                                    L44350,         /* PAYABLE IN CASH? */~
                                    L44350,         /* UNITS            */~
                                    L44380,         /* RATE PER UNIT    */~
                                    L44350,         /* ACCOUNT NUMBER   */~
                                    L44380,         /* USUAL UNITS      */~
                                    L44380,         /* USUAL DOLLARS    */~
                                    L44350,         /* SICK ACCRUAL FLAG*/~
                                    L44350          /* VACA ACCRUAL FLAG*/
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

L44420:     accept                                                       ~
                                                                         ~
               at (01,02), fac(hex(8c)), title$(screen%,1)      , ch(64),~
               at (02,02), fac(hex(8c)), title$(screen%,2)      , ch(64),~
                                                                         ~
               at (01,68),                                               ~
                  "!CAT.:",                                              ~
               at (01,75), fac(hex(84)),   catagory$            , ch(04),~
               at (02,68),                                               ~
                  "+----------",                                         ~
               at (03,02), fac(hex(84)),   infomsg$             , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$( 1)      , ch(79),~
               at (09,02), fac(hex(84)),    separator$( 2)      , ch(79),~
               at (13,02), fac(hex(84)),    separator$( 3)      , ch(79),~
               at (17,02), fac(hex(84)),    separator$( 4)      , ch(79),~
               at (21,02), fac(hex(84)),    separator$( 5)      , ch(79),~
                                                                         ~
               at (06,02), "TYPE"                                       ,~
               at (10,02), "TYPE"                                       ,~
               at (14,02), "TYPE"                                       ,~
               at (18,02), "TYPE"                                       ,~
               at (22,02), "TYPE"                                       ,~
                                                                         ~
               at (06,07), fac(fac$(1, 1)), type$     (line%+ 1), ch(12),~
               at (10,07), fac(fac$(2, 1)), type$     (line%+ 2), ch(12),~
               at (14,07), fac(fac$(3, 1)), type$     (line%+ 3), ch(12),~
               at (18,07), fac(fac$(4, 1)), type$     (line%+ 4), ch(12),~
               at (22,07), fac(fac$(5, 1)), type$     (line%+ 5), ch(12),~
                                                                         ~
               at (06,20), "CASH?"                                      ,~
               at (10,20), "CASH?"                                      ,~
               at (14,20), "CASH?"                                      ,~
               at (18,20), "CASH?"                                      ,~
               at (22,20), "CASH?"                                      ,~
                                                                         ~
               at (06,26), fac(fac$(1, 2)), cash$     (line%+ 1), ch(01),~
               at (10,26), fac(fac$(2, 2)), cash$     (line%+ 2), ch(01),~
               at (14,26), fac(fac$(3, 2)), cash$     (line%+ 3), ch(01),~
               at (18,26), fac(fac$(4, 2)), cash$     (line%+ 4), ch(01),~
               at (22,26), fac(fac$(5, 2)), cash$     (line%+ 5), ch(01),~
                                                                         ~
               at (06,28), "UNIT DESCR."                                ,~
               at (10,28), "UNIT DESCR."                                ,~
               at (14,28), "UNIT DESCR."                                ,~
               at (18,28), "UNIT DESCR."                                ,~
               at (22,28), "UNIT DESCR."                                ,~
                                                                         ~
               at (06,40), fac(fac$(1, 3)), units$    (line%+ 1), ch(06),~
               at (10,40), fac(fac$(2, 3)), units$    (line%+ 2), ch(06),~
               at (14,40), fac(fac$(3, 3)), units$    (line%+ 3), ch(06),~
               at (18,40), fac(fac$(4, 3)), units$    (line%+ 4), ch(06),~
               at (22,40), fac(fac$(5, 3)), units$    (line%+ 5), ch(06),~
                                                                         ~
               at (06,47), "RATE"                                       ,~
               at (10,47), "RATE"                                       ,~
               at (14,47), "RATE"                                       ,~
               at (18,47), "RATE"                                       ,~
               at (22,47), "RATE"                                       ,~
                                                                         ~
               at (06,52), fac(fac$(1, 4)), rate$     (line%+ 1), ch(10),~
               at (10,52), fac(fac$(2, 4)), rate$     (line%+ 2), ch(10),~
               at (14,52), fac(fac$(3, 4)), rate$     (line%+ 3), ch(10),~
               at (18,52), fac(fac$(4, 4)), rate$     (line%+ 4), ch(10),~
               at (22,52), fac(fac$(5, 4)), rate$     (line%+ 5), ch(10),~
                                                                         ~
               at (06,63), "ACCT"                                       ,~
               at (10,63), "ACCT"                                       ,~
               at (14,63), "ACCT"                                       ,~
               at (18,63), "ACCT"                                       ,~
               at (22,63), "ACCT"                                       ,~
                                                                         ~
               at (06,68), fac(fac$(1, 5)), acct$     (line%+ 1), ch(12),~
               at (10,68), fac(fac$(2, 5)), acct$     (line%+ 2), ch(12),~
               at (14,68), fac(fac$(3, 5)), acct$     (line%+ 3), ch(12),~
               at (18,68), fac(fac$(4, 5)), acct$     (line%+ 4), ch(12),~
               at (22,68), fac(fac$(5, 5)), acct$     (line%+ 5), ch(12),~
                                                                         ~
               at (07,02), "USUAL UNITS"                                ,~
               at (11,02), "USUAL UNITS"                                ,~
               at (15,02), "USUAL UNITS"                                ,~
               at (19,02), "USUAL UNITS"                                ,~
               at (23,02), "USUAL UNITS"                                ,~
                                                                         ~
               at (07,14), fac(fac$(1, 6)), usunits$  (line%+ 1), ch(10),~
               at (11,14), fac(fac$(2, 6)), usunits$  (line%+ 2), ch(10),~
               at (15,14), fac(fac$(3, 6)), usunits$  (line%+ 3), ch(10),~
               at (19,14), fac(fac$(4, 6)), usunits$  (line%+ 4), ch(10),~
               at (23,14), fac(fac$(5, 6)), usunits$  (line%+ 5), ch(10),~
                                                                         ~
               at (07,25), "USUAL DOLLARS"                              ,~
               at (11,25), "USUAL DOLLARS"                              ,~
               at (15,25), "USUAL DOLLARS"                              ,~
               at (19,25), "USUAL DOLLARS"                              ,~
               at (23,25), "USUAL DOLLARS"                              ,~
                                                                         ~
               at (07,39), fac(fac$(1, 7)), usbucks$  (line%+ 1), ch(10),~
               at (11,39), fac(fac$(2, 7)), usbucks$  (line%+ 2), ch(10),~
               at (15,39), fac(fac$(3, 7)), usbucks$  (line%+ 3), ch(10),~
               at (19,39), fac(fac$(4, 7)), usbucks$  (line%+ 4), ch(10),~
               at (23,39), fac(fac$(5, 7)), usbucks$  (line%+ 5), ch(10),~
                                                                         ~
               at (08,02), "SICK ACCRUAL FLAG",                          ~
               at (12,02), "SICK ACCRUAL FLAG",                          ~
               at (16,02), "SICK ACCRUAL FLAG",                          ~
               at (20,02), "SICK ACCRUAL FLAG",                          ~
               at (24,02), "SICK ACCRUAL FLAG",                          ~
                                                                         ~
               at (08,20), fac(fac$(1, 8)), sicka$(line%+1)     , ch(02),~
               at (12,20), fac(fac$(2, 8)), sicka$(line%+2)     , ch(02),~
               at (16,20), fac(fac$(3, 8)), sicka$(line%+3)     , ch(02),~
               at (20,20), fac(fac$(4, 8)), sicka$(line%+4)     , ch(02),~
               at (24,20), fac(fac$(5, 8)), sicka$(line%+5)     , ch(02),~
                                                                         ~
               at (08,23), "VACATION ACCRUAL FLAG",                      ~
               at (12,23), "VACATION ACCRUAL FLAG",                      ~
               at (16,23), "VACATION ACCRUAL FLAG",                      ~
               at (20,23), "VACATION ACCRUAL FLAG",                      ~
               at (24,23), "VACATION ACCRUAL FLAG",                      ~
                                                                         ~
               at (08,45), fac(fac$(1, 9)), vacat$(line%+1)     , ch(02),~
               at (12,45), fac(fac$(2, 9)), vacat$(line%+2)     , ch(02),~
               at (16,45), fac(fac$(3, 9)), vacat$(line%+3)     , ch(02),~
               at (20,45), fac(fac$(4, 9)), vacat$(line%+4)     , ch(02),~
               at (24,45), fac(fac$(5, 9)), vacat$(line%+5)     , ch(02),~
               keys(pfkeys$(screen%)),                                   ~
               key(keyhit%)

               if keyhit% <> 13 then L45520
                  call "MANUAL" ("PRLEARND")
                  goto L44420

L45520:        if keyhit% <> 15 then L45560
                  call "PRNTSCRN"
                  goto L44420

L45560:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
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
                f1%(3) = -6%
                call "PLOWCODE" (#3,catkey$,keydescr$,1004%,-0.12,f1%(3),~
                                 clmhdr$())
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
                                    L51200,         /* PAYABLE IN CASH? */~
                                    L51400,         /* UNITS DESCRIPTION*/~
                                    L51500,         /* RATE PER UNIT    */~
                                    L51600,         /* ACCOUNT NUMBER   */~
                                    L51700,         /* USUAL UNITS      */~
                                    L51800,         /* USUAL BUCKAROOS  */~
                                    L51900,         /* SICK ACCRUAL FLAG*/~
                                    L51980          /* VACA ACCRUAL FLAG*/
                    return
L51100:     REM TEST DATA FOR EARNINGS TYPE
                if type$(c%) <> " " then return
                   errormsg$ = "Earnings Type Cannot Be Blank!!"
                   return
L51200:     REM TEST DATA FOR PAYABLE IN CASH QUESTION
                if cash$(c%) = " " then return
                   if str(cash$(c%), 1, 1) = "Y" then cash$(c%) = "YES"  ~
                                                 else cash$(c%) = "NO"
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
                if acct$(c%) = " " then return
                   call "GETCODE" (#2, acct$(c%), infomsg$, 1%, 0, f1%(2))
                   if f1%(2) = 0 then errormsg$ = "Account NOt On File"
                   return
L51700:     REM TEST DATA FOR USUAL UNITS
                if usunits$(c%) = " " then return
                   convert usunits$(c%) to n, data goto L51740
                   convert round(n,4) to usunits$(c%), pic(-####.####)
                   convert usunits$(c%) to n, data goto L51740
                   return
L51740:               errormsg$ = "Illegal Entry For Usual Units :"      ~
                                         & usunits$(c%)
                   return
L51800:     REM TEST DATA FOR USUAL DOLLARS
                if usbucks$(c%) <> " " and usunits$(c%) = " " then L51880
                   if usbucks$(c%) = " " then return
                convert usbucks$(c%) to n, data goto L51850
                convert n to usbucks$(c%), pic(-######.##)
                convert usbucks$(c%) to n, data goto L51850
                return
L51850:         errormsg$ = "Illegal Entry For Usual Amount :"           ~
                                         & usbucks$(c%)
                return
L51880:         errormsg$ = "Usual Dollars Field Must Be Blank!!!"
                return
L51900:     REM TEST DATA FOR SICK ACCRUAL FLAG
                     if sicka$(c%) = " " then return
                     convert sicka$(c%) to u3%, data goto L51950
                     convert u3% to sicka$(c%), pic(-#)
                     convert sicka$(c%) to u3%, data goto L51950
                     if abs(u3%) < 2 then return
L51950:              errormsg$ = "Enter 1,0, or -1 :" & sicka$(c%)
                     return

L51980:     REM TEST DATA FOR VACATION ACCRUAL FLAG
                     if vacat$(c%) = " " then return
                     convert vacat$(c%) to u3%, data goto L52030
                     convert u3% to vacat$(c%), pic(-#)
                     convert vacat$(c%) to u3%, data goto L52030
                     if abs(u3%) < 2 then return
L52030:              errormsg$ = "Enter 1,0, or -1 :" & vacat$(c%)
                     return

L53000: REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

            errormsg$ = " "
            REM HANDLES CASE FOR "ALL" CATAGORIES
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
                   firstcode$ = firstcode$ addc hex(fffffffe)
                   return
L53200:     REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                errormsg$ = "ILLEGAL RANGE!  Please Respecify."
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
                   gosub L64000
                   REM HANDLE TAG LINE ("+-----+---+...")
                       if tagprinted% = 1 then return
                          print using L64410        /* SEPARATOR LINE   */
                          tagprinted% = 1
                          return                   /* NEXT PART        */
L60200:         gosub L64000              /* PAGE HEADING, IF NECCESSARY*/
                print using L64450, prtcatg$, prttype$, prtunits$,        ~
                            prtrate$, prtacct$, prtacctdescr$,           ~
                            prtusunits$, prtusbucks$, prtcash$
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
                    if cash$ = "Y" then prtcash$ = "YES"                 ~
                                   else prtcash$ = "NO"
                    prtunits$ = units$
                    prttype$  = type$
                    prtacct$ = acct$
                    if rate = 0 then prtrate$ = " "                      ~
                       else call "CONVERT" (rate, 2.4, prtrate$)
                    if acct$ = " " then prtacct$, prtacctdescr$ = " "    ~
                                   else call "GETCODE" (#2, acct$,       ~
                                           prtacctdescr$, 0%, 99, f1%(2))
                    if usunits = 0 then prtusunits$ = " "                ~
                       else call "CONVERT" (usunits, 2.4, prtusunits$)
                    if usbucks = 0 or usunits = 0                        ~
                       then prtusbucks$ = " "                            ~
                       else call "CONVERT" (usbucks, 2.4, prtusbucks$)
                    return
L62300:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    prttype$, prtunits$, prtcash$, prtrate$,             ~
                    prtacct$, prtacctdescr$, prtusunits$, prtusbucks$= " "
                    linenumber%(2) = 4
                    colsdone% = colsdone% + 1
                    return

L64000:     REM PAGE TITLE SUBROUTINE.
                select printer(134)
                line% = line% + 1
                if line% < 60 then return
                   call "DATE" addr ("HD", hdrdate$)
                   if page% = 0 then L64090
                      if tagprinted% = 1 then L64090
                         print using L64410
                         tagprinted% = 1
L64090:            print page
                   page% = page% + 1
                   print using L64210, date$, cmpname$, page%
                   print using L64220, prgm$, rpttitle$, rptid$
                   print
                   print using L64250
                   print using L64290
                   print using L64330
                   print using L64370, "#"
                   print using L64410
                   line% = 9              /* SET STARTING LINE ON PAGE  */
                   return

L64210: %########                          ##############################~
        ~##############################                        PAGE: #####~
        ~#
L64220: %########                            ############################~
        ~##############################                      REPORT: #####~
        ~#

L64250: %+----+------------+------+----------+---------------------------~
        ~----------------+---------------------+-------+

L64290: %!CAT.!  EARNINGS  ! UNIT ! RATE PER !  A C C O U N T  I N F O R ~
        ~M A T I O N     !    USUAL PAYROLL    !PAID IN!

L64330: %!    !            !  OF  !          +------------+--------------~
        ~----------------+----------+----------+ CASH? !

L64370: %!CODE!    TYPE    ! RATE !   UNIT   !  ACCOUNT   !     D E S C R~
        ~ I P T I O N    !# OF UNITS!TOTAL AMT.!       !

L64410: %+----+------------+------+----------+------------+--------------~
        ~----------------+----------+----------+-------+

L64450: %!####!############!######!##########!############!##############~
        ~################!##########!##########!  ###  !

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
