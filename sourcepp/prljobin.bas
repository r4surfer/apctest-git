        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      JJJJJ   OOO   BBBB   IIIII  N   N   *~
            *  P   P  R   R  L        J    O   O  B   B    I    NN  N   *~
            *  PPPP   RRRR   L        J    O   O  BBBB     I    N N N   *~
            *  P      R   R  L      J J    O   O  B   B    I    N  NN   *~
            *  P      R   R  LLLLL   J      OOO   BBBB   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLJOBIN - INPUT JOB EARNINGS DETAIL FOR EMPLOYEES        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/01/81 ! ORIGINAL                                 ! TEM *~
            * 08/20/81 ! ADD GL ACCOUNT ON LINE ITEM              ! TEM *~
            * 10/26/81 ! EXIT WITHOUT PRINTING                    ! TEM *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 01/13/86 ! Fix Double GL Format @ 51391             ! KAB *~
            * 06/09/87 ! File format changes & added GENCODES for ! MJB *~
            *          !    Labor Class                           !  -  *~
            * 10/19/87 ! Made labor class optional, ergo stuff    ! HES *~
            * 12/11/87 ! Validate Labor Class Against Current Set ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 04/06/93 !PRR 11101 - Summary now displayes multiple! MLJ *~
            *          !  screens at 15 lines for a max od 100.   !     *~
            *          !  Earnings Recap Screen also modified to  !     *~
            *          !  display 15 lines per screen.            !     *~
            *          !PRR 11271 - PF(4)Line Above now captures  !     *~
            *          !  W/C Activity Code.                      !     *~
            *          !MISC - Standardization of all screens,    !     *~
            *          !  messages and Startover routine, fixed   !     *~
            *          !  numerous implied integers.              !     *~
            * 10/28/93 !Now accums by type for correct summary    ! MLJ *~
            *          ! display of units within type.            !     *~
            * 06/07/95 ! PRR 13450.  Corrected potential array    ! JDH *~
            *          !  overflow.                               !     *~
            *          ! Corrected subscript error.               !     *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            account$(100)16,             /* GL ACCOUNT ON LINE ITEM    */~
            bighead$79,                  /* Screen Header              */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* BLANK LINE FOR PRINT SCREEN*/~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            cunits$(101,3)6,             /* CURRENT UNITS              */~
            crate$(100)7,                /* CURRENT UNITS              */~
            camt$(101,3)10,              /* CURRENT DOLLARS            */~
            cunits(101),                 /* CURRENT UNITS              */~
            camt(101),                   /* CURRENT DOLLARS            */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            day$(100)8,                  /* DATES FOR DISPLAY          */~
            dayty$(100)12,               /* EARNINGS TYPE ON THE DAY   */~
            desc_map(6),                 /* For PLOWCODE               */~
            dstack$(100)10,              /* HOURS DISPLAY              */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            empname$70,                  /* EMPLOYEE NAME              */~
            ernacct$(100)16,             /* EARNINGS TYPE ACCOUNTS     */~
            erntype$(100)3,              /* EARNINGS TYPE NUMBER       */~
            ernunit$(100)10,             /* HOURS WORKED               */~
            etype%(100),                 /* CONVERTED EARNINGS TYPE    */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(5, 8)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            finder$20,                   /* STACK PUSHING VARIABLE     */~
            header$79,                   /* HEADER FOR SCREEN          */~
            hdr$(2)70,                   /* For PLOWCODE               */~
            i$(24)80,                    /* SCREEN IMAGE--NOT USED     */~
            inex(2),                     /* For PLOWCODE               */~
            inex$(2)1,                   /* For PLOWCODE               */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            job$(100)8,                  /* JOB NUMBER LINE ITEM       */~
            jbd$(100)32,                 /* JOB NUMBER DESCRIPTION     */~
            jobacct$16,                  /* LABOR ACCOUNT THIS JOB     */~
            linfac$(20)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            lct$2,                       /* LOCATOR ARRAY FOR SEARCH   */~
            lct1$2,                      /* ALL HEX ZEROES             */~
            name$60,                     /* For PLOWCODE               */~
            num$(100)3,                  /* TO DISPLAY EARNINGS TYPES  */~
            pf24$79,                     /* Summary Screen Line 24     */~
            pfkeys$(5)17,                /* FUNCTION KEYS ENABLED      */~
            pos%(1),                     /* Search array               */~
            prldate$8,                   /* USER'S PAYROLL DATE        */~
            rate$(101)10,                /* RATES FOR EARNINGS         */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            readkey1$50,                 /* KEY FOR PLOW ROUTINES      */~
            separator$(5)79,             /* SEPARATOR LINES ("---LINE")*/~
            stack$(100)25,               /* STACK                      */~
            stack(100),                  /* NUMERIC STACK              */~
            sumscrn$(3)79,               /* SUMMARY SCREEN INSTRUCTIONS*/~
            this(100),                   /* For Accum by Type          */~
            title$(4,2)64,               /* P.F. KEY TITLES            */~
            tran$(25)80,                 /* CURSOR==>FIELD TRAN FOR EDT*/~
            type$(101)12,                /* TYPE CODES FOR INPUT MODE  */~
            units$(101)6,                /* UNITS PER RATE             */~
            userid$3,                    /* USERID OF CURRENT USER.    */~
            lclass$(100)4,               /* LABOR CLASS CODES          */~
            empsclass$4,                 /* EMPLOYEES LABOR CLASS      */~
            cls$(100)32,                 /* LABOR CLASS DESCRIPTIONS   */~
            wcact$(100)4,                /* WC Activity Code           */~
            wctr$(100)4,                 /* WORKCENTER CODE            */~
            wdate$(100)8,                /* DATE WORK DONE             */~
            worked(100),                 /* For Accumulation           */~
            workcenter$4                 /* DATE WORK DONE             */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * # 2 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 3 ! EMPEARN1 ! EMPLOYEE EARNINGS FILE                   *~
            * # 5 ! JOBMASTR ! JOB MASTER FILE                          *~
            * # 6 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 7 ! GENCODES ! General Codes File                       *~
            * # 9 ! PRLBUFFR ! EMPLOYEE JOB EARNINGS BUFFER             *~
            * #10 ! GLMAIN   ! GENERAL LEDGER                           *~
            * #11 ! JBMASTR2 ! MFG MANAGEMENT JOB MASTER                *~
            * #12 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * #30 ! STCnnnnL ! Std Costing Labor Standards (STCLABOR)   *~
            *-----+----------+------------------------------------------*~
            * where 'nnnn' is a unique cost set identifier generated by *~
            * the call to 'STCFOPEN'.                                   *~
            *************************************************************

            select #1, "SYSFILE2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1, keylen = 20

             select #2, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #3, "EMPEARN1",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 28

            select #5, "JOBMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 700,                                    ~
                       keypos = 1, keylen = 8

            select #6, "USERINFO",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 150,                                    ~
                       keypos = 1, keylen = 3

            select #7,  "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #9, "PRLBUFFR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 150,                                    ~
                       keypos = 1, keylen = 18

            select #10, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #11, "JBMASTR2",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8

            select #12, "WCMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =  2, keylen = 5,                         ~
                        alt key 1, keypos =  1, keylen = 6

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #30, "STCLABOR", varc, indexed, recsize = 323,        ~
                        keypos = 1, keylen = 4


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#2, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#3, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#5, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#6, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#7, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#11, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#12, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#14, 0%, 0%,   0%, " ")

            stc% = 0% : errormsg$ = " "
            call "STCFOPEN" /* Open STCLABOR in 'Shared' mode          */~
                (" ", "xxxSxx", #1, errormsg$, #30,#30,#30,#30,#30,#30)
            if errormsg$ <> " " then stc% = 1% /* Can't use STCLABOR */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZE VARIABLES NEEDED FOR SCREEN I/O, EDIT MODE,    *~
            * AND SYSTEM DATES.                                         *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            header$  = "Select Employee For Input"
            str(header$,62) = "PRLJOBIN: " & cms2v$
            bighead$ = "Earnings Types   Units    Rate     Amount   Units~
        ~     Amount   Units     Amount"

        REM CHECK PAYROLL DATE
            call "EXTRACT" addr ("ID", userid$)
            call "READ100" (#6, userid$, f1%(6))
                 if f1%(6) = 0 then L65000
                 get #6, using L09140, prldate$
L09140:                  FMT XX(15), CH(6)
                 call "WHICHPER" (#1, prldate$, thisperiod%)
                 if thisperiod% = 0 then L65000

            date$ = prldate$
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            REM SET STRINGS FOR TABLE EDIT COMPUTATION.
            init(hex(00)) tran$()
            init(hex(01)) str(tran$(6),1,10)
            init(hex(02)) str(tran$(6), 24)
            init(hex(03)) str(tran$(6), 68)
            init(hex(04)) str(tran$(7),  1)
            init(hex(05)) str(tran$(7), 24)
            init(hex(06)) str(tran$(7), 54)
            init(hex(07)) str(tran$(8), 37)
            init(hex(08)) str(tran$(8), 60)
            copy str(tran$(), 321, 1280) to str(tran$(), 641, 1280)
            tran$(25) = " "

            title$(1,1) ="(1)Start Over   (2)Column One     (4)Line Above"
            title$(1,2) ="(8)See Summary  (13)Instructions  (16)Edit Mode"
            title$(2,1) = "(1)Strt Ovr (2)First (3)Last (4)Prev (5)Next (~
        ~6)Down (7)Up"
            title$(2,2) = "(8)Sumry(9)Header(11)Insrt(12)Del(15)Prt Scrn ~
        ~(16)Save Data"
            title$(3,1) = "Enter Requested Items And Press RETURN -OR-"
            title$(3,2) = "(1)Exit Insert Mode  (8)See Summary"
            title$(4,1) = "Press RETURN To Delete Flashing Line Or (1) To~
        ~ Exit Delete"

            pfkeys$(1) = hex(000102040d0f1008ffffffffffffffffff)
            pfkeys$(2) = hex(0001020304050607090b0c0d0f1008ffff)
            pfkeys$(3) = hex(0001040d0f08ffffffffffffffffffffff)
            pfkeys$(4) = hex(00010fffffffffffffffffffffffffffff)

            mat inex = zer
            inex(1) = 1.011  :  inex$(1) = "C"
            hdr$(1) = "  Employee Code               Employee Name"
            desc_map(1) = -17.10  :  desc_map(2) = 14.0
            desc_map(3) = -17.01  :  desc_map(4) = 25.0
            desc_map(5) = - 2.15  :  desc_map(6) = 27.0

        REM *************************************************************~
            *         I N P U T   E M P L O Y E E   C O D E             *~
            *                                                           *~
            * INPUT EMPLOYEE CODE                                       *~
            *************************************************************

        inputmode
            editmode% = 0: mat etype% = zer
            mat cunits = zer : mat camt = zer
            init(" ") errormsg$, infomsg$, inpmessage$, blankline$,      ~
                      empcode$, empname$, erntype$(), lclass$(), wctr$(),~
                      wdate$(), ernunit$(), type$(), cls$(), units$(),   ~
                      rate$(), num$(), job$(), jbd$(), account$(),       ~
                      ernacct$(), workcenter$

            for fieldnr% = 1 to 2
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10220
L10150:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit%  = 32 and fieldnr% = 1 then L65000
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

L11170:     for fieldnr% = 1 to 8
                gosub'163(fieldnr%)      /* ENABLE FIELDS FOR INPUT.   */
                      if enabled% =  0 then       L11280
L11200:         gosub'203(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L11280
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  4 then gosub lineabove
                      if keyhit%  =  8 then gosub summary
                      if keyhit%  = 16 and fieldnr% = 1 then editmode
                      goto L11200
L11280:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L11200
                next fieldnr%
                maxlines% = maxlines% + 1
                goto L11090

        REM *************************************************************~
            *      E D I T   H E A D E R   M A I N   P R O G R A M      *~
            *                                                           *~
            * EDITS HEADER OF THE EMPLOYEE EARNINGS ENTRY               *~
            *************************************************************

        editmode
            editmode% = 1
            errormsg$, infomsg$, inpmessage$ = " "

L12100:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       L13000
                  if keyhit%  = 16 then       proofcheck
                  if keyhit% <>  0 then       L12100
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 1 then L12100
            go to L12100                  /* NO EDIT          */

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
                  if keyhit%  =  8 then gosub summary
                  if keyhit%  = 16 then       proofcheck
                  call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
                  goto L13130

L13320:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                fieldnr% = val(str(tran$(cursor%(1)),cursor%(2)))
                if fieldnr% = 0 then L13130
                screenline% = (cursor%(1)-5)/4+1
                c%, currentline% = line% + screenline%
                if currentline% > maxlines% then L13130

                gosub'163(fieldnr%)
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
            init(" ") erntype$(c%), lclass$(c%), wdate$(c%), wctr$(c%),  ~
                      ernunit$(c%), cls$(c%), job$(c%), jbd$(c%),        ~
                      errormsg$, infomsg$, account$(c%)
            etype%(c%) = 0
            return clear all
            goto L11170

        lineabove
            if currentline% = 1 then return
            on fieldnr% gosub L14240,               /* EARNINGS TYPE    */~
                              L14260,               /* JOB NUMBER       */~
                              L14267,               /* WORK CENTER      */~
                              L14270,               /* LABOR CLASS CODE */~
                              L14285,               /* W/C ACTIVITY     */~
                              L14290,               /* DATE WORK DONE   */~
                              L14300,               /* UNITS WORKED     */~
                              L14305                /* GL ACCOUNT       */
                    return
L14240:     erntype$(c%) = erntype$   (c%-1)
            etype%  (c%) = etype%     (c%-1): return
L14260:     job$    (c%) = job$       (c%-1)
            jbd$    (c%) = jbd$       (c%-1): return
L14267:     wctr$   (c%) = wctr$      (c%-1): return
L14270:     lclass$ (c%) = lclass$    (c%-1)
            cls$    (c%) = cls$       (c%-1): return
L14285:     wcact$  (c%) = wcact$     (c%-1): return
L14290:     wdate$  (c%) = wdate$     (c%-1): return
L14300:     ernunit$(c%) = ernunit$   (c%-1): return
L14305:     account$(c%) = account$   (c%-1): return

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
                    erntype$(temp%+1) = erntype$(temp%)
                    etype%  (temp%+1) = etype%  (temp%)
                    job$    (temp%+1) = job$    (temp%)
                    jbd$    (temp%+1) = jbd$    (temp%)
                    wctr$   (temp%+1) = wctr$   (temp%)
                    lclass$ (temp%+1) = lclass$ (temp%)
                    wcact$  (temp%+1) = wcact$  (temp%)
                    cls$    (temp%+1) = cls$    (temp%)
                    wdate$  (temp%+1) = wdate$  (temp%)
                    ernunit$(temp%+1) = ernunit$(temp%)
                    account$(temp%+1) = account$(temp%)
                    next temp%

L15340:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") erntype$(c%), lclass$(c%), cls$(c%), wctr$(c%),~
                          wdate$(c%), ernunit$(c%), job$(c%), jbd$(c%),  ~
                          infomsg$, errormsg$, account$(c%), wcact$(c%)
                etype%(c%) = 0

            REM NOW INPUT THE LINE, MAKE SO WE CAN CANCEL OUT IF NECC
                infomsg$ = " "
                for fieldnr% = 1 to 8
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L15530
L15470:             gosub'223(screenline%, fieldnr%)
                          if keyhit%  =  1 then L15590
                          if keyhit%  =  4 then gosub lineabove
                          if keyhit%  =  8 then gosub summary
                          if keyhit% <>  0 then L15470
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L15470
L15530:             next fieldnr%

                maxlines% = maxlines% + 1
                cursor%(1) = min(cursor%(1)+4, 24)
                goto L15090

L15590:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                gosub L15720              /* ACTUALLY DELETE @C%        */

                temp% = maxlines% + 1
                init(" ") erntype$(temp%), lclass$(temp%), cls$(temp%),  ~
                          wdate$(temp%), ernunit$(temp%), infomsg$,      ~
                          errormsg$, job$(temp%), jbd$(temp%),           ~
                          account$(temp%), wctr$(temp%), wcact$(temp%)
                etype%(temp%) = 0

            if currentline% >= maxlines% and screenline% = 5             ~
               then line% = max(0%, line% - 1%)
            return

L15720:     for temp% = currentline% to maxlines%
                erntype$(temp%) = erntype$(temp%+1)
                etype%  (temp%) = etype%  (temp%+1)
                job$    (temp%) = job$    (temp%+1)
                jbd$    (temp%) = jbd$    (temp%+1)
                wctr$   (temp%) = wctr$   (temp%+1)
                lclass$ (temp%) = lclass$ (temp%+1)
                wcact$  (temp%) = wcact$  (temp%+1)
                cls$    (temp%) = cls$    (temp%+1)
                wdate$  (temp%) = wdate$  (temp%+1)
                ernunit$(temp%) = ernunit$(temp%+1)
                account$(temp%) = account$(temp%+1)
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
            if currentline% < maxlines% then gosub L15720
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") erntype$(temp%), lclass$(temp%), cls$(temp%),      ~
                      wdate$(temp%), ernunit$(temp%), infomsg$,          ~
                      errormsg$, job$(temp%), jbd$(temp%), wctr$(temp%), ~
                      account$(temp%), wcact$(temp%)
            etype%(temp%) = 0

            maxlines% = maxlines% - 1
            if currentline% >= maxlines% and screenline% = 5             ~
               then line% = max(0%, line% - 1%)
               return

        REM *************************************************************~
            *                     S A V E   D A T A                     *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE, DELETING PREVIOUS VERSION.   *~
            *************************************************************

        datasave
            lastemp$ = empcode$
            REM DELETE OLD BEFORE WRITING NEW.
                readkey$ = userid$
                str(readkey$, 4) = empcode$
                call "DELETE" (#9, readkey$, 15%)
            gosub L31000
            goto inputmode

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   F I R S T   P A G E *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE FIRST PAGE.  AT  *~
            * PRESENT, THIS ROUTINE DOES EXACTLY NOTHING.               *~
            *************************************************************

            deffn'161(fieldnr%)
                enabled% = 1%
                    on fieldnr% gosub L20100, L20150
                    return

L20100:     REM DEFAULT/ENABLE FOR EMPLOYEE CODE...
                inpmessage$ = "Enter Employee Code."
                return

L20150:     REM DEFAULT/ENABLE FOR WORK CENTER DEFAULT...
                inpmessage$ = "Enter Default Work Center Code."
                return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   L I N E   I T E M S *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR THE LINE ITEM FIELDS. *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  c% = currentline%
                  on fieldnr% gosub L21180,         /* EARNINGS TYPE    */~
                                    L21230,         /* JOB NUMBER       */~
                                    L21280,         /* WORK CENTER      */~
                                    L21340,         /* LABOR CLASS CODE */~
                                    L21420,         /* WC Activity Code */~
                                    L21460,         /* DATE WORK DONE   */~
                                    L21520,         /* UNITS WORKED     */~
                                    L21560          /* ACCOUNT          */
                    return
L21180:     REM DEFAULT/ENABLE FOR EARNINGS TYPE
                infomsg$ = "Enter Earnings Type Number."
                enabled% = 1
                return
L21230:     REM DEFAULT/ENABLE FOR JOB NUMBER
                infomsg$ = "Enter Job Number Or Leave Blank."
                enabled% = 1
                return
L21280:     REM DEFAULT/ENABLE FOR WORK CENTER
                if job$(c%) = " " then return
                infomsg$ = "Enter Work Center Code."
                wctr$(c%) = workcenter$
                enabled% = 1
                return
L21340:     REM DEFAULT/ENABLE FOR LABOR CLASS CODE
                if stc% = 1% then return  /* No Cost Set */
                if job$(c%) = " " then return
                infomsg$ = "Enter Labor Class Code."
                lclass$(c%) = empsclass$
                enabled% = 1
                return
L21420:     REM Default/Enable for WC Activity Code
                if job$(c%) = " " then return
                enabled% = 1%
                infomsg$ = "Enter Work Center Activity Code."
                return
L21460:     REM DEFAULT/ENABLE FOR DATE WORK DONE
                wdate$(c%) = prldate$
                call "DATEFMT" (wdate$(c%))
                enabled% = 1
                infomsg$ = "Enter Date Work Was Done."
                return
L21520:     REM DEFAULT/ENABLE FOR UNITS WORKED
                enabled% = 1
                infomsg$ = "Enter Units Worked."
                return
L21560:     REM DEFAULT/ENABLE FOR ACCOUNT
                infomsg$ = "Enter General Ledger Account Number."
                enabled% = 1
                if job$(c%) = " " then L21650
                     if jobacct$ = " " then L21650
                     if str(jobacct$, 1, 1) = hex(00) then L21650
                     account$(c%) = jobacct$
                     return
L21650:         account$(c%) = ernacct$(etype%(c%))
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
                if k% = 1% then return
                if k% <> 0% then startover
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *       L O A D   D A T A   F R O M   T H E   F I L E       *~
            *                                                           *~
            * LOADS THE DATA FROM THE FILE.                             *~
            *************************************************************

            readkey$ = empcode$
            maximum% = 0

L30090:     call "PLOWNEXT" (#3, readkey$, 12%, f1%(3))
                 if f1%(3) = 0 then return
            maximum%, temp% = maximum% + 1

            get   #3, using L30220, type$(temp%), units$(temp%),          ~
                                   rate, ernacct$(temp%), cunits(temp%), ~
                                   camt(temp%)
            call "CONVERT" (rate, -4.4, rate$(temp%))
            call "GLFMT" (ernacct$(temp%))
            convert temp% to num$(temp%), pic(##)
            str(num$(temp%),3,1) = ")"
            goto L30090

L30220:     FMT XX(27),                  /* SKIP TO TYPE               */~
                CH(12),                  /* EARNINGS TYPE              */~
                XX(6),                   /* SKIP TO UNITS              */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                PD(14,4),                /* UNIT RATE                  */~
                CH(9),                   /* EXPENSE ACCOUNT            */~
                XX(32),                  /* SKIP TO CURRENT            */~
                2*PD(14,4)               /* CURRENT UNITS AND AMOUNT   */~

L31000: REM *************************************************************~
            *   W R I T E S   T H E   D A T A   T O   T H E   F I L E   *~
            *                                                           *~
            * WRITES THE DATA TO THE FILE FROM THE ARRAY.  IT MUST HAVE *~
            * BEEN DELETED IF ALREADY PRESENT BY THE "DATA SAVE" ROUTINE*~
            *************************************************************

            if maxlines% = 0 then return

            for temp% = 1 to maxlines%
                worked = 0
                call "DATUNFMT" (wdate$(temp%))
                if ernunit$(temp%) <> " " then convert ernunit$(temp%)   ~
                                                    to worked
                call "GLUNFMT" (account$(temp%))
                write #9, using L31210, userid$, empcode$, temp%,         ~
                          erntype$(temp%), type$(etype%(temp%)),         ~
                          job$(temp%), lclass$(temp%), wdate$(temp%),    ~
                          worked, account$(temp%), wctr$(temp%),         ~
                          wcact$(temp%), " "
                next temp%
            return

L31210:     FMT CH(3),                   /* USERID  CODE               */~
                CH(12),                  /* EMPLOYEE CODE              */~
                PIC(###),                /* SEQUENCE NUMBER            */~
                CH(03),                  /* EARNINGS TYPE NUMBER       */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(08),                  /* JOB NUMBER                 */~
                CH(4),                   /* LABOR CLASS CODE           */~
                CH(6),                   /* DATE WORK DONE             */~
                PD(14,4),                /* UNITS WORKED               */~
                CH(9),                   /* GL ACCOUNT                 */~
                CH(4),                   /* WORK CENTER                */~
                CH(4),                   /* WC Activity Code           */~
                CH(74)                   /* SPARE CHANGE               */~

L32000: REM *************************************************************~
            *       L O A D   D A T A   F R O M   T H E   F I L E       *~
            *                                                           *~
            * LOADS THE DATA FROM THE FILE.                             *~
            *************************************************************

            REM SET KEY AND COUNTER
                readkey$ = userid$
                str(readkey$, 4) = empcode$
                maxlines% = 0

            REM PLOW THROUGH FILE, EXIT WHEN DONE
L32120:         call "PLOWNEXT" (#9, readkey$, 15%, f1%(9))
                      if f1%(9) = 0 then return
                      maxlines%, temp% = maxlines% + 1

                    get #9, using L32280, erntype$(temp%), job$(temp%),   ~
                                    lclass$(temp%), wdate$(temp%),       ~
                                    worked, account$(temp%),             ~
                                    wctr$(temp%), wcact$(temp%)
            REM FORMAT DATA
                call "GLFMT" (account$(temp%))
                call "CONVERT" (worked, -2.4, ernunit$(temp%))
                readkey1$ = "LBR CLASS" & str(lclass$(temp%))
                call "DESCRIBE" (#7, readkey1$, cls$(temp%), 1%, f1%(7))
                call "DESCRIBE" (#5,job$   (temp%),jbd$(temp%),1%,f1%(5))
                if f1%(5)=0 then                                         ~
                call "DESCRIBE" (#11,job$(temp%),jbd$(temp%),1%,f1%(11))
                call "DATEFMT" (wdate$(temp%))
                convert erntype$(temp%) to etype%(temp%)

            goto L32120

L32280:     FMT XX(18),                  /* SKIP TO TYPE               */~
                CH(3),                   /* EARNINGS TYPE NUMBER       */~
                XX(12),                  /* EARNINGS TYPE              */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(4),                   /* LABOR CLASS CODE           */~
                CH(6),                   /* DATE WORK DONE             */~
                PD(14,4),                /* UNITS WORKED               */~
                CH(9),                   /* GL ACCOUNT                 */~
                CH(4),                   /* WORKCENTER CODE            */~
                CH(4)                    /* WC Activity Code           */~

        REM *************************************************************~
            *    T O T A L   U P   BY DAY BY EARNINGS TYPE              *~
            *                                                           *~
            *************************************************************
        proofcheck
            if maxlines% = 0 then datasave

                init(hex(ff)) stack$()              /* Initialize Stack */
                init(hex(00)) lct1$
                mat stack = zer
                ptr% = 0%

                overall = 0                         /* Load & Total Hrs */
                for t% = 1% to maxlines%
                    convert ernunit$(t%) to worked
                    gosub L35000
                    overall = overall + worked
                next t%
                overall = round(overall, 2%)

                call "SORT" addr(str(stack$(), 1%),100%,25%)

                if ptr% >= 100% then L33320          /* Blank Unused     */
                temp% = ptr% + 1%
                for t% = temp% to 100%
                    stack$(t%) = " "
                next t%

L33320:                                             /* Display Totals   */
                init(" ") dstack$(), day$(), dayty$()
                for t% = 1% to ptr%
                    convert str(stack$(t%), 21%, 5%) to temp%
                    day$(t%) = str(stack$(t%), 1%, 8%)
                    dayty$(t%) = str(stack$(t%), 9%, 12%)
                    call "CONVERT" (stack(temp%), -2.4, dstack$(t%))
                next t%
                sumscrn$(1%) = empname$
                str(sumscrn$(1%), 35%) = "Total Units ="
                convert overall to str(sumscrn$(1%), 50%), pic(###.##)
                line% = 0%
L33410:         gosub L39000
                     if keyhit%  =  1% then editmode
                     if keyhit%  =  2% then line% = 0%
                     if keyhit%  =  3% then line% = max(0%,maxlines%-15%)
                     if keyhit%  =  4% then line% = max(0%,line%-15%)
                     if keyhit%  =  5% then line% = min(line%+15,max(0%, ~
                                                   maxlines%-15%))
                     if keyhit% <> 16% then L33410
                     go to datasave

            REM STACK PUSHING ROUTINE
L35000:          str(finder$,1%) = wdate$(t%)
                 str(finder$,9%) = type$(etype%(t%))
                 search stack$() = finder$ to lct$ step 25
                 if str(lct$,1%,2%) = str(lct1$,1%,2%) then L35065
                    location% = val(lct$,2%)/25% + 1%
                    stack(location%) = stack(location%) + worked
                    stack(location%) = round(stack(location%), 2%)
                    return
L35065:          if ptr% >= 100% then return
                 ptr% = ptr% + 1%
                 stack$(ptr%) = finder$
                 convert ptr% to str(stack$(ptr%),21%,5%), pic(#####)
                 stack(ptr%) = worked
                 return

L39000: REM *************************************************************~
            * D I S P L A Y   H O U R   T O T A L S   R E C A P         *~
            *************************************************************

            sumscrn$(2%) = "(1)Return To Edit          (2)First        "&~
                           " (4)Prev            (15)Print Screen"
            sumscrn$(3%) = "                           (3)Last         "&~
                           " (5)Next            (16)Save Data   "
            pfkeys$ = hex(01020304050f10)
            if ptr% >15% then L39090
                str(sumscrn$(2%),28%,36%) = " "
                str(sumscrn$(3%),28%,36%) = " "
                    str(pfkeys$,2%,4%) = hex(ff)

L39090:     blankline$ = "Numbers Shown Represent Units By Date And Type."


L39120:     accept                                                       ~
               at (01,02), "Earnings Entry Recap Display",               ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (04,02), fac(hex(84)), sumscrn$(1%)            ,ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), day$(line% +  1%)       ,ch(08),~
               at (07,02), fac(hex(8c)), day$(line% +  2%)       ,ch(08),~
               at (08,02), fac(hex(8c)), day$(line% +  3%)       ,ch(08),~
               at (09,02), fac(hex(8c)), day$(line% +  4%)       ,ch(08),~
               at (10,02), fac(hex(8c)), day$(line% +  5%)       ,ch(08),~
               at (11,02), fac(hex(8c)), day$(line% +  6%)       ,ch(08),~
               at (12,02), fac(hex(8c)), day$(line% +  7%)       ,ch(08),~
               at (13,02), fac(hex(8c)), day$(line% +  8%)       ,ch(08),~
               at (14,02), fac(hex(8c)), day$(line% +  9%)       ,ch(08),~
               at (15,02), fac(hex(8c)), day$(line% + 10%)       ,ch(08),~
               at (16,02), fac(hex(8c)), day$(line% + 11%)       ,ch(08),~
               at (17,02), fac(hex(8c)), day$(line% + 12%)       ,ch(08),~
               at (18,02), fac(hex(8c)), day$(line% + 13%)       ,ch(08),~
               at (19,02), fac(hex(8c)), day$(line% + 14%)       ,ch(08),~
               at (20,02), fac(hex(8c)), day$(line% + 15%)       ,ch(08),~
                                                                         ~
               at (06,14), fac(hex(8c)), dayty$(line% +  1%)     ,ch(12),~
               at (07,14), fac(hex(8c)), dayty$(line% +  2%)     ,ch(12),~
               at (08,14), fac(hex(8c)), dayty$(line% +  3%)     ,ch(12),~
               at (09,14), fac(hex(8c)), dayty$(line% +  4%)     ,ch(12),~
               at (10,14), fac(hex(8c)), dayty$(line% +  5%)     ,ch(12),~
               at (11,14), fac(hex(8c)), dayty$(line% +  6%)     ,ch(12),~
               at (12,14), fac(hex(8c)), dayty$(line% +  7%)     ,ch(12),~
               at (13,14), fac(hex(8c)), dayty$(line% +  8%)     ,ch(12),~
               at (14,14), fac(hex(8c)), dayty$(line% +  9%)     ,ch(12),~
               at (15,14), fac(hex(8c)), dayty$(line% + 10%)     ,ch(12),~
               at (16,14), fac(hex(8c)), dayty$(line% + 11%)     ,ch(12),~
               at (17,14), fac(hex(8c)), dayty$(line% + 12%)     ,ch(12),~
               at (18,14), fac(hex(8c)), dayty$(line% + 13%)     ,ch(12),~
               at (19,14), fac(hex(8c)), dayty$(line% + 14%)     ,ch(12),~
               at (20,14), fac(hex(8c)), dayty$(line% + 15%)     ,ch(12),~
                                                                         ~
               at (06,29), fac(hex(84)), dstack$(line% +  1%)    ,ch(10),~
               at (07,29), fac(hex(84)), dstack$(line% +  2%)    ,ch(10),~
               at (08,29), fac(hex(84)), dstack$(line% +  3%)    ,ch(10),~
               at (09,29), fac(hex(84)), dstack$(line% +  4%)    ,ch(10),~
               at (10,29), fac(hex(84)), dstack$(line% +  5%)    ,ch(10),~
               at (11,29), fac(hex(84)), dstack$(line% +  6%)    ,ch(10),~
               at (12,29), fac(hex(84)), dstack$(line% +  7%)    ,ch(10),~
               at (13,29), fac(hex(84)), dstack$(line% +  8%)    ,ch(10),~
               at (14,29), fac(hex(84)), dstack$(line% +  9%)    ,ch(10),~
               at (15,29), fac(hex(84)), dstack$(line% + 10%)    ,ch(10),~
               at (16,29), fac(hex(84)), dstack$(line% + 11%)    ,ch(10),~
               at (17,29), fac(hex(84)), dstack$(line% + 12%)    ,ch(10),~
               at (18,29), fac(hex(84)), dstack$(line% + 13%)    ,ch(10),~
               at (19,29), fac(hex(84)), dstack$(line% + 14%)    ,ch(10),~
               at (20,29), fac(hex(84)), dstack$(line% + 15%)    ,ch(10),~
                                                                         ~
               at (22,02), fac(hex(a4)), blankline$              ,ch(79),~
               at (23,02), fac(hex(8c)), sumscrn$(2%)            ,ch(79),~
               at (24,02), fac(hex(8c)), sumscrn$(3%)            ,ch(79),~
                                                                         ~
               keys(hex(01020304050f10)),                                ~
               key(keyhit%)

               blankline$ = " "
               if keyhit% <> 15% then L39790
                  call "PRNTSCRN"
                  goto L39120

L39790:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *         I N P U T   E M P L O Y E E   C O D E             *~
            *                                                           *~
            * INPUT EMPLOYEE CODE TO PROCESS.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) linfac$()
                  if lastemp$ <> " " then str(header$,,61) =             ~
                                             "Last Employee: " & lastemp$
                  on fieldnr% gosub L40150,         /* EMPLOYEE CODE    */~
                                    L40150          /* WORK CENTER      */
                     goto L40220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linfac$(fieldnr%) = hex(80)
                      return
L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      linfac$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02), "Input Job Earnings Details",                 ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Employee Code",                                       ~
               at (06,30), fac(linfac$( 1)), empcode$           , ch(12),~
               at (06,45), fac(hex(8c))    , empname$           , ch(35),~
               at (07,02),                                               ~
                  "Work Center Default",                                 ~
               at (07,30), fac(linfac$( 2)), workcenter$        , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),     inpmessage$        , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), "(32)SAVE W/O POSTING",                       ~
               at (24,65), "(16)EXIT & POST",                            ~
               keys(hex(00010d0f1020)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40580
                  call "MANUAL" ("PRLJOBIN")
                  goto L40220

L40580:        if keyhit% <> 15 then L40620
                  call "PRNTSCRN"
                  goto L40220

L40620:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          E D I T   E M P L O Y E E   C O D E              *~
            *                                                           *~
            * EDITS THE EMPLOYEE CODE.                                  *~
            *************************************************************

            deffn'211(fieldnr%)
                  edtmessage$ = "Select From The PFKEY Options Listed B"&~
                                "elow."
                  init(hex(84)) linfac$()
                  str(header$,,61) = " "
                  on fieldnr% gosub L41150,         /* EMPLOYEE CODE    */~
                                    L41150          /* WORK CENTER      */
                     goto L41220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linfac$(fieldnr%) = hex(80)
                      return
L41150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      linfac$(fieldnr%) = hex(82)
                      return

L41220:     accept                                                       ~
               at (01,02), "Edit Job Earnings Details",                  ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Employee Code",                                       ~
               at (06,30), fac(linfac$( 1)), empcode$           , ch(12),~
               at (06,45), fac(hex(8c)),     empname$           , ch(35),~
               at (07,02),                                               ~
                  "Work Center Default",                                 ~
               at (07,30), fac(linfac$( 2)), workcenter$        , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),     edtmessage$        , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,32), "(8)See Earnings Summary",                    ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), "(2)Edit Details",                            ~
               at (24,65), "(16)Save Data",                              ~
                                                                         ~
               keys(hex(000102080d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 8% then L41572
                  gosub summary
                  goto L41220

L41572:        if keyhit% <> 13% then L41580
                  call "MANUAL" ("PRLJOBIN")
                  goto L41220

L41580:        if keyhit% <> 15% then L41620
                  call "PRNTSCRN"
                  goto L41220

L41620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          I N S T R U C T I O N S   S C R E E N S          *~
            *                                                           *~
            * DISPLAY MNEMONICS FOR THE VARIOUS INPUT MODES.            *~
            *************************************************************
        summary
            pf24$ = "(2)First  (3)Last  (4)Prev  (5)Next  (6)Down  (7)U"&~
                    "p    (15)Print  (16)Return"
            pfkeys$(5%) = hex(0203040506070f10)
            search type$() = "            " to pos%() step 12%
            if pos%(1%) > 180% then L43025
                str(pf24$,1%,54%) = " "
                str(pfkeys$,1%,7%) = hex(ff)
L43025:     line% = 0%
                mat this = zer : mat worked = zer
                cunits$(), camt$(), crate$(), str(header$,,61) = " "
                gtamt1, gtamt2, gtamt3, gtunt1, gtunt2, gtunt3 = 0
                for t% = 1 to min(maximum%, 100%)
                    rate = 0
                    convert rate$(t%) to rate, data goto L43048
L43048:             call "CONVERT" (rate, 2.2, crate$(t%))
                    call "CONVERT" (cunits(t%), 2.2, cunits$(t%,2%))
                    call "CONVERT" (camt(t%), 2.2, camt$(t%,2%))
                    gtamt2 = gtamt2 + camt(t%)
                    gtunt2 = gtunt2 + cunits(t%)
                    cunits$(t%,3%) = cunits$(t%,2%)
                    camt$(t%,3%) = camt$(t%,2%)
                next t%
                gtamt3 = gtamt2 : gtunt3 = gtunt2
                call "CONVERT" (gtamt2, 2.2, camt$(101%,2%))
                call "CONVERT" (gtunt2, 2.2, cunits$(101%,2%))
                camt$(101%,3%) = camt$(101%,2%)
                cunits$(101%,3%) = cunits$(101%,2%)
                if maxlines% < 1% then L43188
                for t% = 1 to maxlines%
                    i% = etype%(t%)
                    if i% > min(maximum%, 100%) then L43161
                    wkd,tworked, tamt, rate = 0
                    convert ernunit$(t%) to wkd, data goto L43124
                    worked(i%) = worked(i%) + wkd
L43124:             convert rate$(i%) to rate, data goto L43128
L43128:             convert cunits$(i%,3%) to tworked, data goto L43132
L43132:             convert camt$(i%,3%) to tamt, data goto L43136
L43136:             this(i%) = round(rate * worked(i%),2)
                    this = round(rate * wkd, 2)
                    call "CONVERT" (this(i%), 2.2, camt$(i%,1%))
                    call "CONVERT" (wkd + tworked,2.2,cunits$(i%,3%))
                    call "CONVERT" (worked(i%), 2.2, cunits$(i%,1%))
                    call "CONVERT" (tamt + this, 2.2, camt$(i%,3%))
                    gtamt1 = gtamt1 + this : gtamt3 = gtamt3 + this
                    gtunt1 = gtunt1 + wkd  : gtunt3 = gtunt3 + wkd
L43161:         next t%
                    call "CONVERT" (gtamt1, 2.2, camt$(101%,1%))
                    call "CONVERT" (gtamt2, 2.2, camt$(101%,2%))
                    call "CONVERT" (gtamt3, 2.2, camt$(101%,3%))
                    call "CONVERT" (gtunt1, 2.2, cunits$(101%,1%))
                    call "CONVERT" (gtunt2, 2.2, cunits$(101%,2%))
                    call "CONVERT" (gtunt3, 2.2, cunits$(101%,3%))
                goto L43188

L43170:         if keyhit1% = 2% then line% = 0%
                if keyhit1% = 3% then line% = max(0%,maximum%-15%)
                if keyhit1% = 4% then line% = max(0%,line%-14%)
                if keyhit1% = 5% then line% = min(line%+15%,max(0%,      ~
                                                       maximum%-15%))
                if keyhit1% = 6% then line% = max(0%,line%-1%)
                if keyhit1% = 7% then line% = min(line%+1%,max(0%,       ~
                                                       maximum%-15%))
                if keyhit1% = 16% then return

L43188:    accept                                                        ~
               at (01,02), "Earnings Types, Accrued Hours, and Dollars", ~
               at (01,59), "Today's Date:", fac(hex(8c)), date$ , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,19), "-------THIS ENTRY-------  ---PRIOR INPUT---  ~
        ~--PERIOD TOTALS--",                                              ~
               at (05,02), fac(hex(ac)), str(bighead$,,16),              ~
               at (05,19), fac(hex(ac)), str(bighead$,18,24),            ~
               at (05,45), fac(hex(ac)), str(bighead$,44,17),            ~
               at (05,64), fac(hex(ac)), str(bighead$,63,17),            ~
                                                                         ~
               at ( 6,02), fac(hex(8c)), num$(line% +  1%)      , ch(03),~
               at ( 7,02), fac(hex(8c)), num$(line% +  2%)      , ch(03),~
               at ( 8,02), fac(hex(8c)), num$(line% +  3%)      , ch(03),~
               at ( 9,02), fac(hex(8c)), num$(line% +  4%)      , ch(03),~
               at (10,02), fac(hex(8c)), num$(line% +  5%)      , ch(03),~
               at (11,02), fac(hex(8c)), num$(line% +  6%)      , ch(03),~
               at (12,02), fac(hex(8c)), num$(line% +  7%)      , ch(03),~
               at (13,02), fac(hex(8c)), num$(line% +  8%)      , ch(03),~
               at (14,02), fac(hex(8c)), num$(line% +  9%)      , ch(03),~
               at (15,02), fac(hex(8c)), num$(line% + 10%)      , ch(03),~
               at (16,02), fac(hex(8c)), num$(line% + 11%)      , ch(03),~
               at (17,02), fac(hex(8c)), num$(line% + 12%)      , ch(03),~
               at (18,02), fac(hex(8c)), num$(line% + 13%)      , ch(03),~
               at (19,02), fac(hex(8c)), num$(line% + 14%)      , ch(03),~
               at (20,02), fac(hex(8c)), num$(line% + 15%)      , ch(03),~
                                                                         ~
               at ( 6,06), fac(hex(84)), type$(line% +  1%)     , ch(12),~
               at ( 7,06), fac(hex(84)), type$(line% +  2%)     , ch(12),~
               at ( 8,06), fac(hex(84)), type$(line% +  3%)     , ch(12),~
               at ( 9,06), fac(hex(84)), type$(line% +  4%)     , ch(12),~
               at (10,06), fac(hex(84)), type$(line% +  5%)     , ch(12),~
               at (11,06), fac(hex(84)), type$(line% +  6%)     , ch(12),~
               at (12,06), fac(hex(84)), type$(line% +  7%)     , ch(12),~
               at (13,06), fac(hex(84)), type$(line% +  8%)     , ch(12),~
               at (14,06), fac(hex(84)), type$(line% +  9%)     , ch(12),~
               at (15,06), fac(hex(84)), type$(line% + 10%)     , ch(12),~
               at (16,06), fac(hex(84)), type$(line% + 11%)     , ch(12),~
               at (17,06), fac(hex(84)), type$(line% + 12%)     , ch(12),~
               at (18,06), fac(hex(84)), type$(line% + 13%)     , ch(12),~
               at (19,06), fac(hex(84)), type$(line% + 14%)     , ch(12),~
               at (20,06), fac(hex(84)), type$(line% + 15%)     , ch(12),~
                                                                         ~
               at (06,19), fac(hex(84)), cunits$(line% +  1%,1%), ch(05),~
               at (07,19), fac(hex(84)), cunits$(line% +  2%,1%), ch(05),~
               at (08,19), fac(hex(84)), cunits$(line% +  3%,1%), ch(05),~
               at (09,19), fac(hex(84)), cunits$(line% +  4%,1%), ch(05),~
               at (10,19), fac(hex(84)), cunits$(line% +  5%,1%), ch(05),~
               at (11,19), fac(hex(84)), cunits$(line% +  6%,1%), ch(05),~
               at (12,19), fac(hex(84)), cunits$(line% +  7%,1%), ch(05),~
               at (13,19), fac(hex(84)), cunits$(line% +  8%,1%), ch(05),~
               at (14,19), fac(hex(84)), cunits$(line% +  9%,1%), ch(05),~
               at (15,19), fac(hex(84)), cunits$(line% + 10%,1%), ch(05),~
               at (16,19), fac(hex(84)), cunits$(line% + 11%,1%), ch(05),~
               at (17,19), fac(hex(84)), cunits$(line% + 12%,1%), ch(05),~
               at (18,19), fac(hex(84)), cunits$(line% + 13%,1%), ch(05),~
               at (19,19), fac(hex(84)), cunits$(line% + 14%,1%), ch(05),~
               at (20,19), fac(hex(84)), cunits$(line% + 15%,1%), ch(05),~
                                                                         ~
               at (06,25), fac(hex(8c)), crate$(line% +  1%)    , ch(07),~
               at (07,25), fac(hex(8c)), crate$(line% +  2%)    , ch(07),~
               at (08,25), fac(hex(8c)), crate$(line% +  3%)    , ch(07),~
               at (09,25), fac(hex(8c)), crate$(line% +  4%)    , ch(07),~
               at (10,25), fac(hex(8c)), crate$(line% +  5%)    , ch(07),~
               at (11,25), fac(hex(8c)), crate$(line% +  6%)    , ch(07),~
               at (12,25), fac(hex(8c)), crate$(line% +  7%)    , ch(07),~
               at (13,25), fac(hex(8c)), crate$(line% +  8%)    , ch(07),~
               at (14,25), fac(hex(8c)), crate$(line% +  9%)    , ch(07),~
               at (15,25), fac(hex(8c)), crate$(line% + 10%)    , ch(07),~
               at (16,25), fac(hex(8c)), crate$(line% + 11%)    , ch(07),~
               at (17,25), fac(hex(8c)), crate$(line% + 12%)    , ch(07),~
               at (18,25), fac(hex(8c)), crate$(line% + 13%)    , ch(07),~
               at (19,25), fac(hex(8c)), crate$(line% + 14%)    , ch(07),~
               at (20,25), fac(hex(8c)), crate$(line% + 15%)    , ch(07),~
                                                                         ~
               at (06,33), fac(hex(84)), camt$(line% +  1%,1%)  , ch(10),~
               at (07,33), fac(hex(84)), camt$(line% +  2%,1%)  , ch(10),~
               at (08,33), fac(hex(84)), camt$(line% +  3%,1%)  , ch(10),~
               at (09,33), fac(hex(84)), camt$(line% +  4%,1%)  , ch(10),~
               at (10,33), fac(hex(84)), camt$(line% +  5%,1%)  , ch(10),~
               at (11,33), fac(hex(84)), camt$(line% +  6%,1%)  , ch(10),~
               at (12,33), fac(hex(84)), camt$(line% +  7%,1%)  , ch(10),~
               at (13,33), fac(hex(84)), camt$(line% +  8%,1%)  , ch(10),~
               at (14,33), fac(hex(84)), camt$(line% +  9%,1%)  , ch(10),~
               at (15,33), fac(hex(84)), camt$(line% + 10%,1%)  , ch(10),~
               at (16,33), fac(hex(84)), camt$(line% + 11%,1%)  , ch(10),~
               at (17,33), fac(hex(84)), camt$(line% + 12%,1%)  , ch(10),~
               at (18,33), fac(hex(84)), camt$(line% + 13%,1%)  , ch(10),~
               at (19,33), fac(hex(84)), camt$(line% + 14%,1%)  , ch(10),~
               at (20,33), fac(hex(84)), camt$(line% + 15%,1%)  , ch(10),~
                                                                         ~
               at (06,45), fac(hex(8c)), cunits$(line% +  1%,2%), ch(06),~
               at (07,45), fac(hex(8c)), cunits$(line% +  2%,2%), ch(06),~
               at (08,45), fac(hex(8c)), cunits$(line% +  3%,2%), ch(06),~
               at (09,45), fac(hex(8c)), cunits$(line% +  4%,2%), ch(06),~
               at (10,45), fac(hex(8c)), cunits$(line% +  5%,2%), ch(06),~
               at (11,45), fac(hex(8c)), cunits$(line% +  6%,2%), ch(06),~
               at (12,45), fac(hex(8c)), cunits$(line% +  7%,2%), ch(06),~
               at (13,45), fac(hex(8c)), cunits$(line% +  8%,2%), ch(06),~
               at (14,45), fac(hex(8c)), cunits$(line% +  9%,2%), ch(06),~
               at (15,45), fac(hex(8c)), cunits$(line% + 10%,2%), ch(06),~
               at (16,45), fac(hex(8c)), cunits$(line% + 11%,2%), ch(06),~
               at (17,45), fac(hex(8c)), cunits$(line% + 12%,2%), ch(06),~
               at (18,45), fac(hex(8c)), cunits$(line% + 13%,2%), ch(06),~
               at (19,45), fac(hex(8c)), cunits$(line% + 14%,2%), ch(06),~
               at (20,45), fac(hex(8c)), cunits$(line% + 15%,2%), ch(06),~
                                                                         ~
               at (06,52), fac(hex(84)), camt$(line% +  1%,2%)  , ch(10),~
               at (07,52), fac(hex(84)), camt$(line% +  2%,2%)  , ch(10),~
               at (08,52), fac(hex(84)), camt$(line% +  3%,2%)  , ch(10),~
               at (09,52), fac(hex(84)), camt$(line% +  4%,2%)  , ch(10),~
               at (10,52), fac(hex(84)), camt$(line% +  5%,2%)  , ch(10),~
               at (11,52), fac(hex(84)), camt$(line% +  6%,2%)  , ch(10),~
               at (12,52), fac(hex(84)), camt$(line% +  7%,2%)  , ch(10),~
               at (13,52), fac(hex(84)), camt$(line% +  8%,2%)  , ch(10),~
               at (14,52), fac(hex(84)), camt$(line% +  9%,2%)  , ch(10),~
               at (15,52), fac(hex(84)), camt$(line% + 10%,2%)  , ch(10),~
               at (16,52), fac(hex(84)), camt$(line% + 11%,2%)  , ch(10),~
               at (17,52), fac(hex(84)), camt$(line% + 12%,2%)  , ch(10),~
               at (18,52), fac(hex(84)), camt$(line% + 13%,2%)  , ch(10),~
               at (19,52), fac(hex(84)), camt$(line% + 14%,2%)  , ch(10),~
               at (20,52), fac(hex(84)), camt$(line% + 15%,2%)  , ch(10),~
                                                                         ~
               at (06,64), fac(hex(8c)), cunits$(line% +  1%,3%), ch(06),~
               at (07,64), fac(hex(8c)), cunits$(line% +  2%,3%), ch(06),~
               at (08,64), fac(hex(8c)), cunits$(line% +  3%,3%), ch(06),~
               at (09,64), fac(hex(8c)), cunits$(line% +  4%,3%), ch(06),~
               at (10,64), fac(hex(8c)), cunits$(line% +  5%,3%), ch(06),~
               at (11,64), fac(hex(8c)), cunits$(line% +  6%,3%), ch(06),~
               at (12,64), fac(hex(8c)), cunits$(line% +  7%,3%), ch(06),~
               at (13,64), fac(hex(8c)), cunits$(line% +  8%,3%), ch(06),~
               at (14,64), fac(hex(8c)), cunits$(line% +  9%,3%), ch(06),~
               at (15,64), fac(hex(8c)), cunits$(line% + 10%,3%), ch(06),~
               at (16,64), fac(hex(8c)), cunits$(line% + 11%,3%), ch(06),~
               at (17,64), fac(hex(8c)), cunits$(line% + 12%,3%), ch(06),~
               at (18,64), fac(hex(8c)), cunits$(line% + 13%,3%), ch(06),~
               at (19,64), fac(hex(8c)), cunits$(line% + 14%,3%), ch(06),~
               at (20,64), fac(hex(8c)), cunits$(line% + 15%,3%), ch(06),~
                                                                         ~
               at (06,71), fac(hex(84)), camt$(line% +  1%,3%)  , ch(10),~
               at (07,71), fac(hex(84)), camt$(line% +  2%,3%)  , ch(10),~
               at (08,71), fac(hex(84)), camt$(line% +  3%,3%)  , ch(10),~
               at (09,71), fac(hex(84)), camt$(line% +  4%,3%)  , ch(10),~
               at (10,71), fac(hex(84)), camt$(line% +  5%,3%)  , ch(10),~
               at (11,71), fac(hex(84)), camt$(line% +  6%,3%)  , ch(10),~
               at (12,71), fac(hex(84)), camt$(line% +  7%,3%)  , ch(10),~
               at (13,71), fac(hex(84)), camt$(line% +  8%,3%)  , ch(10),~
               at (14,71), fac(hex(84)), camt$(line% +  9%,3%)  , ch(10),~
               at (15,71), fac(hex(84)), camt$(line% + 10%,3%)  , ch(10),~
               at (16,71), fac(hex(84)), camt$(line% + 11%,3%)  , ch(10),~
               at (17,71), fac(hex(84)), camt$(line% + 12%,3%)  , ch(10),~
               at (18,71), fac(hex(84)), camt$(line% + 13%,3%)  , ch(10),~
               at (19,71), fac(hex(84)), camt$(line% + 14%,3%)  , ch(10),~
               at (20,71), fac(hex(84)), camt$(line% + 15%,3%)  , ch(10),~
                                                                         ~
               at (22,06), "Totals:",                                    ~
               at (22,19), fac(hex(8c)), cunits$(101%,1%)       , ch(05),~
               at (22,33), fac(hex(84)),   camt$(101%,1%)       , ch(10),~
               at (22,45), fac(hex(8c)), cunits$(101%,2%)       , ch(05),~
               at (22,52), fac(hex(84)),   camt$(101%,2%)       , ch(10),~
               at (22,64), fac(hex(8c)), cunits$(101%,3%)       , ch(05),~
               at (22,71), fac(hex(84)),   camt$(101%,3%)       , ch(10),~
                                                                         ~
               at (24,02), fac(hex(8c)),   pf24$                , ch(79),~
                                                                         ~
               keys(pfkeys$(5%)),                                        ~
               key(keyhit1%)

               if keyhit1% <> 15% then L43950
                   call "PRNTSCRN"
                   go to L43188

L43950:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               goto L43170

        REM *************************************************************~
            *       I N P U T   L I N E   I T E M S   S C R E E N       *~
            *                                                           *~
            * INPUTS LINE ITEMS.  USES MULTI-LINE "BCKINPUT" FORMAT.    *~
            *************************************************************

            deffn'203(screenline%, fieldnr%): screen% = 1: goto L44200
            deffn'213(screenline%, fieldnr%): screen% = 2
                 init(hex(86)) fac$()
                 if fieldnr% <> 0% then L44200
                     infomsg$ = "Position Cursor And Press RETURN To Mo"&~
                                "dify."
                     goto L44210
            deffn'223(screenline%, fieldnr%): screen% = 3: goto L44200

            deffn'233(screenline%)
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 5
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L44400

L44200:           init(hex(84)) fac$()
L44210:           on fieldnr% gosub L44370,         /* EARNINGS TYPE    */~
                                    L44340,         /* JOB NUMBER       */~
                                    L44340,         /* WORK CENTER      */~
                                    L44340,         /* LABOR CLASS CODE */~
                                    L44340,         /* WC Activity Code */~
                                    L44370,         /* DATE WORK DONE   */~
                                    L44370,         /* UNITS WORKED     */~
                                    L44340          /* ACCOUNT          */
                  goto L44400

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L44340:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L44370:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L44400:     for temp% = 1% to 5% /* Get around subscript error */
                if etype%(line% + temp%) = 0% then                       ~
                   etype%(line% + temp%) = 101% /* Force to blank */
                next temp%

L44410:     accept                                                       ~
                                                                         ~
               at (01,02), fac(hex(8c)), title$(screen%,1)      , ch(60),~
               at (02,02), fac(hex(8c)), title$(screen%,2)      , ch(60),~
                                                                         ~
               at (01,63),                                               ~
                  "!EMP:",                                               ~
               at (01,69), fac(hex(84)),   empcode$             , ch(12),~
               at (02,63),                                               ~
                  "+-----------------",                                  ~
               at (03,02), fac(hex(84)),   infomsg$             , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$( 1)      , ch(79),~
               at (09,02), fac(hex(84)),    separator$( 2)      , ch(79),~
               at (13,02), fac(hex(84)),    separator$( 3)      , ch(79),~
               at (17,02), fac(hex(84)),    separator$( 4)      , ch(79),~
               at (21,02), fac(hex(84)),    separator$( 5)      , ch(79),~
                                                                         ~
               at (06,02), "Type"                                       ,~
               at (10,02), "Type"                                       ,~
               at (14,02), "Type"                                       ,~
               at (18,02), "Type"                                       ,~
               at (22,02), "Type"                                       ,~
                                                                         ~
               at (06,07), fac(fac$(1, 1)), erntype$  (line%+ 1), ch(03),~
               at (10,07), fac(fac$(2, 1)), erntype$  (line%+ 2), ch(03),~
               at (14,07), fac(fac$(3, 1)), erntype$  (line%+ 3), ch(03),~
               at (18,07), fac(fac$(4, 1)), erntype$  (line%+ 4), ch(03),~
               at (22,07), fac(fac$(5, 1)), erntype$  (line%+ 5), ch(03),~
                                                                         ~
               at (06,11), fac(hex(84)), type$(etype%(line%+1)) , ch(12),~
               at (10,11), fac(hex(84)), type$(etype%(line%+2)) , ch(12),~
               at (14,11), fac(hex(84)), type$(etype%(line%+3)) , ch(12),~
               at (18,11), fac(hex(84)), type$(etype%(line%+4)) , ch(12),~
               at (22,11), fac(hex(84)), type$(etype%(line%+5)) , ch(12),~
                                                                         ~
               at (06,25), "Job"                                        ,~
               at (10,25), "Job"                                        ,~
               at (14,25), "Job"                                        ,~
               at (18,25), "Job"                                        ,~
               at (22,25), "Job"                                        ,~
                                                                         ~
               at (06,29), fac(fac$( 1, 2)), job$(line%+1)      , ch(08),~
               at (10,29), fac(fac$( 2, 2)), job$(line%+2)      , ch(08),~
               at (14,29), fac(fac$( 3, 2)), job$(line%+3)      , ch(08),~
               at (18,29), fac(fac$( 4, 2)), job$(line%+4)      , ch(08),~
               at (22,29), fac(fac$( 5, 2)), job$(line%+5)      , ch(08),~
                                                                         ~
               at (06,38), fac(hex(84)), jbd$(line%+1)          , ch(30),~
               at (10,38), fac(hex(84)), jbd$(line%+2)          , ch(30),~
               at (14,38), fac(hex(84)), jbd$(line%+3)          , ch(30),~
               at (18,38), fac(hex(84)), jbd$(line%+4)          , ch(30),~
               at (22,38), fac(hex(84)), jbd$(line%+5)          , ch(30),~
                                                                         ~
               at (06,69), "Wrkcntr"                                    ,~
               at (10,69), "Wrkcntr"                                    ,~
               at (14,69), "Wrkcntr"                                    ,~
               at (18,69), "Wrkcntr"                                    ,~
               at (22,69), "Wrkcntr"                                    ,~
                                                                         ~
               at (06,77), fac(fac$(1, 3)), wctr$     (line%+ 1), ch(04),~
               at (10,77), fac(fac$(2, 3)), wctr$     (line%+ 2), ch(04),~
               at (14,77), fac(fac$(3, 3)), wctr$     (line%+ 3), ch(04),~
               at (18,77), fac(fac$(4, 3)), wctr$     (line%+ 4), ch(04),~
               at (22,77), fac(fac$(5, 3)), wctr$     (line%+ 5), ch(04),~
                                                                         ~
               at (07,02), "Lbr Class"                                  ,~
               at (11,02), "Lbr Class"                                  ,~
               at (15,02), "Lbr Class"                                  ,~
               at (19,02), "Lbr Class"                                  ,~
               at (23,02), "Lbr Class"                                  ,~
                                                                         ~
               at (07,12), fac(fac$(1, 4)), lclass$   (line%+ 1), ch(04),~
               at (11,12), fac(fac$(2, 4)), lclass$   (line%+ 2), ch(04),~
               at (15,12), fac(fac$(3, 4)), lclass$   (line%+ 3), ch(04),~
               at (19,12), fac(fac$(4, 4)), lclass$   (line%+ 4), ch(04),~
               at (23,12), fac(fac$(5, 4)), lclass$   (line%+ 5), ch(04),~
                                                                         ~
               at (07,25), "W/C Activity Code",                          ~
               at (11,25), "W/C Activity Code",                          ~
               at (15,25), "W/C Activity Code",                          ~
               at (19,25), "W/C Activity Code",                          ~
               at (23,25), "W/C Activity Code",                          ~
                                                                         ~
               at (07,43), fac(fac$(1, 5)), wcact$    (line%+ 1), ch(04),~
               at (11,43), fac(fac$(2, 5)), wcact$    (line%+ 2), ch(04),~
               at (15,43), fac(fac$(3, 5)), wcact$    (line%+ 3), ch(04),~
               at (19,43), fac(fac$(4, 5)), wcact$    (line%+ 4), ch(04),~
               at (23,43), fac(fac$(5, 5)), wcact$    (line%+ 5), ch(04),~
                                                                         ~
               at (07,55), "Date"                                       ,~
               at (11,55), "Date"                                       ,~
               at (15,55), "Date"                                       ,~
               at (19,55), "Date"                                       ,~
               at (23,55), "Date"                                       ,~
                                                                         ~
               at (07,60), fac(fac$(1, 6)), str(wdate$(line%+1)), ch(08),~
               at (11,60), fac(fac$(2, 6)), str(wdate$(line%+2)), ch(08),~
               at (15,60), fac(fac$(3, 6)), str(wdate$(line%+3)), ch(08),~
               at (19,60), fac(fac$(4, 6)), str(wdate$(line%+4)), ch(08),~
               at (23,60), fac(fac$(5, 6)), str(wdate$(line%+5)), ch(08),~
                                                                         ~
               at (08,02), "Unit Desc."                                 ,~
               at (12,02), "Unit Desc."                                 ,~
               at (16,02), "Unit Desc."                                 ,~
               at (20,02), "Unit Desc."                                 ,~
               at (24,02), "Unit Desc."                                 ,~
                                                                         ~
               at (08,13), fac(hex(84)), units$(etype%(line%+1)), ch(06),~
               at (12,13), fac(hex(84)), units$(etype%(line%+2)), ch(06),~
               at (16,13), fac(hex(84)), units$(etype%(line%+3)), ch(06),~
               at (20,13), fac(hex(84)), units$(etype%(line%+4)), ch(06),~
               at (24,13), fac(hex(84)), units$(etype%(line%+5)), ch(06),~
                                                                         ~
               at (08,21), "Unit Rate"                                  ,~
               at (12,21), "Unit Rate"                                  ,~
               at (16,21), "Unit Rate"                                  ,~
               at (20,21), "Unit Rate"                                  ,~
               at (24,21), "Unit Rate"                                  ,~
                                                                         ~
               at (08,31), fac(hex(84)), rate$(etype%(line%+1)),  ch(10),~
               at (12,31), fac(hex(84)), rate$(etype%(line%+2)),  ch(10),~
               at (16,31), fac(hex(84)), rate$(etype%(line%+3)),  ch(10),~
               at (20,31), fac(hex(84)), rate$(etype%(line%+4)),  ch(10),~
               at (24,31), fac(hex(84)), rate$(etype%(line%+5)),  ch(10),~
                                                                         ~
               at (08,43), "Units"                                      ,~
               at (12,43), "Units"                                      ,~
               at (16,43), "Units"                                      ,~
               at (20,43), "Units"                                      ,~
               at (24,43), "Units"                                      ,~
                                                                         ~
               at (08,49), fac(fac$(1 , 7)), ernunit$(line%+ 1),  ch(10),~
               at (12,49), fac(fac$(2 , 7)), ernunit$(line%+ 2),  ch(10),~
               at (16,49), fac(fac$(3 , 7)), ernunit$(line%+ 3),  ch(10),~
               at (20,49), fac(fac$(4 , 7)), ernunit$(line%+ 4),  ch(10),~
               at (24,49), fac(fac$(5 , 7)), ernunit$(line%+ 5),  ch(10),~
                                                                         ~
               at (08,61), "Acct"                                       ,~
               at (12,61), "Acct"                                       ,~
               at (16,61), "Acct"                                       ,~
               at (20,61), "Acct"                                       ,~
               at (24,61), "Acct"                                       ,~
                                                                         ~
               at (08,66), fac(fac$( 1, 8)), account$(line%+ 1),  ch(12),~
               at (12,66), fac(fac$( 2, 8)), account$(line%+ 2),  ch(12),~
               at (16,66), fac(fac$( 3, 8)), account$(line%+ 3),  ch(12),~
               at (20,66), fac(fac$( 4, 8)), account$(line%+ 4),  ch(12),~
               at (24,66), fac(fac$( 5, 8)), account$(line%+ 5),  ch(12),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key(keyhit%)

               if keyhit% <> 13 then L45990
                  call "MANUAL" ("PRLJOBIN")
                  goto L44410

L45990:        if keyhit% <> 15 then L46030
                  call "PRNTSCRN"
                  goto L44410

L46030:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TEST DATA FOR THE EMPLOYEE CODE.    BASICALLY, THIS JUST  *~
            * LOADS THE EMPLOYEE'S NAME.                                *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120,         /* EMPLOYEE CODE    */~
                                    L50420          /* WORK CENTER      */
                     return
L50120: REM Test data for employee's code
            name$ = hex(06) & "Employees currently in ACTIVE Status" &   ~
                              " on the Payroll Master File"
            call "PLOWCODE" (#2, empcode$, name$, 9000%, 0.40, f1%(2),   ~
                  hdr$(), 0,   0.0002, inex(), inex$(), " ", " ", #14%,  ~
                               desc_map())
                if f1%(2) = 0 then L50220
            get #2, using L50190, empsclass$
L50190:         FMT XX(12), CH(4)
            goto L50270

L50220:     errormsg$= "Employee Code Not In Payroll Master File"
            return

L50270:     empname$ = str(name$,14,10) & " " & str(name$,25,1) & " " &  ~
                       str(name$,27,15)
                gosub L30000              /* EARNINGS TYPES             */
                if maximum% <> 0 then L50360
                errormsg$= "Employee Has No Earnings Records"
                return
L50360:         gosub L32000              /* CHECK PAYROLL BUFFER       */
                if maxlines% = 0 then return
                return clear
                return clear
                go to editmode

L50420:     REM TEST DATA FOR WORK CENTER
                if workcenter$ = " " then return
                call "GETCODE" (#12, workcenter$, " ", 0%, 0, f1%(12))
                     if f1%(12) <> 0 then return
                errormsg$ = "Work Center Not On File"
                return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TEST DATA FOR LINE ITEMS.                                 *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51160,         /* EARNINGS TYPE    */~
                                    L51290,         /* JOB NUMBER       */~
                                    L51480,         /* WORK CENTER      */~
                                    L51530,         /* LABOR CLASS CODE */~
                                    L51800,         /* WC Activity Code */~
                                    L51600,         /* DATE WORK DONE   */~
                                    L51650,         /* UNITS WORKED     */~
                                    L51720          /* ACCOUNT          */
                    return
L51160:     REM TEST DATA FOR EARNINGS TYPE
                if erntype$(c%) <> " " then L51200
                   errormsg$ = "Earnings Type Number Cannot Be Blank"
                   return
L51200:         if num(erntype$(c%)) >= len(erntype$(c%)) then L51230
                   errormsg$ = "Illegal Entry for Number: " & erntype$(c%)
                   return
L51230:         convert erntype$(c%) to etype%(c%)
                if etype%(c%) <= maximum% then return
                errormsg$ = "Type Exceeds Types Assigned To This Employee"
                etype%(c%) = 0
                return
L51290:     REM TEST DATA FOR JOB NUMBER
                jbd$(c%), jobacct$ = " "
                if job$(c%) = " " then return
                call "GETCODE" (#5, job$(c%), jbd$(c%), 1%, 99, f1%(5))
                      if f1%(5) = 1 then L51380
                call "GETCODE" (#11, job$(c%), jbd$(c%), 1%, 99, f1%(11))
                      if f1%(11) = 1 then L51410
                call "GETCODE" (#5, job$(c%), jbd$(c%), 1%, 0, f1%(5))
                      if f1%(5) = 1 then L51380
                call "GETCODE" (#11, job$(c%), jbd$(c%), 1%, 0, f1%(11))
                      if f1%(11) = 1 then L51410
                      errormsg$ = "Job Number Not On File"
                      return
L51380:         get #5, using L51390, datejobclosed$, jobacct$
L51390:                 FMT XX(44), CH(06), XX(146), CH(9)
                goto L51425
L51410:         get #11, using L51420, datejobclosed$, jobacct$
L51420:                 FMT XX(152),CH(6),CH(9)
L51425:         call "GLFMT" (jobacct$)
                if datejobclosed$ = " " or ~
                   datejobclosed$ = blankdate$ then return
                   call "DATEFMT" (datejobclosed$)
                   errormsg$ = "Job Was Closed On " & datejobclosed$
                   return
L51480:     REM TEST DATA FOR WORK CENTER
                if wctr$(c%) = " " then return
                call "GETCODE" (#12, wctr$(c%), infomsg$, 1%, 0, f1%(12))
                  if f1%(12) <> 0 then return
                errormsg$ = "Work Center Code Not On File"
                return
L51530:     REM TEST DATA FOR LABOR CLASS CODE
                cls$(c%), infomsg$ = " "
                if lclass$(c%) = " " then return
                readkey$ = "LBR CLASS" & str(lclass$(c%),,4) & " "
                call "PLOWCODE" (#7, readkey$, cls$(c%), 9%, .3, f1%(7))
                if f1%(7) <> 0 then L51580
                     errormsg$ = "Labor Class Not On File"
                     return
L51580:         lclass$(c%) = str(readkey$,10,4)
                call "READ100" (#30, lclass$(c%), f1%(30))
                    if f1%(30) <> 0% then L51595
                    errormsg$ = "Class Not Defined In Current Cost Set"
                    return
L51595:         get #30 using L51596, ovrate
L51596:         FMT POS(61), PD(14,4)
                infomsg$ = "Overhead Percent: "
                call "CONVERT" (ovrate, -2.2, str(infomsg$,19,10))
                return
L51600:     REM TEST DATA FOR DATE WORK DONE
                call "DATEOK" (wdate$(c%), u3%, errormsg$)
                return
L51650:     REM TEST DATA FOR UNITS WORKED
                call "NUMTEST" (ernunit$(c%),-9e7,9e7,errormsg$,2.2,0)
                   return
L51720:     REM TEST DATA FOR ACCOUNT
                call "GETCODE" (#10, account$(c%), infomsg$, 1%,0,f1%(10))
                     if f1%(10) = 1 then return
                errormsg$ = "Account Not On File"
                return

L51800: REM Test data for WC Activity Code
                if wcact$(c%) = " " then return
                readkey$ = "WC ACTVTY" & str(wcact$(c%),,4) & " "
                call "PLOWCODE" (#7, readkey$, " ", 9%, 0, f1%(7))
                if f1%(7) <> 1 then L51850
                     wcact$(c%) = str(readkey$,10,4)
                     return
L51850:         errormsg$ = "WC Activity Code Not On File"
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            if keyhit% = 32 then end 0

            temp$ = userid$
            call "PLOWNEXT" (#9, temp$, 3%, f1%(9))

            end f1%(9)
