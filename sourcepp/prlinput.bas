        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  P   P  R   R  L        I    NN  N  P   P  U   U    T     *~
            *  PPPP   RRRR   L        I    N N N  PPPP   U   U    T     *~
            *  P      R   R  L        I    N  NN  P      U   U    T     *~
            *  P      R   R  LLLLL  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLINPUT - INPUT PAYROLL TRANSACTIONS INTO THE COMPUTER TO*~
            *            BE POSTED TO THE EMPLOYEE'S FILE FOR CUTTING   *~
            *            CHECKS.  MOST OF THE DOCUMENTATION IS IN-LINE. *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/04/80 ! ORIGINAL                                 ! BCW *~
            * 06/14/83 ! REMOVED UNNEEDED VARIABLES               ! GLW *~
            * 07/18/83 ! ADDED CALLS TO MANUAL                    ! HES *~
            * 07/06/84 ! EXIT IF FILES NOT OPEN, CALLS 'GETEMPL'  ! HES *~
            * 11/24/87 ! Corrected Error From CONVERT stmnt       ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 07/26/95 ! PRR 13471 - Added Warning to operator    ! JBK *~
            *          !  when the 'add amount' does not equal    !     *~
            *          !  the add units times the default rate.   !     *~
            *          !  The user has the option of accepting    !     *~
            *          !  the add amount or re-entering it.       !     *~
            *          ! Miscellaneous - Replaced call to SHOMSG  !     *~
            *          !  with call to SHOSTAT. Replaced re-start !     *~
            *          !  screen with a call to 'STARTOVR'.       !     *~
            *************************************************************
        dim                                                              ~
            addamount$(100)10,           /* AMOUNT TO ADD TO CHECK     */~
            addunits$(100)10,            /* UNITS TO ADD TO CHECK      */~
            blankline$79,                /* INPUT MESSAGE TEXT         */~
            curramt$(100)10,             /* CURRENT EARNINGS AMOUNT    */~
            currunits$(100)10,           /* CURRENT EARNINGS UNITS     */~
            cursor%(2),                  /* CURSOR LOCATIONS           */~
            date$8,                      /* TODAY'S DATE               */~
            dept$(100)4,                 /* DEPARTMENT CODE STUFF      */~
            empcode$12,                  /* EMPLOYEE CODE THIS GUY     */~
            emprecord$(10)50,            /* ENTIRE EMPLOPYEE RECORD    */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20,2)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            fac1$(18,7)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            heading$79,                  /* HEADER FOR LINE ITEM SCREEN*/~
            header$79,                   /* HEADER FOR SCREEN          */~
            hours$(18,7)5,               /* HOURS TABLE ENTRIES        */~
            hourstype$(6)13,             /* HOURS TABLE PROMPTS        */~
            hoursamt(100),               /* HOURS TABLE RESULTS        */~
            i$(24)80,                    /* SCREEN IMAGE--NOT USED     */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            lastemp$12,                  /* LAST EMPLOYEE CODE ENTERED */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARS      */~
            location$2,                  /* LOCATOR STRING FOR SEARCH  */~
            name$(3)20,                  /* NAME OF EMPLOYEE           */~
            nextcheck$(100)10,           /* UPDATED AMOUNT THIS LINE.  */~
            pfkeys$(2)17,                /* FUNCTION KEYS FOR KICKS    */~
            promptfac$(6)1,              /* FIELD ATTRIBUTE CHARACTERS */~
            rate(100),                   /* RATE FOR EACH LINE ITEM    */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            status$1,                    /* ACTIVE EMPLOYEE STATUS BYTE*/~
            title$(2)64,                 /* P.F. KEY TITLES FOR FUN    */~
            tran$(24)80,                 /* TRAN FROM CURSOR==>FIELD # */~
            type$(100)12,                /* EARNINGS TYPE STUFF        */~
            units$(100)6,                /* UNITS (GOD KNOWS WHAT FOR) */~
            usbucks(100),                /* USUAL AMOUNT               */~
            usunits(100),                /* USUAL UNITS LINE ITEMS     */~
            weekname$(3)8                /* "WEEK 1, 2, 3" PROMPTS     */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (PAYROLL DEFAULT)*~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * #14 ! PERMASTR ! PERSONNEL EMPLOYEE MASTER FILE           *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

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

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))

            if f2%(1) + f2%(3) + f2%(4) + f2%(14) = 0 then L09000
            stop "ALL FILES NEEDED WHERE NOT FOUND. PLEASE CREATE AND RET~
        ~URN"  : goto L65000

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES NECESSARY FIELDS FOR PROGRAM.                 *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            title$(1)   = "(1)START OVER (2)COL 1 (4)LINE ABOVE (15)PRNT ~
        ~SCRN  (16)EDITMODE"
            title$(2)   = "(1)STRT OVR(2)1ST(3)LAST(4)PREV(5)NEXT(6)DOWN(~
        ~7)UP(16)SAVE DATA"

            pfkeys$(1) = hex(000102040f10ffffffffffffffffffffff)
            pfkeys$(2) = hex(00010203040506070f10ffffffffffffff)

            heading$ = "    TYPE    !DEPT!UNITS !ADD  UNITS!ADD AMOUNT!CU~
        ~RR UNITS!CURR. AMT.!GROSSCHECK"
            header$  = "Select Employee For Input"
            str(header$,62) = "PRLINPUT: " & cms2v$

            REM SET TRAN STRINGS FOR EDIT MODE.
                init(hex(01)) tran$()
                init(hex(02)) str(tran$(5), 37, 11)

                copy str(tran$(), 321, 1520) to str(tran$(), 401, 1520)

            mat read weekname$
            data "WEEK 1",                         /* (1)              */~
                 "WEEK 2",                         /* (2)              */~
                 "WEEK 3"                          /* (3)              */~

        REM *************************************************************~
            *            I N P U T   M A I N   P R O G R A M            *~
            *                                                           *~
            * INPUTS THE EMPLOYEE CODE AND SUNDRY (SATURDRY, DEPENDING?)*~
            * FIELDS IN LINEAR MODE BEFORE BOUNCING TO THE LINE ITEMS.  *~
            *************************************************************

        inputmode
            init(" ") type$(), dept$(), units$(), addamount$(),          ~
                      addunits$(), nextcheck$(), curramt$(), hours$(),   ~
                      empcode$, errormsg$, infomsg$, currunits$()
            mat usunits = zer
            mat usbucks = zer
            mat hoursamt = zer
            hoursptr% = 0                /* POINTER - NEXT TABLE ENTRY */
            pf_threed% = 0%

            call "STRTRLSE" addr(#3)
            call "STRTRLSE" addr(#4)

            for fieldnr% = 1 to 1
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10290
L10220:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 then       L10270 /* FAKE (ENTER)*/
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10220
L10270:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10220
L10290:         next fieldnr%

            if keyhit% = 3 then L13000              /* GET HOURS TABLE. */

L11000: REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEM INFORMATION FOR THE EARNINGS GUY.        *~
            *************************************************************

            REM NOW INPUT MODE FOR THE LINE ITEMS.
                line%, screenline% = 0
                infomsg$, errormsg$ = " "

L11100:         screenline% = screenline% + 1
                if screenline% < 20 then L11140
                   line% = line% + 20
                   screenline% = 1
L11140:         currentline%, c% = line% + screenline%
                if currentline% > maxlines% then L12000

L11170:         for fieldnr% = 1 to 2
                    gosub'163(fieldnr%)
                          if enabled% =  0 then       L11280
L11200:             gosub'203(screenline%, fieldnr%)
                          if keyhit%  =  0 then       L11280
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  =  2 then gosub columnone
                          if keyhit%  =  4 then gosub lineabove
                          if keyhit%  <> 16 or fieldnr% <> 1 then L11270
                             addunits$(c%) = " "
                             goto L12000
L11270:                   goto L11200
L11280:             gosub'153(fieldnr%)
                          if errormsg$ <> " " then L11200
                    next fieldnr%

                REM Now We Update The "Next Check" Field
                    curramt, addamount = 0
                    convert curramt$(c%) to curramt, data goto L11340
L11340:             convert addamount$(c%) to addamount, data goto L11341
L11341:             nextcheck$(c%) = " "
                    if curramt + addamount <> 0 then                     ~
                     call "CONVERT"(curramt+addamount,2.2,nextcheck$(c%))
                    goto L11100

L12000: REM *************************************************************~
            *         E D I T   L I N E   I N F O R M A T I O N         *~
            *                                                           *~
            * EDITS LINE INFO.  NOTE THAT THE JOB IS REALLY SIMPLE      *~
            * SINCE THERE CAN BE NO INSERT MODE (TOO MUCH TROUBLE DOWN  *~
            * THE LINE) AND YOU ONLY HAVE ABOUT 2 FIELDS TO EDIT.       *~
            * NOTE ALSO THAT THERE IS NO SUCH THING AS EDIT HEADER INFO *~
            * SINCE THE THING HAS TO HAVE THE SAME EMPLOYEE CODE SINCE  *~
            * EVERYONE WILL PROBABLY HAVE DIFFERENT EARNINGS.           *~
            *************************************************************

        editmode
            line%, currentline%, screenline%, pf_threed% = 0%
            infomsg$ = " "

L12150:     gosub'213(0%, 0%)
                  if keyhit%  =  0 then       L12290
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0, maxlines% - 20)
                  if keyhit%  =  4 then line% = max(0, line% - 15)
                  if keyhit%  =  5 then line% = min(line% + 15, max(0,   ~
                                                maxlines% - 20))
                  if keyhit%  =  6 then line% = max(0, line% - 1)
                  if keyhit%  =  7 then line% = min(line% + 1, max(0,    ~
                                                maxlines% - 20))
                  if keyhit%  = 16 then       datasave
                  goto L12150

L12290:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                fieldnr% = val(str(tran$(cursor%(1)),cursor%(2)))
                if fieldnr% = 0 then L12150
                screenline% = max(0, cursor%(1) - 4)
                c%, currentline% = line% + screenline%
                if currentline% > maxlines% then L12150

L12360:         gosub'223(screenline%, fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then L12360
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L12360
                REM Now We Update The "Next Check" Field
                    curramt, addamount = 0
                    convert curramt$(c%) to curramt, data goto L12440
L12440:             convert addamount$(c%) to addamount, data goto L12441
L12441:             nextcheck$(c%) = " "
                    if curramt + addamount <> 0 then                     ~
                     call "CONVERT"(curramt+addamount,2.2,nextcheck$(c%))
                    goto L12150

L13000: REM *************************************************************~
            *  D R I V E R   F O R   H O U R S   I N P U T   T A B L E  *~
            *                                                           *~
            * MAIN PROGRAM FOR HOURS TABLE INPUT.                       *~
            *************************************************************

            pf_threed% = 1%
            gosub'162                    /* ENABLE FOR HOURS TABLE.    */

L13080:     gosub'202
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L13080
            gosub'152
                  if errormsg$ <> " " then L13080

            REM NOW BUILD THE EARNINGS TABLE ENTRIES FOR THIS EMPLOYEE
                for earnings% = 1 to 6
                    if hourstype$(earnings%) = " " then L13300
                       search type$() = str(hourstype$(earnings%),,12)   ~
                              to location$ step 12
                       if location$ = hex(0000) then L13300
                          sub% = val(location$, 2)/12 + 1
                          for i% = 1 to 3
                              for j% = 1 to 7
                                  if hours$(3*(earnings%-1)+i%, j%) = " "~
                                     then L13280
                                  convert hours$(3*(earnings%-1)+i%, j%) ~
                                          to hours
                                  hoursamt(sub%) = hoursamt(sub%) + hours
L13280:                           next j%
                              next i%
L13300:             next earnings%
                init(" ") hours$()
                if hoursptr% > maxlines% then L11000                      ~
                                         else L13000

        REM *************************************************************~
            *  C O L U M N   O N E ,   L I N E   A B O V E   L O G I C  *~
            *                                                           *~
            * DOES COLUMN ONE AND LINE ABOVE HANDLING.  THIS IS THE     *~
            * FIRST PLACE ON THE SYSTEM WHERE THE COLUMN ONE KEY RETURNS*~
            * YOU TO ONE LINE UP WHEN IT'S AT COLUMN ONE OF THE LINE.   *~
            *************************************************************

        columnone
            init(" ") addunits$(c%), addamount$(c%)
            return clear all
            goto L11170

        lineabove
            if c% = 1 then return
               on fieldnr% gosub L14180,  /* UNITS TO ADD TO GROSS      */~
                                 L14190   /* AMOUNT TO ADD TO GROSS     */
                  return
L14180:        addunits$ (c%) = addunits$ (c%-1): return
L14190:        addamount$(c%) = addamount$(c%-1): return
            return

        REM *************************************************************~
            *                    W R I T E   D A T A                    *~
            *                                                           *~
            * WRITES DATA TO THE FILE.  IN THIS CASE, WHAT WE DO IS TO  *~
            * UPDATE THE "BUFFER" AMOUNTS IN THE EMPLOYEE EARNINGS      *~
            * RECORDS.  NOTE AS USUAL THE READ HOLD TO THE EMPLOYEE     *~
            * MASTER RECORD SO THAT WE CAN AVOID UPDATE OVERLAP         *~
            * (SPELLED N-U-K-E!!)                                       *~
            *************************************************************

        datasave
            gosub L31000
            lastemp$ = empcode$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   L I N E A R     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR PAGE 1 OF LINEAR INPUT*~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* EMPLOYEE CODE    */
                     return
L20100:     REM DEFAULT/ENABLE FOR EMPLOYEE CODE
                enabled% = 1
                blankline$  = "To Input Hours Table For This Employee, En~
        ~ter His Code And Press (3)."
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULT/ENABLE FOR TABULAR INPUT (THIS IS KIND OF TRICKY  *~
            * SO HANG ON TO YOUR HATS, HAIRPIECES, OR WHATEVER)         *~
            * WE GET THE USUAL # OF UNITS IF NON-ZERO AND FORMAT IT TO  *~
            * THE DEFAULT FIELD.  THEN FOR THE AMOUNT, IF THE NUMBER OF *~
            * UNITS IS THE SAME AS THE USUAL UNITS, AND THE USUAL UNITS *~
            * IS NON-ZERO, THEN THE AMOUNT TO ADD GETS THE USUAL AMOUNT *~
            * OTHERWISE, IT GETS THE AMOUNT TO ADD * RATE PER UNIT      *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L21100,         /* UNITS FIELD      */~
                                    L21200          /* AMOUNT FIELD     */
                     return
L21100:     REM SET DEFAULTS/ENABLE FIELD FOR UNITS FIELD
                if pf_threed% = 1% then L21130
                enabled% = 1
                if hoursamt(c%) = 0 then L21150
L21130:            call "CONVERT" (hoursamt(c%), -0.4, addunits$(c%))
                   return
L21150:         if addunits$(c%) <> " " then return
                   if usunits(c%) = 0 then return
                      call "CONVERT" (usunits(c%), -0.4, addunits$(c%))
                      return
L21200:     REM DEFAULT/ENABLE FOR AMOUNT FIELD.
                if addunits$(c%) = "0" then L21260
                if pf_threed% = 1% then L21216
                   enabled% = 1
                   if addamount$(c%) <> " " then return
L21216:               addunits = 0
                      convert addunits$(c%) to addunits, data goto L21225
L21225:               if addunits <> usunits(c%) then L21245
                      if pf_threed% = 1% then L21245
                         if usunits(c%) = 0 then L21245
                            addamount = usbucks(c%)
                            goto L21250
L21245:               addamount = rate(c%) * addunits
L21250:               call "CONVERT" (addamount, -2.2, addamount$(c%))
                      return
L21260:         REM CASE FOR NO UNITS TO BE ADDED.
                    addamount$(c%) = "0.00"
                    return
                return

        REM *************************************************************~
            *    D E F A U L T / E N A B L E   H O U R S   T A B L E    *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR HOURS TABLE.          *~
            * FIND ALL EARNINGS TYPE WITH HOURS AND GO FOR IT.          *~
            *************************************************************

            deffn'162
                  blankline$ = " "

                  init(hex(9c)) fac1$()
                  init(hex(9c)) promptfac$()
                  for temp% = 1 to 6
L22130:               hoursptr% = hoursptr% + 1
                      if hoursptr% > maxlines% then return
                         if units$(hoursptr%) <> "HOURS" then L22130
                            hourstype$(temp%) = type$(hoursptr%)
                            promptfac$(temp%) = hex(8c)
                            for i% = 1 to 3
                                for j% = 1 to 7
                                    fac1$(3 * (temp%-1) + i%, j%)=hex(82)
                                    next j%
                                next i%
                      next temp%
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            * R E A D   E M P L O Y E E   E A R N I N G S   R E C O R D *~
            *                                                           *~
            * READS THE EMPLOYEE'S NAME AND THEN THE EARNINGS RECORDS.  *~
            *************************************************************

            REM DISPLAY TO USER EXACTLY WHO WE'RE LOADING NOW...
                get #14, using L30090, status$, name$(3), name$(1),       ~
                                      name$(2)
L30090:                 FMT CH(1), CH(15), CH(10), CH(1)

                infomsg$ = "LOADING EMPLOYEE" & hex(84) & empcode$ & " ("~
                             & name$(1) & " " & name$(2) & " " & name$(3)~
                             & ")"
                call "SHOSTAT" (infomsg$)

            REM NOW LOAD EARNINGS RECORDS.
                maxlines%, c%, flag% = 0
                readkey$ = empcode$

L30200:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then return
                maxlines%, c% = c% + 1
                get   # 4, using L30430,                                  ~
                           type$(c%), dept$(c%), units$(c%), rate(c%),   ~
                           usunits(c%), usbucks(c%), addunits, addamount,~
                           currunits, curramount

                if addunits <> 0 then flag% = 1    /* EDIT IF NON-ZERO */

                if addunits  = 0 and addamount = 0 then L30340
                   call "CONVERT" (addunits, -0.4, addunits$(c%))
                   call "CONVERT" (addamount, -2.2, addamount$(c%))
L30340:         if currunits = 0 and curramount = 0 then L30400
                   call "CONVERT" (currunits,  0.4, currunits$(c%))
                   call "CONVERT" (curramount, 2.2, curramt$(c%))
L30400:         if curramount + addamount <> 0                           ~
                   then call "CONVERT" (curramount + addamount,  2.2,    ~
                                                   nextcheck$(c%))
                goto L30200

L30430:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(4),                   /* DEPARTMENT CODE            */~
                XX(1),                   /* PAID IN CASH? FLAG         */~
                XX(1),                   /* IS THIS TAXABLE?           */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                PD(14,4),                /* UNIT RATE                  */~
                XX(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                PD(14,4),                /* USUAL PAYROLL # OF UNITS   */~
                PD(14,4),                /* USUAL PAYROLL AMOUNT       */~
                PD(14,4),                /* BUFFER NUMBER OF UNITS     */~
                PD(14,4),                /* BUFFER AMOUNT              */~
                PD(14,4),                /* CURRENT UNITS              */~
                PD(14,4),                /* CURRENT AMOUNT             */~
                CH(48)                   /* MTD, QTD, YTD ACCUMULATORS */~

L31000: REM *************************************************************~
            *        W R I T E   E A R N I N G S   R E C O R D S        *~
            *                                                           *~
            * WRITE EARNINGS RECORDS TO THE BUFFER AMOUNT.  LATER ON    *~
            * THEY WILL BE PUT INTO THE "CURRENT" FIELD (GOTTA POST TO  *~
            * G/L...IF YOU UNPLUG G/L YOU CAN SHORTCUT HERE...)         *~
            *************************************************************

            REM FIRST, WE GOTTA WRITE THAT THIS GUY HAS NEW EARNINGS.
                call "READ101" (#3, empcode$, f1%(3))
                     if f1%(3) <> 0 then L31110
                return clear : goto editmode
L31110:         get #3, using L31120, str(emprecord$(), 1)
L31120:                 FMT CH(136)
                str(emprecord$(),70, 1)="Y" /*EARNINGS HAVE BEEN ENTERED*/
                rewrite #3, using L31120, str(emprecord$(), 1)

            REM NOW WRITE THE EARNINGS RECORDS FOR THIS EMPLOYEE.
                readkey$ = empcode$
                call "READ103" (#4, readkey$, f1%(4))

                for temp% = 1 to maxlines%
                    REM UNFORMAT A LINE ITEM
                        if addunits$(temp%) = " "                        ~
                           then addunits = 0                             ~
                           else convert addunits$(temp%) to addunits
                        if addamount$(temp%) = " "                       ~
                           then addamount = 0                            ~
                           else convert addamount$(temp%) to addamount

                    get #4, using L31300, str(emprecord$(), 1)
L31300:                     FMT CH(200)
                    if str(emprecord$(), 1, 12) <> empcode$ then return
                    put str(emprecord$(), 85, 16), using L31340,          ~
                               addunits, addamount
L31340:             FMT 2 * PD(14,4)
                    rewrite #4, using L31300, str(emprecord$(), 1)
                    call "READNXT1" (#4, f1%(4))
                    next temp%
               call "STRTRLSE" addr(#4)
               writeflag% = 1
               return

        REM *************************************************************~
            *       I N P U T   S C R E E N   F O R   L I N E A R       *~
            *                                                           *~
            * INPUT SCREEN FOR LINEAR MODE.                             *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  if lastemp$ <> " " then str(header$,,61) =             ~
                                             "Last Employee: " & lastemp$
                  on fieldnr% gosub L40140          /* EMPLOYEE CODE    */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      lfac$(fieldnr%) = hex(82)
                      return

L40210:     accept                                                       ~
               at (01,02), "Input Employee Earnings",                    ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYEE CODE",                                       ~
               at (06,30), fac(lfac$( 1)), empcode$             , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   blankline$           , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,02),                                               ~
                  "(3)HOURLY INPUT TABLE THIS EMPLOYEE",                 ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
               keys(hex(0001030d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40510
                  call "MANUAL" ("PRLINPUT")
                  goto L40210

L40510:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *        E D I T   S C R E E N   F O R   L I N E A R        *~
            *                                                           *~
            * EDITS LINEAR ENTRIES.  NOTHING OUT OF STANDARD HERE.      *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41140          /* EMPLOYEE CODE    */
                     goto L41210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      lfac$(fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "EDIT EMPLOYEE EARNINGS",                              ~
               at (01,54),                                               ~
                  "LAST EMPLOYEE:",                                      ~
               at (01,69), fac(hex(84)), empcode$               , ch(12),~
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
               at (21,02), fac(hex(a4)),   blankline$           , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41490
                  call "MANUAL" ("PRLINPUT")
                  goto L41210

L41490:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41210

        REM *************************************************************~
            *  G E T   H O U R S   T A B L E   I N P U T   S C R E E N  *~
            *                                                           *~
            * GETS HOURS INPUT SCREEN.  NOTE THAT THERE IS NO           *~
            * CORRESPONDING EDIT MODE SCREEN. (RANDOM COINCIDENCE)      *~
            *************************************************************

            deffn'202

L42045:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Hours Table",                                   ~
               at (01,54),                                               ~
                  "This Employee:",                                      ~
               at (01,69), fac(hex(84)), empcode$               , ch(12),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,34),                                               ~
                  "-MON-  -TUE-  -WED-  -THU-  -FRI-  -SAT-  -SUN-",     ~
                                                                         ~
               at (05,02), fac(promptfac$(1)), hourstype$(1)    , ch(12),~
               at (08,02), fac(promptfac$(2)), hourstype$(2)    , ch(12),~
               at (11,02), fac(promptfac$(3)), hourstype$(3)    , ch(12),~
               at (14,02), fac(promptfac$(4)), hourstype$(4)    , ch(12),~
               at (17,02), fac(promptfac$(5)), hourstype$(5)    , ch(12),~
               at (20,02), fac(promptfac$(6)), hourstype$(6)    , ch(12),~
                                                                         ~
               at (05,15), fac(promptfac$(1)), weekname$(1)     , ch(08),~
               at (06,15), fac(promptfac$(1)), weekname$(2)     , ch(08),~
               at (07,15), fac(promptfac$(1)), weekname$(3)     , ch(08),~
               at (08,15), fac(promptfac$(2)), weekname$(1)     , ch(08),~
               at (09,15), fac(promptfac$(2)), weekname$(2)     , ch(08),~
               at (10,15), fac(promptfac$(2)), weekname$(3)     , ch(08),~
               at (11,15), fac(promptfac$(3)), weekname$(1)     , ch(08),~
               at (12,15), fac(promptfac$(3)), weekname$(2)     , ch(08),~
               at (13,15), fac(promptfac$(3)), weekname$(3)     , ch(08),~
               at (14,15), fac(promptfac$(4)), weekname$(1)     , ch(08),~
               at (15,15), fac(promptfac$(4)), weekname$(2)     , ch(08),~
               at (16,15), fac(promptfac$(4)), weekname$(3)     , ch(08),~
               at (17,15), fac(promptfac$(5)), weekname$(1)     , ch(08),~
               at (18,15), fac(promptfac$(5)), weekname$(2)     , ch(08),~
               at (19,15), fac(promptfac$(5)), weekname$(3)     , ch(08),~
               at (20,15), fac(promptfac$(6)), weekname$(1)     , ch(08),~
               at (21,15), fac(promptfac$(6)), weekname$(2)     , ch(08),~
               at (22,15), fac(promptfac$(6)), weekname$(3)     , ch(08),~
                                                                         ~
               at (05,34), fac(fac1$( 1,1)), hours$( 1, 1)      , ch(05),~
               at (05,41), fac(fac1$( 1,2)), hours$( 1, 2)      , ch(05),~
               at (05,48), fac(fac1$( 1,3)), hours$( 1, 3)      , ch(05),~
               at (05,55), fac(fac1$( 1,4)), hours$( 1, 4)      , ch(05),~
               at (05,62), fac(fac1$( 1,5)), hours$( 1, 5)      , ch(05),~
               at (05,69), fac(fac1$( 1,6)), hours$( 1, 6)      , ch(05),~
               at (05,76), fac(fac1$( 1,7)), hours$( 1, 7)      , ch(05),~
                                                                         ~
               at (06,34), fac(fac1$( 2,1)), hours$( 2, 1)      , ch(05),~
               at (06,41), fac(fac1$( 2,2)), hours$( 2, 2)      , ch(05),~
               at (06,48), fac(fac1$( 2,3)), hours$( 2, 3)      , ch(05),~
               at (06,55), fac(fac1$( 2,4)), hours$( 2, 4)      , ch(05),~
               at (06,62), fac(fac1$( 2,5)), hours$( 2, 5)      , ch(05),~
               at (06,69), fac(fac1$( 2,6)), hours$( 2, 6)      , ch(05),~
               at (06,76), fac(fac1$( 2,7)), hours$( 2, 7)      , ch(05),~
                                                                         ~
               at (07,34), fac(fac1$( 3,1)), hours$( 3, 1)      , ch(05),~
               at (07,41), fac(fac1$( 3,2)), hours$( 3, 2)      , ch(05),~
               at (07,48), fac(fac1$( 3,3)), hours$( 3, 3)      , ch(05),~
               at (07,55), fac(fac1$( 3,4)), hours$( 3, 4)      , ch(05),~
               at (07,62), fac(fac1$( 3,5)), hours$( 3, 5)      , ch(05),~
               at (07,69), fac(fac1$( 3,6)), hours$( 3, 6)      , ch(05),~
               at (07,76), fac(fac1$( 3,7)), hours$( 3, 7)      , ch(05),~
                                                                         ~
               at (08,34), fac(fac1$( 4,1)), hours$( 4, 1)      , ch(05),~
               at (08,41), fac(fac1$( 4,2)), hours$( 4, 2)      , ch(05),~
               at (08,48), fac(fac1$( 4,3)), hours$( 4, 3)      , ch(05),~
               at (08,55), fac(fac1$( 4,4)), hours$( 4, 4)      , ch(05),~
               at (08,62), fac(fac1$( 4,5)), hours$( 4, 5)      , ch(05),~
               at (08,69), fac(fac1$( 4,6)), hours$( 4, 6)      , ch(05),~
               at (08,76), fac(fac1$( 4,7)), hours$( 4, 7)      , ch(05),~
                                                                         ~
               at (09,34), fac(fac1$( 5,1)), hours$( 5, 1)      , ch(05),~
               at (09,41), fac(fac1$( 5,2)), hours$( 5, 2)      , ch(05),~
               at (09,48), fac(fac1$( 5,3)), hours$( 5, 3)      , ch(05),~
               at (09,55), fac(fac1$( 5,4)), hours$( 5, 4)      , ch(05),~
               at (09,62), fac(fac1$( 5,5)), hours$( 5, 5)      , ch(05),~
               at (09,69), fac(fac1$( 5,6)), hours$( 5, 6)      , ch(05),~
               at (09,76), fac(fac1$( 5,7)), hours$( 5, 7)      , ch(05),~
                                                                         ~
               at (10,34), fac(fac1$( 6,1)), hours$( 6, 1)      , ch(05),~
               at (10,41), fac(fac1$( 6,2)), hours$( 6, 2)      , ch(05),~
               at (10,48), fac(fac1$( 6,3)), hours$( 6, 3)      , ch(05),~
               at (10,55), fac(fac1$( 6,4)), hours$( 6, 4)      , ch(05),~
               at (10,62), fac(fac1$( 6,5)), hours$( 6, 5)      , ch(05),~
               at (10,69), fac(fac1$( 6,6)), hours$( 6, 6)      , ch(05),~
               at (10,76), fac(fac1$( 6,7)), hours$( 6, 7)      , ch(05),~
                                                                         ~
               at (11,34), fac(fac1$( 7,1)), hours$( 7, 1)      , ch(05),~
               at (11,41), fac(fac1$( 7,2)), hours$( 7, 2)      , ch(05),~
               at (11,48), fac(fac1$( 7,3)), hours$( 7, 3)      , ch(05),~
               at (11,55), fac(fac1$( 7,4)), hours$( 7, 4)      , ch(05),~
               at (11,62), fac(fac1$( 7,5)), hours$( 7, 5)      , ch(05),~
               at (11,69), fac(fac1$( 7,6)), hours$( 7, 6)      , ch(05),~
               at (11,76), fac(fac1$( 7,7)), hours$( 7, 7)      , ch(05),~
                                                                         ~
               at (12,34), fac(fac1$( 8,1)), hours$( 8, 1)      , ch(05),~
               at (12,41), fac(fac1$( 8,2)), hours$( 8, 2)      , ch(05),~
               at (12,48), fac(fac1$( 8,3)), hours$( 8, 3)      , ch(05),~
               at (12,55), fac(fac1$( 8,4)), hours$( 8, 4)      , ch(05),~
               at (12,62), fac(fac1$( 8,5)), hours$( 8, 5)      , ch(05),~
               at (12,69), fac(fac1$( 8,6)), hours$( 8, 6)      , ch(05),~
               at (12,76), fac(fac1$( 8,7)), hours$( 8, 7)      , ch(05),~
                                                                         ~
               at (13,34), fac(fac1$( 9,1)), hours$( 9, 1)      , ch(05),~
               at (13,41), fac(fac1$( 9,2)), hours$( 9, 2)      , ch(05),~
               at (13,48), fac(fac1$( 9,3)), hours$( 9, 3)      , ch(05),~
               at (13,55), fac(fac1$( 9,4)), hours$( 9, 4)      , ch(05),~
               at (13,62), fac(fac1$( 9,5)), hours$( 9, 5)      , ch(05),~
               at (13,69), fac(fac1$( 9,6)), hours$( 9, 6)      , ch(05),~
               at (13,76), fac(fac1$( 9,7)), hours$( 9, 7)      , ch(05),~
                                                                         ~
               at (14,34), fac(fac1$(10,1)), hours$(10, 1)      , ch(05),~
               at (14,41), fac(fac1$(10,2)), hours$(10, 2)      , ch(05),~
               at (14,48), fac(fac1$(10,3)), hours$(10, 3)      , ch(05),~
               at (14,55), fac(fac1$(10,4)), hours$(10, 4)      , ch(05),~
               at (14,62), fac(fac1$(10,5)), hours$(10, 5)      , ch(05),~
               at (14,69), fac(fac1$(10,6)), hours$(10, 6)      , ch(05),~
               at (14,76), fac(fac1$(10,7)), hours$(10, 7)      , ch(05),~
                                                                         ~
               at (15,34), fac(fac1$(11,1)), hours$(11, 1)      , ch(05),~
               at (15,41), fac(fac1$(11,2)), hours$(11, 2)      , ch(05),~
               at (15,48), fac(fac1$(11,3)), hours$(11, 3)      , ch(05),~
               at (15,55), fac(fac1$(11,4)), hours$(11, 4)      , ch(05),~
               at (15,62), fac(fac1$(11,5)), hours$(11, 5)      , ch(05),~
               at (15,69), fac(fac1$(11,6)), hours$(11, 6)      , ch(05),~
               at (15,76), fac(fac1$(11,7)), hours$(11, 7)      , ch(05),~
                                                                         ~
               at (16,34), fac(fac1$(12,1)), hours$(12, 1)      , ch(05),~
               at (16,41), fac(fac1$(12,2)), hours$(12, 2)      , ch(05),~
               at (16,48), fac(fac1$(12,3)), hours$(12, 3)      , ch(05),~
               at (16,55), fac(fac1$(12,4)), hours$(12, 4)      , ch(05),~
               at (16,62), fac(fac1$(12,5)), hours$(12, 5)      , ch(05),~
               at (16,69), fac(fac1$(12,6)), hours$(12, 6)      , ch(05),~
               at (16,76), fac(fac1$(12,7)), hours$(12, 7)      , ch(05),~
                                                                         ~
               at (17,34), fac(fac1$(13,1)), hours$(13, 1)      , ch(05),~
               at (17,41), fac(fac1$(13,2)), hours$(13, 2)      , ch(05),~
               at (17,48), fac(fac1$(13,3)), hours$(13, 3)      , ch(05),~
               at (17,55), fac(fac1$(13,4)), hours$(13, 4)      , ch(05),~
               at (17,62), fac(fac1$(13,5)), hours$(13, 5)      , ch(05),~
               at (17,69), fac(fac1$(13,6)), hours$(13, 6)      , ch(05),~
               at (17,76), fac(fac1$(13,7)), hours$(13, 7)      , ch(05),~
                                                                         ~
               at (18,34), fac(fac1$(14,1)), hours$(14, 1)      , ch(05),~
               at (18,41), fac(fac1$(14,2)), hours$(14, 2)      , ch(05),~
               at (18,48), fac(fac1$(14,3)), hours$(14, 3)      , ch(05),~
               at (18,55), fac(fac1$(14,4)), hours$(14, 4)      , ch(05),~
               at (18,62), fac(fac1$(14,5)), hours$(14, 5)      , ch(05),~
               at (18,69), fac(fac1$(14,6)), hours$(14, 6)      , ch(05),~
               at (18,76), fac(fac1$(14,7)), hours$(14, 7)      , ch(05),~
                                                                         ~
               at (19,34), fac(fac1$(15,1)), hours$(15, 1)      , ch(05),~
               at (19,41), fac(fac1$(15,2)), hours$(15, 2)      , ch(05),~
               at (19,48), fac(fac1$(15,3)), hours$(15, 3)      , ch(05),~
               at (19,55), fac(fac1$(15,4)), hours$(15, 4)      , ch(05),~
               at (19,62), fac(fac1$(15,5)), hours$(15, 5)      , ch(05),~
               at (19,69), fac(fac1$(15,6)), hours$(15, 6)      , ch(05),~
               at (19,76), fac(fac1$(15,7)), hours$(15, 7)      , ch(05),~
                                                                         ~
               at (20,34), fac(fac1$(16,1)), hours$(16, 1)      , ch(05),~
               at (20,41), fac(fac1$(16,2)), hours$(16, 2)      , ch(05),~
               at (20,48), fac(fac1$(16,3)), hours$(16, 3)      , ch(05),~
               at (20,55), fac(fac1$(16,4)), hours$(16, 4)      , ch(05),~
               at (20,62), fac(fac1$(16,5)), hours$(16, 5)      , ch(05),~
               at (20,69), fac(fac1$(16,6)), hours$(16, 6)      , ch(05),~
               at (20,76), fac(fac1$(16,7)), hours$(16, 7)      , ch(05),~
                                                                         ~
               at (21,34), fac(fac1$(17,1)), hours$(17, 1)      , ch(05),~
               at (21,41), fac(fac1$(17,2)), hours$(17, 2)      , ch(05),~
               at (21,48), fac(fac1$(17,3)), hours$(17, 3)      , ch(05),~
               at (21,55), fac(fac1$(17,4)), hours$(17, 4)      , ch(05),~
               at (21,62), fac(fac1$(17,5)), hours$(17, 5)      , ch(05),~
               at (21,69), fac(fac1$(17,6)), hours$(17, 6)      , ch(05),~
               at (21,76), fac(fac1$(17,7)), hours$(17, 7)      , ch(05),~
                                                                         ~
               at (22,34), fac(fac1$(18,1)), hours$(18, 1)      , ch(05),~
               at (22,41), fac(fac1$(18,2)), hours$(18, 2)      , ch(05),~
               at (22,48), fac(fac1$(18,3)), hours$(18, 3)      , ch(05),~
               at (22,55), fac(fac1$(18,4)), hours$(18, 4)      , ch(05),~
               at (22,62), fac(fac1$(18,5)), hours$(18, 5)      , ch(05),~
               at (22,69), fac(fac1$(18,6)), hours$(18, 6)      , ch(05),~
               at (22,76), fac(fac1$(18,7)), hours$(18, 7)      , ch(05),~
                                                                         ~
               at (23,02), fac(hex(a4)),    blankline$          , ch(79),~
               at (24,02),                                               ~
                  "P.F. KEYS ACTIVE: (1)START OVER",                     ~
               at (24,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L42980
                  call "MANUAL" ("PRLINPUT")
                  goto L42045

L42980:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42045

        REM *************************************************************~
            *                 T A B L E   S C R E E N S                 *~
            *                                                           *~
            * NOTE THAT THESE TABLE SCREENS ARE A SLIGHT DEPARTURE FROM *~
            * THE NORM--FIRST, THERE ARE NO INSERT AND DELETE MODES,    *~
            * (FORTUNATELY), AND SECOND, THERE IS SLIGHTLY DIFFERENT    *~
            * TITLING THAN THE IN-LINE MNEMONICS CURRENTLY USED.        *~
            * ALSO, THE INCOMPREHENSIBLE MASS IN DEFFN'213 SETS FAC$()  *~
            * TO TABABLE WHERE THERE ARE VALID FIELDS ON THE SCREEN, AND*~
            * NON-TABABLE FOR THE REST (FOR PARTIALLY FULL SCREENS).    *~
            * ALSO NOTE SPECIAL FOR EDIT SINGLE FIELDS.  OTHERWISE,     *~
            * TABS WILL GET IN THE WAY.                                 *~
            *************************************************************

            deffn'203(screenline%, fieldnr%)
                  init(hex(84)) fac$()
                  screen% = 1
                  goto L44120

            deffn'213(screenline%, fieldnr%)
                  init(hex(86)) fac$()
                  if maxlines% - line% > 19% then L44092
                  init(hex(84)) str(fac$(), 1 + (2*(maxlines% - line%)))

L44092:           screen% = 2
                  goto L44120

            deffn'223(screenline%, fieldnr%)
                  init(hex(84)) fac$()
                  screen% = 2

L44120:           on fieldnr% gosub L44160,         /* UNITS TO ADD     */~
                                    L44160          /* AMOUNT TO ADD    */
                     goto L44176

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L44160:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L44176:     accept                                                       ~
               at (01,02), fac(hex(8c)),    title$(screen%)     , ch(64),~
               at (02,02), fac(hex(84)),    infomsg$            , ch(64),~
               at (03,02), fac(hex(94)),    errormsg$           , ch(64),~
               at (04,02), fac(hex(ac)),    heading$            , ch(79),~
                                                                         ~
               at (01,67), "! EMPLOYEE:   "                             ,~
               at (02,67), "!             "                             ,~
               at (03,67), "+-------------"                             ,~
               at (02,69), fac(hex(84)),    empcode$            , ch(12),~
                                                                         ~
               at (05,02), fac(hex(8c)),    type$     (line%+ 1), ch(12),~
               at (06,02), fac(hex(8c)),    type$     (line%+ 2), ch(12),~
               at (07,02), fac(hex(8c)),    type$     (line%+ 3), ch(12),~
               at (08,02), fac(hex(8c)),    type$     (line%+ 4), ch(12),~
               at (09,02), fac(hex(8c)),    type$     (line%+ 5), ch(12),~
               at (10,02), fac(hex(8c)),    type$     (line%+ 6), ch(12),~
               at (11,02), fac(hex(8c)),    type$     (line%+ 7), ch(12),~
               at (12,02), fac(hex(8c)),    type$     (line%+ 8), ch(12),~
               at (13,02), fac(hex(8c)),    type$     (line%+ 9), ch(12),~
               at (14,02), fac(hex(8c)),    type$     (line%+10), ch(12),~
               at (15,02), fac(hex(8c)),    type$     (line%+11), ch(12),~
               at (16,02), fac(hex(8c)),    type$     (line%+12), ch(12),~
               at (17,02), fac(hex(8c)),    type$     (line%+13), ch(12),~
               at (18,02), fac(hex(8c)),    type$     (line%+14), ch(12),~
               at (19,02), fac(hex(8c)),    type$     (line%+15), ch(12),~
               at (20,02), fac(hex(8c)),    type$     (line%+16), ch(12),~
               at (21,02), fac(hex(8c)),    type$     (line%+17), ch(12),~
               at (22,02), fac(hex(8c)),    type$     (line%+18), ch(12),~
               at (23,02), fac(hex(8c)),    type$     (line%+19), ch(12),~
               at (24,02), fac(hex(8c)),    type$     (line%+20), ch(12),~
                                                                         ~
               at (05,15), fac(hex(8c)),    dept$     (line%+ 1), ch(04),~
               at (06,15), fac(hex(8c)),    dept$     (line%+ 2), ch(04),~
               at (07,15), fac(hex(8c)),    dept$     (line%+ 3), ch(04),~
               at (08,15), fac(hex(8c)),    dept$     (line%+ 4), ch(04),~
               at (09,15), fac(hex(8c)),    dept$     (line%+ 5), ch(04),~
               at (10,15), fac(hex(8c)),    dept$     (line%+ 6), ch(04),~
               at (11,15), fac(hex(8c)),    dept$     (line%+ 7), ch(04),~
               at (12,15), fac(hex(8c)),    dept$     (line%+ 8), ch(04),~
               at (13,15), fac(hex(8c)),    dept$     (line%+ 9), ch(04),~
               at (14,15), fac(hex(8c)),    dept$     (line%+10), ch(04),~
               at (15,15), fac(hex(8c)),    dept$     (line%+11), ch(04),~
               at (16,15), fac(hex(8c)),    dept$     (line%+12), ch(04),~
               at (17,15), fac(hex(8c)),    dept$     (line%+13), ch(04),~
               at (18,15), fac(hex(8c)),    dept$     (line%+14), ch(04),~
               at (19,15), fac(hex(8c)),    dept$     (line%+15), ch(04),~
               at (20,15), fac(hex(8c)),    dept$     (line%+16), ch(04),~
               at (21,15), fac(hex(8c)),    dept$     (line%+17), ch(04),~
               at (22,15), fac(hex(8c)),    dept$     (line%+18), ch(04),~
               at (23,15), fac(hex(8c)),    dept$     (line%+19), ch(04),~
               at (24,15), fac(hex(8c)),    dept$     (line%+20), ch(04),~
                                                                         ~
               at (05,20), fac(hex(8c)),    units$    (line%+ 1), ch(06),~
               at (06,20), fac(hex(8c)),    units$    (line%+ 2), ch(06),~
               at (07,20), fac(hex(8c)),    units$    (line%+ 3), ch(06),~
               at (08,20), fac(hex(8c)),    units$    (line%+ 4), ch(06),~
               at (09,20), fac(hex(8c)),    units$    (line%+ 5), ch(06),~
               at (10,20), fac(hex(8c)),    units$    (line%+ 6), ch(06),~
               at (11,20), fac(hex(8c)),    units$    (line%+ 7), ch(06),~
               at (12,20), fac(hex(8c)),    units$    (line%+ 8), ch(06),~
               at (13,20), fac(hex(8c)),    units$    (line%+ 9), ch(06),~
               at (14,20), fac(hex(8c)),    units$    (line%+10), ch(06),~
               at (15,20), fac(hex(8c)),    units$    (line%+11), ch(06),~
               at (16,20), fac(hex(8c)),    units$    (line%+12), ch(06),~
               at (17,20), fac(hex(8c)),    units$    (line%+13), ch(06),~
               at (18,20), fac(hex(8c)),    units$    (line%+14), ch(06),~
               at (19,20), fac(hex(8c)),    units$    (line%+15), ch(06),~
               at (20,20), fac(hex(8c)),    units$    (line%+16), ch(06),~
               at (21,20), fac(hex(8c)),    units$    (line%+17), ch(06),~
               at (22,20), fac(hex(8c)),    units$    (line%+18), ch(06),~
               at (23,20), fac(hex(8c)),    units$    (line%+19), ch(06),~
               at (24,20), fac(hex(8c)),    units$    (line%+20), ch(06),~
                                                                         ~
               at (05,27), fac(fac$( 1,1)), addunits$ (line%+ 1), ch(10),~
               at (06,27), fac(fac$( 2,1)), addunits$ (line%+ 2), ch(10),~
               at (07,27), fac(fac$( 3,1)), addunits$ (line%+ 3), ch(10),~
               at (08,27), fac(fac$( 4,1)), addunits$ (line%+ 4), ch(10),~
               at (09,27), fac(fac$( 5,1)), addunits$ (line%+ 5), ch(10),~
               at (10,27), fac(fac$( 6,1)), addunits$ (line%+ 6), ch(10),~
               at (11,27), fac(fac$( 7,1)), addunits$ (line%+ 7), ch(10),~
               at (12,27), fac(fac$( 8,1)), addunits$ (line%+ 8), ch(10),~
               at (13,27), fac(fac$( 9,1)), addunits$ (line%+ 9), ch(10),~
               at (14,27), fac(fac$(10,1)), addunits$ (line%+10), ch(10),~
               at (15,27), fac(fac$(11,1)), addunits$ (line%+11), ch(10),~
               at (16,27), fac(fac$(12,1)), addunits$ (line%+12), ch(10),~
               at (17,27), fac(fac$(13,1)), addunits$ (line%+13), ch(10),~
               at (18,27), fac(fac$(14,1)), addunits$ (line%+14), ch(10),~
               at (19,27), fac(fac$(15,1)), addunits$ (line%+15), ch(10),~
               at (20,27), fac(fac$(16,1)), addunits$ (line%+16), ch(10),~
               at (21,27), fac(fac$(17,1)), addunits$ (line%+17), ch(10),~
               at (22,27), fac(fac$(18,1)), addunits$ (line%+18), ch(10),~
               at (23,27), fac(fac$(19,1)), addunits$ (line%+19), ch(10),~
               at (24,27), fac(fac$(20,1)), addunits$ (line%+20), ch(10),~
                                                                         ~
               at (05,38), fac(fac$( 1,2)), addamount$(line%+ 1), ch(10),~
               at (06,38), fac(fac$( 2,2)), addamount$(line%+ 2), ch(10),~
               at (07,38), fac(fac$( 3,2)), addamount$(line%+ 3), ch(10),~
               at (08,38), fac(fac$( 4,2)), addamount$(line%+ 4), ch(10),~
               at (09,38), fac(fac$( 5,2)), addamount$(line%+ 5), ch(10),~
               at (10,38), fac(fac$( 6,2)), addamount$(line%+ 6), ch(10),~
               at (11,38), fac(fac$( 7,2)), addamount$(line%+ 7), ch(10),~
               at (12,38), fac(fac$( 8,2)), addamount$(line%+ 8), ch(10),~
               at (13,38), fac(fac$( 9,2)), addamount$(line%+ 9), ch(10),~
               at (14,38), fac(fac$(10,2)), addamount$(line%+10), ch(10),~
               at (15,38), fac(fac$(11,2)), addamount$(line%+11), ch(10),~
               at (16,38), fac(fac$(12,2)), addamount$(line%+12), ch(10),~
               at (17,38), fac(fac$(13,2)), addamount$(line%+13), ch(10),~
               at (18,38), fac(fac$(14,2)), addamount$(line%+14), ch(10),~
               at (19,38), fac(fac$(15,2)), addamount$(line%+15), ch(10),~
               at (20,38), fac(fac$(16,2)), addamount$(line%+16), ch(10),~
               at (21,38), fac(fac$(17,2)), addamount$(line%+17), ch(10),~
               at (22,38), fac(fac$(18,2)), addamount$(line%+18), ch(10),~
               at (23,38), fac(fac$(19,2)), addamount$(line%+19), ch(10),~
               at (24,38), fac(fac$(20,2)), addamount$(line%+20), ch(10),~
                                                                         ~
               at (05,49), fac(hex(8c)),    currunits$(line%+ 1), ch(10),~
               at (06,49), fac(hex(8c)),    currunits$(line%+ 2), ch(10),~
               at (07,49), fac(hex(8c)),    currunits$(line%+ 3), ch(10),~
               at (08,49), fac(hex(8c)),    currunits$(line%+ 4), ch(10),~
               at (09,49), fac(hex(8c)),    currunits$(line%+ 5), ch(10),~
               at (10,49), fac(hex(8c)),    currunits$(line%+ 6), ch(10),~
               at (11,49), fac(hex(8c)),    currunits$(line%+ 7), ch(10),~
               at (12,49), fac(hex(8c)),    currunits$(line%+ 8), ch(10),~
               at (13,49), fac(hex(8c)),    currunits$(line%+ 9), ch(10),~
               at (14,49), fac(hex(8c)),    currunits$(line%+10), ch(10),~
               at (15,49), fac(hex(8c)),    currunits$(line%+11), ch(10),~
               at (16,49), fac(hex(8c)),    currunits$(line%+12), ch(10),~
               at (17,49), fac(hex(8c)),    currunits$(line%+13), ch(10),~
               at (18,49), fac(hex(8c)),    currunits$(line%+14), ch(10),~
               at (19,49), fac(hex(8c)),    currunits$(line%+15), ch(10),~
               at (20,49), fac(hex(8c)),    currunits$(line%+16), ch(10),~
               at (21,49), fac(hex(8c)),    currunits$(line%+17), ch(10),~
               at (22,49), fac(hex(8c)),    currunits$(line%+18), ch(10),~
               at (23,49), fac(hex(8c)),    currunits$(line%+19), ch(10),~
               at (24,49), fac(hex(8c)),    currunits$(line%+20), ch(10),~
                                                                         ~
               at (05,60), fac(hex(8c)),    curramt$  (line%+ 1), ch(10),~
               at (06,60), fac(hex(8c)),    curramt$  (line%+ 2), ch(10),~
               at (07,60), fac(hex(8c)),    curramt$  (line%+ 3), ch(10),~
               at (08,60), fac(hex(8c)),    curramt$  (line%+ 4), ch(10),~
               at (09,60), fac(hex(8c)),    curramt$  (line%+ 5), ch(10),~
               at (10,60), fac(hex(8c)),    curramt$  (line%+ 6), ch(10),~
               at (11,60), fac(hex(8c)),    curramt$  (line%+ 7), ch(10),~
               at (12,60), fac(hex(8c)),    curramt$  (line%+ 8), ch(10),~
               at (13,60), fac(hex(8c)),    curramt$  (line%+ 9), ch(10),~
               at (14,60), fac(hex(8c)),    curramt$  (line%+10), ch(10),~
               at (15,60), fac(hex(8c)),    curramt$  (line%+11), ch(10),~
               at (16,60), fac(hex(8c)),    curramt$  (line%+12), ch(10),~
               at (17,60), fac(hex(8c)),    curramt$  (line%+13), ch(10),~
               at (18,60), fac(hex(8c)),    curramt$  (line%+14), ch(10),~
               at (19,60), fac(hex(8c)),    curramt$  (line%+15), ch(10),~
               at (20,60), fac(hex(8c)),    curramt$  (line%+16), ch(10),~
               at (21,60), fac(hex(8c)),    curramt$  (line%+17), ch(10),~
               at (22,60), fac(hex(8c)),    curramt$  (line%+18), ch(10),~
               at (23,60), fac(hex(8c)),    curramt$  (line%+19), ch(10),~
               at (24,60), fac(hex(8c)),    curramt$  (line%+20), ch(10),~
                                                                         ~
               at (05,71), fac(hex(84)),    nextcheck$(line%+ 1), ch(10),~
               at (06,71), fac(hex(84)),    nextcheck$(line%+ 2), ch(10),~
               at (07,71), fac(hex(84)),    nextcheck$(line%+ 3), ch(10),~
               at (08,71), fac(hex(84)),    nextcheck$(line%+ 4), ch(10),~
               at (09,71), fac(hex(84)),    nextcheck$(line%+ 5), ch(10),~
               at (10,71), fac(hex(84)),    nextcheck$(line%+ 6), ch(10),~
               at (11,71), fac(hex(84)),    nextcheck$(line%+ 7), ch(10),~
               at (12,71), fac(hex(84)),    nextcheck$(line%+ 8), ch(10),~
               at (13,71), fac(hex(84)),    nextcheck$(line%+ 9), ch(10),~
               at (14,71), fac(hex(84)),    nextcheck$(line%+10), ch(10),~
               at (15,71), fac(hex(84)),    nextcheck$(line%+11), ch(10),~
               at (16,71), fac(hex(84)),    nextcheck$(line%+12), ch(10),~
               at (17,71), fac(hex(84)),    nextcheck$(line%+13), ch(10),~
               at (18,71), fac(hex(84)),    nextcheck$(line%+14), ch(10),~
               at (19,71), fac(hex(84)),    nextcheck$(line%+15), ch(10),~
               at (20,71), fac(hex(84)),    nextcheck$(line%+16), ch(10),~
               at (21,71), fac(hex(84)),    nextcheck$(line%+17), ch(10),~
               at (22,71), fac(hex(84)),    nextcheck$(line%+18), ch(10),~
               at (23,71), fac(hex(84)),    nextcheck$(line%+19), ch(10),~
               at (24,71), fac(hex(84)),    nextcheck$(line%+20), ch(10),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 15 then L44920
                  call "PRNTSCRN"
                  goto L44176

L44920:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *   T E S T   D A T A   F O R   L I N E A R   S C R E E N   *~
            *                                                           *~
            * TESTS DATA FOR THE LINEAR INPUT SCREEN.  THIS IS NEEDED   *~
            * TO SEE WHETHER OR NOT THE EMPLOYEE CODE IS ON FILE AND    *~
            * TO DESCRIBE HIM IF HE IS.                                 *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* EMPLOYEE CODE    */
                     return
L50100:     REM TEST DATA FOR EMPLOYEE CODE
                call "GETEMPL" (#14, empcode$, " ", 0%, f1%(14))
                     if f1%(14) = 0 then L50165
                call "READ100" (#3, empcode$, f1%(3))
                     if f1%(3) = 0 then L50160
                        gosub L30000
                              if status$ <> "C" then L50176
                              if maxlines% = 0 then L50168
                        if flag% = 0 then return
                           return clear all
                           goto editmode
L50160:              errormsg$= "Employee Code Not In Payroll Master File"
                     return
L50165:       errormsg$ = "Employee Code Not In Personnel File"
              return
L50168:         errormsg$= "Employee Has No Earnings Records"
                return
L50176:         errormsg$ = "Employee Is Not Active"
                return

        REM *************************************************************~
            *   T E S T   D A T A   F O R   T A B U L A R   I N P U T   *~
            *                                                           *~
            * TESTS THE INFORMATION GIVEN FOR TABULAR INPUT.            *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51110,         /* UNITS FIELD      */~
                                    L51160          /* AMOUNT FIELD     */
                     return
L51110:     REM TEST DATA FOR UNITS FIELD.
                call "NUMTEST" (addunits$(c%),-9e7,9e7,errormsg$,0.4,tmp)
                     if errormsg$ <> " " then return
                call "CONVERT" (rate(c%) * tmp, -2.2, addamount$(c%))
                goto L51190
L51160:     REM TEST DATA FOR AMOUNT.
                call "NUMTEST" (addamount$(c%),-9e7,9e7,errormsg$,2.2,0)
                     if errormsg$ <> " " then return
L51190:         addunits, addamount = 0
                convert addunits$(c%) to addunits, data goto L51210
L51210:         convert addamount$(c%) to addamount, data goto L51220
L51220:         addunits$(c%), addamount$(c%) = " "
                if addunits  = 0 and addamount = 0 then return
                call "CONVERT" (addunits, -0.4, addunits$(c%))
                call "CONVERT" (addamount, -2.2, addamount$(c%))
                if rate(c%) = 0 or addunits = 0 then return
                if addunits * rate(c%) = addamount then return
L51270:         u3% = 2%
                call "ASKUSER" (u3%, "*** PAYRATE CONSISTENCY WARNING " &~
                                "***", "The 'Add Amount': "             &~
                                addamount$(c%)   & " is not consistent" &~
                                " with", "the Units and Normal Rate of "&~
                                "Pay", "Press RETURN to accept the"     &~
                                " 'ADD AMOUNT' -OR- PF12 to re-enter th"&~
                                "e amount.")
                if u3% = 0% then return
                if u3% <> 12% then L51270
                errormsg$ = "Inconsistent Payrate, Units and Amount."
                return

        REM *************************************************************~
            *     T E S T   D A T A   F O R   H O U R S   T A B L E     *~
            *                                                           *~
            * TESTS DATA FOR THE HOURS TABLE INPUT.  TESTS FOR ONE      *~
            * FIELD AT A TIME, WHOSE SUBSCRIPTS GET PASSED IN.  THIS IS *~
            * SO THAT IT'S EASIER TO SET FAC'S FOR DEFECTIVE ENTRIES.   *~
            * NOTE HOW CRAZY BUSINESS (THAT MEANS IT'S STEVE'S DESIGN,  *~
            * NOT MINE) WITH THE ALPHA ABBREVIATIONS FOR THE EARNINGS   *~
            * TYPES ARE HANDLED.  ALL SORTS OF GOOD CLEAN FUN.          *~
            *************************************************************

            deffn'152
                  errormsg$ = " "

                  for column% = 1 to 18
                      for row% = 1 to 7
                          if hours$(column%, row%) = " " then L52260
                             call "NUMTEST" (hours$(column%, row%), -9e7,~
                                                   9e7,errormsg$, 2.4, 0)
                             if errormsg$ = " " then L52250
                                search type$() = hours$(column%, row%)   ~
                                       to location$ step 12
                                if location$ <> hex(0000) then L52250
                                   errormsg$ = "Illegal Entry For An Hour~
        ~s Table Entry: "                         & hours$(column%, row%)
                                   fac1$(column%, row%) = hex(91)
                                   return
L52250:                      fac1$(column%, row%) = hex(81)
L52260:                   next row%
                      next column%
                  return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end writeflag%
