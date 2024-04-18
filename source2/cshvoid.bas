        REM *************************************************************~
            *                                                           *~
            *   CCC    SSS   H   H  V   V   OOO   IIIII  DDDD           *~
            *  C   C  S      H   H  V   V  O   O    I    D   D          *~
            *  C       SSS   HHHHH  V   V  O   O    I    D   D          *~
            *  C   C      S  H   H   V V   O   O    I    D   D          *~
            *   CCC    SSS   H   H    V     OOO   IIIII  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CSHVOID - INPUT CASH DISBURSEMENTS AND POST TO BUFFER.    *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+---------------WHAT-----------------------+-WHO-*~
            * 03/06/84 ! ORIGINAL (FROM CSHINPUT)                 ! HES *~
            * 01/10/85 ! PLOW VENDOR FILE,CSHMASTR,FOUND = NO VOID! JWG *~
            * 12/16/85 ! Vendor file format changes               ! MJB *~
            *************************************************************

        dim                                                              ~
            blankline$79,                /* BLANK LINE FOR INPUT SCREEN*/~
            checkdate$8,                 /* USERINFO$ VENDOR MODULEDATE*/~
            checknr$8,                   /* CHECK NUMBER               */~
            date$8,                      /* DATE FOR SCREEN HEADER     */~
            diskkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            lastcheck$8,                 /* LAST CHECK NUMBER WRITTEN  */~
            lastdate$8,                  /* DISPLAY LAST MODIFIED DATE */~
            linefac$(20)1,               /* FIELD ATTRIBUTE CHARACTERS */~
            message$(2)79,               /*                            */~
            origuserid$3,                /* USERID OF ORIGINAL USER    */~
            vdate_i$6,                   /* ORIGINALLY VOIDED DATE     */~
            vdate_l$6,                   /* LAST MODIFIED DATE; TODAY  */~
            vendate$6,                   /* VENDOR MODULE DATE THISUSER*/~
            othervendor$9,               /* VENDOR PLOWKEY,FOR CSHMASTR*/~
            userid$3                     /* USERID OF CURRENT USER     */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.08 02/14/86 VBK & VENDOR enhancements       "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 7 ! CSHMASTR ! DISBURSEMENTS CHECK HEADER FILE          *~
            * # 8 ! CSHLINES ! DISBURSEMENTS CHECK DETAIL FILE          *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen = 9,                          ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #7, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
                        alt key 1, keypos = 41, keylen = 9, dup,         ~
                            key 2, keypos = 50, keylen = 6, dup,         ~
                            key 3, keypos = 10, keylen = 8, dup

            select  #8, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))

            if f2%(7)=0 then L09000
            call "OPENFILE" (# 7, "OUTPT", f2%( 7), rslt$( 7), axd$( 7))
            close #7
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SET DATES, TRANSLATION TABLES, ETC.                       *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#1, userid$, f1%(1))
                 if f1%(1) = 0 then L65000
                    get #1, using L09170 , date$
L09170:             FMT XX(9), CH(6)
                 vendate$ = str(date$,1,6)
                    call "DATEFMT" (date$)

        REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            *                                                           *~
            * INPUTS HEADER, LOADS OLD CHECKS IF ON FILE, ETC...        *~
            *************************************************************

        inputmode
            init(" ") checknr$, errormsg$, infomsg$, vdate_i$, vdate_l$, ~
                      blankline$, lastdate$
            origuserid$ = userid$ : checkdate$ = date$

            for fieldnr% = 1 to 1
                gosub'160(fieldnr%)
                      if enabled% =  0 then L10220
L10160:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10160
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10160
L10220:         next fieldnr%

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            *                                                           *~
            * EDITS HEADER INFORMATION.                                 *~
            *************************************************************

        editmode
            init(" ") errormsg$, infomsg$

            gosub'202(0%)                /* SHOW WITH NON-MODIFIABLE   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 12 then       datasave
            goto editmode

        REM *************************************************************~
            *                   S A V E   D A T A                       *~
            *                                                           *~
            * SAVES THE VOID CHECK NUMBER IN THE CHECK FILE.            *~
            *************************************************************

        datasave
            REM NOW DELETE OLD CHECK FROM FILE AND WRITE NEW ONE.
                diskkey$ = "**VOID**"
                str(diskkey$, 10) = checknr$
                call "READ101" (#7, diskkey$, f1%(7))
                if f1%(7) = 1 then delete #7
                call "DELETE" (#8, diskkey$, 17%)

                if keyhit% <> 12 then gosub L31000
                lastcheck$ = checknr$
                goto inputmode

        REM *************************************************************~
            * S E T   D E F A U L T S   F O R   L I N E A R   I N P U T *~
            *                                                           *~
            * SETS DEFAULTS FOR LINEAR INPUT INFORMATION.               *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20200          /* CHECK NUMBER     */
                  return
L20200:     REM SET DEFAULTS FOR CHECK NUMBER
                infomsg$ = "Enter the desired check number"
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *      L O A D   O L D   C H E C K   F R O M   F I L E      *~
            *                                                           *~
            * FIND CHECK NUMBER IN DISBURSEMENTS FILE. IF NOT THERE,    *~
            * THEN RETURN FOR INPUT.                                    *~
            *************************************************************

            REM INITIALIZE FOR LOADING UP A VOIDED CHECK.
                oldcheckonfile% = 0
                diskkey$ = "**VOID**"
                str(diskkey$, 10) = checknr$

              REM GO LOOK FOR A VOIDED CHECK IN CSHMASTR USING CHECKNR
                call "READ100" (#7, diskkey$, f1%(7))
                     if f1%(7) = 0 then return     /* IF NO THEN INPUT */
                get #7, using L30140, checkdate$,vendate$, vdate_i$,      ~
                                     origuserid$, vdate_l$
L30140:                 FMT XX(17),CH(6),XX(26),CH(6),CH(6),CH(3),CH(6)

            REM GET AND FORMAT HEADER INFORMATION.
                oldcheckonfile% = 1
                call "DATEFMT" (checkdate$)
                lastdate$ = str(vdate_l$,1,6) & " "
                call "DATEFMT" (lastdate$)

                return

*       *****************************************************************
*        PLEASE NOTE:
*                                 Program was originally intended to fill
*         blank spots in the check register--not to void checks. If one ~
*         has need to eliminate a check - go through CSHINPUT eliminating
*         line items , until there ain't no more.
*       *****************************************************************

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITE DATA TO FILE, DELETING OLD FROM BUFFER SHOULD THAT  *~
            * BE NECESSARY.                                             *~
            *************************************************************

                    call "DATUNFMT" (checkdate$)
                    write #7, using L31150,                               ~
                              "**VOID** ", checknr$, checkdate$,         ~
                              0, " ", " ", checkdate$, date, origuserid$,~
                              date, userid$, 0,                          ~
                              "N"," "
                    return

L31150:                           FMT CH(9),       /* VENDOR   CODE    */~
                                      CH(8),       /* CHECK NUMBER     */~
                                      CH(6),       /* CHECK DATE       */~
                                      PD(14,4),    /* DISCOUNT AMOUNT  */~
                                      CH(9),       /* DISCOUNT ACCOUNT */~
                                      CH(9),       /* CASH IN BANK ACCT*/~
                                      CH(6),       /* DATE POSTED      */~
                                      CH(6),       /* ORIGINAL DATE    */~
                                      CH(3),       /* USERID-ORIGINAL  */~
                                      CH(6),       /* CURRENT DATE     */~
                                      CH(3),       /* USERID-CURRENT   */~
                                      PD(14,4),    /* NET CHECK AMOUNT */~
                                      CH(1),       /* RECONCILED FLAG  */~
                                      CH(18)       /* DATE RECONCIL/FIL*/

        REM *************************************************************~
            *     I N P U T   C H E C K   H E A D E R   S C R E E N     *~
            *                                                           *~
            * GETS CHECK HEADERS FROM THE SCREEN.  STANDARD INPUT STYLE *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) linefac$()
                  on fieldnr% gosub  L40200         /* CHECK NUMBER     */
                  goto L40270

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linefac$(fieldnr%) = hex(80)
                      return
L40200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linefac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linefac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "VOID PAYABLES CHECKS: ENTER THE CHECK NUMBER",        ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,59),                                               ~
                  "LAST CHECK:",                                         ~
               at (02,72), fac(hex(8c)), lastcheck$             , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "CHECK NUMBER",                                        ~
               at (06,30), fac(linefac$(1)), checknr$           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(ac)), infomsg$               , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,73),                                               ~
                  "(16)EXIT",                                            ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40740
                  call "MANUAL" ("CSHVOID ")
                  goto L40270

L40740:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40270

        REM *************************************************************~
            *            E D I T   H E A D E R   S C R E E N            *~
            *                                                           *~
            * EDITS HEADER INFORMATION.  JUST LIKE LINE ITEMS, EXCEPT   *~
            * FOR THE INSTRUCTIONS AND PFKEY DEFINITIONS.               *~
            *************************************************************

            deffn'202(fieldnr%)
                  infomsg$ = " "
                  if oldcheckonfile% = 0 then L41080
                  infomsg$ = "(12)UN-VOID THIS CHECK NUMBER"
L41080:           init(hex(84)) linefac$()
                  on fieldnr% gosub  L41200         /* CHECK NUMBER     */
                  goto L41270

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linefac$(fieldnr%) = hex(80)
                      return
L41200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linefac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linefac$(fieldnr%) = hex(82)
                      return

L41270:     accept                                                       ~
               at (01,02),                                               ~
                  "VOID PAYABLES CHECKS",                                ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,59),                                               ~
                  "LAST CHECK:",                                         ~
               at (02,72), fac(hex(8c)), lastcheck$             , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "CHECK NUMBER",                                        ~
               at (06,30), fac(linefac$(1)), checknr$           , ch(08),~
               at (07,02),                                               ~
                  "CHECK DATE (USER)",                                   ~
               at (07,30), fac(hex(84)), checkdate$             , ch(08),~
               at (08,02),                                               ~
                  "DATE LAST MODIFIED",                                  ~
               at (08,30), fac(hex(84)), lastdate$              , ch(08),~
               at (09,02),                                               ~
                  "ORIGINALLY WRITTEN BY USER:",                         ~
               at (09,30), fac(hex(84)), origuserid$            , ch(03),~
               at (12,05), fac(hex(84)), message$(1)            , ch(79),~
               at (13,05), fac(hex(84)), message$(2)            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),     blankline$         , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,02), fac(hex(84)), infomsg$               , ch(30),~
               at (24,67),                                               ~
                  "(16)VOID CHECK",                                      ~
                                                                         ~
               keys(hex(00010c0d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L41760
                  call "MANUAL" ("CSHVOID ")
                  goto L41270

L41760:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            *                                                           *~
            * MAKES SURE THE VENDOR IS ON FILE, THAT THE CHECK ISN'T,   *~
            * (LOADS AND DROPS INTO EDIT MODE IF IT IS), AND CHECKS     *~
            * OUT THE OTHER NUMBERS TO MAKE SURE THEY'RE OK.            *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50200          /* CHECK NUMBER     */
                  return

L50200:     REM TEST FOR CHECK ON FILE, LOAD UP IF YES.
                blankline$, message$() = " "
                if checknr$ <> " " then L50250
                   errormsg$ = "BLANK CHECK NUMBER NOT ALLOWED."
                   return
L50250:         gosub check_on_file
                   if str(checknr$,1,1)="M" then return

                convert checknr$ to checknr%, data goto L50400
                convert checknr% to str(checknr$,1,8), pic (00000000)

                gosub check_on_file

                str (checknr$,1,1)="M"
                gosub check_on_file

                blankline$ = "To void this check number, press PF 16."

                return

L50400:            errormsg$="SORRY, INVALID CHECK NUMBER " & checknr$
                   return

        check_on_file

            gosub L30000
            if oldcheckonfile% = 0% then L50550
                   blankline$ = "To void this check number, press PF 16."
                   message$(1) = "This check number has already been void~
        ~ed."
                   message$(2) = "If you want to 'unvoid' it, press PF 12"
                   infomsg$ = "(12)UN-VOID THIS CHECK NUMBER"
                      return clear
                         return

L50550:     call "REDALT0" (#7, checknr$, 3%, f1%(7))
                if f1%(7) = 0% then return
            get #7, using L50580, othervendor$
L50580:         FMT CH(9)

                   errormsg$ = "CHECK NUMBER " & checknr$ &              ~
                                   " EXISTS FOR VENDOR: " & othervendor$
                   return clear
                   return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close #u3%
                next u3%

            end
