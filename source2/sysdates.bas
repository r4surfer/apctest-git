        REM *************************************************************~
            *                                                           *~
            *   SSS   Y   Y   SSS   DDDD    AAA   TTTTT  EEEEE   SSS    *~
            *  S      Y   Y  S      D   D  A   A    T    E      S       *~
            *   SSS    YYY    SSS   D   D  AAAAA    T    EEEE    SSS    *~
            *      S    Y        S  D   D  A   A    T    E          S   *~
            *   SSS     Y     SSS   DDDD   A   A    T    EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SYSDATES - INPUT ROUTINE TO ENTER OR CHANGE MODULE DATES. *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/02/80 ! ORIGINAL                                 ! BCW *~
            * 11/01/85 ! ADDED SFC DATE, FACE LIFT                ! HES *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 04/09/86 ! Fixed Several "Anomalities"              ! HES *~
            * 09/24/86 ! Removed SA Open Periods                  ! ERN *~
            * 11/30/92 ! Unix fix.  No hardcoded @SYSTEM@.        ! JDH *~
            * 06/26/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            adm$1,                       /* DATABASE/SYSTEM ADMINISTROR*/~
            askhdr$40,                   /* ASK USER TEXT              */~
            askpf1$80,                   /* ASK USER TEXT              */~
            askmid$80,                   /* ASK USER TEXT              */~
            askpf2$80,                   /* ASK USER TEXT              */~
            blankdate$8,                 /* Blank date for comparison  */~
            blankline$79,                /* FOR INPUT SCREEN.          */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* DATE INFORMATION FOR STUFF */~
            date$(10)10,                 /* DATE STUFF BEING INPUT.    */~
            dfac$1,                      /* FIELD ATTRIBUTE CHARACTERS */~
            errormsg$79,                 /* ERROR MESSAGE FOR TESTING  */~
            fac$(20)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* JUNK SCREEN IMAGE ("SCREEN"*/~
            inlib$8,                     /* CURRENT DATA BASE          */~
            newdate$10,                  /* CHANGE ALL TO DATE         */~
            gldates$(17)8,               /* GL date structure          */~
            glfdates$(32)8,              /* GL date structure          */~
            glmsg$79,                    /* GL open dates check        */~
            pfdescr$(3)79,               /* Description Of Keys Active */~
            pfkeys$32,                   /* P.F. Keys Active           */~
            temp1$10,                    /* WORK                       */~
            title$79,                    /* SCREEN HEADER              */~
            saperiods$(13)6,             /* SA Periods                 */~
            store$3,                     /* USERS DEFAULT STORE NUMBER */~
            storedescr$32,               /* STORE DESCRIPTION          */~
            syslib$8,                    /* System Library             */~
            sysvol$6,                    /* System Volume              */~
            user$3,                      /* USERID TO CHANGE DATES FOR */~
            userdescr$32,                /* USERID TO CHANGE DATES FOR */~
            userid$3                     /* USERID THIS USER.          */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                   S E L E C T   F I L E S                 *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! DESCRIPTION                              *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! INDIVIDUAL USER DEFAULT INFORMATION      *~
            * # 2 ! SYSFILE2 ! SYSTEM DEFAULT INFORMATION (OPEN MONTHS) *~
            * # 3 ! STORNAME ! Store Master File                        *~
            * # 4 ! USERLCMS ! Program Access Control User Info file    *~
            *************************************************************

            select #1, "USERINFO",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #2, "SYSFILE2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #3,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #4,  "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos =    1, keylen =   3,                     ~
                        alt key 1, keypos =  4, keylen = 30, dup

            call "SHOSTAT"  ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "EXTRACT" addr("XV", sysvol$, "XL", syslib$)
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%(4%))
                if f2%(4) <> 0% then L65000
            call "PUTNAMES" addr(#4, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#4, "SHARE", 0%, f2%(1))
        REM   OPEN NOGETPARM, #4, SHARED, FILE = "USERLCMS", LIBRARY =   ~
                   "@SYSTEM@", VOLUME = SYSVOL$

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SET USERID OF THIS USER, GET LIST OF VALID MONTHS OPEN,   *~
            * AND GET DATES FOR THIS USER.  IF NO DATES THIS USER, THEN *~
            * INPUT DATES, THUS CREATING THIS USER'S RECORD IN          *~
            * THE "USERINFO" FILE.                                      *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            newdate$ = date$

            blankdate$ = " "
	    call "DATUFMTC" (blankdate$)

            REM GET USER ID INFORMATION
                call "EXTRACT" addr  ("ID", userid$, "IL", inlib$)
                call "CMSDBADM" (adm$, " ")

            call "READ100" (#2, "FISCAL DATES", f1%(2))
            if f1%(2) = 0% then L09430
                get #2, using L09260, pp%, gldates$(), mo%, glfdates$()
L09260:              FMT XX(20), BI(2), 17*CH(8), BI(2), 32*CH(8)
                s% = max(mo%-1%,1%)
                if s% = 13% and pp% = 12% then s% = 12%
                e% = min(mo%+1%,17%)
                if e% = 13% and pp% = 12% then e% = 14%
                temp1$ = gldates$(s%)
                call "DATEFMT" (temp1$)
                call "DATEFMT" (glfdates$(e%+15%))
                put glmsg$, using L09350, temp1$, glfdates$(e%+15%)
L09350: %General Ledger Posting is allowed for ######## through ########

L09430:     title$ = "Your USERID: " & userid$
            call "EXTRACT" addr("NA", str(title$,18,24))
            str(title$, 62) = "Database: " & inlib$

            askhdr$ = "* * * WARNING * * *"
            askpf1$ = "DELETION OF THIS USER FROM THIS DATA BASE IS ABOUT~
        ~ TO OCCUR."
            askmid$ = "PRESS PF1 TO DELETE."
            askpf2$ = "USE CAUTION IF THIS USER IS CURRENTLY LOGGED ON TH~
        ~E SYSTEM."

        REM *************************************************************~
            *                   I N P U T   D A T E S                   *~
            *                                                           *~
            * GET DATES FOR THIS USER.                                  *~
            *************************************************************

        inputmode
            date$(), errormsg$, blankline$, store$, storedescr$,         ~
            userdescr$, newdate$ = " "
            if adm$ <> "Y" then user$ = userid$

            for fieldnr% = 1 to 8
                if adm$ <> "Y" and fieldnr% = 1 then L10210
L10130:         gosub'200(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  <> 2 then L10190
                         gosub allfive
                         if errormsg$=" " then editmode
                         goto L10130
L10190:               if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10130
L10210:         gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

L11000: REM *************************************************************~
            *               E D I T   M O D E   S T U F F               *~
            *                                                           *~
            * STANDARD LINEAR EDIT MODE STUFF FOR THE MODULE DATES.     *~
            *************************************************************

        editmode:
            blankline$ = "To Modify Displayed Values, Position Cursor To ~
        ~Desired Line And Press (ENTER)"
            gosub'201(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then goto  L11245
                  if keyhit%  = 12 then       datasave
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then L11000
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 2 or fieldnr% > 8 then L11000

L11180:     gosub'201(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'150(fieldnr%)
                  if errormsg$ <> " " then L11180
            goto  L11000

L11245:     fieldnr% = 9%
L11250:     gosub'201(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11250
            gosub allfive
                  if errormsg$ <> " " then L11250
            goto  L11000

        REM *************************************************************~
            *    S E T   A L L   D A T E S   T O   N E W   D A T E      *~
            *                                                           *~
            * SETS ALL THE DATES TO THE DATE INDICATED.                 *~
            *************************************************************

        allfive:
            errormsg$ = " "    /* FIRST CHECK IF MONTH IS OPEN */
            call "DATEOK" (newdate$, 0%, errormsg$)
            if errormsg$ <> " " then return
                temp1$ = newdate$
                call "DATUNFMT" (temp1$)
                call "WHICHPER" (#2, temp1$, err%)
                if err% <> 0% then L12160
                   errormsg$="Date is out of range for G/L Postings"
                   return
L12160:         call "SAPERIOD" (temp1$, #2, saperiods$(), u3%, errormsg$)
                if errormsg$ <> " " then return
                     for temp% = 1 to 10
                          date$(temp%) = newdate$
                     next temp%
                     return

        REM *************************************************************~
            *           W R I T E   D A T A   T O   F I L E             *~
            *                                                           *~
            * OVERWRITE USERINFO RECORD THIS USER WITH NEW INFORMATION. *~
            *************************************************************

        datasave

            call "READ101" (#1, user$, f1%(1))
                 if f1%(1) = 0 then L65000
            if keyhit% <> 12 then L19180

            keyhit1% = 0%
            call "ASKUSER" (keyhit1%, askhdr$, askpf1$, askmid$, askpf2$)
            if keyhit1% <> 1% then editmode
               delete #1
               goto L19190

L19180:     call "SHOSTAT" ("Update User Posting Dates & default Store")
            delete #1 : gosub L31000

L19190:     if adm$ <> "Y" then L65000
            goto inputmode

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE SAFER.          *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29920:     keyhit1% = 2%

            call "STARTOVR" (keyhit1%)

            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29920
               return clear
               startover% = 1%
               goto inputmode

L30000: REM *************************************************************~
            *      R E A D   O L D   D A T E S   F R O M   F I L E      *~
            *                                                           *~
            * READS OLD DATES FROM FILE AND FORMATS THEM FOR SCREEN     *~
            * DISPLAY.                                                  *~
            *************************************************************

            datesonfile% = 0 : errormsg$ = " "
            call "READ100" (#1, user$, f1%(1))
                 if f1%(1) = 0 then return
            get #1, using L30190, date$(), store$
            call "DESCRIBE" (#3, store$, storedescr$, 1%, f1%(3))
            datesonfile% = 1

            for temp% = 1 to 10
                if date$(temp%) <> " " and  ~
        	   date$(temp%) <> blankdate$ then call "DATEFMT" (date$(temp%))
                if temp% > 6% then L30160
                fieldnr% = temp% + 2%
                if errormsg$ = " " then gosub'150(fieldnr%)
L30160:     next temp%

            return

L30190:     FMT XX(3),                   /* SKIP USERID                */~
                10*CH(6),                /* DATES(UNFORMATTED)         */~
                CH(3)                    /* DEFAULT STORE NUMBER       */

L31000: REM *************************************************************~
            *         W R I T E S   D A T E S   T O   F I L E           *~
            *                                                           *~
            * DELETES OLD DATES, UPDATES FILE WITH NEW DATES.           *~
            *************************************************************

            for temp% = 1 to 10
                call "DATUNFMT" (date$(temp%))
            next temp%
            write #1, using L31120, user$, date$(), store$, " "
            return

L31120:     FMT CH(3), 10*CH(6), CH(3), CH(84)

        REM *************************************************************~
            *             I N P U T   M O D E   S C R E E N             *~
            *                                                           *~
            * GET MODULE DATES.                                         *~
            *************************************************************

            deffn'200(fieldnr%)
                  init(hex(84)) fac$()
                  pfdescr$(1) = "(1)Start Over                           ~
        ~                       (13)Instructions"
                  pfdescr$(2) = "(2)Set All Equal To XXXXXXXX            ~
        ~                       (15)Print Screen"
                  pfdescr$(3) = "                                        ~
        ~                       (16)Exit Program"
                  str(pfdescr$(3),63,1) = hex(84)
                  pfkeys$ = hex(0001020d0f10)
                  if fieldnr% > 2 then L40220
                     str(pfdescr$(2),,30)   = " "
                     str(pfkeys$,3,1) = hex(ff)
                     dfac$ = hex(9c)
                  if fieldnr% <> 1 then L40450
                     str(pfdescr$(1),,13) = " "
                     str(pfkeys$,2,1) = hex(ff)
                     goto L40450
L40220:           str(pfdescr$(3),63) = " "
                  str(pfkeys$,6,1) = hex(ff)
                  goto L40450

            deffn'201(fieldnr%)
                  pfdescr$(1) = "(1)Start Over            (12)Delete User~
        ~                       (13)Instructions"
                  init(hex(84)) fac$()
                  if fieldnr% = 0 or fieldnr% = 9% then L40370
                  str(pfdescr$(1),26,16) = " "
                  pfdescr$(2) = "(RETURN)Continue                        ~
        ~                       (15)Print Screen"
                  pfdescr$(3) = " "
                  pfkeys$ = hex(00010d0f)
                  dfac$ = hex(9c)
                  goto L40450

L40370: REM       INIT(HEX(86)) FAC$()
                  dfac$ =hex(9c)
                  pfdescr$(2) = "(2)Set All Equal To XXXXXXXX            ~
        ~                       (15)Print Screen"
                  pfdescr$(3) = "                                        ~
        ~                       (16)Save Dates  "
                  str(pfdescr$(3),63,1) = hex(84)
                  pfkeys$ = hex(0001020c0d0f10)
                  if fieldnr% = 0% then L40420
                  dfac$ =hex(81)
                  pfdescr$(2) = "(2)Set All Equal To XXXXXXXX            ~
        ~                       (15)Print Screen"
                  str(pfdescr$(1),26,16), pfdescr$(3) = " "
                  pfkeys$ = hex(00010d0f)
L40420:           if adm$ <> "Y" then str(pfdescr$(1),26,16) = " "
                  if adm$ <> "Y" then str(pfkeys$,4,1) = " "

L40450:           on fieldnr% gosub L40580,         /* USER TO MANAGE   */~
                                    L40580,         /* DEFAULT STORE    */~
                                    L40580,         /* RECEIVABLES DATE */~
                                    L40580,         /* PAYABLES DATE    */~
                                    L40580,         /* PAYROLL DATE     */~
                                    L40580,         /* G/L DATE         */~
                                    L40580,         /* INVENTORY DATE   */~
                                    L40580          /* SHOP FLOOR DATE  */
                  goto L40650

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(fieldnr%) = hex(80)
                      return
L40580:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(fieldnr%) = hex(82)
                      return

L40650:     accept                                                       ~
               at (01,02), "Manage user posting dates by module",        ~
               at (01,60), "Todays Date:", fac(hex(8c)), date$  , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "USER TO MANAGE",                             ~
               at (06,30), fac(fac$(1)), user$                  , ch(03),~
               at (06,39), fac(hex(8c)), userdescr$             , ch(32),~
                                                                         ~
               at (07,02), "DEFAULT STORE CODE",                         ~
               at (07,30), fac(fac$(2)), store$                 , ch(03),~
               at (07,39), fac(hex(8c)), storedescr$            , ch(32),~
                                                                         ~
               at (08,02), "RECEIVABLES DATE",                           ~
               at (08,30), fac(fac$(3)), date$(1)               , ch(08),~
                                                                         ~
               at (09,02), "PAYABLES DATE",                              ~
               at (09,30), fac(fac$(4)), date$(2)               , ch(08),~
                                                                         ~
               at (10,02), "PAYROLL DATE",                               ~
               at (10,30), fac(fac$(5)), date$(3)               , ch(08),~
                                                                         ~
               at (11,02), "GENERAL LEDGER DATE",                        ~
               at (11,30), fac(fac$(6)), date$(4)               , ch(08),~
                                                                         ~
               at (12,02), "INVENTORY DATE",                             ~
               at (12,30), fac(fac$(7)), date$(5)               , ch(08),~
                                                                         ~
               at (13,02), "SHOP FLOOR CONTROL DATE",                    ~
               at (13,30), fac(fac$(8)), date$(6)               , ch(08),~
                                                                         ~
               at (15,05),                                               ~
                  "To Select Today's System Date, enter a blank for the s~
        ~pecific module.",                                                ~
               at (16,05),                                               ~
                  "To Select a specific date for all the module dates, us~
        ~e PF key 2.",                                                    ~
               at (18,05), "Note:",                                      ~
               at (19,10), fac(hex(8c)), glmsg$                 , ch(69),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
               at (23,22), fac(dfac$),   newdate$               , ch(08),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

            if keyhit% <> 13 then L41190
                call "MANUAL" ("SYSDATES")
                goto L40650

L41190:        if keyhit% <> 15 then L41230
                  call "PRNTSCRN"
                  return

L41230:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TEST DATES ENTERED FOR CORRECTNESS.  USES "DATEOK" SUB-   *~
            * ROUTINE.  ALSO CHECKS THAT THE MONTHS ENTERED ARE ACTUALLY*~
            * OPEN BY SEARCHING "MONTHS$()".                            *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* USER TO MANAGE   */~
                                    L50490,         /* DEFAULT STORE    */~
                                    L50570,         /* RECEIVABLES DATE */~
                                    L50570,         /* PAYABLES DATE    */~
                                    L50570,         /* PAYROLL DATE     */~
                                    L50570,         /* G/L DATE         */~
                                    L50570,         /* INVENTORY DATE   */~
                                    L50570          /* SHOP FLOOR DATE  */
                  return

L50200:     REM NOW TRY AND GET CURRENT INFORMATION.
                REM Note- STARTOVER% will always be zero first time
                REM through here if your not a Sec. Adm.
                REM This ensures that if your not on file, out you go
                if adm$ <> "Y" then L50290
                call "GETCODE" (#4, user$, userdescr$, 1%, 1.3, f1%(4))
                     if f1%(4) <> 0 then L50330
                     errormsg$ = "Unknown Userid."
                     return
L50290:         call "DESCRIBE" (#4, user$, userdescr$, 1%, f1%(4))
                     if f1%(4) = 0 then L65000
                if adm$ <> "Y" and startover% = 1 then return

L50330:         gosub L30000
                if datesonfile% = 1 then L50390
                     errormsg$ = "User Is Not In Userlist For This Data B~
        ~ase."
                     if adm$ <> "Y" then L65000
                     return
L50390:         if errormsg$ = " " then L50440
                  errormsg$ = "At least one date is out of range or inval~
        ~id."
                  if f1%(3) = 0 then fieldnr% = 2% else fieldnr% = 3%
                  return
L50440:         return clear all
                if f1%(3) <> 0 then editmode
                   fieldnr% = 2%
                   goto L11180

L50490:     REM TEST USERS DEFAULT STORE NUMBER.
                storedescr$ = " "
                if store$ = " " then return
                call "GETCODE" (#3, store$, storedescr$, 1%, 0, f1%(3))
                     if f1%(3) <> 0 then return
                     errormsg$ = "Invalid Entry For Default Store Code."
                     return

L50570:     REM TEST A DATE TO MAKE SURE IT'S OK.
                temp1% = fieldnr% - 2
                if date$(temp1%) = " " or ~
		   date$(temp1%) = blankdate$ then date$(temp1%) = date$
                call "DATEOKC" (date$(temp1%), u3%, errormsg$)
                if errormsg$ <> " " then return    /* IF ALREADY AN ERR*/

                temp1$ = date$(temp1%)
                call "DATUFMTC" (temp1$)
		date$(temp1%) = temp1$
		call "DATEFMT" (date$(temp1%))

                call "WHICHPER" (#2, temp1$, u3%)
                if u3% <> 0% then L50690
                     errormsg$ = "Date is out of range for G/L Posting"
                     return
L50690:         call "SAPERIOD" (temp1$, #2, saperiods$(), u3%, errormsg$)
                return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
            call "SHOSTAT" ("One Moment Please")
        end
