        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP   IIIII   CCC   K   K   CCC   RRRR    *~
            *    J    B   B  P   P    I    C   C  K  K   C      R   R   *~
            *    J    BBBB   PPPP     I    C      KKK    C      RRRR    *~
            *  J J    B   B  P        I    C   C  K  K   C      R   R   *~
            *   J     BBBB   P      IIIII   CCC   K   K   CCC   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPICKCR - Call subroutine. Called By JBPICKPR   and      *~
            *            JBMANAG2.  Calls JBPICKSL for Picklits -       *~
            *            JBPICKBP for By-Products Lists - JBTRAVEL for  *~
            *            Travelers and BOMSLSUB for single level parts  *~
            *            list.                                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/07/83 ! ORIGINAL                                 ! KEN *~
            * 07/08/83 ! PRINT PICK SLIPS FOR PROJECTS & JOBS     ! JRW *~
            * 08/22/84 ! PRINT TRAVEL ALONE OR W/PICK SLIP        ! DSH *~
            * 10/01/86 ! PRINT By-Products List                   ! HDC *~
            * 04/28/87 ! PRINT Single Level Parts List            ! MJB *~
            * 02/12/88 ! Don't allow blank range on job fields    ! BRD *~
            * 02/18/88 ! Retain Selections off screen, Job search ! HES *~
            * 03/09/88 ! Changed parameters sent to BOMSLSUB      ! RJM *~
            * 03/09/88 ! Now will not print closed jobs,          ! RJM *~
            *          ! & tells user if all jobs were closed.    !     *~
            * 08/05/88 ! Fixed bug, displayed all jobs closed     ! TLJ *~
            *          ! nothing was printed when open & printed. !     *~
            * 01/03/89 ! Removed Projects Option from selection   ! MJB *~
            *          !  and JOBMASTR from calling argument list.!     *~
            * 04/20/89 ! Added new Arg to BOMSLSUB Call (part text! RJM *~
            * 12/21/93 ! Honors SFC defaults for printing.        ! JDH *~
            * 07/13/94 ! Print Vendor Service Sheets.             ! RJH *~
            * 07/17/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBPICKCR" (#1,              /* SYSFILE2  File UFB         */~
                        #2,              /* BOMMASTR  File UFB         */~
                        #3,              /* PIPOUT    File UFB         */~
                        #4,              /* WCOUT     File UFB         */~
                        #5,              /* HNYMASTR  File UFB         */~
                        #6,              /* JBMASTR2  File UFB         */~
                        #8,              /* RTEMASTR  File UFB         */~
                        #9,              /* WCMASTR   File UFB         */~
                        #10,             /* JBCROSS2  File UFB         */~
                        #11,             /* HNYQUAN   File UFB         */~
                        #12,             /* ENGMASTR  File UFB         */~
                        #13)             /* CALMASTR  File UFB         */

        dim                                                              ~
            advice$8,                    /* Vendor Service Advise No.  */~
            beg_date$8,                  /* Disp begin of cal yymmdd(1)*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* BOMID for assembly         */~
            clsdate$6,                   /* DATE JOB WAS CLOSED        */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            end_date$8,                  /* Disp end of call yymmdd(2) */~
            enddate$8,                   /* DATE FOR SCREEN DISPLAY    */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            exline$2,                    /* EXTRA LINES (0,1,2)        */~
            firstjob$8,                  /* FIRST JOB NUMBER           */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jobcode$8,                   /* JOB NUMBER                 */~
            lastjob$8,                   /* LAST  JOB NUMBER           */~
            line2$79,                    /* Screen Line                */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            part$25,                     /* PART CODE                  */~
            pfac$1,                      /* Prompt fac for traveler    */~
            pf4$17,                      /* Prompt pf key 4            */~
            pf14$31,                     /* Prompt pf key 4            */~
            plowkey$60,                  /* Plowing key                */~
            plowkey1$60,                 /* Plowing key again          */~
            plowkey2$60,                 /* Plowing key again & again  */~
            startdate$6,                 /* Job Start Date             */~
            temp$8,                      /* Temp. Date String          */~
            traveler$(5)1,               /* Trav/Pick/By-Prod/SL Parts */~
            yymmdd$(490)6                /* Field attribute characters */~

        dim f1%(64)                      /* Record-on-file flags       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #14 ! VBKVSA   ! Vendor Service Advices file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #14, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  6, keypos =   50, keylen =   4, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  12          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#14, fs%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            call "READ100" (#13, "10", f1%(13))
                if f1%(13) <> 1 then goto L65000
            get #13, using L09150 , str(yymmdd$(),1,1470)
L09150:         FMT XX(2), CH(1470)

            call "READ100" (#13, "11", f1%(13))
            get #13, using L09150 , str(yymmdd$(),1471,1470)

            str(line2$,62) = "JBPICKCR: " & str(cms2v$,,8%)
            select printer(134)
            exline$="0"
            enddate$ = date$
            init("X") traveler$()

*        Get defaults for printing
            plowkey$ = "SWITCHS.SFC"
            call "READ100" (#01, plowkey$, f1%(1%))
            if f1%(1%) = 0% then L10000
                get #01 using L09300, traveler$(1%), traveler$(2%),        ~
                                    traveler$(3%), traveler$(4%),        ~
                                    traveler$(5%)
L09300:             FMT POS(127), 5*CH(1)
                tran(traveler$(), hex(204e5859))replacing

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            errormsg$, inpmessage$, lastjob$ = " "
            firstjob$ = "ALL"
            close printer

            for fieldnr% = 1 to  3
L10110:         gosub L20000
                      if enabled% = 0 then L10280
L10130:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10200
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub L20000
                         if fieldnr% < 2% or enabled% > 0% then L10210
                         goto L10160
L10200:               if keyhit%  = 16 and fieldnr% = 1 then L65000
L10210:               if keyhit% <>  0 then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                      if fieldnr% <> 3% then L10280
                      fieldnr% = 4%
                      pfac$ = hex(8d)
                      goto L10110
L10280:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

L11060:     inpmessage$ = edtmessage$
            gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 5 then L11060
            if fieldnr% > 1 then fieldnr% = fieldnr% - 1%
            gosub L20000
L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
           printed% = 0%
           if firstjob$="ALL" then init(hex(ff)) lastjob$
           if firstjob$="ALL" then init(hex(00)) firstjob$
           call "SHOSTAT" ("Printing Requested Document(s)")

            call "READ100" (#6, firstjob$, f1%(6))
                if f1%(6) = 1% then L19200
            init(hex(00)) plowkey$
                str(plowkey$,,8) = firstjob$
L19180:     call "PLOWNEXT" (#6, plowkey$, 0%, f1%(6))
                if f1%(6) <> 1% then L19800
L19200:     get #6, using L19210, jobcode$, part$, quantity, clsdate$,    ~
                                 startdate$
L19210:         FMT CH(8), XX(49), CH(25), PD(14,4), POS(153), CH(6),    ~
                    POS(168), CH(6)
            if clsdate$ <> " " and clsdate$ <> blankdate$ then L19180

            init(hex(00)) plowkey$
                str(plowkey$,,8) = jobcode$
            if jobcode$ > lastjob$ then L19800
            init(hex(00)) plowkey1$
            bomid$ = " "
            str(plowkey1$,1) = "JOB ORDER: " & jobcode$
            call "READ100" (#10, plowkey1$, f1%(10))
                if f1%(10) = 0% then L19305
                get #10 using L19300, bomid$
L19300:             FMT POS(73), CH(3)
L19305:     printed% = 1%
            if traveler$(2) = " " then L19340
                call "JBPICKSL" (jobcode$, jobcode$, #6, #3, #4, #5, #11,~
                                 #1, exline%, yymmdd$(1), cutoff%)
L19340:     if traveler$(3) = " " then L19370
                call "JBPICKBP" (jobcode$, jobcode$, #6, #3, #4, #5, #11,~
                                 #1, exline%, yymmdd$(1), cutoff%)
L19370:     if traveler$(1) = " " then L19400
                call "JBTRAVEL" (part$, jobcode$, quantity, #8, #9, #4,  ~
                                 #10, #5, #2, #12, #1, #6)
L19400:     if traveler$(4) = " " then L19430
                                /* NOTE: Arguments 8 & 9 are dummy */
                call "BOMSLSUB" (part$, bomid$, "N", "N", "N", "N", #2,  ~
                                 #5, #1, #1, #12, #1, " ", startdate$,   ~
                                 " ", jobcode$)
L19430:     if traveler$(5%) = " " or fs% <>  1%  then L19500
                plowkey2$ = jobcode$
L19440:         call "PLOWALTS" (#14, plowkey2$, 3%, 8%, f1%(14%))
                if f1%(14%) = 0% then L19500
                get #14 using L19455, advice$
L19455:      FMT POS(5), CH(8)
                if advice$ = " "  then L19480
               /* We'll print the Sheet w/o any shipper info */
                call "VSASHTSB" (advice$, " ",  0 , " ", " ", " ")
L19480:         goto L19440    /* Loop for more steps into Vendor WC */



L19500:     goto L19180

L19800: REM CHECK & SEE IF ANYTHING WAS PRINTED
            if printed% = 1% then L10000
            call "ASKUSER" (keyhit%, "Job Pick List Report",             ~
                                     "Nothing was Printed",              ~
                                     "The Jobs selected were",           ~
                                     "probably Closed.")
            goto L10000

L20000: REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L20160,         /* JOB NUMBERS      */~
                                    L20250,         /* EXTRA LINES      */~
                                    L20350,         /* END DATE         */~
                                    L20400          /* TRAVELER         */
                     return
L20160:     REM DEFAULT/ENABLE FOR JOB NUMBERS
                inpmessage$ = "Enter First & Last Job numbers, or 'ALL'"
                enabled%=1
                return

L20250:     REM DEFAULT/ENABLE FOR EXTRA LINES (0,1,2)
                inpmessage$ = "Enter Number of Extra Lines per Part " &  ~
                              "for Pick List Printing"
                enabled%=1
                return

L20350:    REM DEFAULT/ENABLE FOR CUT OFF DATE
               inpmessage$ = "Materials Required After This Date Will" & ~
                             " *NOT* Be Printed On Pick List."
               enabled% = 1%
               return

L20400:    REM DEFAULT/ENABLE FOR TRAVELER
               inpmessage$ = "'X' Indicates Print.  Blank To Omit"
               enabled% = 1%
               return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: /* Allow user opportunity to start over.            */
            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% <> 0% then return
            return clear
            goto inputmode

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                pf4$ = "(4)Previous Field"
                pf14$ = "(14)Search For Job Number"
                pf16$ = "(16)Exit Program"
                if fieldnr% > 1 then pf16$, pf14$ = " "  else pf4$ = " "
                goto L40160

            deffn'111(fieldnr%)
                pf4$ = " "
                pf16$ = "(16)Print Report"

L40160:           init(hex(84)) lfac$()
                  on fieldnr% gosub L40280,         /* JOB NUMBERS      */~
                                    L40310,         /* EXTRA LINES      */~
                                    L40280,         /* END DATE         */~
                                    L40280          /* TRAVELER         */
                     goto L40350

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40280:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40310:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40350:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Selected Job Related Documents",                ~
               at (01,66),                                               ~
                  "Today:",                                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Job Number",                                    ~
               at (06,30), fac(lfac$( 1)), firstjob$            , ch(08),~
               at (07,02),                                               ~
                  "Last Job Number",                                     ~
               at (07,30), fac(lfac$( 1)), lastjob$             , ch(08),~
               at (08,02),                                               ~
                  "Extra Lines (0-99)",                                  ~
               at (08,30), fac(lfac$( 2)), exline$              , ch(02),~
               at (09,02),                                               ~
                  "Cut-Off Date",                                        ~
               at (09,30), fac(lfac$(3)), enddate$              , ch(08),~
               at (10,02), fac(lfac$(4)), traveler$(1)          , ch(01),~
               at (10,04),                                               ~
                  "Traveler",                                            ~
               at (10,16), fac(lfac$(4)), traveler$(2)          , ch(01),~
               at (10,18),                                               ~
                  "Pick List",                                           ~
               at (10,31), fac(lfac$(4)), traveler$(3)          , ch(01),~
               at (10,33),                                               ~
                  "By-Products List",                                    ~
               at (10,53), fac(lfac$(4)), traveler$(4)          , ch(01),~
               at (10,55), "Single Level Parts List",                    ~
               at (11,02), fac(lfac$(4)), traveler$(5%)         , ch(01),~
               at (11,04),                                               ~
                  "Vendor Service Sheet",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,18), fac(hex(8c)), pf4$                   , ch(17),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,18), fac(hex(8c)), pf14$                  , ch(31),~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(0001040d0e0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40842
                  call "MANUAL" ("JBPICKPR")
                  goto L40350

L40842:        if keyhit% <> 14 then L40850
                  if fieldnr% <> 1% then L40350
                  call "GETCODE" (#6, temp$, " ", 0%, 0, f1%(6))
                     if f1%(6) <> 0% then firstjob$, lastjob$ = temp$
                  goto L40350

L40850:        if keyhit% <> 15 then L40890
                  call "PRNTSCRN"
                  goto L40350

L40890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* JOB NUMBERS      */~
                                    L50350,         /* EXTRA LINES      */~
                                    L50460,         /* END DATE         */~
                                    L50570          /* TRAVELER         */
                     return
L50140:     REM TEST DATA FOR JOB NUMBERS
                     if firstjob$<>"ALL" then L50180
                     lastjob$=" "
                     return
L50180:          if firstjob$ = " " and lastjob$ = " " then L50300
                 if lastjob$=" " then lastjob$=firstjob$
                 if firstjob$ > lastjob$ then L50300
                 if firstjob$ <> lastjob$ then return
                 call "READ100" (#6, firstjob$, f1%(6))
                     if f1%(6) = 0% then L50320
                 get #6, using L50250, temp$
L50250:          FMT POS(153), CH(6)
                 if temp$ = " " or temp$ = blankdate$ then return
                 call "DATEFMT" (temp$)
                 errormsg$ = "Job Number Closed On " & temp$
                 return
L50300:     errormsg$="Invalid Job Range, Please Re-Enter"
            return
L50320:     errormsg$ = "Job Number Not On File"
            return

L50350:     REM TEST DATA FOR EXTRA LINES (0,1,2)
                call "NUMTEST" (exline$, 0, 99, errormsg$, 0.0, 0)
                     if errormsg$ <> " " then return
                convert exline$ to exline%
                return

L50460: REM TEST DATA FOR END DATE (CUT OFF DATE)
               call "DATEOK" (enddate$, u3%, errormsg$)
               if errormsg$ <> " " then return
               call "DATUNFMT" (enddate$)
               search yymmdd$() = str(enddate$,,6) to cursor%() step 6
               call "DATEFMT" (enddate$)
               beg_date$ = yymmdd$(1)
               end_date$ = yymmdd$(490)
               call "DATEFMT" (beg_date$)
               call "DATEFMT" (end_date$)
               if cursor%(1) = 0 then errormsg$ = "Date Must be Between "~
                     & beg_date$ & " and " & end_date$
               cutoff% = (cursor%(1) + 5)/6
               return

L50570:     REM TEST DATA FOR TRAVELER, PICKSLIP OR BOTH
               if traveler$() <> " " then return
               errormsg$ = "You Must Choose One or More Options"
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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
