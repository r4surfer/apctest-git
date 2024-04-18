        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    AAA   L      IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C   C  A   A  L        I    NN  N  P   P  U   U    T     *~
            *  C      AAAAA  L        I    N N N  PPPP   U   U    T     *~
            *  C   C  A   A  L        I    N  NN  P      U   U    T     *~
            *   CCC   A   A  LLLLL  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CALINPUT - ALLOWS USER TO REVIEW OR GENERATE THE PROD.    *~
            *            PLANNING CALENDAR.  IF A NEW CALENDAR IS       *~
            *            'POSTED', A BUNCH OF FILES NEED TO BE ADJSTED. *~
            *            NEW CALENDAR STARTS WITH THE FIRST DAY OF THE  *~
            *            CURRENT MONTH AND GO OUT 490 DAYS.             *~
            *            PROGRAM GENERATES-                             *~
            *              yymmdd$(490)                                 *~
            *              YY%(490)                                     *~
            *              MM%(490)                                     *~
            *              DD%(490)                                     *~
            *              DAY OF WEEK NAME$(490) MON, TUE, WED, ETC.   *~
            *              WEEK OF YEAR%(490) START W/ FIRST MONDAY     *~
            *              WEEK OF YEAR%(490) START W/ FIRST DAY        *~
            *              CAL QUARTER OF YEAR%(490)                    *~
            *              FISCAL QUART OF YEAR%(490)                   *~
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
            * 12/07/82 ! ORIGINAL                                 ! GLW *~
            * 08/16/83 ! MODIFIED TO REWRITE ALL ASSOCIATED FILES ! HES *~
            *          ! BY INITIATING PROG 'CALCHNGE', WHICH DOES!     *~
            *          ! ALL THE DIRTY WORK.                      !     *~
            * 10/20/86 ! WCOUT File format change                 ! HES *~
            * 12/02/87 ! Don't let PIPIN start dates stop roll if ! HES *~
            *          ! PIPIN is 'released'                      ! HES *~
            * 08/23/88 ! Stopped skip of 1st Recs in files @52800 ! RJM *~
            * 09/14/88 ! Stopped skip of 1st Recs RIGHT THIS TIME ! RJM *~
            * 06/15/89 ! Added SEVERE WARNING regarding Exclusive ! MJB *~
            *          !  usage of the database during execution  !     *~
            *          !  of PROCCAL!                             !     *~
            *          ! Also made it Mandatory that the user be  !     *~
            *          !  a Database Administrator to run proc.   !     *~
            * 04/08/91 ! (PRR 11735) Corr. Spelling of Calendar & ! RJB *~
            *          !      added call to ALLFREE 'new standard'!     *~
            * 06/17/91 ! QC - Moved ALLFREE to the right palce.   ! RJB *~
            * 06/21/91 ! QC-FIXES One more time on 'ALLFREE'      ! RJB *~
            * 10/11/93 ! Purchased Job 'BW's treated same as 'WO's! MLJ *~
            * 11/09/93 ! Purchased Job 'RW's treated same as 'RO's! JBK *~
            *          !  'BO', 'BW', 'RO', 'RW' & 'WO' treated as!     *~
            *          !  unreleased.                             !     *~
            * 06/07/94 ! PRR 11849 - Added additional screen to   ! JBK *~
            *          !  report more information about what      !     *~
            *          !  might cause failure of new calendar.    !     *~
            *          ! Miscellaneous - Added lots of '%' signs. !     *~
            * 10/11/95 ! Corrected counting of problems and       ! JDH *~
            *          !  setting of earliest problem date.       !     *~
            * 09/17/96 ! Millie date conversion                   ! DER *~
            * 08/06/97 ! Changes for the year 2000.               ! DXL *~
            * 08/11/97 ! Added View Problems (calls PLOWCODE)     ! DXL *~
            * 12/02/97 ! Fixed testing of earliest problem date.  ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            fm$2,                                                        ~
            fmn$10,                                                      ~
            yymmdd$(490)6,                                               ~
            ccyymmdd$(490)10,                                            ~
            dow$(490)3,                                                  ~
            yy%(490),                                                    ~
            mm%(490),                                                    ~
            dd%(490),                                                    ~
            mwoy%(490),                                                  ~
            fwoy%(490),                                                  ~
            cqoy%(490),                                                  ~
            fqoy%(490),                                                  ~
            tgt$8,                                                       ~
            yr$4,                                                        ~
            blank$79,                                                    ~
            key$50,                                                      ~
            ok$3,                                                        ~
            t$10,                                                        ~
            o$10,                                                        ~
            problems%(3,3),                                              ~
            problems$(3,3)8,                                             ~
            fmon$(100)8,                                                 ~
            search%(2),                                                  ~
            hdate$45
        dim                                                              ~
            adm$1,                       /* DB Administrator Flag      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filestart$(3)10,             /* Start Dates for PIP Files  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* SCREEN LINE #2             */~
            oldstart$10,                 /* OLD PLAN PERIOD START DATE */~
            oldstartu$10,                /* Old Plan Period Start Date */~
            oldend$10,                   /* OLD PLAN PERIOD END DATE   */~
            newstart$10,                 /* NEW PLAN PERIOD START DATE */~
            newstartdescr$10,            /* NEW PLAN PERIOD START DATE */~
            newend$10,                   /* NEW PLAN PERIOD END DATE   */~
            newenddescr$10,              /* NEW PLAN PERIOD END DATE   */~
            lowest_possible$10,          /* BASED ON CURRENT ACTIVITY  */~
            oldleft$3,                   /* NUM DAYS LEFT IN PLANPERIOD*/~
            problem_date$8,              /* PIP problem event date     */~
            problem_descr$27,            /* PIP problem event descript.*/~
            problem_file$6,              /* PIPIN, PIPOUT, or WCOUT    */~
            problem_tag$19,              /* PIP problem event tag #    */~
            results$(3,2)50,             /* Results of check of files  */~
            rfac$(3,2)1,                 /* Screen FACs                */~
            target$10,                   /* Target Date                */~
            tempdate$10,                 /* Temp Date for display      */~
            newleft$3,                   /* NUM DAYS LEFT IN PLANPERIOD*/~
            warn$43                      /* Screen Warning Header      */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

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
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE FOR FISCAL YEAR  *~
            * # 5 ! PIPIN    ! Planned inventory additions detail  feed *~
            * # 6 ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            * # 8 ! WCOUT    ! Planned work center use detail rec  feed *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos =   1, keylen =  20

            select #05, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #06, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #08, "WCOUT",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   68,                                  ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =   1, keylen =  27

           select #12, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

           select #20, "WORKFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 63,                                    ~
                        keypos = 1, keylen = 63

        REM 1st check to see if user is a database administrator
            call "CMSDBADM" (adm$, " ")
            if adm$ = "Y" then L03200
L03100:         ask% = 2%
                call "ASKUSER" (ask%, "***** DB ADMINISTRATOR *****",    ~
                     "You Must be a DATABASE ADMINISTRATOR to run this" &~
                     " Procedure", "You are NOT a DBA!", "Press PF-16" & ~
                     " to terminate this Procedure.")
                if ask% <> 16% then L03100
                return_code% = ask%
                goto L65000

        REM Now display a scary message to warn off the cowardly
L03200:     warn$ = "*****  W A R N I N G - W A R N I N G  *****"

L03210:     accept                                                       ~
            at(03,20), fac(hex(a4)), warn$,                       ch(43),~
            at(05,05), "To ROLL the Planning Calendar, You MUST Have EXCL~
        ~USIVE use of the DATABASE.",                                     ~
            at(07,05), "You should Ensure that ALL users are OFF the Syste~
        ~m for This Procedure.",                                            ~
            at(09,05), "If you continue WITHOUT EXCLUSIVE use of the Data~
        ~base, DAMAGE MAY OCCUR,",                                        ~
            at(10,05), " and your ONLY Recourse will be to RESTORE from Y~
        ~our BACKUP!",                                                    ~
            at(13,05), "If your Database is Backed up and you are ready t~
        ~o Roll the Calendar,",                                           ~
            at(14,05), " Press RETURN to CONTINUE.",                     ~
                                                                         ~
            at(16,05), "If NOT, Press PF-16 to EXIT this Procedure.",    ~
            at(20,20), "(RETURN) - CONTINUE with CALENDAR ROLL",         ~
            at(22,20), " (PF-16) - EXIT Back to Menu",                   ~
                                                                         ~
            keys(hex(0010)), key(keyhit%)

            if keyhit% = 0% then L07000
                if keyhit% <> 16% then L03210
                    return_code% = 16%
                    goto L65000

L07000:     select pool #5, #6, #8, #1, #12, blocks = 60

            call "SHOSTAT" ("Opening Files, One Moment Please.")

            call "OPENFILE" (#1, "SHARE", f2%(1%), rslt$(1%), axd$(1%))
                if f2%(1%) <> 0 then                                     ~
                  call "OPENFILE" (#1,"OUTPT",f2%(1%),rslt$(1%),axd$(1%))
            close #1
            call "OPENFILE" (#1, "IO", f2%(1%), rslt$(1%), axd$(1%))

            call "OPENFILE" (#5, "SHARE", f2%(5%), rslt$(5%), axd$(5%))
                if f2%(5%) <> 0 then L07140
                    close #5  :  f2%(5%) = 1%
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$(5%))

L07140:     call "OPENFILE" (#6, "SHARE", f2%(6%), rslt$(6%), axd$(6%))
                if f2%(6%) <> 0% then L07180
                    close #6  :  f2%(6%) = 1%
            call "OPENFILE" (#6, "IO   ", f2%(6%), rslt$(6%), axd$(6%))

L07180:     call "OPENFILE" (#8, "SHARE", f2%(8%), rslt$(8%), axd$(8%))
                if f2%(8%) <> 0% then L07230
                    close #8  :  f2%(8%) = 1%
            call "OPENFILE" (#8, "IO   ", f2%(8%), rslt$(8%), axd$(8%))

L07230:     call "OPENFILE" (#12, "SHARE", f2%(12%),rslt$(12%),axd$(12%))
                if f2%(12%) <> 0% then                                   ~
                call "OPENFILE" (#12,"OUTPT",f2%(12),rslt$(12),axd$(12))
            close #12
            call "OPENFILE" (#12, "IO   ", f2%(12%),rslt$(12%),axd$(12%))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "ALLFREE"
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            call "READ101" (#1, "FISCAL YEAR BEGINS  ", f1%(1%))
                 if f1%(1%) = 1% then goto L09180
                fm$ = "12"
                fmn$ = "UNKNOWN "
                goto L09210

L09180:     get #1, using L09190, fm$, fmn$
L09190:             FMT XX(20), CH(02), CH(10)

L09210:     init(" ") errormsg$, inpmessage$, yymmdd$(), ccyymmdd$(), dow$(), ~
              newstart$, newstartdescr$, newend$, newenddescr$, newleft$,~
                             tgt$, yr$, t$, o$, target$

            mat yy% = zer          : mat cqoy% = zer
            mat mm% = zer          : mat fqoy% = zer
            mat dd% = zer
            mat mwoy% = zer
            mat fwoy% = zer
            return_code% = 0%

            gosub L30000                  /* LOAD EXISTING CALENDAR */

            str(line2$,62%) = "CALINPUT: " & str(cms2v$,1%,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

            for fieldnr% = 1% to  1%
                gosub'051 (fieldnr%)
                      if enabled% = 0% then L10230
L10170:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  9% then gosub printcalendar
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 25% then L10210
                      if keyhit% <>  0% then       L10170
L10210:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10230:         next fieldnr%
                edtmessage$ = inpmessage$

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************
        editmode
L11060:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then       datasave
                  if keyhit%  = 16% then       L65000
                  if keyhit% <>  0% then       L11060
            fieldnr% = cursor%(1%) - 13%
            if fieldnr% < 1% or fieldnr% >  1% then L11060

L11150:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11060


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * IF THE OLD CALENDAR IS OK THE EXIT, OTHERWISE CALC NEW ONE*~
            *************************************************************

        datasave
            if oldstart$ = " " then L19080
            if oldstart$ = blankdate$ then L19080
            gosub caution
            if keyhit% <> 32% then editmode
L19080:     return_code% = hit%  /* The return code will tell the proc */
            goto L65000           /* 1 if there is no old, 2 if there is*/
                                 /* an existing calendar in use.       */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L20110          /* NEW ST DATE      */
                  return

L20110:     REM DEFAULT/ENABLE FOR PLAN PERIOD START DATE
                enabled%     = 1%
                if newstart$ <> " " and newstart$ <> blankdate$ then return
                if oldstart$ <> " " and oldstart$ <> blankdate$ then return
                call "DATFMTC" (date, 0%, newstart$)
                str(newstart$,7%,2%) = "01"
                call "DATFMTC" (newstart$)
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

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto L09000

L30000: rem**************************************************************~
            *      l o a d   o l d   calmastr   d a t a                 *~
            *************************************************************

            hit% = 2%
            newstart$, newend$, oldstart$, oldend$ = " "

            call "READ100" (#12, "A0", f1%(12%))
                if f1%(12%) = 0% then goto L30210
            get #12, using L30196, newstart$
                call "DATFMTC" (newstart$)
L30196:         FMT XX(2), CH(6)

L30210:     gosub loadcalendar

            REM Expand out the date for descriptive display, get the
            REM number of days left in the current calendar.

            on hit% goto L30540         /* NO OLD (CURRENT) ON FILE */
            oldstart$ = ccyymmdd$(1%)
            oldend$   = ccyymmdd$(490%)
            if oldstart$ = " " then L30540
            if oldstart$ = blankdate$ then L30540
            call "DATE" addr("G-", date, oldend$, d%, r%)
            oldstartu$ = oldstart$
            call "DATFMTC" ( oldstart$ )
            call "DATFMTC" ( oldend$ )
            convert d% to oldleft$, pic(###)
            if d% > 50% then goto L30540
            errormsg$ =   "You have less than 50 days left on your produc~
        ~tion calendar"
L30540:     return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  if errormsg$ <> " " then  newend$ = " "
                  if errormsg$ <> " " then newleft$ = " "
                  init(hex(84)) lfac$()
          inpmessage$ = "The starting date must lie within the current ca~
        ~lendar (If there is a current)"
                  on fieldnr% gosub L40170          /* CUR ST DATE      */
                     goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:    accept                                                        ~
               at (01,02),                                               ~
                  "MANAGE PRODUCTION CALENDAR",                          ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Current plan period start date",                      ~
               at (06,38), fac(hex(84)), oldstart$              , ch(10),~
               at (07,02),                                               ~
                  "Current plan period end date",                        ~
               at (07,38), fac(hex(84)),  oldend$               , ch(10),~
               at (08,02),                                               ~
                  "Days left in current plan period",                    ~
               at (08,41), fac(hex(84)), oldleft$               , ch(03),~
               at (09,02), "The fiscal year begins in",                  ~
               at (09,38), fac(hex(84)), fmn$                   , ch(10),~
               at (11,02), fac(hex(ac)),   blank$               , ch(79),~
               at (14,02),                                               ~
                  "New plan period START Date",                          ~
               at (14,38), fac(lfac$( 1)), newstart$            , ch(10),~
               at (14,49), fac(hex(8c)),  newstartdescr$        , ch(10),~
               at (15,02),                                               ~
                  "New plan period END Date",                            ~
               at (15,38), fac(hex(84)), newend$                , ch(10),~
               at (15,49), fac(hex(8c)), newenddescr$           , ch(10),~
               at (16,02),                                               ~
                  "Days left in new period would be",                    ~
               at (16,41), fac(hex(84)), newleft$               , ch(03),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(9)Print current calendar",                           ~
               at (24,29),                                               ~
                  "(25)Calculate & print new calendar",                  ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001090d0f1019)),                                ~
               key (keyhit%)

               if keyhit% <> 13% then L40660
                  call "MANUAL" ("CALINPUT")
                  goto L40240

L40660:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *      E D I T     M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * EDITS USER INPUT                                          *~
            *************************************************************

            deffn'111(fieldnr%)
                  if errormsg$ <> " " then  newend$ = " "
                  if errormsg$ <> " " then newleft$ = " "
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40840          /* CUR ST DATE      */
                     goto L40910

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40840:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40910:    accept                                                        ~
               at (01,02),                                               ~
                  "MANAGE PRODUCTION CALENDAR",                          ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Current plan period start date",                      ~
               at (06,38), fac(hex(8c)), oldstart$              , ch(10),~
               at (07,02),                                               ~
                  "Current plan period end date",                        ~
               at (07,38), fac(hex(8c)),  oldend$               , ch(10),~
               at (08,02),                                               ~
                  "Days left in current plan period",                    ~
               at (08,41), fac(hex(8c)), oldleft$               , ch(03),~
               at (09,02), "The fiscal year begins in",                  ~
               at (09,38), fac(hex(8c)), fmn$                   , ch(10),~
               at (11,02), fac(hex(ac)),   blank$               , ch(79),~
               at (14,02),                                               ~
                  "New plan period START date",                          ~
               at (14,38), fac(lfac$( 1)), newstart$            , ch(10),~
               at (14,49), fac(hex(8c)),  newstartdescr$        , ch(10),~
               at (15,02),                                               ~
                  "New plan period END date",                            ~
               at (15,38), fac(hex(8c)), newend$                , ch(10),~
               at (15,49), fac(hex(8c)), newenddescr$           , ch(10),~
               at (16,02),                                               ~
                  "Days left in new period would be",                    ~
               at (16,41), fac(hex(8c)), newleft$               , ch(03),~
               at (21,02), fac(hex(ac)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,02),                                               ~
                  "(12)IMPLEMENT the new calendar",                      ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010c0d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13% then L41670
                  call "MANUAL" ("CALINPUT")
                  goto L40910

L41670:        if keyhit% <> 15% then L41710
                  call "PRNTSCRN"
                  goto L40910

L41710:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *             C A U T I O N   M E S S A G E                 *~
            *                                                           *~
            *  TELL ABOUT WHAT THIS WILL INVOLVE, GIVE CHANCE TO ABORT  *~
            *************************************************************

        caution

L41840: accept                                                           ~
          at(1,02),"**CAUTION ** CAUTION ** CAUTION ** CAUTION ** CAUTION~
        ~ ** CAUTION ** CAUTION**",                                       ~
            at(3,02),"Changing the calendar requires that all the current~
        ~ planning data be changed.",                                     ~
            at(4,7),"This can be somewhat involved, so please note the fo~
        ~llowing :",                                                      ~
         at(8,02),"If you choose to continue, you may be forced to run a ~
        ~backup.",                                                        ~
           at(10,2),"No other users should be accessing the Caelus Manage~
        ~ment System now,",                                               ~
             at(11,7),"and they will not be allowed access until this tas~
        ~k is complete.",                                                 ~
                at(13,2),"This task may take some time, so be sure that n~
        ~o conflicts will arise.",                                        ~
            at(22,2),"If you are sure nobody is using the planning module~
        ~ and you want to",                                               ~
            at(23,2),"proceed, key PF 32. PRESS ANY OTHER PF KEY TO RETUR~
        ~N TO PREVIOUS SCREEN",                                           ~
            key(keyhit%)
            if keyhit% = 0% or keyhit% = 16% then L41840
            if keyhit% <> 15% then return
               call "PRNTSCRN"
               goto L41840


        REM *************************************************************~
            *             R E S U L T S   M E S S A G E                 *~
            *                                                           *~
            *  Display the results of the file checking                 *~
            *************************************************************

        display_check_results
            str(line2$,1%, 60%) = "Results of Planning File Check"
            edtmessage$ = "Press PF(16) or RETURN to return to 'Set " &  ~
                          "Calandar Start Screen'"

L43090:    accept                                                        ~
               at (01,02),                                               ~
                  "Manage Production Calendar",                          ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "PIPIN:",                                     ~
               at (07,08), "# of Records Reviewed:",                     ~
               at (07,32), fac(hex(84)), problems$(1%,3%)       , ch(08),~
               at (07,45), "# of Problem Records:",                      ~
               at (07,68), fac(hex(84)), problems$(1%,2%)       , ch(08),~
                                                                         ~
               at (08,08), fac(rfac$(1%,1%)), results$(1%,1%)   , ch(50),~
               at (08,60), fac(rfac$(1%,2%)), filestart$(1%)    , ch(10),~
                                                                         ~
               at (09,08), fac(rfac$(1%,1%)), results$(1%,2%)   , ch(50),~
                                                                         ~
               at (11,02), "PIPOUT:",                                    ~
               at (12,08), "# of Records Reviewed:",                     ~
               at (12,32), fac(hex(84)), problems$(2%,3%)       , ch(08),~
               at (12,45), "# of Problem Records:",                      ~
               at (12,68), fac(hex(84)), problems$(2%,2%)       , ch(08),~
                                                                         ~
               at (13,08), fac(rfac$(2%,1%)), results$(2%,1%)   , ch(50),~
               at (13,60), fac(rfac$(2%,2%)), filestart$(2%)    , ch(10),~
                                                                         ~
               at (14,08), fac(rfac$(2%,1%)), results$(2%,2%)   , ch(50),~
                                                                         ~
               at (16,02), "WCOUT:",                                     ~
               at (17,08), "# of Records Reviewed:",                     ~
               at (17,32), fac(hex(84)), problems$(3%,3%)       , ch(08),~
               at (17,45), "# of Problem Records:",                      ~
               at (17,68), fac(hex(84)), problems$(3%,2%)       , ch(08),~
                                                                         ~
               at (18,08), fac(rfac$(3%,1%)), results$(3%,1%)   , ch(50),~
               at (18,60), fac(rfac$(3%,2%)), filestart$(3%)    , ch(10),~
                                                                         ~
               at (19,08), fac(rfac$(3%,1%)), results$(3%,2%)   , ch(50),~
                                                                         ~
               at (21,02), fac(hex(ac)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,32),                                               ~
                  "(8)View Problems",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Return",                                          ~
                                                                         ~
               keys(hex(0001080d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 8% then L43670
                  call "PLOWCODE" (#20,all(hex(00)), ~
                  hex(06) &"  File   Tag                 Date     Description",~
                   0%, 0.0, f1%(20%))
                  goto L43090

L43670:        if keyhit% <> 13% then L43710
                  call "MANUAL" ("CALINPUT")
                  goto L43090

L43710:        if keyhit% <> 15% then L43750
                  call "PRNTSCRN"
                  goto L43090

L43750:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               init (" ")  str(line2$,1%, 60%)
               edtmessage$ = "To Modify Displayed Values, Position Cursor~
        ~ To Desired Value And Press RETURN."
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110          /* NEW ST DATE      */
                     return

L50110:     REM TEST DATA FOR START DATE
                call "DATEOKC" (newstart$, search%(1%), errormsg$)
                if errormsg$ <> " " then return

                call "DATUFMTC" (newstart$,0%,newstart$)
                str(newstart$,7%,2%) = "01"
                call "DATFMTC" (newstart$, 0%, t$)
                call "DATECONV" (t$)
                if  t$ > date then errormsg$ = "Date cannot be gre~
        ~ater than todays date"
                if errormsg$ <> " " then return

                if hit% = 1% then L50330

                search ccyymmdd$() = str(t$,,6%) to search%()
                if search%(1%)=0% then errormsg$ = "Date must be within t~
        ~he current calendar"
                if errormsg$ <> " " then return

                if str(ccyymmdd$(1%),,4%) = str(t$,,4%) then        ~
                   errormsg$ = "Invalid Response, Re-enter."
                if errormsg$ <> " " then return

               if lowest_possible$ = blankdate$ then L50330
               if lowest_possible$ = " " or t$ <= lowest_possible$~
                then L50330
                    newstart$ = lowest_possible$
                    tempdate$ = lowest_possible$ : call "DATFMTC" (tempdate$)
                    errormsg$ = "Because of current activity, date must b~
        ~e less than " & tempdate$
                    call "DATFMTC" (newstart$)
                    call "DATUFMTC" (newstart$,0%,newstart$)
                    str(newstart$,7%,2%) = "01"
                    call "DATFMTC" (newstart$)
                    return

L50330:         gosub calc_it_up
*               newend$ = ccyymmdd$(490%)
*               call "DATFMTC" (newend$)
                return

        calc_it_up  :

            rem**********************************************************~
              * get the last day of the prev mo in terms of the old cal *~
              * when the new cal is calculated we start with the first  *~
              * day beyond target date, which will be the first day     *~
              * of the specified month.                                 *~
              ***********************************************************

            target$ = newstart$
            call "DATUFMTC" (target$)
            call "DATE" addr ("G+", target$, -1%, target$, r%)
            convert fm$ to fm%, data goto L51095
                goto L51170
L51095:         fm% = 100%

L51170:     print at(4, 1, 80); at(21,1,400) ; at (21,26); hex(84);      ~
                             "Calculating a new calendar"
            for j% = 1990% to 2089%
                convert j% to yr$, pic(####)
                t$ = str(yr$,1%,4%) & "0101"
                call "DATECONV" (t$)
                     for i% = 1% to 6%
                     call "DATE" addr ("GD", t$, o$, r%)
                     if str(o$,1,3) = "MON" then goto L51215
                     call "DATE" addr ("G+", t$, 1%, t$, r%)
                          next i%
L51215:              fmon$(j%-1989%) = t$
                next j%

            init(" ") ccyymmdd$(), yymmdd$(), dow$()

            mat yy% = zer          : mat cqoy% = zer
            mat mm% = zer          : mat fqoy% = zer
            mat dd% = zer
            mat mwoy% = zer
            mat fwoy% = zer

            for i% = 1% to 490%
                call "DATE" addr("G+", target$, i%, ccyymmdd$(i%), r%)
                call "DATFMTC" (ccyymmdd$(i%), 0%, ccyymmdd$(i%))
                convert str(ccyymmdd$(i%),1%,4%) to yy%(i%)
                convert str(ccyymmdd$(i%),5%,2%) to mm%(i%)
                convert str(ccyymmdd$(i%),7%,2%) to dd%(i%)
                call "DATECONV" (ccyymmdd$(i%))
                call "DATE" addr("GD", ccyymmdd$(i%), o$, r%)
                dow$(i%) = str(o$,1%,3%)

                REM Calculate the week of the year from the first Mon.

         call "DATE" addr("G-",fmon$(yy%(i%)-1989%),ccyymmdd$(i%),mwoy%(i%),r%)
                mwoy%(i%) = (mwoy%(i%) / 7%) + 1%
                t$ = ccyymmdd$(i%): call "DATEFMT" (t$, 0%, t$)
                if str(t$,5%,2%) <> "01" then goto L51340
                if ccyymmdd$(i%) < fmon$(yy%(i%)-1989%) then mwoy%(i%) = 53%

L51340:         REM CALCULATE THE WEEK OF THE YEAR FROM DAY 1
            t$ = ccyymmdd$(i%)   : call "DATEFMT" ( t$, 0%, t$)
            str(t$,5%) = "0101": call "DATECONV"( t$ )
            call "DATE" addr("G-", t$,      ~
                ccyymmdd$(i%), fwoy%(i%),r%)
                fwoy%(i%) =  (fwoy%(i%)/ 7%) + 1%

                REM NOW GET THE CALENDAR QUARTER
            if mm%(i%) > 3% then goto L51390
                cqoy%(i%) = 1%
                goto L51440

L51390:     if mm%(i%) > 6% then goto L51410
                cqoy%(i%) = 2%
                goto L51440

L51410:     if mm%(i%) > 9% then goto L51430
                cqoy%(i%) = 3%
                goto L51440

L51430:         cqoy%(i%) = 4%

L51440:         REM NOW THE FISCAL QUARTER OF THE YEAR IF FY IS SET
                if fm% < 1% or fm% > 12% then goto L51575
                if mm%(i%) > fm% then goto L51515
                if fm% - mm%(i%) > 2% then goto L51470
                     fqoy%(i%) = 4%
                     goto   L51585
L51470:         if fm% - mm%(i%) > 5% then goto  L51485
                     fqoy%(i%) = 3%
                     goto  L51585
L51485:         if fm% - mm%(i%) > 8% then goto  L51500
                     fqoy%(i%) = 2%
                     goto L51585
L51500:         fqoy%(i%) = 1%
                     goto L51585

L51515:         if mm%(i%) - fm% <= 9% then goto  L51530
                     fqoy%(i%) = 4%
                     goto L51585
L51530:         if mm%(i%) - fm% <= 6% then goto  L51545
                     fqoy%(i%) = 3%
                     goto L51585
L51545:         if mm%(i%) - fm% <= 3% then goto  L51560
                     fqoy%(i%) = 2%
                     goto L51585
L51560:         fqoy%(i%) = 1%
                goto L51585

L51575:     fqoy%(i%) = cqoy%(i%)

L51585:         next i%

            newstartdescr$, newstart$ = ccyymmdd$(1%)
            newenddescr$, newend$ = ccyymmdd$(490%)
            if newstart$ = blankdate$ then newstart$ = " "
            if newstart$ = " " then L09000     /* HOW COULD THIS BE ?? */
            call "DATEOKC" ( newstartdescr$, 0%, " " )
            call "DATEOKC" ( newenddescr$, 0%, " " )
            call "DATUFMTC" (newend$)
            call "DATE" addr("G-", date, newend$, d%, r%)
            call "DATFMTC" (newend$)
            call "DATFMTC" (newstart$)
            convert d% to newleft$, pic(###)
            print at (14,48) ; hex(8c) ; newstartdescr$ ;                ~
                  at (15,48) ; hex(8c) ; newenddescr$

            for u3% = 1% to 490% : yymmdd$(u3%) = ccyymmdd$(u3%) : next u3%

            call "READ101" (#12, "A0", f1%(12%))
                put #12, using L52030, "A0", str(yymmdd$(),1%,1470%)
L52030:         FMT CH(02), CH(1470)
                if f1%(12%) = 1 then rewrite #12 else write #12

            call "READ101" (#12, "A1", f1%(12%))
                put #12,using L52030,"A1",str(yymmdd$(),1471%)
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "B0", f1%(12%))
                put #12, using L52120, "B0", yy%()
L52120:         FMT CH(02),   490*BI(4)
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "C0", f1%(12%))
                put #12, using L52120,  "C0", mm%()
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "D0", f1%(12%))
            put #12, using L52120,  "D0", dd%()
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "E0", f1%(12%))
            put #12, using L52250,  "E0",  dow$()
L52250:         FMT CH(2), 490*CH(3)
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "F0", f1%(12%))
            put #12, using L52300,  "F0", mwoy%()
L52300:         FMT CH(2), 490*BI(4)
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "F1", f1%(12%))
            put #12, using L52300,  "F1", fwoy%()
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "G0", f1%(12%))
            put #12, using L52300,  "G0", cqoy%()
                if f1%(12%) = 1% then rewrite #12 else write #12

            call "READ101" (#12, "G1", f1%(12%))
            put #12, using L52300,  "G1", fqoy%()
                if f1%(12%) = 1% then rewrite #12 else write #12

            if keyhit% = 25% then gosub printcalendar
            gosub L30000
            newend$ = newenddescr$
            if (lowest_possible$ <> " " and lowest_possible$ <> blankdate$) ~
                or oldstart$ = " " or oldstart$ = blankdate$ then return

            init (hex(0b)) ok$
            print at(21,1,400) ; at (21,01); hex(84);                    ~
            "Before the new calendar can be implemented, it must be check~
        ~ed against"; at(22,01); hex(84) ;                                ~
            "your current planned inventory.  This will take a few minute~
        ~s."; at(24,01); hex(8c); "Do you Want to do it now ?";           ~
            hex(840b0b0b) ; at(24,27); : input ok$

            if ok$ <> "YES" then L52670
            call "FILEBGON" (#20)
            call "WORKOPEN" (#20, "SHARE", 100%, f2%(20%))
            gosub check_planning_files
            gosub report_check_results
            if z% = 9999% then return
            z%=min(490%,max(1%,zmin%))
            t$ = newstart$
            call "DATUFMTC" (t$)
            if ccyymmdd$(z%) >= t$ then return
            newstart$, lowest_possible$ = ccyymmdd$(z%)
            call "DATFMTC" (newstart$)
            errormsg$ = "Calendar won't conform w/your data base. Cale~
        ~ndar must start before " & newstart$
            call "DATUFMTC" (newstart$,0%,newstart$)
            str(newstart$,7%,2%) = "01"
            call "DATFMTC" (newstart$)
            return

L52670:     return clear all
            goto L09000

            rem**********************************************************~
            *  read throu pip & wc file to validate new planning date   *~
            *************************************************************

        check_planning_files

            errormsg$ = " "
            z%, zmin% = 9999%
            mat problems% = zer
            problems%(1%,1%), problems%(2%,1%), problems%(3%,1%) = 9999%
            xyz% = 1%  :  q% = 1%
            print at(21,1,400) ; at (21,28); hex(84);                    ~
                   "Checking 'PIPIN' File"
            call "PLOWALTS" (#5, hex(00), 1%, 0%, f1%(5%))
                if f1%(5%) = 0% then L52825
            first% = 1%
            problem_file$ = "PIPIN"
L52770:     gosub'88 (5%, 26%, 4%)
                get #5 using L52805, problem_descr$, problem_tag$, x%
                if xyz% = 2% then L52815 /* Don't Process last one Twice*/
            key$ = key(#5%, 0%)    /* Ignore 'start dates' if released */
            if str(key$,,2%)  = "BO" then L52800
            if str(key$,,2%)  = "BW" then L52800
            if str(key$,,2%)  = "RO" then L52800
            if str(key$,,2%)  = "RW" then L52800
            if str(key$,,2%) <> "WO" then L52815

L52805:            FMT CH(25), POS(30), CH(19), POS(57), BI(4)
L52800:            z% = min(x% ,z%)   /* Two Dates in this one */

L52815:     call "DATE" addr ("G+", oldstartu$, z%-1%, problem_date$, ret%)
            call "DATEFMT" (problem_date$)
            if xyz% = 1% then gosub detail_check
            on xyz% goto L52770

L52825:     xyz% = 1%  :  q% = 2%
            print at(21,1,78) ; at (21,28); hex(84);                     ~
                   "Checking 'PIPOUT' File"
            call "PLOWALTS" (#6, all(hex(00)), 1%, 0%, f1%(6%))
                if f1%(6%) = 0% then L52870
            first% = 1%
            problem_file$ = "PIPOUT"
L52855:     gosub'88 (6%, 26%, 4%)
                   get #6 using pipout_fmt, problem_tag$, problem_descr$
                   call "DATE"addr("G+", oldstartu$, z%-1%, problem_date$, ret%)
                   call "DATEFMT" (problem_date$)
pipout_fmt: fmt ch(19), ch(25)
            if xyz% = 1% then gosub detail_check
            on xyz% goto L52855

L52870:     xyz% = 1%  :  q% = 3%
            print at(21,1,78) ; at (21,28); hex(84);                     ~
                   "Checking 'WCOUT' File"
            call "PLOWALTS" (#8, hex(00), 1%, 0%, f1%(8%))
                if f1%(8%) = 0% then L52910
            first% = 1%
            problem_file$ = "WCOUT"
            problem_descr$ = "WC:     Step:     Act:"
L52900:     gosub'88 (8%, 5%, 2%)
                   get #8 using wcout_fmt, str(problem_descr$,4%,4%), ~
                                           problem_tag$,              ~
                                           str(problem_descr$,14%,4%),~
                                           str(problem_descr$,23%,4%)
                   call "DATE" addr("G+",oldstartu$, z%-1%, problem_date$, ret%)
                   call "DATEFMT" (problem_date$)
wcout_fmt: fmt ch(4), pos(9), ch(19), pos(40), ch(4), pos(48), ch(4)
            if xyz% = 1% then gosub detail_check
            on xyz% goto L52900
L52910:     return

        deffn'88 (x%, y%, v%)
            if first% = 0% then read #x%, eod goto L52960
            key$ = key(#x%, 1%)
            first% = 0%
            problems%(q%,3%) = problems%(q%,3%) + 1%
            on v%/2% goto L52940, L52950
L52940:     z% = val(str(key$,y%),2%)
            goto L52965
L52950:     z% = val(str(key$,y%),4%)
            goto L52965
L52960:     xyz% = 2%
L52965: return

        detail_check
            if z% = 9999% then return
            zz% = min(490%, max(1%,z%))
            t$ = newstart$: call "DATUFMTC" (t$)
            if ccyymmdd$(zz%) >= t$ then return
            write #20 using problem_fmt,   problem_file$, problem_tag$, ~
                            problem_date$, problem_descr$, eod goto problem_fmt
problem_fmt: fmt ch(7), ch(20), ch(9), ch(27)
            zmin% = min(zz%, zmin%)
            problems%(q%,1%) = min(zz%, problems%(q%,1%))
            problems%(q%,2%) = problems%(q%,2%) + 1%
            return

        rem**************************************************************~
            *      p r i n t   c a l e n d a r   r o u t i n e          *~
            *                                                           *~
            *************************************************************

        printcalendar
            if oldstart$ = " " then return
            if keyhit% = 25% then L53070
            call "SHOSTAT" ("Printing a Production Calendar")
L53070:     linecnt% = 1000%             /* TRIGGER NEW PAGE */
            select printer(134)

L53100: % PERIOD   DATE    DAY-OF-WEEK   YEAR     MONTH     DAY     WK-OF~
        ~-YR    WK-OF-YR   QRT-OF-YEAR    QTR-OF-YR
L53120: %                                                           1ST-M~
        ~ON     1ST-DAY    CALENDAR       FISCAL
L53140: %   ###   ########     ###       ####      ##        ##        ##~
        ~         ##          #             #
L53160: %  PRODUCTION CALENDAR FOR ######################################~
        ~#################
            for i = 1 to 490
               gosub newpage
               linecnt% = linecnt% + 1%
               t$ = ccyymmdd$(i): call "DATEFMT" (t$)
               print using L53140, i, t$, dow$(i), yy%(i), mm%(i),~
                     dd%(i), mwoy%(i), fwoy%(i), cqoy%(i), fqoy%(i)
            next i

            goto L53320
            print page:print
            for i = 1990% to 2089%
                print using L53300,i , fmon$(i-1989%)
            next i
L53300: %     ##   ######

L53320:     close printer  :  select ws

            return

        newpage
            if linecnt% < 55% then return
            call "DATE" addr("HD", hdate$)
            print page
            print using L53160,    hdate$
            print
            print
            print using L53100
            print using L53120
            print
            linecnt% = 7%
            return

        REM *************************************************************~
            *  Report the results of the trip thru the planning files.  *~
            *************************************************************

        report_check_results
            if z% = 9999% then return
            if problems%(1%, 2%) = 0% and                                ~
               problems%(2%, 2%) = 0% and                                ~
               problems%(3%, 2%) = 0% then return

            init (" ")  results$(), filestart$()
            init (hex(9c))  rfac$()

            for i% = 1% to 3%
                convert problems%(i%, 2%) to problems$(i%, 2%),          ~
                                                            pic(#######0)
                call "STRING" addr("LJ", problems$(i%, 2%), 8%)
                convert problems%(i%, 3%) to problems$(i%, 3%),          ~
                                                            pic(#######0)
                call "STRING" addr("LJ", problems$(i%, 3%), 8%)
                if problems%(i%, 2%) = 0% then L54290
                     filestart$(i%) = ccyymmdd$(problems%(i%, 1%))
                     call "DATFMTC" (filestart$(i%))
                     results$(i%, 1%) = "For this file the calendar " &  ~
                                        "must start before"
                     results$(i%, 2%) = "Consult the 'PIPLATE' report" & ~
                                        " for more information"
                     rfac$(i%, 1%) = hex(8c)
                     rfac$(i%, 2%) = hex(84)
                     if i% = 3% then results$(i%, 2%) = "Consult 'WCD" & ~
                                "SPLY' or 'WCUTIL' for more information"
L54290:     next i%

            gosub display_check_results
            return

        REM LOAD FULL CALENDAR

        loadcalendar

L60040:         FMT XX(2), CH(1470)
L60050:         FMT XX(2), 490*BI(4)
L60060:         FMT XX(2), 490*CH(3)

            call "READ100" (#12,"10", f1%(12%))
                if f1%(12%) = 0% then goto L60500
            get #12, using L60040, str(yymmdd$(),1%,1470%)

            call "READ100" (#12,"11", f1%(12%))
                if f1%(12%) = 0% then goto L60500
            get #12, using L60040, str(yymmdd$(),1471%,1470%)

            for u3% = 1% to 490%: ccyymmdd$(u3%) = yymmdd$(u3%): next u3%

            call "READ100" (#12,"20", f1%(12%))
                if f1%(12%) = 0% then goto L60200
            get #12, using L60050, yy%()

L60200:     call "READ100" (#12,"30", f1%(12%))
                if f1%(12%) = 0% then goto L60240
            get #12, using L60050, mm%()

L60240:     call "READ100" (#12,"40", f1%(12%))
                if f1%(12%) = 0% then goto L60280
            get #12, using L60050, dd%()

L60280:     call "READ100" (#12,"50", f1%(12%))
                if f1%(12%) = 0% then goto L60320
            get #12, using L60060, dow$()

L60320:     call "READ100" (#12,"60", f1%(12%))
                if f1%(12%) = 0% then goto L60360
            get #12, using L60050, mwoy%()

L60360:     call "READ100" (#12,"61", f1%(12%))
                if f1%(12%) = 0% then goto L60400
            get #12, using L60050, fwoy%()

L60400:     call "READ100" (#12,"70", f1%(12%))
                if f1%(12%) = 0% then goto L60440
            get #12, using L60050, cqoy%()

L60440:     call "READ100" (#12,"71", f1%(12%))
                if f1%(12%) = 0% then goto L60480
            get #12, using L60050, fqoy%()

L60480:     return

L60500:         hit% = 1%
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

            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#20)
            end return_code%
