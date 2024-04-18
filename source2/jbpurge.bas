        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP   U   U  RRRR    GGG   EEEEE          *~
            *    J    B   B  P   P  U   U  R   R  G      E              *~
            *    J    BBBB   PPPP   U   U  RRRR   G GGG  EEEE           *~
            *  J J    B   B  P      U   U  R   R  G   G  E              *~
            *   J     BBBB   P       UUU   R   R   GGG   EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPURGE  - NUKES ALL JOB DETAIL AND OPTIONALLY THE JOB    *~
            *            MASTER RECORD FOR A RANGE OF JOBS/CLOSE DATE   *~
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
            * 05/03/84 ! ORIGINAL                                 ! HES *~
            * 01/07/86 ! Replaced JBACTST2 With JBSTATUS file     ! HES *~
            * 03/18/87 ! Added delete for TEXT File & Standards   ! MJB *~
            * 06/15/87 ! JBMASTR2, JBCREDIT file format changes   ! JIM *~
            * 07/14/94 ! Added purging of VBKVSA and VSAOUTIN     ! ERN *~
            * 04/09/95 ! Added purging of JBMASTRC                ! KAB *~
            * 08/13/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dateclosed$6,                /* DATE JOB WAS CLOSED        */~
            ddate$8,                     /* PURGE DETAIL DATE          */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            ejob$8,                      /* Ending job # for plow      */~
            endjob$8,                    /* ENDING JOB NUMBER          */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            killkey$50,                  /* FINDS RECORDS TO ELIMINATE */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            mdate$8,                     /* PURGE MASTER DATE          */~
            plowkey$50,                  /* Misc use key variable      */~
            sjob$8,                      /* Starting job # for plow    */~
            startjob$8,                  /* STARTING JOB NUMBER        */~
            textid$4                     /* Job Text ID                */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! JBMASTR2 ! Production job master file               *~
            * #02 ! JBMATER2 ! Production job material used detail file *~
            * #03 ! JBVALUE2 ! Production job value added detail file   *~
            * #04 ! JBCREDIT ! Production job credits received detail f *~
            * #05 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #06 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #07 ! JBPIPXRF ! option part harder peg                   *~
            * #08 ! TXTFILE  ! System Text File                         *~
            * #09 ! VBKVSA   ! Vendor Service Advices                   *~
            * #10 ! VSAOUTIN ! Vendor Service Shipping Log              *~
            * #11 ! JBMASTRC ! JBMASTR2 core appendix file              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #02, "JBMATER2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #03, "JBVALUE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  23

            select #04, "JBCREDIT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48

            select #05, "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   94,                                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #06, "JBSTATUS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 12,                          ~
                        alt key 1, keypos =  21, keylen = 44,            ~
                            key 2, keypos =  29, keylen = 36

            select #07, "JBPIPXRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   63,                                  ~
                        keypos =    1, keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19          ~

            select  #8, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #09, "VBKVSA",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 5,    keylen = 8,                       ~
                        alt key  1, keypos =    1, keylen =  12,         ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  6, keypos =   50, keylen =   4, dup

            select #10, "VSAOUTIN",                                      ~
                        varc, indexed, recsize =  316,                   ~
                        keypos =   1, keylen =  26,                      ~
                        alt key    1, keypos =  44, keylen =  8, dup,    ~
                            key    2, keypos =  52, keylen = 23, dup

            select #11, "JBMASTRC",                                      ~
                        varc, indexed, recsize =  600,                   ~
                        keypos =   1, keylen =  8

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, "SHARE", f2%(05), rslt$(05), axd$(05))
            call "OPENFILE" (#06, "SHARE", f2%(06), rslt$(06), axd$(06))
            call "OPENFILE" (#07, "SHARE", f2%(07), rslt$(07), axd$(07))
            call "OPENFILE" (#09, "SHARE", f2%(09), rslt$(09), axd$(09))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = " JBPURGE: " & str(cms2v$,,8) : u3% = 0%
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, startjob$, endjob$, ddate$,~
                      mdate$

            for fieldnr% = 1 to  4
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  4 then L11060

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

            call "SHOSTAT" ("Purge of requested data in progress")

            call "DATUNFMT" (ddate$)
            call "DATUNFMT" (mdate$)
            sjob$ = startjob$ : ejob$ = endjob$
            if sjob$ <> "ALL" then L19160
                sjob$ = all(hex(00))
                ejob$ = all(hex(ff))
                goto L19170
L19160:     sjob$ = sjob$ addc all(hex(ff))
L19170:     call "PLOWNXT1" (#1, sjob$, 0%, f1%(1))
                if sjob$ > ejob$ or f1%(1) = 0 then L55000
            get #1, using L19290, dateclosed$, textid$
            if dateclosed$ = " " or dateclosed$ = blankdate$ then L19170
            if dateclosed$ > mdate$ then L19230
                delete #1
                killkey$ = sjob$
                call "DELETE" (#11, killkey$, 8%)
                call "TXTFUTIL" (#8, f2%(8), "DELE", textid$)
                mcount% = mcount% + 1
L19230:     if dateclosed$ <= ddate$ then gosub purge_detail
            goto L19170

*         ALL THE FIELD SPECS ARE HERE SO THAT IF A PURGE REPORT IS
*         NEEDED, WE WON'T HAVE TO GO FAR

L19290: FMT                      /* FILE #1- JBMASTR2                  */~
            POS(153), CH(6),     /* Date production job actually ended */~
            POS(228), CH(04)     /* Job text ID                        */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20130,         /* FIRST JOB        */~
                                    L20160,         /* ENDING JOB NUMBER*/~
                                    L20200,         /* PURGE DETAIL DATE*/~
                                    L20230          /* PURGE MASTER DATE*/
                     return
L20130:     REM DEFAULT/ENABLE FOR STARTING JOB NUMBER
                startjob$ = "ALL"
                enabled% = 1
                return
L20160:     REM DEFAULT/ENABLE FOR ENDING JOB NUMBER
                if startjob$ = "ALL" then return
                enabled% = 1
                return
L20200:     REM DEFAULT/ENABLE FOR PURGE DETAIL DATE
                ddate$ = date$
                enabled% = 1
                return
L20230:     REM DEFAULT/ENABLE FOR PURGE MASTER DATE
                mdate$ = ddate$
                enabled% = 1
                return

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
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
L29200:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29200
                return clear all
                goto inputmode


        REM *************************************************************~
            *        P U R G E   T H E   J O B   D E T A I L            *~
            *************************************************************

        purge_detail

*          MATERIALS FIRST.
            killkey$ = sjob$
            call "DELETE" (#2, killkey$, 8%)

*          THEN THE VALUE ADDED DETAIL GET'S IT
            killkey$ = sjob$
            call "DELETE" (#3, killkey$, 8%)

*          NOW LET'S MOVE ON TO THE CREDITS RECEIVED FILE
            killkey$ = sjob$
            call "DELETE" (#4, killkey$, 8%)

*          JBCROSS2...
            killkey$ = "JOB ORDER: " & str(sjob$)
            call "READ101" (#5, killkey$, f1%(5))
                if f1%(5) <> 0 then delete #5

*          AND DON'T FORGET THE CREDITS RECEIVED FILE
            killkey$ = sjob$
            call "DELETE" (#6, killkey$, 8%)

*          Do VSA files...
            plowkey$ = str(sjob$,,8) & hex(00)
L30284:     call "PLOWAL1" (#9, plowkey$, 3%, 8%, f1%(9%))
            if f1%(9%) = 0% then L30290
                delete #9
                goto L30284

L30290:     plowkey$ = str(sjob$,,8) & hex(00)
            call "DELETE" (#10, plowkey$, 8%, f1%(10%))

*          LAST BUT NOT LEAST, WE PUT THE AXE TO JBPIPXRF
            killkey$ = "JOB ORDER: " & str(sjob$)
            call "REDALT1" (#7, killkey$, 1%, f1%(7))
                if f1%(7) <> 0 then delete #7
            call "DELETE" (#7, killkey$, 19%)
            dcount% = dcount% + 1
        return

        FMT                      /* FILE: JBPIPXRF                     */~
            CH(19),              /* Tag number in level 2 planning sec */~
            CH(25),              /* Part code                          */~
            CH(19)               /* same as TAGNR$                     */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40170,         /* FIRST JOB        */~
                                    L40170,         /* ENDING JOB NUMBER*/~
                                    L40170,         /* PURGE DETAIL DATE*/~
                                    L40170          /* PURGE MASTER DATE*/
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

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "CLEAR OLD JOB DETAIL",                                ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting Job Number",                                 ~
               at (06,30), fac(lfac$( 1)), startjob$            , ch(08),~
               at (07,02),                                               ~
                  "Ending Job Number",                                   ~
               at (07,30), fac(lfac$( 2)), endjob$              , ch(08),~
               at (08,02),                                               ~
                  "Detail Purge Date",                                   ~
               at (08,30), fac(lfac$( 3)), ddate$               , ch(08),~
               at (09,02),                                               ~
                  "Master Purge Date",                                   ~
               at (09,30), fac(lfac$( 4)), mdate$               , ch(08),~
               at (12,02),               "This Program ONLY effects CLOSE~
        ~D Jobs. The dates above refer to the date the",                  ~
               at (13,02),               "  job was closed. For all jobs ~
        ~closed on or before the 'DETAIL PURGE DATE, the",                ~
               at (14,02),               "  supporting job detail will be~
        ~ purged.  For all jobs closed on or before the ",                ~
               at (15,02),               "  'MASTER PURGE DATE', the job ~
        ~master record will also be purged. Note that the",               ~
               at (16,02),                "  DETAIL PURGE DATE must be th~
        ~e same as or later then the MASTER PURGE DATE.",                 ~
               at (17,02),                "  (You can't delete the master~
        ~ record and leave the detail). Also note: once",                 ~
               at (18,02),                "  supporting detail has been p~
        ~urged for a job, the job shouldn't be re-opened.",               ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40750
                  call "MANUAL" ("JBPURGE ")
                  goto L40240

L40750:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41170,         /* FIRST JOB        */~
                                    L41170,         /* ENDING JOB NUMBER*/~
                                    L41170,         /* PURGE DETAIL DATE*/~
                                    L41170          /* PURGE MASTER DATE*/
                     goto L41240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02),                                               ~
                  "CLEAR OLD JOB DETAIL",                                ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting Job Number",                                 ~
               at (06,30), fac(lfac$( 1)), startjob$            , ch(08),~
               at (07,02),                                               ~
                  "Ending Job Number",                                   ~
               at (07,30), fac(lfac$( 2)), endjob$              , ch(08),~
               at (08,02),                                               ~
                  "Detail Purge Date",                                   ~
               at (08,30), fac(lfac$( 3)), ddate$               , ch(08),~
               at (09,02),                                               ~
                  "Master Purge Date",                                   ~
               at (09,30), fac(lfac$( 4)), mdate$               , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Begin Purge",                            ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41650
                  call "MANUAL" ("JBPURGE ")
                  goto L41240

L41650:        if keyhit% <> 15 then L41690
                  call "PRNTSCRN"
                  goto L41240

L41690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* FIRST JOB        */~
                                    L50150,         /* ENDING JOB NUMBER*/~
                                    L50180,         /* PURGE DETAIL DATE*/~
                                    L50210          /* PURGE MASTER DATE*/
                     return
L50130:     REM TEST DATA FOR STARTING JOB NUMBER
                return
L50150:     REM TEST DATA FOR ENDING JOB NUMBER
                if endjob$ < startjob$ then endjob$ = startjob$
                return
L50180:     REM TEST DATA FOR PURGE DETAIL DATE
                call "DATEOK" (ddate$, x, errormsg$)
                return
L50210:     REM TEST DATA FOR PURGE MASTER DATE
                call "DATEOK" (mdate$, y, errormsg$)
                if errormsg$ <> " " then return
                if x < y then errormsg$ = "DATE MUST BE LESS THEN OR EQUA~
        ~L TO THE DETAIL PURGE DATE"
                return

L55000: REM *************************************************************~
            *              S H O W   T H E   S C O R E                  *~
            *                                                           *~
            * DISPLAYS THE NUMBER OF JOBS PURGED.                       *~
            *************************************************************

            edtmessage$ = " "
            call "DATEFMT" (ddate$)
            call "DATEFMT" (mdate$)

L55100:     accept                                                       ~
               at (01,02),                                               ~
                  "CLEAR OLD JOB DETAIL -- SUMMARY PURGE COUNT",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting Job Number",                                 ~
               at (06,30), fac(hex(8c)), startjob$              , ch(08),~
               at (07,02),                                               ~
                  "Ending Job Number",                                   ~
               at (07,30), fac(hex(8c)), endjob$                , ch(08),~
               at (08,02),                                               ~
                  "Purge Detail Date",                                   ~
               at (08,30), fac(hex(8c)), ddate$                 , ch(08),~
               at (09,02),                                               ~
                  "Purge Master Date",                                   ~
               at (09,30), fac(hex(8c)), mdate$                 , ch(08),~
               at (12,02),                                               ~
                  "Number of Jobs Having Detail Purged:",                ~
               at (12,39), fac(hex(84)), dcount%           , pic(######),~
               at (13,02),                                               ~
                  "Number of Jobs Having Master Purged:",                ~
               at (13,39), fac(hex(84)), mcount%           , pic(######),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0f10)),                                          ~
               key (keyhit%)

               if keyhit% <> 15 then L65000
                  call "PRNTSCRN"
                  goto L55100

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

            call "SHOSTAT" ("One moment please")
            end
