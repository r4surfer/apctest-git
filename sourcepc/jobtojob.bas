        REM *************************************************************~
            *                                                           *~
            *  JJJJJ   OOO   BBBB   TTTTT   OOO   JJJJJ   OOO   BBBB    *~
            *    J    O   O  B   B    T    O   O    J    O   O  B   B   *~
            *    J    O   O  BBBB     T    O   O    J    O   O  BBBB    *~
            *  J J    O   O  B   B    T    O   O  J J    O   O  B   B   *~
            *   J      OOO   BBBB     T     OOO    J      OOO   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JOBTOJOB - THIS PROGRAM ALLOWS THE OPERATOR TO POST A COST*~
            *      FROM AN INPUT JOB TO ANY COST FIELD ON AN OUTPUT JOB *~
            *      THE LABOR, PURCHASES, MATERIAL, AND OH DOLLARS ARE   *~
            *      SUBTRACTED FROM THE INPUT JOB FIRST.        THEN     *~
            *      THE PROGRAM POSTS THE COST AMOUNT TO THE AMOUNT      *~
            *      OF DOLLARS TRANSFERRED TO MISCELLANEOUS IN THE INPUT *~
            *      PROGRAM AND POSTS THE AMOUNT OF THE COST SPECIFIED   *~
            *      BY THE OPERATOR TO THE OUTPUT JOB.                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/04/81 ! ORIGINAL                                 ! TOM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            * 09/15/97 ! Changed SHOWMSG to SHOSTAT (2 calls)     ! MLJ *~
            *************************************************************

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSUR POSITION SCREEN     */~
            date$8,                      /* SCREEN DATE                */~
            datejobclosed$8,             /* DATE JOB WAS CLOSED        */~
            edtmessage$79,               /* SCREEN EDUT MESSAGE        */~
            errormsg$79,                 /* SCREEN ERROR MESSAGE       */~
            freetext$40,                 /* FREE TEXT FOR TRANSACTION  */~
            i$(24)80,                    /* SCREEN CURSOR POSITION     */~
            infomsg$79,                  /* SCREEN INFORMATION MESSAGE */~
            inpmessage$79,               /* SCREEN INFO MESASAGE       */~
            jobdescr$(2)30,              /* JOB DESCR IN DESCRIBES     */~
            job$(2),                     /* JOBS TO BE ENTERED         */~
            labor(2),                    /* LOAD UP FROM BOTH JOBS     */~
            labor$10,                    /* PRINT LABOR AMOUNT SCREEN  */~
            lastjob$(2),                 /* LAST JOBS WE WORKED ON     */~
            lfac$(24)1,                  /* SCREEN LINE FAC            */~
            hdrdate$50,                  /* HEADER FOR PRINT OUT       */~
            material$10,                 /* PRINT MATERIAL  AMOUNT     */~
            material(2),                 /* GET 2 VALUES FROM JOB MASTR*/~
            overhead$10,                 /* PRINT OVERHEAD AMOUNT      */~
            overhead(2),                 /* LOAD 2 OVERHEAD AMOUNTS    */~
            purchases$10,                /* PRINT PURCHASES AMOUNT     */~
            purchases(2),                /* LOAD 2 AMOUNTS FROM JOB MAS*/~
            record$(10)70,               /* LOAD ENTIRE JOB RECORD     */~
            temp$10,                     /* PRINT WITH ERROR ON SCREEN */~
            total$10,                    /* PRINTS THE TOTAL TRANSFERED*/~
            trantomisc(2),               /* LOAD 2 TRANSFER TO MISC    */~
            userid$3,                    /* ID OF THIS USER            */~
            workspace1$24,               /* JOB MASTR NOT USED         */~
            workspace2$32                /* JOB MASTR NOT USED         */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
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
            * # 1 ! JOBMASTR ! JOB COST MASTER FILE                     *~
            * # 2 ! USERINFO ! CHECK TO SEE IF USER IS ON SYSTEM FILE   *~
            *************************************************************

            select #1, "JOBMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select  #2, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))
            call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$(2))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#2, userid$, f1%(2))
            if f1%(2) = 0 then L65000

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, infomsg$, datejobclosed$,  ~
                      labor$, purchases$, material$, overhead$, job$(),  ~
                      freetext$, jobdescr$(), total$
            mat labor = zer : mat purchases = zer : mat material = zer
            mat overhead = zer
            labor, purchases, material, overhead = 0
            pageline% = 1000

            for fieldnr% = 1 to  7
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10230
L10170:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10230:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

L11070:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11070

L11150:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11070

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *SET J% TO 1 FOR JOB (FROM) PROCESSING                      *~
            *SET J% TO 2 FOR JOB (DESTINATION) PROCESSING               *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            lastjob$(1) = job$(1)
            lastjob$(2) = job$(2)
            j% = 1
            gosub L31000
            j% = 2
            gosub L31000
            total = (labor + purchases + material + overhead)
            call "NUMSMASH" (total, 2, total$)
            gosub L51000
            close printer
            go to inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* JOB NUMBER (FROM)*/~
                                    L20200,         /* JOB NUMBER (DEST)*/~
                                    L20300,         /* LABOR     (FROM) */~
                                    L20400,         /* PURCHASES (FROM) */~
                                    L20500,         /* MATERIAL   (FROM)*/~
                                    L20600,         /* OVERHEAD (FROM)  */~
                                    L20700          /* FREE TEXT FIELD  */
                     return
L20100:     REM DEFAULT/ENABLE FOR JOB NUMBER (FROM)
                enabled% = 1
                return

L20200:     REM DEFAULT/ENABLE FOR JOB NUMBER (DESTINATION)
                enabled% = 1
                return

L20300:     REM DEFAULT/ENABLE FOR COST OF LABOR (FROM)
                call "READ100" (#1, job$(1), f1%(1))
                if f1%(1) = 0 then L20360
                get #1, using L20340, labor
L20340:         FMT XX(100), PD(14,4)
                call "NUMSMASH" (labor, 2, labor$)
L20360:         enabled% = 1
                return
L20400:     REM DEFAULT/ENABLE FOR PURCHASES COST (FROM)
                call "READ100" (#1, job$(1), f1%(1))
                if f1%(1) = 0 then L20460
                get #1, using L20440, purchases
L20440:         FMT XX(108), PD(14,4)
                call "NUMSMASH" (purchases, 2, purchases$)
L20460:         enabled% = 1
                return
L20500:     REM DEFAULT/ENABLE FOR MATERIAL COST (FROM)
                call "READ100" (#1, job$(1), f1%(1))
                if f1%(1) = 0 then L20560
                get #1, using L20540, material
L20540:         FMT XX(116), PD(14,4)
                call "NUMSMASH" (material, 2, material$)
L20560:         enabled% = 1
                return
L20600:     REM DEFAULT/ENABLE FOR LABOR OVERHEAD COST (FROM)
                call "READ100" (#1, job$(1), f1%(1))
                if f1%(1) = 0 then L20660
                get #1, using L20640, overhead
L20640:         FMT XX(188), PD(14,4)
                call "NUMSMASH" (overhead, 2, overhead$)
L20660:         enabled% = 1
                return
L20700:     REM DEFAULT/ENABLE FOR FREE TEXT FIELD
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
L29918:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L29942, L29948, L29952
               return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L29952:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L29918


L31000: REM *************************************************************~
            *      W R I T E S  T H E  D A T A                          *~
            * WRITES THE DATA TO THE INPUT (FROM) JOB RECORD TO THE     *~
            * OUTPUT (DESTINATION) JOB RECORD                           *~
            * THE (FROM) JOB--THE AMOUNT THAT THE OPERATOR SPECIFIED    *~
            * FROM LABOR, PURCHASES, MATERIAL OR OVERHEAD IS SUBTRACTED *~
            * FROM THOSE FIELDS IN THE (FROM) JOB AND THE *TOTAL* OF    *~
            * THE AMOUNTS FROMEACH FIELD IS PUT INTO TRANSFERRED TO MISC*~
            * FIELD                                                     *~
            * THE DESTINATION JOB--THE AMOUNTS THAT THE OPERATOR        *~
            * SPECIFIED IN LABOR, PURCHASES, MATERIAL OR OVERHEAD ARE   *~
            * POSTED TO THOSE SAME FIELDS IN THE DESTINATION JOB RECORD *~
            * (FROM) JOB IS JOB$(1) => J% = 1                           *~
            * (DESTINATION) JOB IS JOB$(2) => J% = 2                    *~
            *************************************************************

                if j% < 1 or j% > 2 then return
           REM CONVERT THE AMOUNTS KEYED IN AND ASSAULT THEM AS NUMBERS
                convert labor$ to labor
                convert material$ to material
                convert purchases$ to purchases
                convert overhead$ to overhead

           REM GET THE JOB RECORD (FROM) AND (DESTITNATION)
                call "READ101" (#1, job$(j%), f1%(1))
                if f1%(1) = 0 then return
                get #1, using L31350, str(record$(), 1, 700)
L31350:         FMT CH(700)
           REM NOW THAT WE GET THE 4 COSTS AND THE TRAN TO MICELLANEOUS
                get str(record$(), 101, 96), using L31460,                ~
                                             labor(j%),                  ~
                                             purchases(j%),              ~
                                             material(j%),               ~
                                             workspace1$,                ~
                                             trantomisc(j%),             ~
                                             workspace2$,                ~
                                             overhead(j%)

L31460:         FMT 3 * PD(14,4), CH(24), PD(14,4), CH(32), PD(14,4)

           REM NOW LETS UPDATE THE JOBS INVOVLED IN THIS
                if j% = 2 then  L31600
           REM UPDATED JOB$(1) HERE (FROM) JOB
                labor(j%) = labor(j%) - labor
                purchases(j%) = purchases(j%) - purchases
                material(j%) = material(j%) - material
                overhead(j%) = overhead(j%) - overhead
                trantomisc(j%) = trantomisc(j%) + (labor + purchases +   ~
                                                   material + overhead)

                go to L31700

L31600:    REM UPDATE JOB$(2) HERE (DESTINATION) JOB
                labor(j%) = labor(j%) + labor
                purchases(j%) = purchases(j%) + purchases
                material(j%) = material(j%) + material
                overhead(j%) = overhead(j%) + overhead

L31700:    REM NOW PUT VARIALBLES BACK IN THE RECORD AND RESAVE THE RECORD
                put str(record$(), 101, 96), using L31460,                ~
                                             labor(j%),                  ~
                                             purchases(j%),              ~
                                             material(j%),               ~
                                             workspace1$,                ~
                                             trantomisc(j%),             ~
                                             workspace2$,                ~
                                             overhead(j%)
                rewrite #1, using L31350, str(record$(), 1, 700)
                return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40190,         /* JOB NUMBER (FROM)*/~
                                    L40190,         /* JOB NUMBER (DEST)*/~
                                    L40220,         /* LABOR     (FROM) */~
                                    L40220,         /* PURCHASES (FROM) */~
                                    L40220,         /* MATERIAL   (FROM)*/~
                                    L40220,         /* OVERHEAD (FROM)  */~
                                    L40160          /* FREE TEXT        */
                     goto L40260

L40160:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40220:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "TRANSFER FUNDS BETWEEN JOBS",                         ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "JOB NUMBER (FROM)",                                   ~
               at (06,30), fac(lfac$( 1)), job$(1)              , ch(08),~
               at (06,49), fac(hex(8c)),   jobdescr$(1)         , ch(30),~
               at (07,02),                                               ~
                  "JOB NUMBER (DESTINATION)",                            ~
               at (07,30), fac(lfac$( 2)), job$(2)              , ch(08),~
               at (07,49), fac(hex(8c)),   jobdescr$(2)         , ch(30),~
               at (08,02),                                               ~
                  "LABOR COST (FROM)",                                   ~
               at (08,30), fac(lfac$( 3)), labor$               , ch(10),~
               at (09,02),                                               ~
                  "PURCHASES COST (FROM)",                               ~
               at (09,30), fac(lfac$( 4)), purchases$           , ch(10),~
               at (10,02),                                               ~
                  "MATERIAL COST (FROM)",                                ~
               at (10,30), fac(lfac$( 5)), material$            , ch(10),~
               at (11,02),                                               ~
                  "LABOR OVERHEAD COST (FROM)",                          ~
               at (11,30), fac(lfac$( 6)), overhead$            , ch(10),~
               at (12,02),                                               ~
                    "FREE TEXT",                                         ~
               at (12,30), fac(lfac$( 7)), freetext$            , ch(40),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("JOBTOJOB")
                  goto L40260

L40690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40260

        REM *************************************************************~
            *      E D I T  P A G E   1                                 *~
            *                                                           *~
            * EDITS THE FIRST PAGE OF THE SCREEN                        *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41200,         /* JOB NUMBER (FROM)*/~
                                    L41200,         /* JOB NUMBER (DEST)*/~
                                    L41230,         /* LABOR     (FROM) */~
                                    L41230,         /* PURCHASES (FROM) */~
                                    L41230,         /* MATERIAL   (FROM)*/~
                                    L41230,         /* OVERHEAD (FROM)  */~
                                    L41170          /* FREE TEXT        */
                     goto L41270

L41170:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41200:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41230:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41270:     accept                                                       ~
               at (01,02),                                               ~
                  "TRANSFER FUNDS BETWEEN JOBS",                         ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "JOB NUMBER (FROM)",                                   ~
               at (06,30), fac(lfac$( 1)), job$(1)              , ch(08),~
               at (06,49), fac(hex(8c)),   jobdescr$(1)         , ch(30),~
               at (07,02),                                               ~
                  "JOB NUMBER (DESTINATION)",                            ~
               at (07,30), fac(lfac$( 2)), job$(2)              , ch(08),~
               at (07,49), fac(hex(8c)),   jobdescr$(2)         , ch(30),~
               at (08,02),                                               ~
                  "LABOR COST (FROM)",                                   ~
               at (08,30), fac(lfac$( 3)), labor$               , ch(10),~
               at (09,02),                                               ~
                  "PURCHASES COST (FROM)",                               ~
               at (09,30), fac(lfac$( 4)), purchases$           , ch(10),~
               at (10,02),                                               ~
                  "MATERIAL COST (FROM)",                                ~
               at (10,30), fac(lfac$( 5)), material$            , ch(10),~
               at (11,02),                                               ~
                  "LABOR OVERHEAD COST (FROM)",                          ~
               at (11,30), fac(lfac$( 6)), overhead$            , ch(10),~
               at (12,02),                                               ~
                    "FREE TEXT",                                         ~
               at (12,30), fac(lfac$( 7)), freetext$            , ch(40),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS    (15)PRINT SCREEN",                ~
               at (24,65),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41730
                  call "MANUAL" ("JOBTOJOB")
                  goto L41270

L41730:        if keyhit% <> 15 then L41770
                  call "PRNTSCRN"
                  goto L41270

L41770:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50100,         /* JOB NUMBER (FROM)*/~
                                    L50200,         /* JOB NUMBER (DEST)*/~
                                    L50300,         /* MATERIALS (FROM) */~
                                    L50400,         /* PURCHASES (FROM) */~
                                    L50500,         /* LABOR COST (FROM)*/~
                                    L50700,         /* OVERHEAD (FROM)  */~
                                    L50830          /* FREE TEXT FIELD  */
                     return
L50100:     REM TEST DATA FOR JOB NUMBER (FROM)
               if job$(1) = " " then L50180
               call "DESCRIBE" (#1, job$(1), jobdescr$(1), 0%, f1%(1))
               if f1%(1) = 0 then L50175
               get #1, using L50150, datejobclosed$
L50150:        FMT XX(44), CH(6)
               if datejobclosed$ = " " or datejobclosed$ = blankdate$ ~
                                                          then return
               call "DATEFMT" (datejobclosed$)
               infomsg$ = "JOB CLOSING DATE: " & datejobclosed$
               return
L50175:        errormsg$ = "JOB NOT ON FILE: " & job$(1)  :  return
L50180:        errormsg$ = "JOB NUMBER CANNOT BE BLANK"   :  return
L50200:     REM TEST DATA FOR JOB NUMBER (DESTINATION)
               if job$(2) = " " then L50255
               call "DESCRIBE" (#1, job$(2), jobdescr$(2), 0%, f1%(1))
               if f1%(1) = 0 then L50250
               get #1, using L50225, datejobclosed$
L50225:        FMT XX(44), CH(6)
               if datejobclosed$ = " " or datejobclosed$ = blankdate$ ~
                                                          then return
               call "DATEFMT" (datejobclosed$)
               infomsg$ = "JOB CLOSING DATE: " & datejobclosed$
               return
L50250:        errormsg$ = "JOB NOT ON FILE: " & job$(2)  :  return
L50255:        errormsg$ = "JOB NUMBER CANNOT BE BLANK"   :  return
L50300:     REM TEST DATA FOR COST OF LABOR (FROM)
                if labor$ = " " then labor$ = "0.00"
                   call "NUMVALID" (labor$, err%, 2)
                   if err% = 0 then L50380
                      errormsg$="ILLEGAL ENTRY FOR UNIT COST: "          ~
                                         & labor$
                   return
L50380:         convert labor$ to temp
                if temp <= labor then return
                call "NUMSMASH" (labor, 2, temp$)
                errormsg$ = "AMOUNT POSTED CANNOT BE GREATER THAN TOTAL L~
        ~ABOR AMOUNT: " & temp$
                return
L50400:     REM TEST DATA FOR PURCHASES COST (FROM)
                if purchases$ = " " then purchases$ = "0.00"
                   call "NUMVALID" (purchases$, err%, 2)
                   if err% = 0 then L50460
                      errormsg$="ILLEGAL ENTRY FOR PURCHASE COST: "      ~
                                         & purchases$
                   return
L50460:         convert purchases$ to temp
                if temp <= purchases then return
                call "NUMSMASH" (purchases, 2, temp$)
                errormsg$ = "AMOUNT POSTED CANNOT BE GREATER THAN TOTAL P~
        ~URCHASES AMOUNT: " & temp$
                return
L50500:     REM TEST DATA FOR MATERIAL COST (FROM)
                if material$ = " " then material$ = "0.00"
                   call "NUMVALID" (material$, err%, 2)
                   if err% = 0 then L50560
                      errormsg$="ILLEGAL ENTRY FOR MATERIAL COST: "      ~
                                         & material$
                   return
L50560:         convert material$ to temp
                if temp <= material then return
                call "NUMSMASH" (material, 2, temp$)
                errormsg$ = "AMOUNT POSTED CANNOT BE GREATER THAN TOTAL M~
        ~ATERIAL AMOUNT: " & temp$
                return
L50700:     REM TEST DATA FOR LABOR OVERHEAD COST (FROM)
                if overhead$ = " " then overhead$ = "0.00"
                   call "NUMVALID" (overhead$, err%, 2)
                   if err% = 0 then L50770
                      errormsg$="ILLEGAL ENTRY FOR OVERHEAD COST"        ~
                                         & overhead$
                   return
L50770:         convert overhead$ to temp
                if temp <= overhead then return
                call "NUMSMASH" (overhead, 2, temp$)
                errormsg$ = "AMOUNT POSTED CANNOT BE GREATER THAN TOTAL O~
        ~VERHEAD AMOUNT: " & temp$
                return
L50830:     REM TEST DATA FOR FREETEXT FIELD
                return

L51000: rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                if pageline% + 6 > 64 then pageline% = 1000
                               /* PAGE FEED IF NOT ENOUGH THIS PAGE.   */
                               /* (FOOLS LINE COUNT BY SAYING WE'RE AT */
                               /* WAY BEYOND END OF PAGE.              */

                           pageline% = pageline% + 1 /* FOR EXTRA LINE */

                gosub L60000              /* PAGE HEADING, IF NECCESSARY*/

             print using L52200, job$(1), job$(2), labor$, purchases$,    ~
                      material$, overhead$, total$
             print using L52120
             print using L52220, freetext$
             return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING.                         *~
            *************************************************************

L52070: %PAGE ######         W I P  /  J C    T R A N S F E R   F U N D S~
        ~ T O  J O B  ####################################################~
        ~###
L52120: %+------------+-------------+----------+-----------+----------+--~
        ~---------+--------------------------------------+
L52140: %! JOB NUMBER ! JOB NUMBER  !   LABOR  ! PURCHASES ! MATERIAL ! O~
        ~VERHEAD  ! TOTAL TRANSFERRED OUT OF (FROM) JOB  !
L52200: %!  ########  !   ########  !##########! ##########!##########! #~
        ~#########             ##########                !
L52220: %!  COMMENTS; ###########################################        ~
        ~                                                !
L52230: %!  (FROM)    !(DESTINATION)! (DEST.)  !  (DEST.)  !  (DEST.) ! (~
        ~DEST.)   ! RECORDED IN TRANSFER TO MISCELLANEOUS!

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************
            select printer (134)
            pageline% = pageline% + 1
            if pageline% < 64 then return
               print page
               call "DATE" addr ("HD", hdrdate$)
               pagenumber% = pagenumber% + 1
               print using L52070, pagenumber%, hdrdate$
               print skip (1)
               print using L52120
               print using L52140
               print using L52230
               print using L52120
               pageline% = 8
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
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
