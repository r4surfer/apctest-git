        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC    OOO   M   M                                      *~
            *  C   C  O   O  MM MM                                      *~
            *  C      O   O  M M M  S U B       A SUBROUTINE VERSION    *~
            *  C   C  O   O  M   M              OF COMINPUT WITH SEARCH *~
            *   CCC    OOO   M   M              CAPABILITY              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * COMSUB   - GENERAL SUBROUTINE TO LOCATE COMMON TERMS BY   *~
            *            TERM OR BY DESCRIPTION.  'DO%' DETERMINES THE  *~
            *            NATURE OF THE ACTION.  'PASS$' HOLDS ALL 160   *~
            *            CHARACTERS IN THE 'COMTERM' FILE FOR THE TERM. *~
            *            'RSLT%' HOLDS THE RESULTS OF THE USERS ACTIONS.*~
            *            #1 = THE CHANNEL FOR THE 'COMTERM' FILE.       *~
            *                                                           *~
            *      WHERE DO% = 1 = FIND THIS ONE AND PASS BACK ALL DATA *~
            *       IF RSLT% = 1 = TERM IS ON FILE & PASS$ = RECORD     *~
            *                  9 = DIFFERENT TERM THAN WAS PASSED IN IS *~
            *                      NOW IN PASS$, USER EITHER SELECTED A *~
            *                      DIFFERENT ONE OR DEFINED A NEW ONE.  *~
            *      WHERE DO% = 0 = SEARCH FUNCTION, USER SETS CRITERIA  *~
            *       IF RSLT% = 0 = PASS$ WAS NOT CHANGED, USER LOOKED   *~
            *                      BUT DID NOT SELECT OR ENTER ONE.     *~
            *                  9 = PASS$ HAS BEEN CHANGED AS ABOVE,     *~
            *                      USER EXITED FROM SEARCH BY (ENTER)   *~
            *                      "RETURN WITH THE ONE MARKED".        *~
            *      WHERE DO% = 9 = EXPLICIT TEST, FIND THIS ONE ONLY    *~
            *                                                           *~
            *     WHERE DEL% = 0 = USER CANNOT DELETE TERMS FROM FILE   *~
            *           DEL% = 1 = USER CAN    DELETE TERMS FROM FILE   *~
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
            * 11/10/83 ! ORIGINAL                                 ! GLW *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "COMSUB" (#1, do%, pass$, del%, rslt%)

        dim                                                              ~
            a$(10)1,                     /* FAC HOLDERS FOR SCREEN     */~
            ctdescr$30,                  /* DESCRIPTION OF TERM        */~
            ctterm$16,                   /* COMMON TERM                */~
            cttext1$50,                  /* FREE TEXT FIELD            */~
            cttext2$50,                  /* FREE TEXT FIELD            */~
            cttype$16,                   /* TYPE OR CLASS OF THIS TERM */~
            dterm$(10)16, ddescr$(10)30, dtype$(10)16, dbox$(10)1,       ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            topmsg$79,                   /* TOP OF INPUT SCREEN MSG    */~
            sclass$16,                   /* SEARCH CLASS               */~
            sterm$16,                    /* SEARCH TERM                */~
            sdescr$30,                   /* SEARCH DESCRIPTION         */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lline$(2)79,                 /* PF KEY PROMPT LINES        */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            plowkey$100,                 /* GENERAL PURPOSE PLOW KEY   */~
            pass$162                     /* RECORD PASSED BACK         */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

            mat f2% = con : cms2v$ = "YYMMCCCCCCCCCCCCCCCC" : cms2v$ = " "

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *    THIS IS A SUBROUTINE SO THE FILES ARE PASSED IN        *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! COMTERM  ! File of common terms for personel.       *~
            *************************************************************~
            *  THIS FILE MUST BE SELECTED AND OPENED IN CALLING PGM     *~
            *       FILE SELECTION AND OPEN CALLS                       *

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "ALL FIELDS ARE FREELY MODIFYABLE BY Cursor To ~
        ~ Desired Value And Press (ENTER)."

           lline$(1%) =                                                  ~
        "(5)SEE NEXT  PAGE OF TERMS                              (15)PRIN~
        ~T THIS SCREEN"

           lline$(2%) =                                                  ~
        "(5)SEE NEXT  PAGE OF TERMS    (12)DELETE TERMS MARKED   (15)PRIN~
        ~T THIS SCREEN"

        REM SET DEL% TO BE SUBSCRIPT IN ACCEPT.  ADD 1 TO IT, THEN TEST
           del% = del% + 1%
           if del% = 1% then goto L01170
           if del% = 2% then goto L01170
           del% = 1%

L01170: REM *************************************************************~
            *   DETERMINE THE TYPE OF ACTION DESIRED AND BRANCH         *~
            *                                                           *~
            *************************************************************

           if do% = 1% then goto L01300        /* TEST, FIND, ENTER */
           if do% = 9% then goto  L01410       /* TEST ONLY         */
           gosub searchmode                  /* SEARCH ONLY       */
           goto L09000

        REM THIS IS CHECK MODE, SIMULAR TO 'DESCRIBE' BUT WITH THE       ~
            ABILITY TO SELECT ONE CLOSE, OR ENTER A NEW ONE.

L01300:    call "READ100" (#1, str(pass$,47,16), f1%(1))
                     if f1%(1) =  1% then goto L01360
                     gosub searchmode
                     goto L09000

                     REM DESIRED ONE FOUND, GET IT AND PASS BUFFER BACK
L01360:              get #1, using L01380, pass$
                     rslt% = 1%
L01380:              FMT CH(162)
                     goto L09000

L01410:    call "READ100" (#1, str(pass$,47,16), f1%(1))
                     if f1%(1) =  1% then goto L01450
                     rslt% = 0%
                     goto L09000
L01450:              get #1, using L01470, pass$
                     rslt% = 1%
L01470:              FMT CH(162)
                     goto L09000

        REM *************************************************************~
            *       I N P U T   M O D E   E N T E R   N E W   O N E     *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, ctterm$, ctdescr$, cttype$,~
                     cttext1$, cttext2$

            ctterm$ = str(pass$,47,16)

            for fieldnr% = 1 to  5
L01630:         gosub'051(fieldnr%)
                      if enabled% = 0 then  L01720
L01650:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
               if keyhit%  = 16 and fieldnr% = 1 then goto L06710
                      if keyhit% =  4% then goto  L01730
                      if keyhit% <>  0 then       L01650
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L01650
L01720:         if keyhit% <> 4% then goto L01750
L01730:               fieldnr% = max(1%, fieldnr% - 1%)
                      goto L01630
L01750:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
        editmode

L01840:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L01840
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  5 then L01840

L01910:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L01910
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L01910
            goto L01840

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L03020

         put pass$ , using L03420 ,                                        ~
                     cttype$,         /* THE TYPE OR CLASS OF TERM     */~
                     ctdescr$,        /* DESCRIPTION OF THIS  TERM     */~
                     ctterm$,         /* THE TERM ITSELF               */~
                     cttext1$,        /* FREE TEXT LINE 1              */~
                     cttext2$         /* FREE TEXT LINE 2              */

           if do% = 1% then goto L02160
           goto L01170
L02160:    rslt% = 9%
           goto L09000

        REM *************************************************************~
            *             L O A D   D A T A   O N   F I L E             *~
            *                                                           *~
            *************************************************************

        dataload
            gosub L03220
            goto editmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L02420 ,         /* COMMON TERM      */~
                                    L02450 ,         /* TYPE OF TERM     */~
                                    L02490 ,         /* DESCRIPTION      */~
                                    L02530 ,         /* FREE TEXT FIELD  */~
                                    L02570           /* FREE TEXT FIELD  */
                     return
L02420:     REM DEFAULT/ENABLE FOR COMMON TERM
           inpmessage$ = "ENTER THE TERM YOU WISH TO DEFINE"
                return
L02450:     REM DEFAULT/ENABLE FOR TYPE OR CLASS OF THIS TERM
           inpmessage$ = "WHAT TYPE (CLASS) OF TERM IS THIS, IE: 'JOB TIT~
        ~LE', 'SKILL', 'INSURANCE TYPE', ECT."
                return
L02490:     REM DEFAULT/ENABLE FOR DESCRIPTION OF TERM
           inpmessage$ = "HOW WOULD YOU BRIEFLY DESCRIB THIS TERM?       ~
        ~                                    "
                return
L02530:     REM DEFAULT/ENABLE FOR FREE TEXT FIELD
           inpmessage$ = "ENTER ANY FURTHER EXPLANATIONS OR SPECIAL CONSI~
        ~DERATIONS FOR THE TERM " & ctterm$
                return
L02570:     REM DEFAULT/ENABLE FOR FREE TEXT FIELD
           inpmessage$ = "ENTER ANY FURTHER EXPLANATIONS OR SPECIAL CONSI~
        ~DERATIONS FOR THE TERM " & ctterm$
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
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L02810:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L02930 , L02960 , L02980
               return

L02930:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L02960:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L02980:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L02810

L03020: REM *************************************************************~
            *        W R I T E   D A T A   T O   D I S K                *~
            *                                                           *~
            *************************************************************

           REM FIRST DELETE WHATEVER IS TO BE SAVED
                     call "READ101" (#1, ctterm$, f1%(1))
                     if f1%(1) = 1% then delete #1

           REM NOW WRITE IT TO FILE

           write #1, using L03420 ,                                        ~
                     cttype$,         /* THE TYPE OR CLASS OF TERM     */~
                     ctdescr$,        /* DESCRIPTION OF THIS  TERM     */~
                     ctterm$,         /* THE TERM ITSELF               */~
                     cttext1$,        /* FREE TEXT LINE 1              */~
                     cttext2$         /* FREE TEXT LINE 2              */

           return

L03220: REM *************************************************************~
            *         L O A D   D A T A   F R O M   D I S K             *~
            *                                                           *~
            *************************************************************

             get #1, using L03420 ,                                        ~
                     cttype$,         /* THE TYPE OR CLASS OF TERM     */~
                     ctdescr$,        /* DESCRIPTION OF THIS  TERM     */~
                     ctterm$,         /* THE TERM ITSELF               */~
                     cttext1$,        /* FREE TEXT LINE 1              */~
                     cttext2$         /* FREE TEXT LINE 2              */

           return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L03420: FMT                      /* FILE: COMTERM                      */~
            CH(16),              /* Common terms type                  */~
            CH(30),              /* Common terms description           */~
            CH(16),              /* Common term for personel           */~
            CH(50),              /* Common terms free text 1           */~
            CH(50)               /* Common terms free text 2           */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L03670 ,         /* COMMON TERM      */~
                                    L03670 ,         /* TYPE OF TERM     */~
                                    L03670 ,         /* DESCRIPTION      */~
                                    L03640 ,         /* FREE TEXT FIELD  */~
                                    L03640           /* FREE TEXT FIELD  */
                     goto L03740

L03640:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L03670:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L03740:     accept                                                       ~
               at (01,02),                                               ~
                  "MANAGE (ENTER OR EDIT) PERSONNEL COMMON TERMS",       ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "COMMON TERM",                                         ~
               at (06,30), fac(lfac$( 1)), ctterm$              , ch(16),~
               at (07,02),                                               ~
                  "TYPE OR CLASS OF THIS TERM",                          ~
               at (07,30), fac(lfac$( 2)), cttype$              , ch(16),~
               at (08,02),                                               ~
                  "DESCRIPTION OF TERM",                                 ~
               at (08,30), fac(lfac$( 3)), ctdescr$             , ch(30),~
               at (09,02),                                               ~
                  "FREE TEXT FIELD",                                     ~
               at (09,30), fac(lfac$( 4)), cttext1$             , ch(50),~
               at (10,02),                                               ~
                  "FREE TEXT FIELD",                                     ~
               at (10,30), fac(lfac$( 5)), cttext2$             , ch(50),~
                                                                         ~
               at (15,02),"VALID TERM TYPES ARE :  LANGUAGE, JOB TITLE, E~
        ~EO JOB CLASS, INTERVIEWER,",                                     ~
               at (16,26),                        "SKILL, DEPARTMENT, DIV~
        ~ISION, INSURANCE, AND BENEFIT.",                                 ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER     (4)PREVIOUS FIELD",                 ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,20),                                               ~
               "(16)ABORT NEW TERM INPUT AND RESELECT SEARCH CRITERIA",  ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)


               if keyhit% <> 13 then L04260
                  call "MANUAL" ("COMINPUT")
                  goto L03740

L04260:        if keyhit% <> 15 then L04300
                  call "PRNTSCRN"
                  goto L03740

L04300:    return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L04500 ,         /* COMMON TERM      */~
                                    L04500 ,         /* TYPE OF TERM     */~
                                    L04500 ,         /* DESCRIPTION      */~
                                    L04470 ,         /* FREE TEXT FIELD  */~
                                    L04470           /* FREE TEXT FIELD  */
                     goto L04570

L04470:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L04500:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L04570:     accept                                                       ~
               at (01,02),                                               ~
                  "EDIT PERSONNEL COMMON TERMS",                         ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "COMMON TERM",                                         ~
               at (06,30), fac(lfac$( 1)), ctterm$              , ch(16),~
               at (07,02),                                               ~
                  "TYPE OR CLASS OF THIS TERM",                          ~
               at (07,30), fac(lfac$( 2)), cttype$              , ch(16),~
               at (08,02),                                               ~
                  "DESCRIPTION OF TERM",                                 ~
               at (08,30), fac(lfac$( 3)), ctdescr$             , ch(30),~
               at (09,02),                                               ~
                  "FREE TEXT FIELD",                                     ~
               at (09,30), fac(lfac$( 4)), cttext1$             , ch(50),~
               at (10,02),                                               ~
                  "FREE TEXT FIELD",                                     ~
               at (10,30), fac(lfac$( 5)), cttext2$             , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(00010d0e0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L05010
                  call "MANUAL" ("COMINPUT")
                  goto L04570

L05010:        if keyhit% <> 15 then L05060
                  call "PRNTSCRN"
                  goto L04570


L05060:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L05240 ,         /* COMMON TERM      */~
                                    L05340 ,         /* TYPE OF TERM     */~
                                    L05390 ,         /* DESCRIPTION      */~
                                    L05430 ,         /* FREE TEXT FIELD  */~
                                    L05450           /* FREE TEXT FIELD  */
                     return
L05240:     REM TEST DATA FOR COMMON TERM
            if ctterm$ <> " " then goto L05290
            errormsg$ = "TERM CAN NOT BE BLANK, PLEASE REENTER"
            return

L05290:              call "READ100" (#1, ctterm$, f1%(1))
                     if f1%(1) = 0% then return
                     return clear
                     goto dataload

L05340:     REM TEST DATA FOR TYPE OR CLASS OF THIS TERM
            gosub testtypes
                return


L05390:     REM TEST DATA FOR DESCRIPTION OF TERM
           if ctdescr$ <> " " then return
           errormsg$ = "DESCRIPTION CANNOT BE BLANK, PLEASE ENTER"
                return
L05430:     REM TEST DATA FOR FREE TEXT FIELD
                return
L05450:     REM TEST DATA FOR FREE TEXT FIELD
                return

        REM *************************************************************~
            *          S E A R C H   M O D E                            *~
            * SEARCHES FOR RECORDS CLOSEST TO THE RANGE ENTERED.        *~
            *    SEARCHES ON EITHER TERM OR DESCRIPTION                 *~
            *    IF DO% = 1%, YOU GET HERE BECAUSE THE TERM CHECKED IS  *~
            *             NOT NOW ON FILE.  SO PLOW ON 1/2 LEN TO GET   *~
            *             THOSE THAT ARE CLOSE TO THE ONE WANTED.       *~
            *    IF DO% = 0%, YOU JUST WANT TO SEE THE OPTIONS SO LET   *~
            *             USER SET PLOW CRITERIA                        *~
            *    IN EITHER CASE THE USER CAN SELECT ONE SHOWN OR CAN    *~
            *    ENTER A NEW ONE.  RSLT% WILL DEPEND ON THE ACTION      *~
            *    TAKEN.                                                 *~
            *************************************************************

        searchmode

           if do% <> 1% then goto L05950  /* SELECT SEARCH CRITERIA */
           goto L05820                    /* FIND CLOSEST TERMS     */

        REM THIS IS THE ENTRY POINT FOR THE INPUTMODE GOSUB

           init(hex(00)) plowkey$
           b% = len(cttype$)
           if b% = 0% then goto L05750
           if b% = 1% then goto L05740
           b% = b% / 2%
L05740:    str(plowkey$,1,b%)= str(cttype$,1, b%)
L05750:    key% = 2%
           topmsg$ =                                                     ~
        "THE CLOSEST TYPES (CLASSES) OF TERMS TO " & cttype$ & " ARE SHOW~
        ~N BELOW"
           goto  L07000

        REM FIND THE CLOSEST TERMS TO THE ONE PASSED IN
L05820:    temp$ = str(pass$,47,16)
           init(hex(00)) plowkey$
           b% = len(temp$)
           if b% = 0% then goto L05890
           if b% = 1% then goto L05880
           b% = b% / 2%
L05880:    str(plowkey$,1,b%)= str(pass$,47, b%)
L05890:    key% = 0%
           topmsg$ = "THE TERM " & str(pass$,47,16) & " IS NOT ON FILE.  ~
        ~ THE CLOSEST TO IT ARE SHOWN BELOW"
           goto  L07000


L05950: REM MANUALLY SPECIFY THE SEARCH CRITERIA

           sterm$ = str(pass$,47,16)
           sdescr$ = str(pass$,17,30)

L06000: accept                                                           ~
               at (01,20),                                               ~
        "FIND COMMON PERSONNEL TERMS, DESCRIPTIONS, AND CLASSES OF TERMS"~
             , at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "! YOU CAN LOCATE COMMON PERSONNEL TERMS BY SEARCHING FOR SOME PO~
        ~RTION       !",                                                  ~
               at (05,03),                                               ~
        "! OF THE TERM ITSELF, OR IT'S DESCRIPTION, OR PORTIONS OF CLASSE~
        ~S.  PLEASE  !",                                                  ~
               at (06,03),                                               ~
        "! SELECT THE SEARCH CRITERIA YOU WISH BY FILLING IN WHATEVER YOU~
        ~ KNOW ABOUT !",                                                  ~
               at (07,03),                                               ~
        "! THE TERM, IT'S DESCRIPTION, OR A CLASS, THEN PRESS (ENTER).  I~
        ~F YOU LEAVE !",                                                  ~
               at (08,03),                                               ~
        "! THE SEARCH CRITERIA BLANK THEN THE FIRST ONES ON FILE WILL BE ~
        ~SHOWN.      !",                                                  ~
               at (09,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (10,03),                                               ~
        "! THE TERMS THAT START WITH YOUR CRITERIA WILL BE SHOWN UP TO 10~
        ~ AT A TIME. !",                                                  ~
               at (11,03),                                               ~
        "! A BRIGHT BOX WILL APPEAR BESIDE EACH ONE.  TO TRANSPORT A GIVE~
        ~N TERM      !",                                                  ~
               at (12,03),                                               ~
        "! BACK WITH YOU, PLACE AN 'X' IN THE BOX BEFORE YOU PRESS (ENTER~
        ~).          !",                                                  ~
               at (13,03),                                               ~
        "! IF YOU DON'T NEED TO TRANSPORT A GIVEN TERM BACK, JUST PRESS (~
        ~ENTER).     !",                                                  ~
               at (14,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (16,03),                                               ~
        "SEARCH FOR ALL TERMS        BEGINNING WITH",                    ~
               at (16,47),                                               ~
        fac(hex(81)), sterm$, ch(16),                                    ~
               at (16,65),                                               ~
        fac(hex(98)), x$, ch(1),                                         ~
               at (17,03),                                               ~
        "SEARCH FOR ALL DESCRIPTIONS BEGINNING WITH",                    ~
               at (17,47),                                               ~
        fac(hex(81)), sdescr$, ch(30),                                   ~
               at (17,79),                                               ~
        fac(hex(98)), x$, ch(1),                                         ~
               at (18,03),                                               ~
        "SEARCH FOR ALL CLASSES      BEGINNING WITH",                    ~
               at (18,47),                                               ~
        fac(hex(81)), sclass$, ch(16),                                   ~
               at (18,79),                                               ~
        fac(hex(98)), x$, ch(1),                                         ~
               at (24,13),                                               ~
        "FILL IN THE DESIRED SEARCH CRITERIA AND THEN PRESS (ENTER)",    ~
           keys(hex(000f)), key(hk%)

           if hk% <> 15% then goto L06650
                     call "PRNTSCRN"
                     goto L06000

L06650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
L06670:        if cursor%(1%) = 16% or cursor%(1%) = 17% then goto L06710
               if cursor%(1%) = 18%                      then goto L06710
                     goto L06000

L06710:    topmsg$ = "            THESE ARE THE TERMS THAT START WITH YOU~
        ~R SEARCH CRITERIA"
           if cursor%(1%) = 17% then goto L06850     /* DESCR */

           if cursor%(1%) = 18% then goto L06930     /* CLASS */

           b% = len(sterm$)
           if sterm$ = " " then b% = 0%
           init(hex(00)) plowkey$
           if b% = 0% then goto L06820
           str(plowkey$, 1, b%) = str(sterm$, 1, b%)
L06820:    key% = 0%
           goto L07000

L06850:    b% = len(sdescr$)
           if sdescr$ = " " then b% = 0%
           init(hex(00)) plowkey$
           if b% = 0% then goto L06900
           str(plowkey$, 1,b%) = str(sdescr$,1, b%)
L06900:    key% = 1%
           goto L07000

L06930:    b% = len(sclass$)
           if sclass$ = " " then b% = 0%
           init(hex(00)) plowkey$
           if b% = 0% then goto L06980
           str(plowkey$, 1,b%) = str(sclass$,1, b%)
L06980:    key% = 2%

L07000:    init (" ") dterm$(), ddescr$(), dtype$(), dbox$()
           cnt%, maxcnt% = 0%
           ddescr$(1%) = "ALL MATCHING TERMS SHOWN"

L07040:    call "PLOWALTS" (#1, plowkey$, key%, b%, f1%(1%))
                     if f1%(1%) <> 1% then goto   L07110
           cnt%, maxcnt% = cnt% + 1%
           get #1, using L07080 , dtype$(cnt%), ddescr$(cnt%), dterm$(cnt%)
L07080:              FMT CH(16), CH(30), CH(16)
           if maxcnt% < 10% then goto  L07040

L07110:    init(hex(9c)) a$()
           if maxcnt% = 0% then goto L07170
           for i% = 1% to maxcnt%
           init(hex(80)) a$(i%)
           next i%

L07170: accept                                                           ~
               at (01,31),                                               ~
        "COMMON PERSONNEL TERMS",                                        ~
               at (03,03),                                               ~
        fac(hex(84)), topmsg$, ch(78),                                   ~
               at (05,06),                                               ~
        "+---------------------------------------------------------------~
        ~-------+",                                                       ~
               at (06,06),                                               ~
        "!  TERM              DESCRIPTION NOW ON FILE         TYPE OF TER~
        ~M      !",                                                       ~
               at (07,06),                                               ~
        "+---------------------------------------------------------------~
        ~-------+",                                                       ~
               at (08,06),                                               ~
        "!",                                                             ~
               at (08,77),                                               ~
        "!",                                                             ~
               at (09,06),                                               ~
        "!",                                                             ~
               at (09,77),                                               ~
        "!",                                                             ~
               at (10,06),                                               ~
        "!",                                                             ~
               at (10,77),                                               ~
        "!",                                                             ~
               at (11,06),                                               ~
        "!",                                                             ~
               at (11,77),                                               ~
        "!",                                                             ~
               at (12,06),                                               ~
        "!",                                                             ~
               at (12,77),                                               ~
        "!",                                                             ~
               at (13,06),                                               ~
        "!",                                                             ~
               at (13,77),                                               ~
        "!",                                                             ~
               at (14,06),                                               ~
        "!",                                                             ~
               at (14,77),                                               ~
        "!",                                                             ~
               at (15,06),                                               ~
        "!",                                                             ~
               at (15,77),                                               ~
        "!",                                                             ~
               at (16,06),                                               ~
        "!",                                                             ~
               at (16,77),                                               ~
        "!",                                                             ~
               at (17,06),                                               ~
        "!",                                                             ~
               at (17,77),                                               ~
        "!",                                                             ~
               at (18,06),                                               ~
        "+---------------------------------------------------------------~
        ~-------+",                                                       ~
               at (19,08),                                               ~
        "PLACE AN 'X' IN THE BOX BESIDE A TERM YOU WISH TO TRANSPORT WITH~
        ~ YOU",                                                           ~
               at (21,03),                                               ~
        "PF KEYS ACTIVE:",                                               ~
               at (22,03),                                               ~
        "(ENTER)RETURN - WITH THE FIRST ONE MARKED           (8)SELECT SE~
        ~ARCH CRITERIA",                                                  ~
               at (23,03),                                               ~
        "(1)SEE FIRST PAGE OF TERMS                              (10)DEFI~
        ~NE A NEW TERM",                                                  ~
               at (24,03),                                               ~
        fac(hex(8c)), lline$(del%), ch(78),                              ~
           at(08,03), fac(a$(01) ),    dbox$(01%)             ,ch(01),   ~
           at(08,09), fac(hex(84)),   dterm$(01%)             ,ch(16),   ~
           at(08,27), fac(hex(84)),  ddescr$(01%)             ,ch(30),   ~
           at(08,59), fac(hex(84)),   dtype$(01%)             ,ch(16),   ~
           at(09,03), fac(a$(02) ),    dbox$(02%)             ,ch(01),   ~
           at(09,09), fac(hex(84)),   dterm$(02%)             ,ch(16),   ~
           at(09,27), fac(hex(84)),  ddescr$(02%)             ,ch(30),   ~
           at(09,59), fac(hex(84)),   dtype$(02%)             ,ch(16),   ~
           at(10,03), fac(a$(03) ),    dbox$(03%)             ,ch(01),   ~
           at(10,09), fac(hex(84)),   dterm$(03%)             ,ch(16),   ~
           at(10,27), fac(hex(84)),  ddescr$(03%)             ,ch(30),   ~
           at(10,59), fac(hex(84)),   dtype$(03%)             ,ch(16),   ~
           at(11,03), fac(a$(04) ),    dbox$(04%)             ,ch(01),   ~
           at(11,09), fac(hex(84)),   dterm$(04%)             ,ch(16),   ~
           at(11,27), fac(hex(84)),  ddescr$(04%)             ,ch(30),   ~
           at(11,59), fac(hex(84)),   dtype$(04%)             ,ch(16),   ~
           at(12,03), fac(a$(05) ),    dbox$(05%)             ,ch(01),   ~
           at(12,09), fac(hex(84)),   dterm$(05%)             ,ch(16),   ~
           at(12,27), fac(hex(84)),  ddescr$(05%)             ,ch(30),   ~
           at(12,59), fac(hex(84)),   dtype$(05%)             ,ch(16),   ~
           at(13,03), fac(a$(06) ),    dbox$(06%)             ,ch(01),   ~
           at(13,09), fac(hex(84)),   dterm$(06%)             ,ch(16),   ~
           at(13,27), fac(hex(84)),  ddescr$(06%)             ,ch(30),   ~
           at(13,59), fac(hex(84)),   dtype$(06%)             ,ch(16),   ~
           at(14,03), fac(a$(07) ),    dbox$(07%)             ,ch(01),   ~
           at(14,09), fac(hex(84)),   dterm$(07%)             ,ch(16),   ~
           at(14,27), fac(hex(84)),  ddescr$(07%)             ,ch(30),   ~
           at(14,59), fac(hex(84)),   dtype$(07%)             ,ch(16),   ~
           at(15,03), fac(a$(08) ),    dbox$(08%)             ,ch(01),   ~
           at(15,09), fac(hex(84)),   dterm$(08%)             ,ch(16),   ~
           at(15,27), fac(hex(84)),  ddescr$(08%)             ,ch(30),   ~
           at(15,59), fac(hex(84)),   dtype$(08%)             ,ch(16),   ~
           at(16,03), fac(a$(09) ),    dbox$(09%)             ,ch(01),   ~
           at(16,09), fac(hex(84)),   dterm$(09%)             ,ch(16),   ~
           at(16,27), fac(hex(84)),  ddescr$(09%)             ,ch(30),   ~
           at(16,59), fac(hex(84)),   dtype$(09%)             ,ch(16),   ~
           at(17,03), fac(a$(10) ),    dbox$(10%)             ,ch(01),   ~
           at(17,09), fac(hex(84)),   dterm$(10%)             ,ch(16),   ~
           at(17,27), fac(hex(84)),  ddescr$(10%)             ,ch(30),   ~
           at(17,59), fac(hex(84)),   dtype$(10%)             ,ch(16),   ~
           keys(hex(000105080a0c0f)), key(hitkey%)

           if hitkey% = 1% then goto L06670
           if hitkey% = 5% then goto L07000
           if hitkey% = 8% then goto L05950
           if hitkey% = 10% then goto inputmode
           if del% = 1% then goto L08400
           if hitkey% <> 12% then goto L08400
                     for i% = 1% to 10%
                     if dbox$(i%) <> " " then gosub  L08640
                     next i%
                     goto L07170

L08400:    if hitkey% <> 15% then goto L08440
                     call "PRNTSCRN"
                     goto L07170

L08440:    if hitkey% <> 0% then goto L07170

        REM (ENTER) IS THE EXIT POINT.  THE PROPER ACTION DEPENDS ON     ~
            WHETHER THEY MARKED ONE OR NOT

           for i% = 1% to maxcnt%
           if dbox$(i%) <> " " then goto  L08560
           next i%

L08530:    rslt% = 0%   /* EXIT WITHOUT MARKING ONE */
           return

L08560:    call "READ100" (#1, dterm$(i%), f1%(1%))
                     if f1%(1%) <> 1% then goto L08530
                     get #1, using L08590 , pass$
L08590:              FMT CH(162)
                     rslt% = 9%    /* EXIT WITH DIFFERENT ONE */
                     return


L08640:    call "READ101" (#1, dterm$(i%), f1%(1%))
                     if f1%(1%) =  1% then delete #1
               init(" ") dbox$(i%), dterm$(i%), ddescr$(i%), dtype$(i%)
                     return



        REM *************************************************************~
            *          T E S T   T Y P E                                *~
            * PUT THE LIST OF VALID TYPES (CLASSES) OF TERMS HERE.      *~
            *************************************************************
        testtypes

           init(" ") errormsg$

           if cttype$ = "                " then return
           if cttype$ = "LANGUAGE        " then return
           if cttype$ = "JOB TITLE       " then return
           if cttype$ = "EEO JOB CLASS   " then return
           if cttype$ = "INTERVIEWER     " then return
           if cttype$ = "SKILL           " then return
           if cttype$ = "                " then return
           if cttype$ = "DEPARTMENT      " then return
           if cttype$ = "DIVISION        " then return
           if cttype$ = "                " then return
           if cttype$ = "INSURANCE       " then return
           if cttype$ = "                " then return
           if cttype$ = "                " then return
           if cttype$ = "                " then return
           if cttype$ = "BENEFIT         " then return
           if cttype$ = "                " then return
           if cttype$ = "                " then return

           errormsg$ = "NO SUCH TYPE (CLASS) OF TERM, PLEASE REENTER"
           return

L09000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * SET PASS$ WITH THE CORRECT RECORD DATA AND EXIT BACK      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN


            end
