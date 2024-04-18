        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   K   K  IIIII  L      L       SSS   U   U  BBBB    *~
            *  S      K  K     I    L      L      S      U   U  B   B   *~
            *   SSS   KKK      I    L      L       SSS   U   U  BBBB    *~
            *      S  K  K     I    L      L          S  U   U  B   B   *~
            *   SSS   K   K  IIIII  LLLLL  LLLLL   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SKILLSUB - GENERAL SUBROUTINE TO HANDLE ALL ENTRY AND     *~
            *            REVIEW OF SKILLS FOR APPLICANTS, REQUISITIONS, *~
            *            AND EMPLOYEES.  PART OF THE PERSONNEL SYSTEM.  *~
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
            * 11/22/83 ! ORIGINAL                                 ! GLW *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        REM  THE ARGUEMENT STRING IS -                                   ~
           #1 = APLSKILL FILE                                            ~
           #2 = REQSKILL FILE                                            ~
           #3 = EMPSKILL FILE                                            ~
           #4 = COMTERM  FILE                                            ~
           #5 = APLMASTR FILE                                            ~
           #6 = REQMASTR FILE                                            ~
           #8 = PERMASTR FILE                                            ~
           WHICH% = WHICH OF THE FILES TO PLOW 1, 2 OR 3.                ~
           FILEKEY$ =                                                    ~
              11 CH  FOR #1 = SOCIAL SECURITY NUMBER OF APPLICANT        ~
              05 CH  FOR #2 = REQUISITION NUMBER                         ~
              12 CH  FOR #3 = EMPLOYEE CODE                              ~
           NAMEIN$ = 30 CH NAME FOR USE IN SCREEN TITLES

        sub "SKILLSUB" (#1, #2, #3, #4, #5, #6, #8,                      ~
                                        which%, filekey$, namein$)

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filekey$12,                  /* KEY COMMING IN             */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            header$79,                   /* SCREEN TITLE               */~
            header1$79,                  /* SCREEN TITLE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            plowkey$100,                 /* PLOWING KEY                */~
            plowkey1$100,                /* PLOWING KEY                */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            pass$162,                    /* ARGUEMENT PASS TO 'COMSUB' */~
            namein$30,                   /* NAME FOR SCREEN TITLES     */~
            seekey$(15)20,               /* KEY TO FILES               */~
            rest$(15)35,                 /* REST OF FILE               */~
            profic$(15)4,                /* PROFICIENCY                */~
            skill$(15)16,                /* SKILL                      */~
            text$(15)42,                 /* FREE TEXT                  */~
            seeke1$(15)25,               /* KEY TO FILES               */~
            res1$(15)35,                 /* REST OF FILE               */~
            profi1$(15)4,                /* PROFICIENCY                */~
            skil1$(15)16,                /* SKILL                      */~
            tex1$(15)42                  /* FREE TEXT                  */~

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$,                            ~
            seekey$(),                   /* THE FILE KEY               */~
            rest$(),                     /* FILLER IN RECORD           */~
            profic$(),                   /* PROFICIENCY                */~
            skill$(),                    /* SKILL                      */~
            text$()                      /* FREE TEXT                  */~

           a%, r%, e% = 0%               /* COUNTERS                   */

           plowkey$ = str(filekey$,1, len(filekey$)) & " "

L10090:    on which% goto   L10130,       /* APPLICANT SKILLS           */~
                            L10200,       /* REQUISITION SKILLS NEEDED  */~
                            L10290        /* EMPLOYEE SKILLS            */

           which% = 1%               /* IN CASE WHICH% IS MISSPECIFIED */
           goto L10090

L10130:    header$ = "SKILLS INVENTORY FOR APPLICANT " & namein$
L10140:    call "PLOWNEXT" (#1, plowkey$, 11%, f1%(1))
           if f1%(1) <> 1% then goto  seeskill
           a% = a% + 1%
           if a% > 15% then goto  seeskill
           get #1, using L35030, skill$(a%), seekey$(a%), i$, profic$(a%),~
                                text$(a%), rest$(a%)
           goto L10140

L10200:  header$ = "SKILLS REQUIRED FOR "  & namein$
L10210:    call "PLOWNEXT" (#2, plowkey$,  5%, f1%(2))
           if f1%(2) <> 1% then goto  seeskill
           r% = r% + 1%
           if r% > 15% then goto  seeskill
           get #2, using L35070, skill$(r%), seekey$(r%), i$, profic$(r%),~
                                text$(r%), rest$(r%)
           goto L10210

L10290:    header$ = "SKILLS INVENTORY FOR EMPLOYEE " & namein$
L10300:    call "PLOWNEXT" (#3, plowkey$, 12%, f1%(3))
           if f1%(3) <> 1% then goto  seeskill
           e% = e% + 1%
           if e% > 15% then goto  seeskill
           get #3, using L35110, skill$(e%), seekey$(e%), i$, profic$(e%),~
                                text$(e%), rest$(e%)
           goto L10300

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        seeskill

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060

            fieldnr% = cursor%(1%) - 5%

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

           plowkey$ = str(filekey$,1, len(filekey$)) & " "

           on which% goto   L19100,       /* APPLICANT SKILLS           */~
                            L19190,       /* REQUISITION SKILLS NEEDED  */~
                            L19300        /* EMPLOYEE SKILLS            */

L19100:    call "DELETE" (#1, plowkey$, 11%)
           for i% = 1% to 15%
                     if skill$(i%) = " " then goto L19160
               seekey$(i%) = str(plowkey$,1,11) & " "
               convert i% to i$, pic(##)
           put #1, using L35030, skill$(i%), seekey$(i%), i$, profic$(i%),~
                                text$(i%), rest$(i%)
                     write #1
L19160:              next i%
                     goto L65000

L19190:    call "DELETE" (#2, plowkey$, 5%)
           for i% = 1% to 15%
                     if skill$(i%) = " " then goto L19270
               seekey$(i%)  = str(plowkey$,1,5)  & " "
               convert i% to i$, pic(##)
           put #2, using L35070, skill$(i%), seekey$(i%), i$, profic$(i%),~
                                text$(i%), rest$(i%)
                     write #2
L19270:              next i%
                     goto L65000

L19300:    call "DELETE" (#3, plowkey$, 12%)
           for i% = 1% to 15%
                     if skill$(i%) = " " then goto L19380
                 seekey$(i%) = str(plowkey$,1,12) & " "
                 convert i% to i$, pic(##)
           put #3, using L35110, skill$(i%), seekey$(i%), i$, profic$(i%),~
                                text$(i%), rest$(i%)
                     write #3
L19380:              next i%
                     goto L65000

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

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: APLSKILL                     */~
            CH(16),              /* Skill for personnel system         */~
            CH(11),              /* Social security number             */~
            CH(2),               /* General purpose sequence number    */~
            CH(4),               /* Proficiency in a skill - personnel */~
            CH(42),              /* the text                           */~
            CH(25)               /* filler for rest of record or inter */~

L35070: FMT                      /* FILE: REQSKILL                     */~
            CH(16),              /* Skill for personnel system         */~
            CH(05),              /* Requisition number - personnel sys */~
            CH(2),               /* General purpose sequence number    */~
            CH(4),               /* Proficiency in a skill - personnel */~
            CH(42),              /* the text                           */~
            CH(31)               /* filler for rest of record or inter */~

L35110: FMT                      /* FILE: EMPSKILL                     */~
            CH(16),              /* Skill for personnel system         */~
            CH(12),              /* employee code                      */~
            CH(2),               /* General purpose sequence number    */~
            CH(4),               /* Proficiency in a skill - personnel */~
            CH(42),              /* the text                           */~
            CH(24)               /* filler for rest of record or inter */~


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then goto L41092
                  lfac$(fieldnr%) = hex(81)

L41092: accept                                                           ~
               at (01,03),                                               ~
         fac(hex(84)), header$, ch(78),                                  ~
           at(02,03), fac(hex(94)), errormsg$, ch(78),                   ~
               at (03,03),                                               ~
        "+--------------------+-------+----------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!  SKILL             ! LEVEL !                                  ~
        ~            !",                                                  ~
               at (05,03),                                               ~
        "+--------------------+-------+----------------------------------~
        ~------------+",                                                  ~
               at (06,03),                                               ~
        "!",                                                             ~
               at (06,24),                                               ~
        "!",                                                             ~
               at (06,32),                                               ~
        "!",                                                             ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!",                                                             ~
               at (07,24),                                               ~
        "!",                                                             ~
               at (07,32),                                               ~
        "!",                                                             ~
               at (07,79),                                               ~
        "!",                                                             ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,24),                                               ~
        "!",                                                             ~
               at (08,32),                                               ~
        "!",                                                             ~
               at (08,79),                                               ~
        "!",                                                             ~
               at (09,03),                                               ~
        "!",                                                             ~
               at (09,24),                                               ~
        "!",                                                             ~
               at (09,32),                                               ~
        "!",                                                             ~
               at (09,79),                                               ~
        "!",                                                             ~
               at (10,03),                                               ~
        "!",                                                             ~
               at (10,24),                                               ~
        "!",                                                             ~
               at (10,32),                                               ~
        "!",                                                             ~
               at (10,79),                                               ~
        "!",                                                             ~
               at (11,03),                                               ~
        "!",                                                             ~
               at (11,24),                                               ~
        "!",                                                             ~
               at (11,32),                                               ~
        "!",                                                             ~
               at (11,79),                                               ~
        "!",                                                             ~
               at (12,03),                                               ~
        "!",                                                             ~
               at (12,24),                                               ~
        "!",                                                             ~
               at (12,32),                                               ~
        "!",                                                             ~
               at (12,79),                                               ~
        "!",                                                             ~
               at (13,03),                                               ~
        "!",                                                             ~
               at (13,24),                                               ~
        "!",                                                             ~
               at (13,32),                                               ~
        "!",                                                             ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "!",                                                             ~
               at (14,24),                                               ~
        "!",                                                             ~
               at (14,32),                                               ~
        "!",                                                             ~
               at (14,79),                                               ~
        "!",                                                             ~
               at (15,03),                                               ~
        "!",                                                             ~
               at (15,24),                                               ~
        "!",                                                             ~
               at (15,32),                                               ~
        "!",                                                             ~
               at (15,79),                                               ~
        "!",                                                             ~
               at (16,03),                                               ~
        "!",                                                             ~
               at (16,24),                                               ~
        "!",                                                             ~
               at (16,32),                                               ~
        "!",                                                             ~
               at (16,79),                                               ~
        "!",                                                             ~
               at (17,03),                                               ~
        "!",                                                             ~
               at (17,24),                                               ~
        "!",                                                             ~
               at (17,32),                                               ~
        "!",                                                             ~
               at (17,79),                                               ~
        "!",                                                             ~
               at (18,03),                                               ~
        "!",                                                             ~
               at (18,24),                                               ~
        "!",                                                             ~
               at (18,32),                                               ~
        "!",                                                             ~
               at (18,79),                                               ~
        "!",                                                             ~
               at (19,03),                                               ~
        "!",                                                             ~
               at (19,24),                                               ~
        "!",                                                             ~
               at (19,32),                                               ~
        "!",                                                             ~
               at (19,79),                                               ~
        "!",                                                             ~
               at (20,03),                                               ~
        "!",                                                             ~
               at (20,24),                                               ~
        "!",                                                             ~
               at (20,32),                                               ~
        "!",                                                             ~
               at (20,79),                                               ~
        "!",                                                             ~
               at (21,03),                                               ~
        "+--------------------+-------+----------------------------------~
        ~------------+",                                                  ~
               at (22,03),                                               ~
        "MOVE CURSOR TO LINE, THEN:                                   (13~
        ~)INSTRUCTIONS",                                                  ~
               at (23,03),                                               ~
        "(ENTER)TO ADD/EDIT SKILLS INFO  (12)TO DELETE LINE   (15)PRNTSCR~
        ~N  (16)RETURN",                                                  ~
               at (24,03),                                               ~
        "(8)APPLICANTS WITH SKILL  (9)REQUISITIONS FOR SKILL  (10)EMPLOYE~
        ~ES WITH SKILL",                                                  ~
           at(6, 6), fac(lfac$( 1%)), skill$        ( 1%),        ch(16),~
           at(6,27), fac(lfac$( 1%)), profic$       ( 1%),        ch(04),~
           at(6,35), fac(lfac$( 1%)), text$         ( 1%),        ch(42),~
           at(7, 6), fac(lfac$( 2%)), skill$        ( 2%),        ch(16),~
           at(7,27), fac(lfac$( 2%)), profic$       ( 2%),        ch(04),~
           at(7,35), fac(lfac$( 2%)), text$         ( 2%),        ch(42),~
           at(8, 6), fac(lfac$( 3%)), skill$        ( 3%),        ch(16),~
           at(8,27), fac(lfac$( 3%)), profic$       ( 3%),        ch(04),~
           at(8,35), fac(lfac$( 3%)), text$         ( 3%),        ch(42),~
           at(9, 6), fac(lfac$( 4%)), skill$        ( 4%),        ch(16),~
           at(9,27), fac(lfac$( 4%)), profic$       ( 4%),        ch(04),~
           at(9,35), fac(lfac$( 4%)), text$         ( 4%),        ch(42),~
          at(10, 6), fac(lfac$( 5%)), skill$        ( 5%),        ch(16),~
          at(10,27), fac(lfac$( 5%)), profic$       ( 5%),        ch(04),~
          at(10,35), fac(lfac$( 5%)), text$         ( 5%),        ch(42),~
          at(11, 6), fac(lfac$( 6%)), skill$        ( 6%),        ch(16),~
          at(11,27), fac(lfac$( 6%)), profic$       ( 6%),        ch(04),~
          at(11,35), fac(lfac$( 6%)), text$         ( 6%),        ch(42),~
          at(12, 6), fac(lfac$( 7%)), skill$        ( 7%),        ch(16),~
          at(12,27), fac(lfac$( 7%)), profic$       ( 7%),        ch(04),~
          at(12,35), fac(lfac$( 7%)), text$         ( 7%),        ch(42),~
          at(13, 6), fac(lfac$( 8%)), skill$        ( 8%),        ch(16),~
          at(13,27), fac(lfac$( 8%)), profic$       ( 8%),        ch(04),~
          at(13,35), fac(lfac$( 8%)), text$         ( 8%),        ch(42),~
          at(14, 6), fac(lfac$( 9%)), skill$        ( 9%),        ch(16),~
          at(14,27), fac(lfac$( 9%)), profic$       ( 9%),        ch(04),~
          at(14,35), fac(lfac$( 9%)), text$         ( 9%),        ch(42),~
          at(15, 6), fac(lfac$(10%)), skill$        (10%),        ch(16),~
          at(15,27), fac(lfac$(10%)), profic$       (10%),        ch(04),~
          at(15,35), fac(lfac$(10%)), text$         (10%),        ch(42),~
          at(16, 6), fac(lfac$(11%)), skill$        (11%),        ch(16),~
          at(16,27), fac(lfac$(11%)), profic$       (11%),        ch(04),~
          at(16,35), fac(lfac$(11%)), text$         (11%),        ch(42),~
          at(17, 6), fac(lfac$(12%)), skill$        (12%),        ch(16),~
          at(17,27), fac(lfac$(12%)), profic$       (12%),        ch(04),~
          at(17,35), fac(lfac$(12%)), text$         (12%),        ch(42),~
          at(18, 6), fac(lfac$(13%)), skill$        (13%),        ch(16),~
          at(18,27), fac(lfac$(13%)), profic$       (13%),        ch(04),~
          at(18,35), fac(lfac$(13%)), text$         (13%),        ch(42),~
          at(19, 6), fac(lfac$(14%)), skill$        (14%),        ch(16),~
          at(19,27), fac(lfac$(14%)), profic$       (14%),        ch(04),~
          at(19,35), fac(lfac$(14%)), text$         (14%),        ch(42),~
          at(20, 6), fac(lfac$(15%)), skill$        (15%),        ch(16),~
          at(20,27), fac(lfac$(15%)), profic$       (15%),        ch(04),~
          at(20,35), fac(lfac$(15%)), text$         (15%),        ch(42),~
           keys(hex(0008090a0c0d0f10)), key(keyhit%)

               if keyhit% = 16% then return

               if keyhit% <> 13 then L41610
                  call "MANUAL" ("SKILLSUB")
                  goto L41092

L41610:        if keyhit% <> 15 then L41650
                  call "PRNTSCRN"
                  goto L41092

L41650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                     u3% = u3%   /* JUST TO COMPILE WITHOUT ERRORS */
               if cursor%(1%) < 6%  then goto L41092
               if cursor%(1%) > 20%  then goto L41092

               if keyhit% <> 0% then goto  L41720
                     return

L41720:        if keyhit% <> 12% then goto L41790
               init(" ")                                                 ~
                     skill$             (cursor%(1%) - 5%),              ~
                     seekey$            (cursor%(1%) - 5%),              ~
                     profic$            (cursor%(1%) - 5%),              ~
                     text$              (cursor%(1%) - 5%),              ~
                     rest$              (cursor%(1%) - 5%)

                     goto L41092

L41790:        if keyhit% <>  8% then goto  L41860
           header1$ = "LIST OF ALL APPLICANTS WITH THE SKILL: " &        ~
                                 skill$(cursor%(1%) - 5%)
           which1% = 1%
           gosub allwithskill
           goto L41092

L41860:        if keyhit% <>  9% then goto L41930
           header1$ = "LIST OF ALL REQUISITIONS NEEDING THE SKILL: " &   ~
                                 skill$(cursor%(1%) - 5%)
           which1% = 2%
           gosub allwithskill
           goto L41092

L41930:        if keyhit% <> 10% then goto L41092
           header1$ = "LIST OF ALL EMPLOYEES WITH THE SKILL: " &         ~
                                 skill$(cursor%(1%) - 5%)
           which1% = 3%
           gosub allwithskill
           goto L41092

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
           init(" ") pass$
           str(pass$,47,16) = str(skill$(fieldnr%),1,16)
L50065:    call "COMSUB" (#4, 1%, pass$, 1%, rc%)
           if rc% =  0% then goto L50065
           if str(pass$,1,16) =                                          ~
                        "SKILL           " then goto L50125
           errormsg$ = "THAT TERM IS NOT A SKILL, PLEASE REENTER"
                     return
L50125:    str(skill$(fieldnr%),1,16) = str(pass$,47,16)
           return
                return

        REM *************************************************************~
            *   A L L W I T H S K I L L                                 *~
            *                                                           *~
            * SHOWS ALL ON FILE WITH GIVEN SKILLS                       *~
            *************************************************************
L51055: allwithskill

           plowkey1$ = str(skill$(cursor%(1%) - 5%),1,16) & " "

            init(" ") errormsg$, inpmessage$,                            ~
            seeke1$(),                   /* THE FILE KEY               */~
            res1$(),                     /* FILLER IN RECORD           */~
            profi1$(),                   /* PROFICIENCY                */~
            skil1$(),                    /* SKILL                      */~
            tex1$(1)                     /* FREE TEXT                  */~

           x%         = 0%               /* COUNTERS                   */

L51205:   on which1% goto   L51275,       /* APPLICANT SKILLS           */~
                            L51365,       /* REQUISITION SKILLS NEEDED  */~
                            L51455        /* EMPLOYEE SKILLS            */

           which1% = 1%              /* IN CASE WHICH% IS MISSPECIFIED */
           goto L51205

L51275:    REM FOR APPLICANTS
            init(" ") errormsg$, inpmessage$,                            ~
            seeke1$(),                   /* THE FILE KEY               */~
            res1$(),                     /* FILLER IN RECORD           */~
            profi1$(),                   /* PROFICIENCY                */~
            skil1$(),                    /* SKILL                      */~
            tex1$()                      /* FREE TEXT                  */
L51285:    call "PLOWALTS" (#1, plowkey1$, 1%, 16%,   f1%(1))
           if f1%(1) <> 1% then goto  seeskill1
           x% = x% + 1%
           if x% > 15% then goto  seeskill1
           get #1, using L35030, skil1$(x%), seeke1$(x%), i$, profi1$(x%),~
                                tex1$(x%), res1$(x%)
           call "READ100" (#5, seeke1$(x%), f1%(5))
           if f1%(5) <> 1% then goto L51345
           get #5, using L51339 , lname$, fname$, mname$
L51339:    FMT XX(55), CH(15), CH(10), CH(1)
           gosub L51596
L51345:    goto L51285

L51365:    REM FOR REQUISITIONS
            init(" ") errormsg$, inpmessage$,                            ~
            seeke1$(),                   /* THE FILE KEY               */~
            res1$(),                     /* FILLER IN RECORD           */~
            profi1$(),                   /* PROFICIENCY                */~
            skil1$(),                    /* SKILL                      */~
            tex1$()                      /* FREE TEXT                  */
L51400:    call "PLOWALTS" (#2, plowkey1$, 1%, 16%,   f1%(2))
           if f1%(2) <> 1% then goto  seeskill1
           x% = x% + 1%
           if x% > 15% then goto  seeskill1
           get #2, using L35070, skil1$(x%), seeke1$(x%), i$, profi1$(x%),~
                                tex1$(x%), res1$(x%)
           goto L51400

L51455:    REM FOR EMPLOYEES
            init(" ") errormsg$, inpmessage$,                            ~
            seeke1$(),                   /* THE FILE KEY               */~
            res1$(),                     /* FILLER IN RECORD           */~
            profi1$(),                   /* PROFICIENCY                */~
            skil1$(),                    /* SKILL                      */~
            tex1$()                      /* FREE TEXT                  */
L51525:    call "PLOWALTS" (#3, plowkey1$, 1%, 16%,   f1%(3))
           if f1%(3) <> 1% then goto  seeskill1
           x% = x% + 1%
           if x% > 15% then goto  seeskill1
           get #3, using L35110, skil1$(x%), seeke1$(x%), i$, profi1$(x%),~
                                tex1$(x%), res1$(x%)
           call "READ100" (#8, seeke1$(x%), f1%(8))
           if f1%(8) <> 1% then goto L51585
           get #8, using L51579, lname$, fname$, mname$
L51579:    FMT XX(1), CH(15), CH(10), CH(1)
           gosub L51596
L51585:    goto L51525

L51596:    init(" ") seeke1$(x%)
           str(seeke1$(x%),1,len(lname$)) = str(lname$,1,len(lname$))
           str(seeke1$(x%),len(lname$)+2%) = str(fname$,1,len(fname$))
           if len(lname$) + len(fname$) >= 17% then return
           str(seeke1$(x%),len(lname$) + len(fname$) + 4%) = mname$
           return

        seeskill1

           if seeke1$(1%) = " " then tex1$(1%) = "ALL ARE SHOWN"

L51635: accept                                                           ~
               at (01,03),                                               ~
         fac(hex(84)), header1$, ch(78),                                 ~
               at (03,03),                                               ~
        "+-----------------------+----+----------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!  WHO (OR WHICH REQ)   ! PR !  DEGREE OF PROFICIENCY           ~
        ~            !",                                                  ~
               at (05,03),                                               ~
        "+-----------------------+----+----------------------------------~
        ~------------+",                                                  ~
               at (06,03),                                               ~
        "!",                                                             ~
               at (06,27),                                               ~
        "!",                                                             ~
               at (06,32),                                               ~
        "!",                                                             ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!",                                                             ~
               at (07,27),                                               ~
        "!",                                                             ~
               at (07,32),                                               ~
        "!",                                                             ~
               at (07,79),                                               ~
        "!",                                                             ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,27),                                               ~
        "!",                                                             ~
               at (08,32),                                               ~
        "!",                                                             ~
               at (08,79),                                               ~
        "!",                                                             ~
               at (09,03),                                               ~
        "!",                                                             ~
               at (09,27),                                               ~
        "!",                                                             ~
               at (09,32),                                               ~
        "!",                                                             ~
               at (09,79),                                               ~
        "!",                                                             ~
               at (10,03),                                               ~
        "!",                                                             ~
               at (10,27),                                               ~
        "!",                                                             ~
               at (10,32),                                               ~
        "!",                                                             ~
               at (10,79),                                               ~
        "!",                                                             ~
               at (11,03),                                               ~
        "!",                                                             ~
               at (11,27),                                               ~
        "!",                                                             ~
               at (11,32),                                               ~
        "!",                                                             ~
               at (11,79),                                               ~
        "!",                                                             ~
               at (12,03),                                               ~
        "!",                                                             ~
               at (12,27),                                               ~
        "!",                                                             ~
               at (12,32),                                               ~
        "!",                                                             ~
               at (12,79),                                               ~
        "!",                                                             ~
               at (13,03),                                               ~
        "!",                                                             ~
               at (13,27),                                               ~
        "!",                                                             ~
               at (13,32),                                               ~
        "!",                                                             ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "!",                                                             ~
               at (14,27),                                               ~
        "!",                                                             ~
               at (14,32),                                               ~
        "!",                                                             ~
               at (14,79),                                               ~
        "!",                                                             ~
               at (15,03),                                               ~
        "!",                                                             ~
               at (15,27),                                               ~
        "!",                                                             ~
               at (15,32),                                               ~
        "!",                                                             ~
               at (15,79),                                               ~
        "!",                                                             ~
               at (16,03),                                               ~
        "!",                                                             ~
               at (16,27),                                               ~
        "!",                                                             ~
               at (16,32),                                               ~
        "!",                                                             ~
               at (16,79),                                               ~
        "!",                                                             ~
               at (17,03),                                               ~
        "!",                                                             ~
               at (17,27),                                               ~
        "!",                                                             ~
               at (17,32),                                               ~
        "!",                                                             ~
               at (17,79),                                               ~
        "!",                                                             ~
               at (18,03),                                               ~
        "!",                                                             ~
               at (18,27),                                               ~
        "!",                                                             ~
               at (18,32),                                               ~
        "!",                                                             ~
               at (18,79),                                               ~
        "!",                                                             ~
               at (19,03),                                               ~
        "!",                                                             ~
               at (19,27),                                               ~
        "!",                                                             ~
               at (19,32),                                               ~
        "!",                                                             ~
               at (19,79),                                               ~
        "!",                                                             ~
               at (20,03),                                               ~
        "!",                                                             ~
               at (20,27),                                               ~
        "!",                                                             ~
               at (20,32),                                               ~
        "!",                                                             ~
               at (20,79),                                               ~
        "!",                                                             ~
               at (21,03),                                               ~
        "+-----------------------+----+----------------------------------~
        ~------------+",                                                  ~
               at (24,03),                                               ~
        "(1)FIRST PAGE        (5)NEXT PAGE           (15)PRINT SCREEN    ~
        ~   (16)RETURN",                                                  ~
           at(6, 5), fac(lfac$( 1%)), seeke1$       ( 1%),        ch(21),~
           at(6,29), fac(lfac$( 1%)), profi1$       ( 1%),        ch(02),~
           at(6,35), fac(lfac$( 1%)), tex1$         ( 1%),        ch(42),~
           at(7, 5), fac(lfac$( 2%)), seeke1$       ( 2%),        ch(21),~
           at(7,29), fac(lfac$( 2%)), profi1$       ( 2%),        ch(02),~
           at(7,35), fac(lfac$( 2%)), tex1$         ( 2%),        ch(42),~
           at(8, 5), fac(lfac$( 3%)), seeke1$       ( 3%),        ch(21),~
           at(8,29), fac(lfac$( 3%)), profi1$       ( 3%),        ch(02),~
           at(8,35), fac(lfac$( 3%)), tex1$         ( 3%),        ch(42),~
           at(9, 5), fac(lfac$( 4%)), seeke1$       ( 4%),        ch(21),~
           at(9,29), fac(lfac$( 4%)), profi1$       ( 4%),        ch(02),~
           at(9,35), fac(lfac$( 4%)), tex1$         ( 4%),        ch(42),~
          at(10, 5), fac(lfac$( 5%)), seeke1$       ( 5%),        ch(21),~
          at(10,29), fac(lfac$( 5%)), profi1$       ( 5%),        ch(02),~
          at(10,35), fac(lfac$( 5%)), tex1$         ( 5%),        ch(42),~
          at(11, 5), fac(lfac$( 6%)), seeke1$       ( 6%),        ch(21),~
          at(11,29), fac(lfac$( 6%)), profi1$       ( 6%),        ch(02),~
          at(11,35), fac(lfac$( 6%)), tex1$         ( 6%),        ch(42),~
          at(12, 5), fac(lfac$( 7%)), seeke1$       ( 7%),        ch(21),~
          at(12,29), fac(lfac$( 7%)), profi1$       ( 7%),        ch(02),~
          at(12,35), fac(lfac$( 7%)), tex1$         ( 7%),        ch(42),~
          at(13, 5), fac(lfac$( 8%)), seeke1$       ( 8%),        ch(21),~
          at(13,29), fac(lfac$( 8%)), profi1$       ( 8%),        ch(02),~
          at(13,35), fac(lfac$( 8%)), tex1$         ( 8%),        ch(42),~
          at(14, 5), fac(lfac$( 9%)), seeke1$       ( 9%),        ch(21),~
          at(14,29), fac(lfac$( 9%)), profi1$       ( 9%),        ch(02),~
          at(14,35), fac(lfac$( 9%)), tex1$         ( 9%),        ch(42),~
          at(15, 5), fac(lfac$(10%)), seeke1$       (10%),        ch(21),~
          at(15,29), fac(lfac$(10%)), profi1$       (10%),        ch(02),~
          at(15,35), fac(lfac$(10%)), tex1$         (10%),        ch(42),~
          at(16, 5), fac(lfac$(11%)), seeke1$       (11%),        ch(21),~
          at(16,29), fac(lfac$(11%)), profi1$       (11%),        ch(02),~
          at(16,35), fac(lfac$(11%)), tex1$         (11%),        ch(42),~
          at(17, 5), fac(lfac$(12%)), seeke1$       (12%),        ch(21),~
          at(17,29), fac(lfac$(12%)), profi1$       (12%),        ch(02),~
          at(17,35), fac(lfac$(12%)), tex1$         (12%),        ch(42),~
          at(18, 5), fac(lfac$(13%)), seeke1$       (13%),        ch(21),~
          at(18,29), fac(lfac$(13%)), profi1$       (13%),        ch(02),~
          at(18,35), fac(lfac$(13%)), tex1$         (13%),        ch(42),~
          at(19, 5), fac(lfac$(14%)), seeke1$       (14%),        ch(21),~
          at(19,29), fac(lfac$(14%)), profi1$       (14%),        ch(02),~
          at(19,35), fac(lfac$(14%)), tex1$         (14%),        ch(42),~
          at(20, 5), fac(lfac$(15%)), seeke1$       (15%),        ch(21),~
          at(20,29), fac(lfac$(15%)), profi1$       (15%),        ch(02),~
          at(20,35), fac(lfac$(15%)), tex1$         (15%),        ch(42),~
           keys(hex(01050f10)), key(xxxhit%)

               if xxxhit% = 16% then return

               if xxxhit% <> 15 then goto L53655
                  call "PRNTSCRN"
                  goto L51635

L53655:    if which1% <> 1% then goto L53695
                     if xxxhit% = 1% then goto L51055
                     if xxxhit% = 5% then goto L51275

L53695:    if which1% <> 2% then goto L53735
                     if xxxhit% = 1% then goto L51055
                     if xxxhit% = 5% then goto L51365

L53735:    if which1% <> 3% then goto  L51635
                     if xxxhit% = 1% then goto L51055
                     if xxxhit% = 5% then goto L51455

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

            end
