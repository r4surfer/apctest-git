        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   EEEEE  TTTTT  PPPP   RRRR   N   N  TTTTT          *~
            *  S      E        T    P   P  R   R  NN  N    T            *~
            *   SSS   EEEE     T    PPPP   RRRR   N N N    T            *~
            *      S  E        T    P      R   R  N  NN    T            *~
            *   SSS   EEEEE    T    P      R   R  N   N    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SETPRNT  - SETS USAGE CONSTANTS FOR A GIVEN REPORT.  MAY  *~
            *            ALSO BE USED JUST TO SAVE AND RESTORE USERS    *~
            *            USAGE CONSTANTS AND/OR TO SET PRINT FILE SIZE. *~
            *            JUST PASS NAME$ = " ".                         *~
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
            * 01/02/86 ! ORIGINAL                                 ! KAB *~
            * 05/31/89 ! Added Close Printer before RESET in case ! MJB *~
            *          !  calling program didn't.                 !     *~
            * 10/20/93 ! Allow Print Mode 'P' (Unix Only)         ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "SETPRNT" (name$, prtname$, records%, mode%)

                      /* RPTID$   - THE RECORD TO QUERY                */~
                      /* RECORDS% - THE NUMBER OF RECORDS DESIRED      */~
                      /* MODE%    - ( =  0% (MOD 10)) => SAVE AND SET  */~
                      /*            ( >  9%) => PROGRAMMER FORCED PMPT */~
                      /*            ( OTHER) => RESET (IF SAVED)       */

        dim                                                              ~
            devinfo$32,                                                  ~
            file$8,                                                      ~
            first4$,                                                     ~
            library$8,                                                   ~
            volume$6,                                                    ~
                                                                         ~
            name$6,                      /* USER PASSED NAME           */~
            programmerprompt$1,          /* PROGRAMMER FOCRED PROMPT   */~
            prtypes$(10)1,               /* FOR GETPARM TYPES          */~
            prtname$8,                   /* PGMMER NAME FOR PRINT FILE */~
            records$7,                   /* FOR GETPARM                */~
            pmtext$24,                   /* PRINT MODE TEXT            */~
                                                                         ~
            rptid$6,                     /* ID OF THE REPORT           */~
            rptname$8,                   /* NAME FOR PRINT FILE        */~
            rptprompt$1,                 /* PROMPT USER?               */~
            rptform$3,                   /* FORM NUMBER TO USE         */~
            rptpmode$1,                  /* PRINT MODE                 */~
            rptpclass$1,                 /* PRINT CLASS                */~
            rptprinter$3,                /* PRINTER DEVICE NUMBER      */~
            rptpfileclass$1,             /* PRINT FILE PROTECT CLASS   */~
            rptspoollib$8,               /* SPOOL LIBRARY              */~
            rptspoolvol$6,               /* SPOOL VOLUME               */~
            rptspoolsys$8,               /* SPOOL SYSTEM               */~
                                                                         ~
            usrid$3,                     /* ID OF THE USER             */~
            usrpmode$1,                  /* PRINT MODE                 */~
            usrpclass$1,                 /* PRINT CLASS                */~
            usrpfileclass$1,             /* PRINT FILE PROTECT CLASS   */~
            usrspoollib$8,               /* SPOOL LIBRARY              */~
            usrspoolvol$6,               /* SPOOL VOLUME               */~
            usrspoolsys$8,               /* SPOOL SYSTEM               */~
                                                                         ~
            version$6                    /* System Version             */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

            f2% = 1%

        REM SELECT THE ONE FILE WE WILL USE

            select #1, "REPORTS",                                        ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 80,                                     ~
                       keypos = 1, keylen = 6

            rptid$ = name$
              /* JUST SO GETPARM WON'T BLOW IF " " IS  PASSED */

        REM *************************************************************~
            *                                                           *~
            *  IT ALL STARTS HERE, FOLKS                                *~
            *                                                           *~
            *************************************************************

            if mode%  > 9% then programmerprompt$ = "Y"                  ~
                           else programmerprompt$ = " "

            mode% = mod(mode%, 10%)

            if mode% <> 0% then L15000    /* RESTORE FUNCTION           */

            if usrid$ <> " " then L10210  /* DON'T WANT TO RE-EXTRACT   */

            call "EXTRACT" addr("ID", usrid$,                            ~
                                "FN", usrform%,                          ~
                                "PM", usrpmode$,                         ~
                                "PC", usrpclass$,                        ~
                                "P#", usrprinter%,                       ~
                                "PF", usrpfileclass$,                    ~
                                "SL", usrspoollib$,                      ~
                                "SV", usrspoolvol$,                      ~
                                "S#", version$)

            call "EXTSPSYS" addr(usrspoolsys$)

            pmtext$ = "Print Mode (P,O,S,H,K)  "
            unix% = -1% : convert version$ to unix%, data goto L10201
            pmtext$ = "Print Mode (O,S,H,K)    "

L10201:     usrprinter% = max(0%, min(512%, usrprinter%))

L10210:     init (" ")                                                   ~
                rptprompt$,                                              ~
                rptform$,                                                ~
                rptpmode$,                                               ~
                rptpclass$,                                              ~
                rptprinter$,                                             ~
                rptpfileclass$,                                          ~
                rptspoollib$,                                            ~
                rptspoolvol$,                                            ~
                rptspoolsys$,                                            ~
                rptname$                                                 ~

            if rptid$ = " " then L11300   /* RESET STANDARDS            */

            call "OPENCHCK" (#1, f1%, f2%, 0%, " ")
                if f1% < 1% then L11300
            call "READ100" (#1, rptid$, f1%)
                if f1% = 0% then L11300

            get #1, using L11180,                                         ~
                rptprompt$,                                              ~
                rptform$,                                                ~
                rptpmode$,                                               ~
                rptpclass$,                                              ~
                rptprinter$,                                             ~
                rptpfileclass$,                                          ~
                rptspoollib$,                                            ~
                rptspoolvol$,                                            ~
                rptspoolsys$,                                            ~
                rptname$                                                 ~

L11180:     FMT                                                          ~
               XX(6),                    /* ID OF THE REPORT           */~
               CH(1),                    /* PROMPT USER?               */~
               CH(3),                    /* FORM NUMBER TO USE         */~
               CH(1),                    /* PRINT MODE                 */~
               CH(1),                    /* PRINT CLASS                */~
               CH(3),                    /* PRINTER DEVICE NUMBER      */~
               CH(1),                    /* PRINT FILE PROTECT CLASS   */~
               CH(8),                    /* SPOOL LIBRARY              */~
               CH(6),                    /* SPOOL VOLUME               */~
               CH(8),                    /* SPOOL SYSTEM               */~
               CH(4)                     /* 4 CHAR RPT ID              */~

L11300:     rptform%    = usrform%
                convert rptform$    to rptform%,    data goto L11320
L11320:     rptprinter% = usrprinter%
                convert rptprinter$ to rptprinter%, data goto L11340
L11340:     if rptpmode$      = " " then rptpmode$      = usrpmode$
            if rptpclass$     = " " then rptpclass$     = usrpclass$
            if rptpfileclass$ = " " then rptpfileclass$ = usrpfileclass$
            if rptspoollib$   = " " then rptspoollib$   = usrspoollib$
            if rptspoolvol$   = " " then rptspoolvol$   = usrspoolvol$
            if rptspoolsys$   = " " then rptspoolsys$   = usrspoolsys$

            convert rptprinter% to rptprinter$, pic(000)
            convert rptform%    to rptform$   , pic(000)

            file$, library$, volume$, first4$ = " "

            if prtname$ <> " " then first4$ = prtname$
            if first4$  <> " " then L11520
               if rptname$ <> " " then first4$ = rptname$
            if first4$  <> " " then L11520
               call "EXTRACT" addr("CF", file$)
               first4$ = str(file$,1,4)
               file$ = " "

L11520:     if records% < 0% then L11550
               convert records%    to records$   , pic(#######)
                 goto L11570
L11550:        convert records%    to records$   , pic(-######)

L11570:     if programmerprompt$ = "Y" then L11600
            if rptprompt$ <> "Y" then L11830  /* NO GETPARM, VALIDATE */

L11600:     init ("K") prtypes$()

L11620:     call "GETPARM" addr("I ", "R", "RPTPARMS", " ",              ~
                        "0001", rptid$, "VERIFY REPORT PARAMETERS", 24%, ~
                        "T", "Printer Device Number   ", 24%, 0%, 0%,    ~
               prtypes$( 1), "PRINTER ", rptprinter$   , 3%, 0%, 0%, "I",~
                        "T", "Printer Form Number     ", 24%, 1%, 0%,    ~
               prtypes$( 2), "FORM#   ", rptform$      , 3%, 0%, 0%, "I",~
                        "T",  pmtext$                  , 24%, 2%, 0%,    ~
               prtypes$( 3), "PRNTMODE", rptpmode$     , 1%, 0%, 0%, "U",~
                        "T", "Print Class (A - Z)     ", 24%, 1%, 0%,    ~
               prtypes$( 4), "PRTCLASS", rptpclass$    , 1%, 0%, 0%, "U",~
                        "T", "Print File Class        ", 24%, 1%, 0%,    ~
               prtypes$( 5), "PRTFCLAS", rptpfileclass$, 1%, 0%, 0%, "A",~
                        "T", "Spool Library           ", 24%, 2%, 0%,    ~
               prtypes$( 6), "SPOOLLIB", rptspoollib$  , 8%, 0%, 0%, "A",~
                        "T", "Spool Volume            ", 24%, 1%, 0%,    ~
               prtypes$( 7), "SPOOLVOL", rptspoolvol$  , 6%, 0%, 0%, "A",~
                        "T", "Spool System            ", 24%, 1%, 0%,    ~
               prtypes$( 8), "SPOOLSYS", rptspoolsys$  , 8%, 0%, 0%, "A",~
                        "T", "Est. Number of Lines    ", 24%, 2%, 0%,    ~
               prtypes$( 9), "RECORDS ", records$      , 7%, 0%, 0%, "N",~
                        "T", "Print File Identifier:  ", 24%, 1%, 0%,    ~
               prtypes$(10), "FIRST4  ", first4$       , 4%, 0%, 0%, "A")

L11830:     gosub L50000   /* VALIDATION, WHAT ELSE */

            if pos(str(prtypes$()) = "R") <> 0% then L11620

            call "SET"     addr("FN", rptform%,                          ~
                                "PM", rptpmode$,                         ~
                                "PC", rptpclass$,                        ~
                                "P#", rptprinter%,                       ~
                                "PF", rptpfileclass$,                    ~
                                "SL", rptspoollib$,                      ~
                                "SV", rptspoolvol$)

            call "SETSPSYS" addr(rptspoolsys$, err%)
               if err% = 0% then L12140
            call "SETSPSYS" addr(usrspoolsys$, err%)
               if err% = 0% then L12140
            call "SETSPSYS" addr("        ", err%)

L12140:     if records% = -1% then L12170       /* WANTS IT BIG */
               records% = min(records%, 9999999%)
                 if records% < 10% then L12190
L12170:     call "SETPRTSZ" addr(records%)

L12190:     file$, library$, volume$ = " "
            if first4$ = " " then L12270
               file$ = "#" & first4$

            call "PUTPRTNM" addr(file$, library$, volume$)

L12270:     end

L15000: REM *************************************************************~
            *                                                           *~
            * RESET USERS CONSTANTS AFTER YOU ARE DONE                  *~
            *                                                           *~
            *************************************************************

            if usrid$ = " " then L15200          /* NEVER EXTRACTED */
            close printer
            call "SET"     addr("FN", usrform%,                          ~
                                "PM", usrpmode$,                         ~
                                "PC", usrpclass$,                        ~
                                "P#", usrprinter%,                       ~
                                "PF", usrpfileclass$,                    ~
                                "SL", usrspoollib$,                      ~
                                "SV", usrspoolvol$)

            call "SETSPSYS" addr(usrspoolsys$, err%)
               if err% = 0% then L15200
            call "SETSPSYS" addr("        ", err%)

L15200:     init (" ") usrid$, usrpmode$, usrpclass$, usrpfileclass$,    ~
                       usrspoollib$, usrspoolvol$, usrspoolsys$

            usrform%, usrprinter% = 0%

            end

L50000: REM *************************************************************~
            *                                                           *~
            *  VALIDATION FOR GETPARM INFORMATION                       *~
            *                                                           *~
            *************************************************************

            init ("K") prtypes$()

            REM TEST DATA FOR PRINTER DEVICE NUMBER
                convert rptprinter$ to rptprinter%, data goto L50190
                convert rptprinter% to rptprinter$, pic(000)
                  if rptprinter% = 0% then L50200
                  if rptprinter% < 0% then L50190
                  if rptprinter% > 512% then  L50190
                devinfo$ = " " : str(devinfo$,1,2) = bin(rptprinter%, 2)
                call "EXTRACT" addr("D-", devinfo$)
                  if str(devinfo$,1,1) = hex(04) then L50200
L50190:              prtypes$(1) = "R"

L50200:     REM TEST DATA FOR PRINTER FORM NUMBER
                convert rptform$ to rptform%, data goto L50250
                convert rptform% to rptform$, pic(000)
                  if rptform% < 0% then L50250
                  if rptform% < 255% then  L50300
L50250:              prtypes$(2) = "R"

L50300:     REM TEST DATA FOR PRINT MODE
                if unix% < 0% then L50323
                if pos("SOHK" = rptpmode$) > 0 then L50400
                   prtypes$(3) = "R"
                   goto L50400
L50323:         if pos("SOHKP" = rptpmode$) > 0 then L50400
                   prtypes$(3) = "R"

L50400:     REM TEST DATA FOR PRINT CLASS
                if pos(" ABCDEFGHIJKLMNOPQRSTUVWXYZ" = rptpclass$)       ~
                     <> 0% then L50500
                   prtypes$(4) = "R"

L50500:     REM TEST DATA FOR PRINT FILE CLASS
                if pos(" $@#ABCDEFGHIJKLMNOPQRSTUVWXYZ"=rptpfileclass$)  ~
                     <> 0% then L50600
                   prtypes$(5) = "R"

L50600:     REM TEST DATA FOR PRINT FILE LIBRARY [SPOOLLIB]
                if rptspoollib$ = " " then L50700
                for i% = 1% to len(rptspoollib$)
                if pos("$@#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"         ~
                               = str(rptspoollib$,i%,1)) = 0% then L50660
                next i%
                goto L50700
L50660:            prtypes$(6) = "R"

L50700:     REM TEST DATA FOR PRINT FILE VOLUME  [SPOOLVOL]
                if rptspoolvol$ = " " then L50800
                for i% = 1% to len(rptspoolvol$)
                if pos("$@#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"         ~
                               = str(rptspoolvol$,i%,1)) = 0% then L50770
                next i%
                goto L50800
L50770:            prtypes$(7) = "R"

L50800:     REM TEST DATA FOR REMOTE PRINT TO SYSTEM [SPOOLSYS]
                if rptspoolsys$ = " " then L50900
                for i% = 1% to len(rptspoolsys$)
                if pos("$@#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"         ~
                               = str(rptspoolsys$,i%,1)) = 0% then L50870
                next i%
                goto L50900
L50870:            prtypes$(8) = "R"

L50900:     REM TEST DATA FOR NUMBER OF RECORDS
                convert records$ to records%, data goto L51000
                   if records% < 0% then L50950
                      convert records% to records$, pic(#######)
                         goto L50970
L50950:               convert records% to records$, pic(-######)

L50970:         if records% = -1% then L51030
                if records% <  0% then L51000
                if records% < 10000000% then L51030
L51000:            prtypes$(9) = "R"
                      return

L51030:     REM TEST DATA FOR PRINT FILE IDENTIFIER (FIRST 4)
                if first4$ = " " then return
                for i% = 1% to len(first4$)
                if pos("$@#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"         ~
                               = str(first4$,i%,1)) = 0% then L51100
                next i%
                return
L51100:            prtypes$(10) = "R"
                   return

