        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  BBBB   FFFFF  L        A     GGG    SSS           *~
            *    J    B   B  F      L       A A   G      S              *~
            *    J    BBBB   FFFF   L      AAAAA  G       SSS           *~
            *   JJ    B   B  F      L      A   A  G  GG      S          *~
            *    J    BBBB   F      LLLLL  A   A   GGG    SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBFLAGS - Set switches for Shop Floor Control Module.     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/24/85 ! ORIGINAL                                 ! HES *~
            * 19/20/86 ! Added flag for type of traveler to print ! HES *~
            * 06/12/87 ! Added GLLOG$ and SAVETIF$ Flags          ! HES *~
            * 02/12/88 ! Added PICKO$ for pick list sorting       ! HES *~
            * 02/19/88 ! Added TFORM$ for Traveler Format Control ! HES *~
            * 06/16/88 ! Added Summary Opt to Pick List by Part   ! KAB *~
            * 07/04/88 ! Re-Added BAR_CODE$ For Shop Floor Control! KAB *~
            * 11/28/88 ! Added call to CMSMAINP (SFC admin.)      ! ERN *~
            *          !  & option to keep summary file by task   !     *~
            * 01/19/90 ! Added the pegging features               ! SID *~
            * 03/08/90 ! Added 2nd screen re-arranged screen 1.   ! JEF *~
            *          ! Added and changed bin sort parameters.   !     *~
            *          ! Added default store code.                !     *~
            * 08/03/91 ! Added Smart Edit Feature to Screen 1.    ! SID *~
            * 04/13/92 ! PRR 12345. Smart Edit sent back to school! JDH *~
            * 05/15/93 ! A Host of New Flags/Defaults             ! KAB *~
            * 07/15/93 ! Purchase Jobs - Major Modifications.     ! MLJ *~
            *          !  1) Grouped flags into categories and    !     *~
            *          !     added category headings.  Now a total!     *~
            *          !     of 4 screens instead of 2 (#4 new).  !     *~
            *          !  2) Next Std, Rework & Purchase job #    !     *~
            *          !     info now all in SWICHS.SFC (72-126). !     *~
            *             3) PRR 12408 - Defaults for printing of !     *~
            *                Traveler, Pick List, By-Product List !     *~
            *                & Single Level BOM added (127 - 130).!     *~
            *          !  4) Due to extent of above changes, prog !     *~
            *          !     renumbered & modkillerd for clarity. !     *~
            * 07/13/94 ! Print Vendor Service Sheet Flag.         ! RJH *~
            * 04/27/95 ! Add BOM Partial Completion Valuation     ! RJH *~
            *          !  method 'B' for Prorated BOM costs.      !     *~
            * 11/01/95 ! Allow more barcode options (Unix only)   ! HES *~
            *************************************************************

        dim                                                              ~
            accttype$1,                  /* G/L Account Type           */~
            ask$(3)80,                   /* ASKUSER, of course         */~
            auto%(2),                    /* Work Array                 */~
            binloc$1,                    /* PRINT BIN LOCS ON PICK LIST*/~
            bin_cnt$1,                   /* Number of Locations to prnt*/~
            bar_code$1,                  /* PRINT BAR CODES  (Y/N)     */~
            byprod$1,                    /* Print By-Prod List Flag    */~
            class$1,                     /* SUBMIT CLASS               */~
            cursor%(2),                  /* CURSOR LOCATION            */~
            comperrkit$1,                /* how to handle Comp after   */~
            comperrcst$1,                /* kitting or cost error      */~
            date$8,                      /* SCREEN DATE                */~
            defaddacct$12,               /* Add Std Value Account      */~
            defaddadescr$30,             /* Add Std Value Acct descr.  */~
            defaddrtef$1,                /* Add Std Val Mthd - First   */~
            defaddrtel$1,                /* Add Std Val Mthd - Last    */~
            defaddrtep$1,                /* Add Std Val Mthd - Partial */~
            defvalbom$1,                 /* Val Mtd (partial) - BOM    */~
            defvalcbom$1,                /* Val Mtd (final  ) - BOM    */~
            defvalrte$1,                 /* Val Mtd (partial) - T/Level*/~
            defvalcrte$1,                /* Val Mtd (final  ) - T/Level*/~
            demand_code$1,               /* Demand code as control #   */~
            edtmessage$79,               /* EDIT INSTRUCTIONS          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            gllog$1,                     /* Generate GL Trans log?     */~
            grp_header$(8)79,            /* Group Labels - Display     */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            idle$3,                      /* IDLE TIME OUT              */~
            inc_loc$1,                   /* HNYLOCNS Searched for bin? */~
            inpmessage$79,               /* INSTRUCTION FOR INPUT      */~
            job_incr$5,                  /* Standard Job # Increment   */~
            last_sort$1,                 /* Last Sort Order Displayed  */~
            lfac$(20)1,                  /* FAC FOR INPUT              */~
            line2$79,                    /* Screen Underline           */~
            next_job$8,                  /* Next Standard Job Number   */~
            next_pjjob$8,                /* Next Purchased Job Number  */~
            next_rwjob$8,                /* Next Rework Job Number     */~
            parts$1,                     /* Part Summary Option        */~
            pf$(3)79,                    /* PF Key Display Lines       */~
            pfkeys$18,                   /* PF Keys Enabled            */~
            picklist$1,                  /* Print Pick List Flag       */~
            picko$1,                     /* Sort Order For Pick List   */~
            pj_incr$5,                   /* Purchased Job # Increment  */~
            platform$6,                  /* For determining platform   */~
            rw_incr$5,                   /* Rework Job # Increment     */~
            rw_scrap$12,                 /* Asset Account for Scrap    */~
            rw_scrapdescr$30,            /* Asset Account Description  */~
            rw_wip$12,                   /* WIP Account for Rework     */~
            rw_wipdescr$30,              /* WIP Account Description    */~
            rwksuccess$1,                /* Rework Success Anticipation*/~
            savetif$1,                   /* Save Tiff Data After Post? */~
            shoerr$1,                    /* Show Existance of 'X' Trans*/~
            slbom$1,                     /* Print Single Level BOM Flag*/~
            store$3,                     /* Default Store Code         */~
            storedescr$32,               /* Default Store Description  */~
            submit$3,                    /* SUBMIT TIME OUT            */~
            task_file$1,                 /* Keep summary by task file? */~
            tform$1,                     /* TRAVELER FORMAT            */~
            traveler$1,                  /* Print Traveler Flag        */~
            ttype$1,                     /* TRAVELER TYPE              */~
            tctojob$1,                   /* POST TIME CARD TO JOB      */~
            vendsrvc$1,                  /* Print Vendor Service Sheet */~
            wipvaracct$12,               /* Comp, Variance Account     */~
            wipvardescr$30               /* Comp, Variance Account     */

        dim f1%(04)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * # 2 ! GLMAIN   ! General Ledger Master File               *~
            * # 3 ! STORNAME ! Store Master File                        *~
            * # 4 ! JBMASTR2 ! Production Job Master File               *~
            * # 9 ! WORKFILE ! Work file for screen greasing            *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #2,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select # 4, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos =    1, keylen =   8

            select # 9, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   50,              ~
                        keypos = 1, keylen = 2

            call "SHOSTAT"  ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#2, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#3, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#4, 0%, 0%,   0%, " ")
            call "WORKOPEN" (#9, "IO", 10%, 1%)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            REM Determine what platform we are running on
            call "EXTRACT" addr("S#", platform$)
            unix% = -1%
            convert platform$ to unix%, data goto L09130
L09130:     REM if UNIX% >= 0, this is a VS Platform, otherwise its UNIX

            edtmessage$ = "Place Cursor At Field To Modify And" &        ~
                          " Press (RETURN); PF-32 To EXIT Program."

            str(line2$,63%) = "JBFLAGS: " & cms2v$

            grp_header$(1%) = "General Shop Floor Control:"
            grp_header$(2%) = "Background Processing Options:"
            grp_header$(3%) = "Rework and Scrap Job Options:"
            grp_header$(4%) = "Pick List, Traveler and Kitting Options:"
            grp_header$(5%) = "Job Release Options:"
            grp_header$(6%) = "Time Card Options:"
            grp_header$(7%) = "Job Completion Options:"
            grp_header$(8%) = "Next Job Number Assignment Options:"

            write #9 using L09340,"N","Do Not Print Barcodes"
            write #9 using L09340,"B","'Bear Rock' PC Barcoding Software"
            if unix% >= 0% then L10000
               write #9 using L09340,"M","HP2300 & HP256xx Matrix Printers"
               write #9 using L09340,"L","HP Laserjet Laser Printers"
L09340:     FMT CH(2), CH(40)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") shoerr$, savetif$, gllog$, submit$, idle$, class$, ~
            rwksuccess$, rw_wip$, rw_wipdescr$,rw_scrap$, rw_scrapdescr$,~
            store$, traveler$, picklist$, byprod$, slbom$, binloc$,      ~
            inc_loc$, bin_cnt$, picko$, parts$, ttype$, tform$,          ~
            bar_code$, demand_code$, last_sort$, tctojob$, task_file$,   ~
            defvalbom$, defvalrte$, defvalcbom$, defvalcrte$,            ~
            defaddrtef$, defaddrtep$, defaddrtel$, defaddacct$,          ~
            defaddadescr$, defkitmthd$, comperrkit$, comperrcst$,        ~
            wipvaracct$, wipvardescr$, next_job$, job_incr$, next_rwjob$,~
            rw_incr$, next_pjjob$, pj_incr$, errormsg$, inpmessage$

            pg%, editmode% = 0%

            call "READ100" (#1, "SWITCHS.SFC", f1%(1%))
                 if f1%(1%) = 0% then L10250
                 gosub L30000
                 goto editpg1

L10250:     for fieldnr% = 1% to 9%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10390
L10280:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10350
L10310:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'161(fieldnr%)
                         if enabled% = 0% then L10310
                         goto L10280
L10350:               if keyhit%  = 16% and fieldnr%= 1% then exit_program
                      if keyhit% <>  0% then L10280
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10280
L10390:     next fieldnr%

            for fieldnr% = 1% to 12%
L10420:         gosub'162(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10530
L10440:         gosub'202(fieldnr%)        /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10520
L10470:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'162(fieldnr%)
                         if enabled% = 1% then L10440
                         if fieldnr% = 1% then L10420
                         goto L10470
L10520:               if keyhit% <> 0% then L10440
L10530:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10440
            next fieldnr%

            for fieldnr% = 1% to 10%
L10580:         gosub'163(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10690
L10600:         gosub'203(fieldnr%)        /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10680
L10630:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'163(fieldnr%)
                         if enabled% = 1% then L10600
                         if fieldnr% = 1% then L10580
                         goto L10630
L10680:               if keyhit% <> 0% then L10600
L10690:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10600
            next fieldnr%

            for fieldnr% = 1% to 6%
L10740:         gosub'164(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10850
L10760:         gosub'204(fieldnr%)        /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10840
L10790:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'164(fieldnr%)
                         if enabled% = 1% then L10760
                         if fieldnr% = 1% then L10740
                         goto L10790
L10840:               if keyhit% <> 0% then L10760
L10850:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10760
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            editmode% = 1%
            pg% = 1%
            inpmessage$ = edtmessage$

L11120:     gosub'201(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then editpg2
                  if keyhit%  =  9% then mod_admin
                  if keyhit%  = 16% then datasave
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then L11120
            if cursor%(1%) < 9% then fieldnr% = cursor%(1%) - 5%
            if cursor%(1%) > 8% and cursor%(1%) < 14% then               ~
                fieldnr% = cursor%(1%) - 7%
            if cursor%(1%) > 13% and cursor%(1%) < 19% then              ~
                fieldnr% = cursor%(1%) - 9%
L11240:     if fieldnr% < 1% or fieldnr% > 9% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'161(fieldnr%)
                  if enabled% = 0% then editpg1
L11280:     gosub'201(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11280
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11280
                  lastfieldnr% = fieldnr%
            goto L11240

        editpg2
            lastfieldnr% = 0%
            pg% = 2%
            inpmessage$ = edtmessage$

            gosub'202(0%)              /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then editpg1
                  if keyhit%  =  5% then editpg3
                  if keyhit%  =  9% then mod_admin
                  if keyhit%  = 16% then datasave
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then editpg2
            if cursor%(1%) <  7% then fieldnr% = 1%
            if cursor%(1%) >  6% and cursor%(1%) < 16% then              ~
                fieldnr% = cursor%(1%) - 5%
            if cursor%(1%) > 15% and cursor%(1%) < 21% then              ~
                fieldnr% = cursor%(1%) - 7%
L11520:     if fieldnr% < 1% or fieldnr% > 12% then editpg2
            if fieldnr% = lastfieldnr% then editpg2
            gosub'162(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then editpg2
L11560:     gosub'202(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11560
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11560
                  lastfieldnr% = fieldnr%
            goto L11520

        editpg3
            lastfieldnr% = 0%
            pg% = 3%
            inpmessage$ = edtmessage$

            gosub'203(0%)              /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then editpg2
                  if keyhit%  =  5% then editpg4
                  if keyhit%  =  9% then mod_admin
                  if keyhit%  = 16% then datasave
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then editpg3
            if cursor%(1%) < 8% then fieldnr% = cursor%(1%) - 5%
            if cursor%(1%) > 7% and cursor%(1%) < 19% then               ~
                 fieldnr% = cursor%(1%) - 8%
L11800:     if fieldnr% < 1% or fieldnr% > 10% then editpg3
            if fieldnr% = lastfieldnr% then editpg3
            gosub'163(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then editpg3
L11840:     gosub'203(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11840
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11840
                  lastfieldnr% = fieldnr%
            goto L11800

        editpg4
            lastfieldnr% = 0%
            pg% = 4%
            inpmessage$ = edtmessage$

            gosub'204(0%)              /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then editpg3
                  if keyhit%  =  9% then mod_admin
                  if keyhit%  = 16% then datasave
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then editpg4
            if cursor%(1%) < 10% then fieldnr% = cursor%(1%) - 5%
            if cursor%(1%) > 7% and cursor%(1%) < 14% then               ~
                fieldnr% = cursor%(1%) - 7%
            if cursor%(1%) > 11% and cursor%(1%) < 16% then              ~
                fieldnr% = cursor%(1%) - 9%
L12090:     if fieldnr% < 1% or fieldnr% > 6% then editpg4
            if fieldnr% = lastfieldnr% then editpg4
            gosub'164(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then editpg4
L12130:     gosub'204(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12130
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12130
                  lastfieldnr% = fieldnr%
            goto L12090

        mod_admin    /* Allow maintenance of SFC Module Administrators */
            call "CMSMAINP" ("SFC", "Shop Floor Control")
            on pg% goto editpg1, editpg2, editpg3, editpg4

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            REM FIRST DELETE OLD ENTRY
                call "DELETE" (#1, "SWITCHS.SFC         ", 20%)

            REM NOW GO SAVE...
                gosub L32000

            REM ... AND EXIT
                goto exit_program_warning

        REM *************************************************************~
            *   D E F A U L T / E N A B L E S   F O R  S C R E E N  1   *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields on screen 1.             *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1%
                  errormsg$ = " "
                  on fieldnr% gosub L20200,    /* Warn If Bad Trans     */~
                                    L20250,    /* Retain 'Update' TIF   */~
                                    L20310,    /* Build SFC G/L Posting */~
                                    L20370,    /* Submit Time Out       */~
                                    L20430,    /* Idle Time - Task End  */~
                                    L20490,    /* Submittal Class       */~
                                    L20550,    /* Rework Job Success    */~
                                    L20600,    /* WIP Acct For Rework   */~
                                    L20640     /* Asset Acct For Scrap  */
                  return

L20200: REM Default/Enable Show Errors...
            inpmessage$ = "Prompt If Rejected Transactions Exist "      &~
                          "(N - Ignore, W - Warn, B - Block)."
            return

L20250: REM Default/Enable For Save TIF Data After Print...
            inpmessage$ = "Retain Left Over TIF Data For Further "      &~
                          "Analysis? (Y/N)."
            if savetif$ = " " then savetif$ = "Y"
            return

L20310: REM Default/Enable for Accumulate a SFC G/L Recap
            inpmessage$ = "Accumulate A Shop Floor Control G/L Recap Fo"&~
                          "r Periodic Printing? (Y/N)."
            if gllog$ = " " then gllog$ = "Y"
            return

L20370: REM Default/Enable Submit Timeout (in Seconds)...
            inpmessage$ = "Maximum Seconds Allowed For Opening Files Wh"&~
                          "en Submitting SFC Bckgnd Task?"
            if submit$ = " " then submit$ = "150"
            return

L20430: REM Default/Enable Idle Timeout (in minutes)...
            inpmessage$ = "How Many Minutes Can The Task Stand Idle Bef"&~
                          "ore It Should Remove Itself?"
            if idle$ = " " then idle$ = "240"
            return

L20490: REM Default/Enable Procedure Submit Class...
            inpmessage$ = "Enter Background Procedure Submit Class.  As"&~
                          "terisk (*) To NOT Submit."
            if class$ = " " then class$ = "A"
            return

L20550: REM Default/Enable Rework Success...
            inpmessage$ = "Enter Anticipated Rework Success Flag (A - " &~
                          "All, N - None)."
            return

L20600: REM Default/Enable WIP Account for Rework...
            inpmessage$ = "Enter WIP Account For Rework."
            return

L20640: REM Default/Enable Asset Account for Scrap...
            inpmessage$ = "Enter Asset Account For Scrap."
            return

        REM *************************************************************~
            *   D E F A U L T / E N A B L E S   F O R  S C R E E N  2   *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields on screen 2.             *~
            *************************************************************

            deffn'162(fieldnr%)
                  enabled% = 1%
                  errormsg$ = " "
                  on fieldnr% gosub L22230,    /* Tvlr,PL,ByProd,SlBom, */~
                                              /* & Vend Service        */~
                                    L22320,    /* Default Store Code    */~
                                    L22370,    /* Print Bin Locations   */~
                                    L22430,    /* Include Locations     */~
                                    L22490,    /* Max Number of Bins    */~
                                    L22550,    /* Sort Pick List        */~
                                    L22610,    /* Part Summary Option   */~
                                    L22660,    /* Traveler Type         */~
                                    L22720,    /* Traveler Format       */~
                                    L22790,    /* Bar Codes             */~
                                    L22850,    /* Control # Implement   */~
                                    L22910     /* Display Sort Order    */
              return

L22230: REM Default/Enable Print Traveler,Pick list,By-Product,Sl Bom...
            inpmessage$ = "Enter 'N' Beside Items You Do NOT Want Autom"&~
                          "atically Printed."
                if traveler$ = " " then traveler$ = "Y"
                if picklist$ = " " then picklist$ = "Y"
                if byprod$   = " " then byprod$   = "Y"
                if slbom$    = " " then slbom$    = "Y"
                if vendsrvc$ = " " then vendsrvc$ = "Y"
            return

L22320: REM Default/Enable Default Store Code...
            inpmessage$ = "Enter Default Store Code For Pick Lists & Ki"&~
                           "tting -or- Blank."
            return

L22370: REM Default/Enable Print Bin Locations on Pick List...
            inpmessage$ = "N= No, Y=Inv Master Loc, Q=Inv Qty Loc, A=Al"&~
                          "l Loc, M=Inv Master or Qty Loc."
            if binloc$ = " " then binloc$ = "N"
            return

L22430: REM Default/Enable Include Locns in Search for Primary Bin...
            inpmessage$ = "Should Warehouse Locations Be Included In Se"&~
                          "arch For Bin Locations? (Y/N)"
            if inc_loc$ = " " then inc_loc$ = "N"
            return

L22490: REM Default/Enable Max Bin Locns to Print on Pick Lists...
            inpmessage$ = "Enter Maximum # Of Bin Locations To Print On"&~
                          " Pick Lists, etc. (0-9)"
            if bin_cnt$ = " " then bin_cnt$ = "0"
            return

L22550: REM Default/Ebanle Sort Pick List...
            inpmessage$ = "P=Part Number, L=Bin Location, T=Part Type, "&~
                          "C=Part Catgry, S=Part Class, B=BOM."
            if picko$ = " " then picko$ = "P"
            return

L22610: REM Default/Enable Part Summary Level...
            inpmessage$ = "F = Full List, D = By Part By Day, P = By Part"
            if parts$ = " " then parts$ = "F"
            return

L22660: REM Default/Enable Type of Traveler to Print...
            inpmessage$ = "Indicate Type(s) Of Traveler You Will Be Usi"&~
                          "ng. (S/D/B)"
            if ttype$ = " " then ttype$ = "B"
            return

L22720: REM Default/Enable Type of Traveler Format...
            inpmessage$ = "Select Format For Traveler. Standard Format "&~
                          " Is Multiple Work Centers On 1 Page."
            if tform$ = " " then tform$ = "S"
            if ttype$ = "S" then enabled% = 0%
            return

L22790: REM Default/Enable Bar Code Printing...
            inpmessage$ = "Enter 'N', or Leave Blank & Press RETURN to "&~
                          "See List Of Supported Printers"
            if bar_code$ = " " then bar_code$ = "N"
            return

L22850: REM Default/Enable Control Number Implementation...
            inpmessage$ = "A = Sys Default, No Edit. B = Sys Default, E"&~
                             "dit. C = No Default, No Edit."
            if demand_code$ = " " then demand_code$ = "C"
            return

L22910: REM Default/Enable Sort Order for Standard Job Display...
            inpmessage$ = "Enter Sort Order For Job Number Display, 'P'"&~
                          "art or 'D'ate."
            if last_sort$ = " " then  last_sort$ = "P"
            return

        REM *************************************************************~
            *   D E F A U L T / E N A B L E S   F O R  S C R E E N  3   *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields on screen 3.             *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 1%
                  errormsg$ = " "
                  on fieldnr% gosub L24210,   /* Post Time Cards To Job */~
                                    L24270,   /* Time Card Task Summary */~
                                    L24320,   /* Default Val - Partial  */~
                                    L24370,   /* Default Val - Final    */~
                                    L24420,   /* Default Add Std Value  */~
                                    L24470,   /* Add Std Value Exp Acct */~
                                    L24520,   /* Default Kit Method     */~
                                    L24570,   /* Block On Kit Error     */~
                                    L24630,   /* Block If Cost > 0      */~
                                    L24690    /* Comp Variance Acct     */
              return

L24210: REM Default/Enable Post Time Cards to Jobs...
            inpmessage$ = "Indicate Whether Or Not Time Cards Are To Be"&~
                          " Posted To Jobs. (Y/N)"
            if tctojob$ = " " then tctojob$ = "Y"
            return

L24270: REM Default/Enable Maintain Task Summary File...
            inpmessage$ = "Keep Summary File By Time Card Task? (Y/N)."
            if task_file$ <> "Y" then task_file$ = "N"
            return

L24320: REM Default/Enable Valuation Methods Partial...
            inpmessage$ = "Completion Valu Method: A-All, P-Prorate, S-"&~
                           "Std, Z-0 or Blank; B-BOM (BOM Only)."
            return

L24370: REM Default/Enable Valuation Methods Final...
            inpmessage$ = "Completion Valuation Method: A - All, P - Pr"&~
                           "orate, S - Std, Z - Zero or Blank."
            return

L24420: REM Default/Enable Valuation Methods Add Std Value...
            inpmessage$ = "Add Std Value To Job Method: A - All, P - Pr"&~
                           "orate or Blank."
            return

L24470: REM Default/Enable Value Added Expense Account...
            inpmessage$ = "Default Expense Account For Standard Value A"&~
                          "dded."
            return

L24520: REM Default/Enable Kit Method for Kitting...
            inpmessage$ = "Default For Completion Kitting Method: A - A"&~
                          "ll, P - Prorate or Blank."
            return

L24570: REM Default/Enable Block Completion on Kit Error...
            if comperrkit$ = " " then comperrkit$ = "Y"
            inpmessage$ = "Block subs. Comp.: Y - Always, N - Never, B "&~
                          "- If Matl. (BOM) Valuation is A/P."
            return

L24630: REM Default/Enable Block Completion if Cost Less Than Zero...
            if comperrcst$ = " " then comperrcst$ = "Y"
            inpmessage$ = "Block Comp. if Costs are less than zero: Y -"&~
                          " Block, T - Total or N - No, cont."
            return

L24690: REM Default/Enable Completion Varaince Account...
            inpmessage$ = "Enter Account To trap Completion Variances"  &~
                          " (WIP <> Inventory Asset Posting)."
            return

        REM *************************************************************~
            *   D E F A U L T / E N A B L E S   F O R  S C R E E N  4   *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields on screen 4.             *~
            *************************************************************

            deffn'164(fieldnr%)
                  enabled% = 1%
                  errormsg$ = " "
                  on fieldnr% gosub L26170,   /* Next Standard Job No.  */~
                                    L26210,   /* Standard Job Increment */~
                                    L26270,   /* Next Rework Job No.    */~
                                    L26310,   /* Rework Job Increment   */~
                                    L26370,   /* Next Purchase Job No.  */~
                                    L26410    /* Purchase Job Increment */
              return

L26170: REM Default/Enable Next Standard Job Number...
            inpmessage$ = "Enter Next Standard Job Number Default."
            return

L26210: REM Default/Enable Next Standard Job Increment...
            if job_incr$ = " " then job_incr$ = "10"
            inpmessage$ = "Enter Increment To Be Used When Assigning St"&~
                          "andard Job Numbers."
            return

L26270: REM Default/Enable Next Rework Job Number...
            inpmessage$ = "Enter Next Rework Job Number Default."
            return

L26310: REM Default/Enable Next Rework Job Increment...
            if rw_incr$ = " " then rw_incr$ = "10"
            inpmessage$ = "Enter Increment To Be Used When Assigning Re"&~
                          "work Job Numbers."
            return

L26370: REM Default/Enable Next Purchase Job Number...
            inpmessage$ = "Enter Next Purchase Job Number Default."
            return

L26410: REM Default/Enable Next Purchase Job Increment...
            if pj_incr$ = " " then pj_incr$ = "10"
            inpmessage$ = "Enter Increment To Be Used When Assigning Pu"&~
                          "rchase Job Numbers."
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
            on k% + 1% goto L29140, L29170
            return

L29140:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29170:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

L30000: REM *************************************************************~
            *           R E C A L L   O L D   S W I T C H E S           *~
            *                                                           *~
            * LOADS EXISTING SWITCHES FROM FILE                         *~
            *************************************************************

            get #1, using L31000, submit, class$, idle, binloc$, tctojob$,~
                    ttype$, savetif$, gllog$, picko$, tform$, parts$,    ~
                    bar_code$, task_file$, demand_code$, store$,         ~
                    inc_loc$, bin_cnt%, defvalbom$, defvalrte$,          ~
                    defaddrtef$, defaddacct$, defkitmthd$,               ~
                    comperrkit$, comperrcst$, defvalcbom$, defvalcrte$,  ~
                    defaddrtel$, defaddrtep$, wipvaracct$, rwksuccess$,  ~
                    shoerr$, next_job$, job_incr%, last_sort$,           ~
                    next_rwjob$, rw_incr%, rw_wip$, rw_scrap$,           ~
                    next_pjjob$, pj_incr%, traveler$, picklist$, byprod$,~
                    slbom$, vendsrvc$

        REM Format Conversion For Screen And Recall Names...
            call "CONVERT" (submit, 0.0, submit$)
            call "CONVERT" (idle, 0.0, idle$)
            convert bin_cnt% to bin_cnt$, pic(0)
            convert job_incr% to job_incr$, pic(#####)
            convert rw_incr% to rw_incr$, pic(#####)
            convert pj_incr% to pj_incr$, pic(#####)

            if savetif$     =  " "  then savetif$     = "Y"
            if gllog$       =  " "  then gllog$       = "Y"
            if submit$      =  " "  then submit$      = "150"
            if idle$        =  " "  then idle$        = "240"
            if class$       =  " "  then class$       = "A"
            if binloc$      =  " "  then binloc$      = "N"
            if inc_loc$     =  " "  then inc_loc$     = "N"
            if bin_cnt$     =  " "  then bin_cnt$     = "N"
            if picko$       =  " "  then picko$       = "P"
            if parts$       =  " "  then parts$       = "F"
            if ttype$       =  " "  then ttype$       = "B"
            if tform$       =  " "  then tform$       = "S"
            if bar_code$    =  " "  then bar_code$    = "N"
            if demand_code$ = " "   then demand_code$ = "C"
            if last_sort$   = " "   then last_sort$   = "P"
            if tctojob$     = " "   then tctojob$     = "Y"
            if task_file$ <> "Y"    then task_file$   = "N"
            if comperrkit$  = " "   then comperrkit$  = "Y"
            if comperrcst$  = " "   then comperrcst$  = "Y"
            if traveler$    = " "   then traveler$    = "Y"
            if picklist$    = " "   then picklist$    = "Y"
            if byprod$      = " "   then byprod$      = "Y"
            if slbom$       = " "   then slbom$       = "Y"

            if store$ <> " " then                                        ~
               call "GETCODE" (#3, store$, storedescr$, 0%, 99, f1%(3%))
            if defaddacct$ = " " then L30560
               call "GLFMT" (defaddacct$)
               call "GETCODE" (#2, str(defaddacct$,1%,9%), defaddadescr$,~
                               0%, 99, f1%(2%))
L30560:     if wipvaracct$ = " " then L30600
               call "GLFMT" (wipvaracct$)
               call "GETCODE" (#2, str(wipvaracct$,1%,9%), wipvardescr$, ~
                              0%, 99, f1%(2%))
L30600:     if rw_wip$ = " " then L30640
               call "GLFMT" (rw_wip$)
               call "GETCODE" (#2, str(rw_wip$,1%,9%), rw_wipdescr$,     ~
                              0%, 99, f1%(2%))
L30640:     if rw_scrap$ = " " then return
               call "GLFMT" (rw_scrap$)
               call "GETCODE" (#2, str(rw_scrap$,1%,9%), rw_scrapdescr$, ~
                              0%, 99, f1%(2%))
               return

L31000:     FMT      XX(20), /*    1/20  /* SKIP IDENTIFIER            */~
                     BI(2),  /*   21/2   /* SUBMIT TIME OUT            */~
                     CH(1),  /*   23/1   /* SUBMIT CLASS               */~
                     BI(2),  /*   24/2   /* IDLE TASK TIME OUT         */~
                     CH(1),  /*   26/1   /* PRINT BIN LOCATIONS?       */~
                     CH(1),  /*   27/1   /* TIME CARDS TO JOBS         */~
                     CH(1),  /*   28/1   /* TRAVELER TYPE              */~
                     CH(1),  /*   29/1   /* SAVE TIF DATA?             */~
                     CH(1),  /*   30/1   /* GENERATE GL LOG?           */~
                     CH(1),  /*   31/1   /* PICK LIST SORT ORDER       */~
                     CH(1),  /*   32/1   /* TRAVELER FORMAT            */~
                     CH(1),  /*   33/1   /* PART SUMMARY LEVEL         */~
                     CH(1),  /*   34/1   /* BAR_CODES                  */~
                     CH(1),  /*   35/1   /* Task Summary File          */~
                     CH(1),  /*   36/1   /* DEMAND CODE                */~
                     CH(3),  /*   37/3   /* Default Store              */~
                     CH(1),  /*   40/1   /* Include Locns in Bin Search*/~
                     BI(1),  /*   41/1   /* Max # of Bins to Print     */~
                     CH(1),  /*   42/1   /* Comp Val. / BOM / Partail  */~
                     CH(1),  /*   43/1   /* Comp Val. / RTE / Partail  */~
                     CH(1),  /*   44/1   /* Add Std Val Mthd - First   */~
                     CH(9),  /*   45/9   /* Add Std Val Acct           */~
                     CH(1),  /*   54/1   /* Comp Kit Method            */~
                     CH(1),  /*   55/1   /* Comp Error - Kit           */~
                     CH(1),  /*   56/1   /* Comp Error - Cost          */~
                     CH(1),  /*   57/1   /* Comp Val. / BOM / Final    */~
                     CH(1),  /*   58/1   /* Comp Val. / RTE / Final    */~
                     CH(1),  /*   59/1   /* Add Std Val Mthd - Last    */~
                     CH(1),  /*   60/1   /* Add Std Val Mthd - Partial */~
                     CH(9),  /*   61/9   /* WIP Variance Account       */~
                     CH(1),  /*   70/1   /* Rework Success             */~
                     CH(1),  /*   71/1   /* Warn if Bad Trans          */~
                     CH(8),  /*   72/8   /* Next Standard Job Number   */~
                     BI(4),  /*   80/4   /* Standard Job # Increment   */~
                     CH(1),  /*   84/1   /* Last Sort Order For Display*/~
                     CH(8),  /*   85/8   /* Next Rework Job Number     */~
                     BI(4),  /*   93/4   /* Rework Job # Increment     */~
                     CH(9),  /*   97/9   /* WIP Account For Rework     */~
                     CH(9),  /*  106/9   /* Asset Account For Scrap    */~
                     CH(8),  /*  115/8   /* Next Purchase Job Number   */~
                     BI(4),  /*  123/4   /* Purchase Job # Increment   */~
                     CH(1),  /*  127/1   /* Print Traveler Flag        */~
                     CH(1),  /*  128/1   /* Print Pick List Flag       */~
                     CH(1),  /*  129/1   /* Print By-Prod List Flag    */~
                     CH(1),  /*  130/1   /* Print Single Level BOM Flag*/~
                     CH(1)   /*  131/1   /* Print Vendor Service Sheet */

L32000: REM *************************************************************~
            *            S A V E   N E W   S W I T C H E S              *~
            *                                                           *~
            * SAVES ENTERED SWITCHES ON FILE                            *~
            *************************************************************

        REM First Convert Conversion Factor to Numeric...
            submit = 150
            idle = 240
            bin_cnt%, job_incr%, rw_incr%, pj_incr% = 0%

            convert submit$   to submit, data goto L32120
L32120:     convert idle$     to idle, data goto L32130
L32130:     convert bin_cnt$  to bin_cnt%, data goto L32140
L32140:     convert job_incr$ to job_incr%, data goto L32150
L32150:     convert rw_incr$  to rw_incr%, data goto L32160
L32160:     convert pj_incr$  to pj_incr%, data goto L32230


L32230:     if defaddacct$ <> " " then call "GLUNFMT" (defaddacct$)
            if wipvaracct$ <> " " then call "GLUNFMT" (wipvaracct$)
            if rw_wip$     <> " " then call "GLUNFMT" (rw_wip$)
            if rw_scrap$   <> " " then call "GLUNFMT" (rw_scrap$)

            write #1, using L33000, "SWITCHS.SFC", submit, class$, idle,  ~
                                   binloc$, tctojob$, ttype$, savetif$,  ~
                                   gllog$, picko$, tform$, parts$,       ~
                                   bar_code$, task_file$, demand_code$,  ~
                                   store$, inc_loc$, bin_cnt%,           ~
                                   defvalbom$, defvalrte$, defaddrtef$,  ~
                                   defaddacct$, defkitmthd$,             ~
                                   comperrkit$, comperrcst$,             ~
                                   defvalcbom$, defvalcrte$,             ~
                                   defaddrtel$, defaddrtep$,             ~
                                   wipvaracct$, rwksuccess$,             ~
                                   shoerr$, next_job$, job_incr%,        ~
                                   last_sort$, next_rwjob$, rw_incr%,    ~
                                   rw_wip$, rw_scrap$, next_pjjob$,      ~
                                   pj_incr%, traveler$, picklist$,       ~
                                   byprod$, slbom$, vendsrvc$,  " ", " "

            return

L33000:     FMT      CH(20), /*    1/20  /* PUT IN INDENTIFIER         */~
                     BI(2),  /*   21/2   /* SUBMIT TIME OUT            */~
                     CH(1),  /*   23/1   /* SUBMIT CLASS               */~
                     BI(2),  /*   24/2   /* IDLE TASK TIME OUT         */~
                     CH(1),  /*   26/1   /* PRINT BIN LOCATIONS?       */~
                     CH(1),  /*   27/1   /* TIME CARD TO JOBS?         */~
                     CH(1),  /*   28/1   /* TRAVELER TYPE              */~
                     CH(1),  /*   29/1   /* SAVE TIF DATA?             */~
                     CH(1),  /*   30/1   /* GENERATE GL LOG?           */~
                     CH(1),  /*   31/1   /* PICK LIST SORT ORDER       */~
                     CH(1),  /*   32/1   /* TRAVELER FORMAT            */~
                     CH(1),  /*   33/1   /* PART SUMMARY LEVEL         */~
                     CH(1),  /*   34/1   /* BAR CODES (Y/N)            */~
                     CH(1),  /*   35/1   /* Task file?                 */~
                     CH(1),  /*   36/1   /* DEMAND_CODES (A/B/C)       */~
                     CH(3),  /*   37/3   /* Default Store              */~
                     CH(1),  /*   40/1   /* Include locns in Bin Search*/~
                     BI(1),  /*   41/1   /* Max # of Bins to Print     */~
                     CH(1),  /*   42/1   /* Comp Val. / BOM / Partial  */~
                     CH(1),  /*   43/1   /* Comp Val. / RTE / Partial  */~
                     CH(1),  /*   44/1   /* Add Std Val Mthd - First   */~
                     CH(9),  /*   45/9   /* Add Std Val Acct           */~
                     CH(1),  /*   54/1   /* Comp Kit Method            */~
                     CH(1),  /*   55/1   /* Comp Error - Kit           */~
                     CH(1),  /*   56/1   /* Comp Error - Cost          */~
                     CH(1),  /*   57/1   /* Comp Val. / BOM / Final    */~
                     CH(1),  /*   58/1   /* Comp Val. / RTE / Final    */~
                     CH(1),  /*   59/1   /* Add Std Val Mthd - Last    */~
                     CH(1),  /*   60/1   /* Add Std Val Mthd - Partial */~
                     CH(9),  /*   61/9   /* WIP Variance Account       */~
                     CH(1),  /*   70/1   /* Rework Success Flag        */~
                     CH(1),  /*   71/1   /* Warn if Bad Trans          */~
                     CH(8),  /*   72/8   /* Next Standard Job Number   */~
                     BI(4),  /*   80/4   /* Standard Job # Increment   */~
                     CH(1),  /*   84/1   /* Last Sort Order For Display*/~
                     CH(8),  /*   85/8   /* Next Rework Job Number     */~
                     BI(4),  /*   93/4   /* Rework Job # Increment     */~
                     CH(9),  /*   97/9   /* WIP Account For Rework     */~
                     CH(9),  /*  106/9   /* Asset Account For Scrap    */~
                     CH(8),  /*  115/8   /* Next Purchase Job Number   */~
                     BI(4),  /*  123/4   /* Purchase Job # Increment   */~
                     CH(1),  /*  127/1   /* Print Traveler Flag        */~
                     CH(1),  /*  128/1   /* Print Pick List Flag       */~
                     CH(1),  /*  129/1   /* Print By Prod List Flag    */~
                     CH(1),  /*  130/1   /* Print Single Level BOM Flag*/~
                     CH(1),  /*  131/1   /* Print Vendor Service Sheet */~
                     CH(184),/*  132/184 /* Filler                     */~
                     CH(185) /*  316/185 /* Filler                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  gosub set_pf1
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L40120:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Shop Floor Control Module Behavior Switches",  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), grp_header$(1%)        , ch(79),~
               at (06,03), "Warn If There Are Bad Transactions For Job?",~
               at (06,55), fac(lfac$(1%)), shoerr$              , ch(01),~
               at (07,03), "Should 'Update' TIF Data Be Retained?",      ~
               at (07,55), fac(lfac$(2%)), savetif$             , ch(01),~
               at (08,03),"Should 'Update' Build SFC G/L Posting Recaps?"~
        ,                                                                ~
               at (08,55), fac(lfac$(3%)), gllog$               , ch(01),~
                                                                         ~
               at (10,02), fac(hex(ac)), grp_header$(2%)        , ch(79),~
               at (11,03), "Time Out For Background Task Submit (in secon~
        ~ds)",                                                            ~
               at (11,53), fac(lfac$(4%)), submit$              , ch(03),~
               at (12,03), "Idle Time Until Task Ends Itself (in minutes)~
        ~",                                                               ~
               at (12,53), fac(lfac$(5%)), idle$                , ch(03),~
               at (13,03), "Background Procedure Submittal Class (A-Z)", ~
               at (13,55), fac(lfac$(6%)), class$               , ch(01),~
                                                                         ~
               at (15,02), fac(hex(ac)), grp_header$(3%)        , ch(79),~
               at (16,03), "Rework Job Success Flag",                    ~
               at (16,55), fac(lfac$(7%)), rwksuccess$          , ch(01),~
               at (17,03), "WIP Account For Rework",                     ~
               at (17,28), fac(lfac$(8%)), rw_wip$              , ch(12),~
               at (17,42), fac(hex(8c)),   rw_wipdescr$         , ch(30),~
               at (18,03), "Asset Account For Scrap",                    ~
               at (18,28), fac(lfac$(9%)), rw_scrap$            , ch(12),~
               at (18,42), fac(hex(8c)),   rw_scrapdescr$       , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40600
                   call "MANUAL" ("JBFLAGS")
                   goto L40120

L40600:        if keyhit% <> 15% then L40640
                   call "PRNTSCRN"
                   goto L40120

L40640:        close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'202(fieldnr%)
                  gosub set_pf1
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L42120:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Shop Floor Control Module Behavior Switches",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), grp_header$(4%)        , ch(79),~
               at (05,03), "Print, Traveler:",                           ~
               at (05,20), fac(lfac$(1%)), traveler$            , ch(01),~
               at (05,22), "Pick List:",                                 ~
               at (05,32), fac(lfac$(1%)), picklist$            , ch(01),~
               at (05,34), "By-Product List:",                           ~
               at (05,51), fac(lfac$(1%)), byprod$              , ch(01),~
               at (05,53), "Single Level BOM:",                          ~
               at (05,71), fac(lfac$(1%)), slbom$               , ch(01),~
               at (06,03), "       Vend Service Sheet:",                 ~
               at (06,30), fac(lfac$(1%)), vendsrvc$            , ch(01),~
               at (07,03), "Default Store Code For Pick List & Kitting", ~
               at (07,53), fac(lfac$(2%)), store$               , ch(03),~
               at (07,57), fac(hex(8c)), storedescr$            , ch(23),~
               at (08,03), "Should Bin Locations Be Printed On Job Pick L~
        ~ist?",                                                           ~
               at (08,55), fac(lfac$(3%)), binloc$              , ch(01),~
               at (09,03), "Warehouse Locs To Be Included In Search For B~
        ~ins?",                                                           ~
               at (09,55), fac(lfac$(4%)), inc_loc$             , ch(01),~
               at (10,03), "Max. # Of Bin Locations To Print On Pick List~
        ~s",                                                              ~
               at (10,55), fac(lfac$(5%)), bin_cnt$             , ch(01),~
               at (11,03), "Sort Order Of Components On Job Pick List Rep~
        ~ort",                                                            ~
               at (11,55), fac(lfac$(6%)), picko$               , ch(01),~
               at (12,04), "If By Part, 'F'ull, By Part By 'D'ay, By 'P'a~
        ~rt",                                                             ~
               at (12,55), fac(lfac$(7%)), parts$               , ch(01),~
               at (13,03), "Traveler Type: 'S'ummary, 'D'etailed, Or 'B'o~
        ~th?",                                                            ~
               at (13,55), fac(lfac$(8%)), ttype$               , ch(01),~
               at (14,03), "On Detailed Traveler: 'S'tandard, 'O'ne WS/Pa~
        ~ge?",                                                            ~
               at (14,55), fac(lfac$(9%)), tform$               , ch(01),~
               at (15,03), "Bar Codes On Traveler And Pick List?",       ~
               at (15,55), fac(lfac$(10%)), bar_code$           , ch(01),~
                                                                         ~
               at (17,02), fac(hex(ac)), grp_header$(5%)        , ch(79),~
               at (18,03), "Control Number Implementation Method",       ~
               at (18,55), fac(lfac$(11%)), demand_code$        , ch(01),~
               at (19,03), "Sort Order For Standard Job Display (P or D)"~
        ,                                                                ~
               at (19,55), fac(lfac$(12%)), last_sort$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42740
                   call "MANUAL" ("JBFLAGS ")
                   goto L42120

L42740:        if keyhit% <> 15% then L42780
                   call "PRNTSCRN"
                   goto L42120

L42780:        close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'203(fieldnr%)
                  gosub set_pf1
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L44120:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Shop Floor Control Module Behavior Switches",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), grp_header$(6%)        , ch(79),~
               at (06,03), "Should Time Cards Be Posted To Jobs?",       ~
               at (06,50), fac(lfac$(1%)), tctojob$             , ch(01),~
               at (07,03), "Maintain A Summary File By Time Card Task?", ~
               at (07,50), fac(lfac$(2%)), task_file$           , ch(01),~
                                                                         ~
               at (10,02), fac(hex(ac)), grp_header$(7%)        , ch(79),~
               at (11,03), "Default Valuation Method (Partial Comp.) BOM:~
        ~",                                                               ~
               at (11,50), fac(lfac$(3%)), defvalbom$           , ch(01),~
               at (11,53), "Val. Added (This Lvl):",                     ~
               at (11,76), fac(lfac$(3%)), defvalrte$           , ch(01),~
               at (12,03), "Default Valuation Method (Final Comp.)   BOM:~
        ~",                                                               ~
               at (12,50), fac(lfac$(4%)), defvalcbom$          , ch(01),~
               at (12,53), "Val. Added (This Lvl):",                     ~
               at (12,76), fac(lfac$(4%)), defvalcrte$          , ch(01),~
               at (13,03), "Default Method for Add Std Value  First COMP:~
        ~",                                                               ~
               at (13,50), fac(lfac$(5%)), defaddrtef$          , ch(01),~
               at (13,53), "Partial:",                                   ~
               at (13,62), fac(lfac$(5%)), defaddrtep$          , ch(01),~
               at (13,64), "Last:",                                      ~
               at (13,70), fac(lfac$(5%)), defaddrtel$          , ch(01),~
               at (14,03), "Default Exp Account For V/Added ",           ~
               at (14,36), fac(lfac$(6%)), defaddacct$          , ch(12),~
               at (14,50), fac(hex(8c)), defaddadescr$          , ch(30),~
               at (15,03), "Default Kit Method For Kitting",             ~
               at (15,50), fac(lfac$(7%)), defkitmthd$          , ch(01),~
               at (16,03), "Block Completion On Kit Error ",             ~
               at (16,50), fac(lfac$(8%)), comperrkit$          , ch(01),~
               at (17,03), "Block Completion If Cost < 0  ",             ~
               at (17,50), fac(lfac$(9%)), comperrcst$          , ch(01),~
               at (18,03), "Completion Variance Account",                ~
               at (18,36), fac(lfac$(10%)), wipvaracct$         , ch(12),~
               at (18,50), fac(hex(8c)), wipvardescr$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L44680
                   call "MANUAL" ("JBFLAGS ")
                   goto L44120

L44680:        if keyhit% <> 15% then L44720
                   call "PRNTSCRN"
                   goto L44120

L44720:        close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'204(fieldnr%)
                  gosub set_pf2
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L46120:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Shop Floor Control Module Behavior Switches",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), grp_header$(8%)        , ch(79),~
               at (06,03), "Next Standard Job Number",                   ~
               at (06,50), fac(lfac$(1%)), next_job$            , ch(08),~
               at (07,03), "Standard Job Number Increment",              ~
               at (07,50), fac(lfac$(2%)), job_incr$            , ch(05),~
               at (10,03), "Next Rework Job Number",                     ~
               at (10,50), fac(lfac$(3%)), next_rwjob$          , ch(08),~
               at (11,03), "Rework Job Number Increment",                ~
               at (11,50), fac(lfac$(4%)), rw_incr$             , ch(05),~
               at (14,03), "Next Purchase Job Number",                   ~
               at (14,50), fac(lfac$(5%)), next_pjjob$          , ch(08),~
               at (15,03), "Purchase Job Number Increment",              ~
               at (15,50), fac(lfac$(6%)), pj_incr$             , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L46450
                   call "MANUAL" ("JBFLAGS ")
                   goto L46120

L46450:        if keyhit% <> 15% then L46490
                   call "PRNTSCRN"
                   goto L46120

L46490:        close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               P F K E Y   S E T U P                       *~
            *-----------------------------------------------------------*~
            * PFkey setup for screens 1, 2, 3 and 4.                    *~
            *************************************************************

        set_pf1
        if editmode% = 1% then L48210     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                   (16)Exit (no change)"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f10ff00)
            if fieldnr% = 1% then L48170
                str(pf$(3%),60%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
L48170:     if fieldnr% > 1% then L48190
                str(pf$(2%),18%,26%)= " " : str(pfkeys$,4%,1%)= hex(ff)
L48190:     return

L48210: if fieldnr% > 0% then L48320  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (4)Prev Screen         " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (5)Next Screen         " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (9)Maintain SFC Module " &        ~
                     "Administrators         (16)Save Data   "
            pfkeys$ = hex(01ffff0405ffffff09ffffff0dff0f102000)
            if pg% > 1% then L48310
                str(pf$(1%),18%,15%) = " "  :  str(pfkeys$,4%,1%)= hex(ff)
L48310:     return
L48320:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffff00)
            return

        set_pf2
        if editmode% = 1% then L48550     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0fffff00)
            if fieldnr% > 1% then L48530
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L48530:     return

L48550: if fieldnr% > 0% then L48640  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (4)Prev Screen         " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (9)Maintain SFC Module " &        ~
                     "Administrators         (16)Save Data   "
            pfkeys$ = hex(01ffff04ffffffff09ffffff0dff0f102000)
            return
L48640:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests Data For Items On Screen 1.                         *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50190,    /* Warn If Bad Trans     */~
                                    L50240,    /* Retain 'Update' TIF   */~
                                    L50290,    /* Build SFC G/L Posting */~
                                    L50340,    /* Submit Time Out       */~
                                    L50380,    /* Idle Time - Task End  */~
                                    L50420,    /* Submittal Class       */~
                                    L50490,    /* Rework Job Success    */~
                                    L50540,    /* WIP Acct For Rework   */~
                                    L50650     /* Asset Acct For Scrap  */
                  return

L50190: REM Test Data For Warn If Bad Transaction...
            if pos("WBN" = shoerr$) <> 0% then return
                errormsg$ = "Please Enter B, N, or W"
                return

L50240: REM Test Data For Retain Update TIF Data...
            if pos("YN" = savetif$) <> 0% then return
                 errormsg$ = "Please Enter Y or N"
                 return

L50290: REM Test Data For Build SFC G/L Posting...
            if pos("YN" = gllog$) <> 0% then return
                 errormsg$ = "Please Enter Y or N"
                 return

L50340: REM Test Data For Submit Time Out (in seconds)...
            call "NUMTEST" (submit$, 20, 480, errormsg$, -0.001, submit)
                return

L50380: REM Test Data For Idle Time Out (in minutes)...
            call "NUMTEST" (idle$, 10, 999, errormsg$, -0.001, idle)
            return

L50420: REM Test Data For Procedure Submittal Class...
            if class$ >= "A" and class$ <= "Z" then return
                if class$ = "*" then return
                     errormsg$ = "Class MUST Be Between A & Z or an Ast"&~
                                 "erisk (*)"
                     return

L50490: REM Test Rework Success...
            if pos("AN" = rwksuccess$) <> 0% then return
                errormsg$ = "Please Enter A or N"
                return

L50540: REM Test WIP Account For Rework...
            rw_wipdescr$ = " "
            if rw_wip$ = " " then return
               call "GETCODE" (#2, rw_wip$, rw_wipdescr$, 0%, 0, f1%(2%))
                   if f1%(2%) <> 0% then return
               errormsg$ = "Please Enter a Valid Account or Leave Blank"
               return

L50650: REM Test Asset Account For Scrap...
            rw_scrapdescr$ = " "
            if rw_scrap$ = " " then return
               call "GETCODE" (#2, rw_scrap$,rw_scrapdescr$,0%,0, f1%(2%))
                   if f1%(2%) <> 0% then L50730
               errormsg$ = "Please Enter a Valid Account or Leave Blank"
               return
L50730:     get #2 using L50740, accttype$
L50740:         FMT POS(40), CH(1)
            if pos("AE" = accttype$) <> 0% then return
                errormsg$ = "MUST Be an Asset or Expense Account"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52220,    /*Tvlr,Pl,Byprod,SL Bom  */~
                                    L52330,    /* Default Store Code    */~
                                    L52410,    /* Print Bin Locations   */~
                                    L52460,    /* Include Locations     */~
                                    L52510,    /* Max Number of Bins    */~
                                    L52570,    /* Sort Pick List        */~
                                    L52620,    /* Part Summary Option   */~
                                    L52670,    /* Traveler Type         */~
                                    L52720,    /* Traveler Format       */~
                                    L52770,    /* Bar Codes             */~
                                    L52820,    /* Control # Implement   */~
                                    L52870     /* Display Sort Order    */
              return

L52220: REM Test Data For Traveler,Pick List,By Prod list,SL Bom,& Service
            if pos("YN" = traveler$) <> 0% then L52250
                goto L52300
L52250:     if pos("YN" = picklist$) <> 0% then L52270
                goto L52300
L52270:     if pos("YN" = byprod$) <> 0% then L52290
                goto L52300
L52290:     if pos("YN" = slbom$) <> 0% then L52294
                goto L52300
L52294:     if pos("YN" = vendsrvc$) <> 0% then return
L52300:         errormsg$ = "Please Enter Y or N"
                return

L52330: REM Test Data For Default Store Code...
            storedescr$ = " "
            if store$ = " " then return
                call "GETCODE" (#3, store$, storedescr$, 0%, 0, f1%(3%))
                     if f1%(3%) <> 0% then return
                errormsg$ = "Please Select or Enter a Valid Store Code"
                return

L52410: REM Test Data For Print Bin Locations On Pick Lists...
            if pos("YNAQM" = binloc$) <> 0% then return
                errormsg$ = "Please Enter Y, N, A, Q, or M"
                return

L52460: REM Test Data For Include Locations In Bin Search...
            if pos("YN" = inc_loc$) <> 0% then return
                errormsg$ = "Please Enter Y or N"
                return

L52510: REM Test Data For Max Bins To Print On Pick Slip...
            convert bin_cnt$ to bin_cnt%, data goto L52540
            if bin_cnt% > -1% and bin_cnt% < 11% then return
L52540:         errormsg$ = "Please Enter a Number From 0 Thru 9"
                return

L52570: REM Test Data For Sort Pick List Option...
            if pos("PLBTCS" = picko$) <> 0% then return
                errormsg$="Please Enter P, L, T, C, S, or B"
                return

L52620: REM Test Data For Part Summary Option...
            if pos("FDP" = parts$) <> 0% then return
                errormsg$="Please Enter F, D, or P"
                return

L52670: REM Test Data For Traveler Type...
            if pos("SDB" = ttype$) <> 0% then return
                errormsg$ = "Please Enter S, D, or B"
                return

L52720: REM Test Data For Traveler Format...
            if pos("SO" = tform$) <> 0% then return
                errormsg$ = "Please Enter S or O"
                return

L52770: REM Test Data For Bar Code Printing...
            call "GETCODE" (#9, bar_code$, " ", 1%, 0.48, f1%(9))
            if f1%(9) <> 0 then return
                errormsg$ = "Invalid Barcode Option.  Please Re-select."
                return

L52820: REM Test Data For Control Number Implementation...
            if pos("ABC" = demand_code$) <> 0% then return
                errormsg$ = "Please Enter A, B, or C"
                return

L52870: REM Test Data For Last Display Sort Order...
            if pos("DP" = last_sort$) <> 0% then return
                errormsg$ = "Please Enter P or D"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54200,   /* Post Time Cards To Job */~
                                    L54250,   /* Time Card Task Summary */~
                                    L54300,   /* Default Val - Partial  */~
                                    L54360,   /* Default Val - Final    */~
                                    L54420,   /* Default Add Std Value  */~
                                    L54490,   /* Add Std Value Exp Acct */~
                                    L54590,   /* Default Kit Method     */~
                                    L54640,   /* Block On Kit Error     */~
                                    L54690,   /* Block If Cost > 0      */~
                                    L54740    /* Comp Variance Acct     */
              return

L54200: REM Test Data For Post Time Cards To Jobs...
            if pos("YN" = tctojob$) <> 0% then return
                errormsg$ = "Please Enter Y or N"
                return

L54250: REM Test Data For Maintain Summary File By TC Task...
            if pos("YN" = task_file$) <> 0% then return
                errormsg$ = "Please Enter Y or N"
                return

L54300: REM Test Data For Valuation Method - Partial...
            if pos(" APSZB" = defvalbom$)  = 0% then L54330
            if defvalrte$ = "B" then L54334
            if pos(" APSZ" = defvalrte$) <> 0% then return
L54330:         errormsg$ = "Please Enter A, P, S, Z, B, or Leave Blank"
                return
L54334:         errormsg$ = "'B' is not valid for Added Value."
                return

L54360: REM Test Data For Valuation Method - Final...
            if pos(" APSZ" = defvalcbom$)  = 0% then L54390
            if pos(" APSZ" = defvalcrte$) <> 0% then return
L54390:         errormsg$ = "Please Enter A, P, S, Z, or Leave Blank"
               return

L54420: REM Test Data For Value Add Method...
            if pos(" AP" = defaddrtef$)   = 0% then L54460
            if pos(" AP" = defaddrtep$)   = 0% then L54460
            if pos(" AP" = defaddrtel$)  <> 0% then return
L54460:         errormsg$ = "Please Enter A, P, or Leave Blank"
            return

L54490: REM Test Default Expense Account
            defaddadescr$ = " "
            if defaddacct$ = " " then return
               call "GETCODE" (#2,defaddacct$,defaddadescr$,0%,0,f1%(2%))
                   if f1%(2%) <> 0% then return
               errormsg$ = "Please Enter a Valid Account or Leave Blank"
               return

L54590: REM Test Data For Default Kit Method...
            if pos(" AP" = defkitmthd$)  <> 0% then return
                errormsg$ = "Please Enter A, P, or Leave Blank"
                return

L54640: REM Test Data For Completeion Error - Kit...
            if pos("YNB" = comperrkit$)  <> 0% then return
                errormsg$ = "Please Enter Y, N, or B"
                return

L54690: REM Test Data For Completeion Error - Cost
            if pos("YNT" = comperrcst$)  <> 0% then return
                errormsg$ = "Please Enter Y, N, or T"
                return

L54740: REM Test Data For Completion Variance Account...
            wipvardescr$ = " "
            if wipvaracct$ = " " then return
               call "GETCODE" (#2,wipvaracct$,wipvardescr$,0%,0, f1%(2%))
                   if f1%(2%) <> 0% then return
               errormsg$ = "Please Enter or Select a Valid Account"
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L56160,   /* Next Standard Job No.  */~
                                    L56410,   /* Standard Job Increment */~
                                    L56450,   /* Next Rework Job No.    */~
                                    L56680,   /* Rework Job Increment   */~
                                    L56720,   /* Next Purchase Job No.  */~
                                    L56960    /* Purchase Job Increment */
              return

L56160: REM Test Data For Next Standard Job Number...
            if next_job$ = " " then return
            if str(next_job$,1%,2%) <> "PJ" and                          ~
               str(next_job$,1%,2%) <> "RW" then L56230
                   errormsg$ = "Standard Jobs CANNOT Begin with 'RW' or"&~
                               " 'PJ'"
                   return
L56230:     temp$ = next_job$
            for i% = 1% to 8%
                if str(temp$,i%,1%) < "0" or str(temp$,i%,1%) > "9"      ~
                    then str(temp$,i%,1%) = " "
            next i%
            temp% = pos(-temp$ <> " ")
                if temp% = 0% then L56330
            auto%(1%) = pos(-str(temp$,,temp%)=" ")+1%
            auto%(2%) = (temp% - auto%(1%)) + 1%
            if auto%(2%) < 3% then L56330 else L56360
L56330:         errormsg$ = "Next Job Nmbr Must Have At Least 3 Consecu"&~
                            "tive Nmbrs In It For Incrementing"
                return
L56360:     call "READ100" (#4, next_job$, f1%(4%))
                if f1%(4%) = 0% then return
            errormsg$ = "Next Job Number Has Already Been Assigned"
            return

L56410: REM Test Data For Standard Job Number Increment...
            call "NUMTEST" (job_incr$, 1, 99999, errormsg$, -0.001, 0)
            return

L56450: REM Test Data For Next Rework Job Number...
            if next_rwjob$ = " " then return
            if str(next_rwjob$,1%,2%) = "RW" then L56500
                errormsg$ = "Rework Jobs MUST Begin With 'RW'"
                return
L56500:     temp$ = next_rwjob$
            for i% = 3% to 8%
                if str(temp$,i%,1%) < "0" or str(temp$,i%,1%) > "9"      ~
                    then str(temp$,i%,1%) = " "
            next i%
            temp% = pos(-temp$ <> " ")
                if temp% = 0% then L56600
            auto%(1%) = pos(-str(temp$,,temp%)=" ")+1%
            auto%(2%) = (temp% - auto%(1%)) + 1%
            if auto%(2%) < 3% then L56600 else L56630
L56600:         errormsg$ = "Next Rework Job Nmbr Must Have At Least 3 "&~
                            "Consecutive Nmbrs In It"
                return
L56630:     call "READ100" (#4, next_rwjob$, f1%(4%))
                if f1%(4%) = 0% then return
            errormsg$ = "Next Rework Job Number Has Already Been Assigned"
            return

L56680: REM Test Data For Rework Job Number Increment...
            call "NUMTEST" (rw_incr$, 1, 99999, errormsg$, -0.001, 0)
            return

L56720: REM Test Data For Next Purchase Job Number...
            if next_pjjob$ = " " then return
                if str(next_pjjob$,1%,2%) = "PJ" then L56770
                    errormsg$ = "Purchase Jobs MUST Begin With 'PJ'"
                    return
L56770:     temp$ = next_pjjob$
            for i% = 3% to 8%
                if str(temp$,i%,1%) < "0" or str(temp$,i%,1%) > "9"      ~
                    then str(temp$,i%,1%) = " "
            next i%
            temp% = pos(-temp$ <> " ")
                if temp% = 0% then L56870
            auto%(1%) = pos(-str(temp$,,temp%)=" ")+1%
            auto%(2%) = (temp% - auto%(1%)) + 1%
            if auto%(2%) < 3% then L56870 else L56900
L56870:         errormsg$ = "Next Purchase Job Nmbr Must Have At Least "&~
                          "3 Consecutive Nmbrs In It"
                return
L56900:     call "READ100" (#4, next_pjjob$, f1%(4%))
                if f1%(4%) = 0% then return
            errormsg$ = "Next Purchas Job Number Has Already Been Assig"&~
                        "ned"
            return

L56960: REM Test Data For Purchase Job Number Increment...
            call "NUMTEST" (pj_incr$, 1, 99999, errormsg$, -0.001, 0)
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

        exit_program_warning
            ask% = 2%
            ask$()  = " "
            ask$(1%)= "Changes to FLAGS will not impact tasks (" &       ~
                      "BACKGROUND or FOREGROUND)"
            ask$(2%)= "currently executing.  They must be termi" &       ~
                      "nated and re-initiated."
            ask$(3%)= "Press any PF key to continue."

            call "ASKUSER" (ask%, "* * * NOTE * * *",                    ~
                            ask$(1%), ask$(2%), ask$(3%))

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
