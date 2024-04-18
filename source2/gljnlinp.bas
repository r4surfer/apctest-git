        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      JJJJJ  N   N  L      IIIII  N   N  PPPP    *~
            *  G      L        J    NN  N  L        I    NN  N  P   P   *~
            *  G GGG  L        J    N N N  L        I    N N N  PPPP    *~
            *  G   G  L      J J    N  NN  L        I    N  NN  P       *~
            *   GGG   LLLLL   J     N   N  LLLLL  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLJNLINP - For each Module, maintains Journal ID's and    *~
            *            Posting Sequence Numbers.  Also maintains      *~
            *            Module Master records with Module Descriptions.*~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/10/85 ! ORIGINAL                                 ! KAB *~
            * 10/04/86 ! Modified for Revenue Project             ! ERN *~
            * 03/23/87 ! Added PIA Journal Id to Break Out        ! LDJ *~
            *          ! Physical Inventory Adjustments separate  !     *~
            *          ! from Inventory Adjustments.              !     *~
            * 04/30/87 ! Added STC Standard Cost Revaluation (G/L)! KAB *~
            * 10/13/92 ! Added journals and auto-create of new ones KAB *~
            * 11/10/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 01/06/93 ! New Module for Core Bank                 ! KAB *~
            * 09/17/93 ! New journal IAA for inventory admin adj. ! WPH *~
            * 11/09/93 ! BackFlush Project- add 'MFL' to 03/Mfg.  ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$(20)25,               /* FILLER FOR COMPRESSION     */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            fac$(20,4)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            hdr$60,                      /* Header for ASKUSER         */~
            hi$80,                       /* 1st line for ASKUSER       */~
            line2$79,                    /* Screen line #2             */~
            lo$80,                       /* 3rd line for ASKUSER       */~
            mid$80,                      /* 2nd line for ASKUSER       */~
            readkey$20,                  /* GEN PURPOSE READKEY        */~
            resetmsg$79,                                                 ~
            stdmod$(30)3,                /* STANDARD MODULES           */~
            warnmsg$(2)79

        dim                                                              ~
            module$  (30)3,              /* MODULE NOS                 */~
            moddescr$(30)30,             /* MODULE DESCRIPTION         */~
            moduleheader$25              /* MODULE SCREEN HEADER       */~

        dim                                                              ~
            jnlmod$3,                                                    ~
            jnlmoddescr$30,                                              ~
            journal$(30)3,                                               ~
            jnldescr$(30)30,                                             ~
            jnlseq$(30)8,                                                ~
            jnlsum$(30)1,                                                ~
            journalheader$79,                                            ~
            tempjnl$3,                                                   ~
            tempjds$30,                                                  ~
            tempjsq$8,                                                   ~
            tempjsm$1

        dim                                                              ~
            prtmod$2,                                                    ~
            prtlastmod$2,                                                ~
            prtjnl$3,                                                    ~
            prtdescr$30,                                                 ~
            prtmoddescr$30,                                              ~
            prtseq$8,                                                    ~
            prtsum$,                                                     ~
            prtmod$(30)32,                                               ~
            prtmodtitle$128,                                             ~
            prtjnltitle$128                                              ~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening File, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
                if f2%(1) = 0% then L09000
                ask% = 2
                hdr$ = "*** SYSFILE2 FILE ERROR ***"
                hi$ = "SYSFILE2 is either NOT OPEN"
                mid$ = "or does NOT EXIST!!!"
                lo$ = "Press RETURN to EXIT program"
L02260:         call "ASKUSER" ( ask%, hdr$, hi$, mid$, lo$)
                if ask% <> 0 then L02260
                goto L65000

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            init (" ") filler$()

            prtmodtitle$ = "GENERAL LEDGER JOURNAL SYSTEM - DEFINED MODUL~
        ~ES LIST"
               call "FMTTITLE" (prtmodtitle$, " ",  2%)
               str(prtmodtitle$,3,14) = "DATE: " & date$
               str(prtmodtitle$,119,5) = "PAGE:"

            prtjnltitle$ = "GENERAL LEDGER JOURNAL SYSTEM - DETAILED JOUR~
        ~NAL INFORMATION"
               call "FMTTITLE" (prtjnltitle$, " ",  2%)
               str(prtjnltitle$,3,14) = "DATE: " & date$
               str(prtjnltitle$,119,5) = "PAGE:"

            moduleheader$ = "Module no.   Description"

            journalheader$ = "Journal ID   Next Seq#  Summary Option   Ti~
        ~tle  (with ' Journal')"

            warnmsg$(1)="***** WARNING *****"
            warnmsg$(2)="is about to be RESET to STANDARD SYSTEM DEFAULTS"

            call "FMTTITLE" (warnmsg$(1), " ", 12%)
            call "FMTTITLE" (warnmsg$(2), " ",  2%)

            init (" ") stdmod$()
            restore line = L32020

            i% = 1%
L09380:     read stdmod$(i%), errormsg$
               if stdmod$(i%) = " " then L09430
            i% = i% + 1%
            if i% < dim(stdmod$(),1) then L09380

L09430:     gosub check_records

            init(" ") errormsg$ : ml% = 0%

            str(line2$,62) = "GLJNLINP: " & str(cms2v$,1,8)

        REM *************************************************************~
            *                                                           *~
            *   MAINTAIN MODULE DESCRIPTORS                             *~
            *                                                           *~
            *************************************************************

        module_edit
                gosub'101(0%,0%)
                      if keyhit%  = 16 then L65000
                      if keyhit%  = 14 then gosub print_list
                      if keyhit%  = 28 then gosub system_reset
                      if keyhit%  =  2 then ml% = 0%
                      if keyhit%  =  4 then ml% = max(0%, ml% - 12%)
                      if keyhit%  =  5 then                              ~
                            ml% = max(0%, min(modmax% - 15%, ml% + 12%))
                      if keyhit%  =  6 then ml% = max(0%, ml% - 1%)
                      if keyhit%  =  7 then                              ~
                            ml% = max(0%, min(modmax% - 15%, ml% + 1%))
                      if keyhit%  = 12 then L10200
                      if keyhit%  = 11 then L10200
                      if keyhit% <>  0 then module_edit

L10200:         fieldnr% = cursor%(1%) - 5%
                if fieldnr% > 15% then module_edit
                if fieldnr% > modmax% - ml% then module_edit
                   if keyhit% = 11% then fieldnr% = fieldnr% + 1%
                if fieldnr% < 1% then module_edit
                   c% = fieldnr% + ml%
                if keyhit% = 11% then module_insert
                if keyhit% = 12% then module_delete

        REM MODULE DESCRIPTION/JOURNAL EDIT
                if cursor%(2%) > 2% then L10350
                       jnlmod$ = module$(c%)
                       jnlmoddescr$ = moddescr$(c%)
                           gosub load_journals
                       goto journal_edit
L10350:            gosub'101 (1%, fieldnr%)
                   gosub save_modules
                   goto module_edit

        module_insert
                if modmax% >= dim(module$(),1) then module_edit
                   if fieldnr% < 16% then L10440
                      ml% = ml% + 1%
                      fieldnr% = 15%
L10440:         if c% <= modmax% then gosub module_push_down

L10460:         gosub'101 (2%, fieldnr%)
                     if keyhit%  <> 1% then L10510
                        errormsg$ = " "
                        gosub load_modules
                        goto module_edit
L10510:              if keyhit% <> 0% then L10460
                gosub'151
                     if errormsg$ <> " " then L10460
                     jnlmod$ = module$(c%)
                     jnlmoddescr$ = moddescr$(c%)
                          modmax% = modmax% + 1%
                          gosub save_modules
                          gosub load_journals
                     c%, fieldnr% = 1%
                     if jnlmax% = 0% then journal_insert                 ~
                                     else journal_edit

        module_push_down
                for i% = modmax% to c% step -1
                     module$(i%+1%) = module$ (i%)
                     moddescr$(i%+1%) = moddescr$(i%)
                next i%
                init (" ") module$(c%), moddescr$(c%)
                return

        module_delete
L10670:         gosub'101 (3%, fieldnr%)
                     if keyhit% = 1% then module_edit
                     if keyhit% <> 0% then L10670

                readkey$ = "MODULENO:" & module$(c%)
                   call "DELETE" (#1, readkey$, 11%)
                readkey$ = "MODULENO:" & "00" & module$(c%)
                   call "DELETE" (#1, readkey$, 13%)
                      gosub load_modules
                      goto module_edit

        system_reset
                resetmsg$ = "The ENTIRE G/L Journal System"
L10800:         gosub'103
                    if keyhit% = 1% then return
                    if keyhit% = 32% then L10850
                    if keyhit% <> 16% then L10800
                gosub print_list
L10850:         readkey$ = "MODULENO:"
                call "DELETE" (#1, readkey$, 9%)
                gosub check_records
                return

        REM *************************************************************~
            *                                                           *~
            *   MAINTAIN JOURNAL RECORDS                                *~
            *                                                           *~
            *************************************************************

        journal_edit
                gosub'102(0%,0%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then journal_datasave
                      if keyhit%  = 14 then gosub print_list
                      if keyhit%  = 28 then gosub journal_reset
                      if keyhit%  =  2 then jl% = 0%
                      if keyhit%  =  4 then jl% = max(0%, jl% - 12%)
                      if keyhit%  =  5 then                              ~
                            jl% = max(0%, min(jnlmax% - 15%, jl% + 12%))
                      if keyhit%  =  6 then jl% = max(0%, jl% - 1%)
                      if keyhit%  =  7 then                              ~
                            jl% = max(0%, min(jnlmax% - 15%, jl% + 1%))
                      if keyhit%  = 12 then L11200
                      if keyhit%  = 11 then L11200
                      if keyhit% <>  0 then journal_edit

L11200:         fieldnr% = cursor%(1%) - 5%
                if fieldnr% > 15% then journal_edit
                if fieldnr% > jnlmax% - jl% then journal_edit
                   if keyhit% = 11% then fieldnr% = fieldnr% + 1%
                if fieldnr% < 1% then journal_edit
                   c% = fieldnr% + jl%
                if keyhit% = 11% then journal_insert
                if keyhit% = 12% then journal_delete

        REM JOURNAL DESCRIPTION/JOURNAL EDIT
L11350:            gosub'102 (1%, fieldnr%)
                   gosub'152
                     if errormsg$<> " " then L11350
                   goto journal_edit

        journal_insert
                if jnlmax% >= dim(journal$(),1) then journal_edit
                   if fieldnr% < 16% then L11440
                      jl% = jl% + 1%
                      fieldnr% = 15%
L11440:         if c% <= jnlmax% then gosub journal_push_down

L11460:         gosub'102 (2%, fieldnr%)
                     if keyhit%  <> 1% then L11510
                        errormsg$ = " "
                        jnlmax% = jnlmax% + 1%
                        gosub journal_pull_up
                        goto journal_edit
L11510:              if keyhit% <> 0% then L11460
                gosub'152
                     if errormsg$ <> " " then L11460
                        jnlmax% = jnlmax% + 1%
                     goto journal_edit

        journal_push_down
                for i% = jnlmax% to c% step -1
                     journal$ (i%+1%) = journal$ (i%)
                     jnldescr$(i%+1%) = jnldescr$(i%)
                     jnlseq$  (i%+1%) = jnlseq$  (i%)
                     jnlsum$  (i%+1%) = jnlsum$  (i%)
                next i%
                init (" ") journal$(c%), jnldescr$(c%), jnlseq$(c%),     ~
                           jnlsum$ (c%)
                return

        journal_delete
L11670:         gosub'102 (3%, fieldnr%)
                     if keyhit% = 1% then journal_edit
                     if keyhit% <> 0% then L11670

                gosub journal_pull_up
                goto journal_edit

        journal_pull_up
                if c% = jnlmax% then L11850
                for i% = c% to jnlmax% - 1%
                     journal$ (i%) = journal$ (i% + 1%)
                     jnldescr$(i%) = jnldescr$(i% + 1%)
                     jnlseq$  (i%) = jnlseq$  (i% + 1%)
                     jnlsum$  (i%) = jnlsum$  (i% + 1%)
                next i%
L11850:         init (" ") journal$(jnlmax%), jnldescr$(jnlmax%),        ~
                           jnlseq$(jnlmax%), jnlsum$(jnlmax%)
                jnlmax% = jnlmax% - 1%
                return

        journal_datasave
                gosub save_journals
                goto module_edit

        journal_reset

                search str(stdmod$(),1) = jnlmod$ to cursor%() step 3
                if cursor%(1%) = 0% then return
                resetmsg$ = "Module: " & jnlmod$ & " - " & jnlmoddescr$
L12050:         gosub'103
                    if keyhit% = 1% then return
                    if keyhit% = 32% then L12100
                    if keyhit% <> 16% then L12050
                gosub print_list
L12100:         readkey$ = "MODULENO:" & jnlmod$
                call "DELETE" (#1, readkey$, 11%)
                gosub create_journals
                gosub load_journals
                return

        REM *************************************************************~
            *                                                           *~
            * PRINT FULL LIST OF MODULES, JOURNALS                      *~
            *                                                           *~
            *************************************************************

        print_list

        REM FIRST PRINT DEFINED JOURNALS
            line% = 999%
            defmod%, page% = 0%
            init (hex(00)) readkey$, prtmod$()
            str(readkey$,1,11) = "MODULENO:00"

L19140:     call "PLOWNEXT" (#1, readkey$, 11%, f1%(1))
                if f1%(1) = 0% then print_journals
            get #1, using L19240, prtmod$, prtdescr$
                defmod% = defmod% + 1%
            prtmod$(defmod%) = str(prtmod$,1,2) & str(prtdescr$,1,30)

            if line% > 60% then gosub print_module_heading
            print using L19600, prtmod$, prtdescr$
            line% = line% + 1%
            goto L19140

L19240:     FMT XX(11), CH(2), XX(7), CH(30)

        print_module_heading
            select printer(134)
            page% = page% + 1%
            convert page% to str(prtmodtitle$,124,3), pic(###)
            print page

            print using L19500
            print using L19540
            print using L19520, prtmodtitle$
            print using L19540
            print using L19500
            print skip (1)
            print using L19560
            print using L19580
            line% = 9%
            return

L19500: %****************************************************************~
        ~******************************************************************
L19520: %*###############################################################~
        ~#################################################################*
L19540: %*                                                               ~
        ~                                                                 *
L19560: %MODULE NUMBER          MODULE DESCRIPTION

L19580: %=============          ==============================

L19600: %     ##                ##############################

L19620: %MODULE NO  MODULE DESCRIPTION              JOURNAL ID  JOURNAL T~
        ~ITLE                   NEXT SEQ. NO.   SUMMARY OPTION
L19640: %=========  ==============================  ==========  =========~
        ~=====================  =============   ==============
L19660: %   ##      ##############################     ###      #########~
        ~#####################       ########          #


        print_journal_heading
            select printer(134)
            page% = page% + 1%
            convert page% to str(prtjnltitle$,124,3), pic(###)
            print page

            print using L19500
            print using L19540
            print using L19520, prtjnltitle$
            print using L19540
            print using L19500
            print skip (1)
            print using L19620
            print using L19640
            line% = 9%
            return

        print_journals

            if page% = 0% then return
            page% = 0%
            line% = 999%

            init (hex(ff)) readkey$:prtlastmod$ = " "
            str(readkey$,1,11) = "MODULENO:00"

L20090:     call "PLOWNEXT" (#1, readkey$, 9%, f1%(1))
                if f1%(1) = 0% then end_print
            get #1, using L20290, prtmod$, prtjnl$, prtdescr$, prtseq%,   ~
                                 prtsum$
                init(" ") prtseq$:convert prtseq% to prtseq$,pic(########)
            search str(prtmod$(),1) = prtmod$ to cursor%() step 32
                if cursor%(1) = 0% then L20170
                if cursor%(1) <= 32*defmod% - 31% then L20190
L20170:            prtmoddescr$ = "UNKNOWN MODULE"
                   goto L20240
L20190:            prtmoddescr$ = str(prtmod$(), cursor%(1%)+2%, 30%)

            if prtlastmod$ = " " then prtlastmod$ = prtmod$
            if prtlastmod$ = prtmod$ then L20240
               prtlastmod$ = prtmod$
               print skip(1)
               line% = line% + 1%

L20240:     if line% > 60% then gosub print_journal_heading
            print using L19660, prtmod$, prtmoddescr$, prtjnl$, prtdescr$,~
                               prtseq$, prtsum$
            line% = line% + 1%
            goto L20090

L20290:     FMT XX(9), CH(2), CH(3), XX(6), CH(30), BI(4), CH(1)

        end_print
            close printer
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto module_edit

        REM *************************************************************~
            * FILE I/O FOR MODULE HEADER RECORDS                        *~
            *************************************************************

        create_modules

            readkey$ = "MODULENO:00"
            call "DELETE" (#1, readkey$, 11%)

            restore line = L32020
            modmax% = 0%
L30140:     read module$(modmax% + 1%), moddescr$(modmax% + 1%)
                     if module$(modmax% + 1%) = " " then save_modules
            modmax% = modmax% + 1%
            if modmax% < dim(module$(),1) then L30140

        save_modules

            readkey$ = "MODULENO:00"
            call "DELETE" (#1, readkey$, 11%)

            if modmax% = 0% then load_modules
            for i% = 1% to modmax%
                write #1, using L30330, readkey$,                         ~
                          module$(i%), " ",                              ~
                          moddescr$(i%),                                 ~
                          hex(00000000),                                 ~
                          str(filler$(),,446)
            next i%

L30330:     FMT CH(11), CH(3), CH(6), CH(30), CH(4), CH(446)

        load_modules

            init (" ") module$(), moddescr$()

            modmax% = 0%
                readkey$ = "MODULENO:00"
L30420:         call "PLOWNEXT" (#1, readkey$, 11%, f1%(1))
                     if f1%(1) = 0 then return
                modmax% = modmax% + 1%
                get #1, using L30480,                                     ~
                          module$(modmax%),                              ~
                          moddescr$(modmax%)
L30480:         FMT XX(11), CH(3), XX(6), CH(30)
                if modmax% < dim(module$(),1) then L30420
                return

        REM *************************************************************~
            * FILE I/O FOR JOURNAL RECORDS                              *~
            *************************************************************

        create_journals

            search str(stdmod$(),1) = jnlmod$ to cursor%() step 3
               if cursor%(1) = 0% then return
            on (cursor%(1)+2%)/3% goto  L31170,                           ~
                                        L31180,                           ~
                                        L31190,                           ~
                                        L31200,                           ~
                                        L31210,                           ~
                                        L31220,                           ~
                                        L31230,                           ~
                                        L31235,                           ~
                                        L31240                            ~

L31170:     restore line = L32150:goto L31260
L31180:     restore line = L32320:goto L31260
L31190:     restore line = L32440:goto L31260
L31200:     restore line = L32560:goto L31260
L31210:     restore line = L32650:goto L31260
L31220:     restore line = L32730:goto L31260
L31230:     restore line = L32790:goto L31260
L31235:     restore line = L32880:goto L31260
L31240:     restore line = L32960:goto L31260

L31260:     jnlmax% = 0%
L31270:     read journal$(jnlmax% + 1%), jnldescr$(jnlmax% + 1%),        ~
                 jnlseq$(jnlmax% + 1%), jnlsum$(jnlmax% + 1%)
                     if journal$(jnlmax% + 1%) = " " then save_journals
            jnlmax% = jnlmax% + 1%
            if jnlmax% < dim(journal$(),1) then L31270

        save_journals

            readkey$ = "MODULENO:" & jnlmod$
            call "DELETE" (#1, readkey$, 11%)

            if jnlmax% = 0% then return
            for i% = 1% to jnlmax%
              jnlseq%= 1%:convert jnlseq$(i%) to jnlseq%, data goto L31410
L31410:         write #1, using L31490, readkey$,                         ~
                          journal$(i%), " ",                             ~
                          jnldescr$(i%),                                 ~
                          jnlseq%,                                       ~
                          jnlsum$(i%),                                   ~
                          str(filler$(),,445)
            next i%
            return
L31490:     FMT CH(11), CH(3), CH(6), CH(30), BI(4), CH(1), CH(445)

        load_journals

            init (" ") journal$(), jnldescr$(), jnlseq$(), jnlsum$()

            jnlmax% = 0%
                readkey$ = "MODULENO:" & jnlmod$
L31570:         call "PLOWNEXT" (#1, readkey$, 11%, f1%(1))
                     if f1%(1) = 0 then L31700
                jnlmax% = jnlmax% + 1%
                get #1, using L31660,                                     ~
                          journal$(jnlmax%),                             ~
                          jnldescr$(jnlmax%),                            ~
                          jnlseq%,                                       ~
                          jnlsum$(jnlmax%)
                convert jnlseq% to jnlseq$(jnlmax%), pic(########)
L31660:         FMT XX(11), CH(3), XX(6), CH(30), BI(4), CH(1)
                if jnlmax% < dim(journal$(),1) then L31570
                return

L31700:     search str(stdmod$(),1) = jnlmod$ to cursor%() step 3
               if cursor%(1) = 0% then return
            on (cursor%(1)+2%)/3% goto  L31755,                           ~
                                        L31760,                           ~
                                        L31765,                           ~
                                        L31770,                           ~
                                        L31775,                           ~
                                        L31780,                           ~
                                        L31785,                           ~
                                        L31786,                           ~
                                        L31790                            ~

L31755:     restore line = L32150:goto L31800
L31760:     restore line = L32320:goto L31800
L31765:     restore line = L32440:goto L31800
L31770:     restore line = L32560:goto L31800
L31775:     restore line = L32650:goto L31800
L31780:     restore line = L32730:goto L31800
L31785:     restore line = L32790:goto L31800
L31786:     restore line = L32880:goto L31800
L31790:     restore line = L32960:goto L31800

L31800:     if jnlmax% >= dim(journal$(),1) then return

L31810:     read tempjnl$, tempjds$, tempjsq$, tempjsm$
               if tempjnl$ = " " then return
            search str(journal$()) = str(tempjnl$) to cursor%() step 3
              if cursor%(1%) <> 0% then L31810

L31835:     ask% = 2
            hdr$ = "*** MISSING JOURNAL ID ***"
            hi$ = "The Following Jounal ID (normally present) is missing"
            mid$ = tempjnl$ & " - " & tempjds$
            lo$ = "Press RETURN to APPEND or Press PF1 to BYPASS"
                call "ASKUSER" ( ask%, hdr$, hi$, mid$, lo$)
                   if ask% = 0% then L31885
                   if ask% = 1% then L31800
                      goto L31835

L31885:     jnlmax% = jnlmax% + 1%
            journal$ (jnlmax%) = tempjnl$
            jnldescr$(jnlmax%) = tempjds$
            jnlseq$  (jnlmax%) = tempjsq$
            jnlsum$  (jnlmax%) = tempjsm$
            goto L31800

        REM DATA STATMENTS FOR LOADING UP STANDARD DEFUAULTS
        REM MODULE_DATA
L32020:     data "01","Accounts Receivable           "
            data "02","Accounts Payable              "
            data "03","Manufacturing                 "
            data "04","Inventory Control             "
            data "05","Purchasing                    "
            data "06","General Ledger                "
            data "09","Payroll                       "
            data "10","Core Bank                     "
            data "99","General Ledger Year End       "
            data "  ","                              "


        REM JOURNAL_DATA
        REM MODULE 01, ACCOUNTS RECEIVABLE
L32150:     data "RCR","CASH RECEIPTS                 ","       1","F"
            data "RIN","INVOICING                     ","       1","F"
            data "RSJ","SHIPMENTS                     ","       1","F"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 02, ACCOUNTS PAYABLE
L32320:     data "VMD","MANUAL DISBURSEMENTS          ","       1","F"
            data "VAD","AUTOMATIC DISBURSEMENTS       ","       1","F"
            data "VPJ","VENDOR INVOICE PURCHASES      ","       1","F"
            data "VRJ","RECURRING PURCHASES           ","       1","F"
            data "VSD","SUPPLEMENTAL DISBURSEMENTS    ","       1","F"
            data "VAJ","PAYABLES ADJUSTMENTS          ","       1","F"
            data "VEZ","INVOICE NON-STOCKED, NO P.O.  ","       1","F"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 03, MANUFACTURING
L32440:     data "MVA","VALUE ADDED                   ","       1","N"
            data "MPR","KIT PARTS TO JOB              ","       1","N"
            data "MPC","PARTS COMPLETED               ","       1","N"
            data "MPW","PARTS RETURNED FROM JOB       ","       1","N"
            data "MJR","MOVE JOB TO JOB               ","       1","N"
            data "MRW","PARTS TO/FROM REWORK          ","       1","N"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "MTC","TIME CARD LABOR POSTING       ","       1","N"
            data "MFL","BACKFLUSH ADDN'S & WDWALS     ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 04, INVENTORY CONTROL
L32560:     data "IAD","INVENTORY ADDITIONS           ","       1","N"
            data "IRT","INVENTORY RETURNS             ","       1","N"
            data "IWJ","INVENTORY WITHDRAWALS         ","       1","N"
            data "IMV","INVENTORY MOVEMENTS           ","       1","N"
            data "IPA","PHYSICAL INVENTORY ADJUSTMENTS","       1","N"
            data "ISC","STANDARD COST REVALUATION     ","       1","N"
            data "ICA","CORE BANK ADDITIONS           ","       1","N"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "IAA","INVENTORY CONVERSION ADJUSTMNT","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 05, PURCHASING/RECEIVING
L32650:     data "PRC","P. O. RECEIVING               ","       1","N"
            data "PQD","Q. C. RECEIVING W/DISPOSITION ","       1","N"
            data "PQC","Q. C. RECEIVING               ","       1","N"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 06, GENERAL LEDGER
L32730:     data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "STC","STANDARD COST REVALUATION     ","       1","N"
            data "STJ","STANDARD                      ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 09, PAYROLL
L32790:     data "EEJ","PAYROLL EARNINGS W/JOB        ","       1","N"
            data "EEN","PAYROLL EARNINGS              ","       1","N"
            data "EAC","PAYROLL CHECKS                ","       1","N"
            data "EEA","PAYROLL AUTO EARNINGS         ","       1","N"
            data "EMC","PAYROLL MANUAL CHECKS         ","       1","N"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 10, CORE BANK
L32880:     data "CAR","CORE A/R TRANSACTIONS         ","       1","N"
            data "CDB","CORE DEBIT TRANSACTIONS       ","       1","N"
            data "CCR","CORE CREDIT TRANSACTIONS      ","       1","N"
            data "CAC","CORE ACCEPTANCE TRANSACTIONS  ","       1","N"
            data "CXP","CORE BANK DROP-OFF            ","       1","N"
            data "CRT","CORE RECEIPT TRANSACTIONS     ","       1","N"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "GNJ","G/L DIRECT ENTRYS             ","       1","N"
            data "   ","                              ","        "," "

        REM MODULE 99, GENERAL LEDGER YEAR END
L32960:     data "GYE","AUTO GENERATED CLOSING ENTRIES","       1","N"
            data "GAJ","G/L DIRECT ENTRYS (ADMIN)     ","       1","N"
            data "   ","                              ","        "," "

        REM *************************************************************~
            * CHECK RECORDS AND BUILD IF NECESSARY                      *~
            *************************************************************

        check_records

            init (hex(00)) readkey$ : str(readkey$,1,11) = "MODULENO:00"

            call "PLOWNEXT" (#1, readkey$, 11%, f1%(1))
                if f1%(1) = 0% then gosub create_modules                 ~
                          else      gosub load_modules

*          IF MODMAX% = 0% THEN RETURN

            restore line = L32020

L35160:     if modmax% >= dim(module$(),1) then L35400

L35180:     read tempjnl$, tempjds$
               if tempjnl$ = " " then L35400
            search str(module$()) = str(tempjnl$) to cursor%() step 3
              if cursor%(1%) <> 0% then L35180

L35230:     ask% = 2
            hdr$ = "*** MISSING MODULE ID ***"
            hi$ = "The Following Module (normally present) is missing"
            mid$ = tempjnl$ & " - " & tempjds$
            lo$ = "Press RETURN to APPEND or Press PF1 to BYPASS"
                call "ASKUSER" ( ask%, hdr$, hi$, mid$, lo$)
                   if ask% = 0% then L35330
                   if ask% = 1% then L35160
                      goto L35230

L35330:     modmax% = modmax% + 1%
            module$  (modmax%) = tempjnl$
            moddescr$(modmax%) = tempjds$
                write #1, using L35366, "MODULENO:00",                    ~
                          module$(modmax%), " ",                         ~
                          moddescr$(modmax%),                            ~
                          hex(00000000),                                 ~
                          str(filler$(),,446), eod goto L35160

L35366:     FMT CH(11), CH(3), CH(6), CH(30), CH(4), CH(446)
            goto L35160

L35400:     if modmax% = 0% then return

            for k% = 1% to modmax%
            init (hex(00)) readkey$
            str(readkey$,1,11) = "MODULENO:" & str(module$(k%),1,2)
                call "PLOWNEXT" (#1, readkey$, 11%, f1%(1))
                     if f1%(1) <> 0% then L35670
                        jnlmod$ = module$(k%)
                          gosub create_journals
L35670:     next k%

            return

        REM *************************************************************~
            *                                                           *~
            * MODULE HEADER SCREEN                                      *~
            *                                                           *~
            *************************************************************

            deffn'101(fn%, field%)
                  init(hex(8c)) fac$()
                  on fn%      goto  L40080,         /* EDIT             */~
                                    L40105,         /* INSERT           */~
                                    L40135          /* DELETE           */~

        REM DISPLAY MODE
            edtmessage$ = "Position cursor and select function"
            for i% = 1 to 20
                if module$(ml%+ i%) <> " " then fac$(i%, 1%) = hex(8e)
            next i%
            str(line2$,1,50) = "Display Mode"
            goto L40160

L40080: REM EDIT MODE
            edtmessage$ = "Enter data and press RETURN"
            fac$(field%, 2%) = hex(81)
            str(line2$,1,50) = "Edit Mode"
            goto L40160

L40105: REM INSERT MODE
            edtmessage$="Enter all data and press RETURN, PF(1) to CANCEL"
            fac$(field%, 1%) = hex(82)
            fac$(field%, 2%) = hex(80)
            str(line2$,1,50) = "Insert Mode"
            goto L40160

L40135: REM DELETE MODE
            edtmessage$ = "Press RETURN to delete entire module, PF(1) to~
        ~ CANCEL"
            fac$(field%, 1%), fac$(field%, 2%) = hex(94)
            str(line2$,1,50) = "Delete Mode"

L40160:     accept                                                       ~
               at (01,02),                                               ~
                  "Maintain G/L Journal System",                         ~
               at (01,67),                                               ~
                  "Date: ",                                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(ac)), moduleheader$          , ch(45),~
                                                                         ~
               at (06,02), fac(fac$( 1%, 1%)), module$(ml%+ 1%) , ch( 2),~
               at (07,02), fac(fac$( 2%, 1%)), module$(ml%+ 2%) , ch( 2),~
               at (08,02), fac(fac$( 3%, 1%)), module$(ml%+ 3%) , ch( 2),~
               at (09,02), fac(fac$( 4%, 1%)), module$(ml%+ 4%) , ch( 2),~
               at (10,02), fac(fac$( 5%, 1%)), module$(ml%+ 5%) , ch( 2),~
               at (11,02), fac(fac$( 6%, 1%)), module$(ml%+ 6%) , ch( 2),~
               at (12,02), fac(fac$( 7%, 1%)), module$(ml%+ 7%) , ch( 2),~
               at (13,02), fac(fac$( 8%, 1%)), module$(ml%+ 8%) , ch( 2),~
               at (14,02), fac(fac$( 9%, 1%)), module$(ml%+ 9%) , ch( 2),~
               at (15,02), fac(fac$(10%, 1%)), module$(ml%+10%) , ch( 2),~
               at (16,02), fac(fac$(11%, 1%)), module$(ml%+11%) , ch( 2),~
               at (17,02), fac(fac$(12%, 1%)), module$(ml%+12%) , ch( 2),~
               at (18,02), fac(fac$(13%, 1%)), module$(ml%+13%) , ch( 2),~
               at (19,02), fac(fac$(14%, 1%)), module$(ml%+14%) , ch( 2),~
               at (20,02), fac(fac$(15%, 1%)), module$(ml%+15%) , ch( 2),~
                                                                         ~
               at (06,15), fac(fac$( 1%, 2%)),moddescr$(ml%+ 1%), ch(30),~
               at (07,15), fac(fac$( 2%, 2%)),moddescr$(ml%+ 2%), ch(30),~
               at (08,15), fac(fac$( 3%, 2%)),moddescr$(ml%+ 3%), ch(30),~
               at (09,15), fac(fac$( 4%, 2%)),moddescr$(ml%+ 4%), ch(30),~
               at (10,15), fac(fac$( 5%, 2%)),moddescr$(ml%+ 5%), ch(30),~
               at (11,15), fac(fac$( 6%, 2%)),moddescr$(ml%+ 6%), ch(30),~
               at (12,15), fac(fac$( 7%, 2%)),moddescr$(ml%+ 7%), ch(30),~
               at (13,15), fac(fac$( 8%, 2%)),moddescr$(ml%+ 8%), ch(30),~
               at (14,15), fac(fac$( 9%, 2%)),moddescr$(ml%+ 9%), ch(30),~
               at (15,15), fac(fac$(10%, 2%)),moddescr$(ml%+10%), ch(30),~
               at (16,15), fac(fac$(11%, 2%)),moddescr$(ml%+11%), ch(30),~
               at (17,15), fac(fac$(12%, 2%)),moddescr$(ml%+12%), ch(30),~
               at (18,15), fac(fac$(13%, 2%)),moddescr$(ml%+13%), ch(30),~
               at (19,15), fac(fac$(14%, 2%)),moddescr$(ml%+14%), ch(30),~
               at (20,15), fac(fac$(15%, 2%)),moddescr$(ml%+15%), ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(RETURN)to Edit",                                     ~
               at (23,02),                                               ~
                  "                 (4)Prev (6)Down (11)Insert",         ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
               at (24,02),                                               ~
                  "(2)First         (5)Next (7)Up   (12)Delete  (14)Print~
        ~ List",                                                          ~
                                                                         ~
               keys(hex(000102040506070b0c0d0e0f101c)),                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40895
                  call "MANUAL" ("GLJNLINP")
                  goto L40160

L40895:        if keyhit% <> 15 then L40920
                  call "PRNTSCRN"
                  goto L40160

L40920:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *                                                           *~
            * JOURNAL MAINTENANCE SCREEN                                *~
            *                                                           *~
            *************************************************************

            deffn'102(fn%, field%)
                  init(hex(8c)) fac$()
                  str(line2$,1,50) = "Module: " & jnlmod$ & " " &        ~
                                     jnlmoddescr$
                  on fn%      goto  L41080,         /* EDIT             */~
                                    L41105,         /* INSERT           */~
                                    L41135          /* DELETE           */~

        REM DISPLAY MODE
            edtmessage$ = "Position cursor and select function"
            goto L41160

L41080: REM EDIT MODE
            edtmessage$ = "Enter data and press RETURN"
            fac$(field%, 2%) = hex(81)
            fac$(field%, 3%),fac$(field%, 4%) = hex(81)
            goto L41160

L41105: REM INSERT MODE
            edtmessage$="Enter all data and press RETURN, PF(1) to CANCEL"
            fac$(field%, 2%) = hex(82)
            fac$(field%, 1%),fac$(field%, 3%),fac$(field%, 4%) = hex(81)
            goto L41160

L41135: REM DELETE MODE
            edtmessage$ ="Press RETURN to DELETE Journal, PF(1) to CANCEL"
            fac$(field%, 1%), fac$(field%, 2%) = hex(94)
            fac$(field%, 3%), fac$(field%, 4%) = hex(94)

L41160:     accept                                                       ~
               at (01,02),                                               ~
                  "MAINTAIN G/L JOURNAL SYSTEM",                         ~
               at (01,67),                                               ~
                  "Date: ",                                              ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(ac)), journalheader$         , ch(79),~
                                                                         ~
               at (06,02), fac(fac$( 1%, 1%)),journal$(jl%+ 1%) , ch( 3),~
               at (07,02), fac(fac$( 2%, 1%)),journal$(jl%+ 2%) , ch( 3),~
               at (08,02), fac(fac$( 3%, 1%)),journal$(jl%+ 3%) , ch( 3),~
               at (09,02), fac(fac$( 4%, 1%)),journal$(jl%+ 4%) , ch( 3),~
               at (10,02), fac(fac$( 5%, 1%)),journal$(jl%+ 5%) , ch( 3),~
               at (11,02), fac(fac$( 6%, 1%)),journal$(jl%+ 6%) , ch( 3),~
               at (12,02), fac(fac$( 7%, 1%)),journal$(jl%+ 7%) , ch( 3),~
               at (13,02), fac(fac$( 8%, 1%)),journal$(jl%+ 8%) , ch( 3),~
               at (14,02), fac(fac$( 9%, 1%)),journal$(jl%+ 9%) , ch( 3),~
               at (15,02), fac(fac$(10%, 1%)),journal$(jl%+10%) , ch( 3),~
               at (16,02), fac(fac$(11%, 1%)),journal$(jl%+11%) , ch( 3),~
               at (17,02), fac(fac$(12%, 1%)),journal$(jl%+12%) , ch( 3),~
               at (18,02), fac(fac$(13%, 1%)),journal$(jl%+13%) , ch( 3),~
               at (19,02), fac(fac$(14%, 1%)),journal$(jl%+14%) , ch( 3),~
               at (20,02), fac(fac$(15%, 1%)),journal$(jl%+15%) , ch( 3),~
                                                                         ~
               at (06,16), fac(fac$( 1%, 2%)),jnlseq$  (jl%+ 1%), ch( 8),~
               at (07,16), fac(fac$( 2%, 2%)),jnlseq$  (jl%+ 2%), ch( 8),~
               at (08,16), fac(fac$( 3%, 2%)),jnlseq$  (jl%+ 3%), ch( 8),~
               at (09,16), fac(fac$( 4%, 2%)),jnlseq$  (jl%+ 4%), ch( 8),~
               at (10,16), fac(fac$( 5%, 2%)),jnlseq$  (jl%+ 5%), ch( 8),~
               at (11,16), fac(fac$( 6%, 2%)),jnlseq$  (jl%+ 6%), ch( 8),~
               at (12,16), fac(fac$( 7%, 2%)),jnlseq$  (jl%+ 7%), ch( 8),~
               at (13,16), fac(fac$( 8%, 2%)),jnlseq$  (jl%+ 8%), ch( 8),~
               at (14,16), fac(fac$( 9%, 2%)),jnlseq$  (jl%+ 9%), ch( 8),~
               at (15,16), fac(fac$(10%, 2%)),jnlseq$  (jl%+10%), ch( 8),~
               at (16,16), fac(fac$(11%, 2%)),jnlseq$  (jl%+11%), ch( 8),~
               at (17,16), fac(fac$(12%, 2%)),jnlseq$  (jl%+12%), ch( 8),~
               at (18,16), fac(fac$(13%, 2%)),jnlseq$  (jl%+13%), ch( 8),~
               at (19,16), fac(fac$(14%, 2%)),jnlseq$  (jl%+14%), ch( 8),~
               at (20,16), fac(fac$(15%, 2%)),jnlseq$  (jl%+15%), ch( 8),~
                                                                         ~
               at (06,33), fac(fac$( 1%, 3%)),jnlsum$  (jl%+ 1%), ch( 1),~
               at (07,33), fac(fac$( 2%, 3%)),jnlsum$  (jl%+ 2%), ch( 1),~
               at (08,33), fac(fac$( 3%, 3%)),jnlsum$  (jl%+ 3%), ch( 1),~
               at (09,33), fac(fac$( 4%, 3%)),jnlsum$  (jl%+ 4%), ch( 1),~
               at (10,33), fac(fac$( 5%, 3%)),jnlsum$  (jl%+ 5%), ch( 1),~
               at (11,33), fac(fac$( 6%, 3%)),jnlsum$  (jl%+ 6%), ch( 1),~
               at (12,33), fac(fac$( 7%, 3%)),jnlsum$  (jl%+ 7%), ch( 1),~
               at (13,33), fac(fac$( 8%, 3%)),jnlsum$  (jl%+ 8%), ch( 1),~
               at (14,33), fac(fac$( 9%, 3%)),jnlsum$  (jl%+ 9%), ch( 1),~
               at (15,33), fac(fac$(10%, 3%)),jnlsum$  (jl%+10%), ch( 1),~
               at (16,33), fac(fac$(11%, 3%)),jnlsum$  (jl%+11%), ch( 1),~
               at (17,33), fac(fac$(12%, 3%)),jnlsum$  (jl%+12%), ch( 1),~
               at (18,33), fac(fac$(13%, 3%)),jnlsum$  (jl%+13%), ch( 1),~
               at (19,33), fac(fac$(14%, 3%)),jnlsum$  (jl%+14%), ch( 1),~
               at (20,33), fac(fac$(15%, 3%)),jnlsum$  (jl%+15%), ch( 1),~
                                                                         ~
               at (06,43), fac(fac$( 1%, 4%)),jnldescr$(jl%+ 1%), ch(30),~
               at (07,43), fac(fac$( 2%, 4%)),jnldescr$(jl%+ 2%), ch(30),~
               at (08,43), fac(fac$( 3%, 4%)),jnldescr$(jl%+ 3%), ch(30),~
               at (09,43), fac(fac$( 4%, 4%)),jnldescr$(jl%+ 4%), ch(30),~
               at (10,43), fac(fac$( 5%, 4%)),jnldescr$(jl%+ 5%), ch(30),~
               at (11,43), fac(fac$( 6%, 4%)),jnldescr$(jl%+ 6%), ch(30),~
               at (12,43), fac(fac$( 7%, 4%)),jnldescr$(jl%+ 7%), ch(30),~
               at (13,43), fac(fac$( 8%, 4%)),jnldescr$(jl%+ 8%), ch(30),~
               at (14,43), fac(fac$( 9%, 4%)),jnldescr$(jl%+ 9%), ch(30),~
               at (15,43), fac(fac$(10%, 4%)),jnldescr$(jl%+10%), ch(30),~
               at (16,43), fac(fac$(11%, 4%)),jnldescr$(jl%+11%), ch(30),~
               at (17,43), fac(fac$(12%, 4%)),jnldescr$(jl%+12%), ch(30),~
               at (18,43), fac(fac$(13%, 4%)),jnldescr$(jl%+13%), ch(30),~
               at (19,43), fac(fac$(14%, 4%)),jnldescr$(jl%+14%), ch(30),~
               at (20,43), fac(fac$(15%, 4%)),jnldescr$(jl%+15%), ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(RETURN)to Edit",                                     ~
               at (23,02),                                               ~
                  "(1)Start Over    (4)Prev (6)Down (11)Insert",         ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Save Data",                              ~
               at (24,02),                                               ~
                  "(2)First         (5)Next (7)Up   (12)Delete  (14)Print~
        ~ List",                                                          ~
                                                                         ~
               keys(hex(00010204050607ff0b0c0d0e0f101c)),                ~
               key (keyhit%)

               if keyhit% <> 13 then L41885
                  call "MANUAL" ("GLJNLINP")
                  goto L41160

L41885:        if keyhit% <> 15 then L41905
                  call "PRNTSCRN"
                  goto L41160

L41905:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                                                           *~
            * MODULE/JOURNAL RESET WARNING                              *~
            *                                                           *~
            *************************************************************

            deffn'103

            edtmessage$ = "Press PF(1) to CANCEL, PF(16), or PF(32) to ex~
        ~ecute RESET"
            str(line2$,1,50) = "Reset Function"
            call "FMTTITLE" (resetmsg$, " ",  2%)

L49110:     accept                                                       ~
               at (01,02),                                               ~
                  "MAINTAIN G/L JOURNAL SYSTEM",                         ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (07,02), fac(hex(94)), warnmsg$(1)            , ch(79),~
                                                                         ~
               at (09,02), fac(hex(84)), resetmsg$              , ch(79),~
                                                                         ~
               at (11,02), fac(hex(84)), warnmsg$(2)            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Cancel Reset                     (13)Instructions  ~
        ~(16)Print List and RESET ",                                      ~
               at (24,02),                                               ~
                  "                                    (15)Print Screen  ~
        ~(32)Reset WITHOUT Listing",                                      ~
                                                                         ~
               keys(hex(0110200d0f)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L49410
                  call "MANUAL" ("GLJNLINP")
                  goto L49110

L49410:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L49110

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151
                  errormsg$ = " "

                if module$(c%) <> " " then L50130
                   errormsg$ = "Module cannot be blank"
                   return

L50130:         temp% = 0%
                convert module$(c%) to temp%, data goto L50150
L50150:         convert temp% to str(module$(c%),1,2), pic(00)

                if module$(c%) <> "00" then L50210
                   errormsg$ = "Reserved module #, please respecify"
                   return

L50210:         search str(module$(),1) = module$(c%) to cursor%() step 3

                if (cursor%(1%)+2%)/3% <> c% then L50250
                if cursor%(2%) = 0% then return
L50250:            errormsg$ = "Module already exists, please respecify"
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152
                errormsg$ = " "

        REM TEST JOURNAL ID
                if journal$(c%) <> " " then L51150
                   errormsg$ = "Journal ID cannot be blank"
                   return

L51150:         search str(journal$(),1) =                               ~
                                        journal$(c%) to cursor%() step 3

                if (cursor%(1%)+2%)/3% <> c% then L51200
                if cursor%(2%) = 0% then L51300
L51200:            errormsg$ = "Journal exists in this module, respecify"
                   return

L51300: REM TEST NEXT SEQ# (NO ERROR, SET IT)
                jnlseq% = 1%
                convert jnlseq$(c%) to jnlseq%, data goto L51330
L51330:         jnlseq% = min(99999999%, max(jnlseq%, 1%))
                convert jnlseq% to jnlseq$(c%), pic(########)

        REM TEST POST FLAG
                if jnlsum$(c%) = "F" then return
                if jnlsum$(c%) = "D" then return
                if jnlsum$(c%) = "Y" then return
                if jnlsum$(c%) = "N" then return
                   errormsg$ = "Summary option must be 'F, D, Y, OR N'"
                   return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end
