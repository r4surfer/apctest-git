        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M   SSS   U   U  M   M  RRRR   Y   Y   *~
            *  B   B  O   O  MM MM  S      U   U  MM MM  R   R  Y   Y   *~
            *  BBBB   O   O  M M M   SSS   U   U  M M M  RRRR    YYY    *~
            *  B   B  O   O  M   M      S  U   U  M   M  R   R    Y     *~
            *  BBBB    OOO   M   M   SSS    UUU   M   M  R   R    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMSUMRY - PROCESSES SUMMARY BOM'S.  ALLOWS UP TO 16      *~
            *            DIFFERENT ASSEMBLIES TO BE EXPRESSED AT ONCE.  *~
            *            THE SYSTEM EXPLODES ALL REQUIREMENTS ALL THE   *~
            *            WAY DOWN FOR EACH ASSEMBLY, THEN SHOWS A       *~
            *            SUMMARY OF THE TOTAL REQUIRMENTNS.             *~
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
            * 08/02/83 ! ORIGINAL                                 ! GLW *~
            * 02/01/84 ! CORRECT "MONTHS OPEN" FMT @ 1700         ! ECR *~
            * 12/04/85 ! Fixed EDITMODE processing                ! MJB *~
            * 09/19/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 10/25/88 ! Added Part Type to Reports               ! KAB *~
            *          ! Fixed Loop logic, deals with deleted BOM !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            b$(490)3,                    /* ALL EFFECTIVE BOM'S        */~
            bom$3,                       /* WHICH BOM IS EFFECTIVE     */~
            component$25,                /* COMPONENT PART NUMBER      */~
            count%(16),                  /* INDENT INDEXES THIS LEVEL  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITS  */~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            descr$32,                    /* DESCRIPTION OF PART        */~
            edttran$80,                  /* TRANSLATION STRING FOR EDIT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(17,2)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            hdrdate$45,                  /* DATE FOR REPORT PRINTING   */~
            hdrdescr$132,                /* BUILD XXX OF XXX MESSAGE   */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            maxlevels$2,                 /* MAXIMUM NUMBER OF LEVELS   */~
            mkr$2, mkk$2,                /* BOM MARKER                 */~
            p$(16)31,                    /* NEXT BOM REC EACH LEVEL    */~
            part$25,                     /* PART TO DO THIS LEVEL      */~
            part$(16)25,                 /* PART NUMBERS FOR MULTIPLE  */~
            part22$25,                   /* PART NUMBERS FOR MULTIPLE  */~
            print$(5)10,                 /* 5 NUMBERS, PRINT FORMATTED */~
            prtpart$100,                 /* PART NUMBER/DESCR INDENTED */~
            quantity$(16)10,             /* QUANTITIES FOR MULTIPLE OA */~
            r(16),                       /* QUANTITY REQUIRED TO HERE  */~
            readkey$100,                 /* KEY TO PROCESS IN READING  */~
            tod$8,                       /* FIRST DAY OF PLAN PERIOD   */~
            type$3                       /* PART TYPE                  */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R5.01.03 11/15/88 Patch Release                   "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 9 ! WORKFILE ! WORK FILE FOR CUMULATIVE ANALYZES        *~
            * #24 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #34 ! SYSFILE2 ! SYSTEM FILE FOR MONTHS OPEN              *~
            *************************************************************

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #34, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select # 9, "SORTWORK",                                      ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos =   1, keylen =  25                      ~

           select #24, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#24, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#34, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES FIELDS NECESSARY FOR PROCESSING THE REPORT.   *~
            *************************************************************

            date$ = date
            call "READ100" (#34, "MONTHS OPEN         ", f1%(34))
                     if f1%(34) <> 1% then goto L09140
            get #34, using L09100, tod$
L09100:     FMT XX(32), CH(6)
            call "DATE" addr ("G-", tod$, date$, today%, r%)
            today% = today% + 1%
            goto L09170
L09140:     today% = 1%
            r% = r%

L09170:     call "DATEFMT" (date$)
            maxlevels% = 15%
            REM SET UP STRINGS FOR EDIT TRANSLATION.
                init(hex(00)) str(edttran$,  1, 80)
                init(hex(01)) str(edttran$,  1, 27)
                init(hex(02)) str(edttran$, 28, 11)
                init(hex(01)) str(edttran$, 44, 26)
                init(hex(02)) str(edttran$, 70, 11)

        REM *************************************************************~
            *               I N P U T   Q U E S T I O N S               *~
            *                                                           *~
            * INPUTS THE QUESTIONS AND THE PART NUMBER INFORMATION      *~
            * AND EXITS WHEN A BLANK PART NUMBER IS ENTERED.            *~
            *************************************************************

        inputmode
            init(" ") maxlevels$, part$(), quantity$(), inpmessage$,     ~
                      errormsg$, infomsg$
            if anotherflag% = 1% then infomsg$ = "YOUR SUMMARY BOM HAS BE~
        ~EN PRINTED, DO YOU WISH ANOTHER?"
            pf16$ = "(16)EXIT PROGRAM"

            for screenline% = 1 to 16
                c% = screenline%
                for fieldnr% = 1 to 2
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L10270
L10190:             gosub'203(fieldnr%)
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  = 16 then       L65000
                          if keyhit% <>  0 then       L10190
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L10190
                    next fieldnr%
                if part$(c%) <> " " then maxpart% = c%
L10270:         next screenline%

        REM *************************************************************~
            *                E D I T   R E S P O N S E S                *~
            *                                                           *~
            * EDITS BOTH THE HEADER AND LINE ITEMS RESPONSES FOR THE    *~
            * ORDER ANALYSIS PARAMETERS.                                *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, infomsg$
         inpmessage$ = "To EDIT, Position cursor to appropriate field and~
        ~ press RETURN"
            pf16$ = "(16)PRINT REPORT"

        REM EDIT TABULAR DATA FIELDS.
L11130:     gosub'203(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L30000
                  if keyhit% <>  0 then       L11130
            if cursor%(1) < 12 or cursor%(1) > 19 then L11130
                   fieldnr% = val(str(edttran$, cursor%(2)))
                   if fieldnr% < 1% then L11130
                   c%, screenline% =                                     ~
                     (2% * (cursor%(1) - 12%) + 1%) + int(cursor%(2)/44%)
                   c%, screenline% =  min(maxpart% + 1%, c%)
                   if fieldnr% = 2% and part$(c%) = " " then L11130

L11250:               gosub'203(fieldnr%)
                            if keyhit%  =  1 then gosub startover
                            if keyhit% <>  0 then       L11250
                      gosub'153(fieldnr%)
                            if errormsg$ <> " " then L11250
                            if part$(c%) <> " " then                     ~
                                         maxpart% = max(maxpart%, c%)
                      goto L11130

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   P A R T S      *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR THE PART NUMBER SCREEN*~
            * A FIELD IS DISABLED IF THE PREVIOUS PART FIELD IS BLANK.  *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L20130,         /* PART NUMBER      */~
                                    L20180          /* QUANTITY TO BUILD*/
                     return
L20130:     REM DEFAULT/ENABLE FOR PART NUMBER
                if screenline% < 2 then L20160
                if part$(screenline%-1) = " " then return
L20160:            enabled% = 1
                   return
L20180:     REM DEFAULT/ENABLE FOR QUANTITY TO BUILD
                if part$(screenline%) = " " then return
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode


L30000: REM *************************************************************~
            *   P E R F O R M   S U M M A R Y    A N A L Y S I S        *~
            *                                                           *~
            *                                                           *~
            * SAME TECHNIQUE AS IN BOMOA.  THE SYSTEM DOESN'T SEE ANY   *~
            * ON-HAND INVENTORY SO THE COMPLETE SUMMARY BILL IS PROC-   *~
            * SSED FOR EACH PART, THEN CUMULATED FOR ALL ASSEMBLIES     *~
            * SPECIFIED.                                                *~
            *************************************************************

            page% = 0
            call "WORKOPEN" (# 9, "IO   ", 1000%, f29%)
            call "SHOSTAT" ("SUMMARY Bill of Materials Processing Under W~
        ~ay")
            for part% = 1 to 16
                if str(part$(part%),1,25) = " " then L30290
                   l% = 0
                   line% = 1000
                   convert quantity$(part%) to qtyreqd
        put #5, using L30200, part$(part%), " ","001"," ",  qtyreqd, 1, " "
L30200:            FMT CH(25), CH(25), CH(3),CH(3), 2*PD(14,4),POS(89),  ~
                       CH(14)
                   gosub L33200           /* FORMAT HDRDESCR$           */
                   gosub'7(" ", 1)       /* START WITH FAKE RECORD.    */
                   REM PRINT TAG LINE FOR THIS PART'S OA.
                       if tagprinted% <> 0 then L30290
                          gosub L31390    /* PAGE CONTROL SUBROUTINE    */
                          if tagprinted% <> 0 then L30290
                             print using L31660
                             tagprinted% = 1
L30290:         next part%
            goto L32080                   /* SUMMARY REPORT             */

            deffn'7(part$, r(l%+1))
                  l% = l% + 1
                     gosub'22 (part$, bom$)
                  p$(l%) = str(part$,1,25) & str(bom$,1,3)
                  count%(l%) = 0         /* SEQUENCE NUMBER FOR PRINT  */
                  if l% = 1 then L30410   /* HANDLE FAKE REC FOR LEV 1  */

L30390:           call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                       if f1%(5) = 0 then L30560
                       if str(p$(l%),29,3) = "  0" then L30390
L30410:           gosub L60000            /* LOAD BOM RECORD & INFO.    */
                  gosub'10(component$)   /* LOAD COMPONENT INFORMATION */
                  needed = r(l%) * quantity * xused
                  REM CHECK TO SEE IF THIS PART HAS COMPONENTS OR NOT
                     gosub'22 (component$, bom$)
                      readkey$ = str(component$,1,25) & str(bom$,1,3)
L30470:               call "PLOWNEXT" (#5, readkey$, 28%, f1%(5))
                      if f1%(5) <> 0 then L30485
                         component% = 0%
                         goto L30500
L30485:               if str(readkey$,29,3) = "  0" then L30470
                         component% = 1%
L30500:           count%(l%) = count%(l%) + 1
                  if component% = 0 or l% = maxlevels% + 1               ~
                     then gosub L30990              /* ATOM CASE        */~
                     else gosub L30600              /* (SUB)ASSEMBLY    */
                  goto L30390

L30560:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

L30600:     REM ROUTINE TO COMMIT PARTS FOR THOSE THAT ARE MANUFACTURED
                if needed <= avail then dig% = 0% else dig% = 1%
                build  = max(0, needed - avail)
                if mkr$ = "UU" then goto L30660
                if mkr$ = "PH" then goto L30660
                manuf = manuf + build
L30660:         if mkr$ = "PH" then goto L30680
                committed = committed + needed
L30680:         if mkr$ = "UU" then mkk$ = "UU"
                if mkr$ = "PH" then mkk$ = "PH"
                if mkr$ = "OP" then mkk$ = "OP"
                rewrite #9, using L31960, component$, descr$, onhand,     ~
                            committed, buy, manuf, mkk$, type$
                REM PRINT AN ENTRY.
                    init(" ") print$()
                    if mkr$ <> "PH" then goto L30800
                    init(" ") print$()
                     print$(3) = "PHANTOM "
                     goto L30930

L30800:             call "CONVERT" (needed, 2.4, print$(1))
                    if avail = 0                                         ~
                       then print$(2) = " "                              ~
                       else call "CONVERT" (avail, 2.4, print$(2))
                    if build = 0                                         ~
                       then print$(3) = " "                              ~
                       else call "CONVERT" (build, 2.4, print$(3))

                     if mkr$ <> "UU" then goto L30930
                     print$(3) = "USEUP PART"
                     if needed = 0 then print$(1) = "SEE BELOW "
                     if needed = 0 then print$(2) = "  NONE    "

L30930:             gosub L31210          /* PRINT INDENTED ENTRY.      */
                REM NOW DIG IF ANY TO BUILD FROM COMPONENTS.
                    if dig% = 0 then return
                       gosub'7 (component$, build)
                       return

L30990:     REM ROUTINE TO COMMIT PARTS FOR THOSE THAT ARE PURCHASED
                committed = committed + needed
                buyy   = max(0, needed - avail)
                buy = buy + buyy
                if mkr$ = "UU" then mkk$ = "UU"
                if mkr$ = "PH" then mkk$ = "PH"
                if mkr$ = "OP" then mkk$ = "OP"
                rewrite #9, using L31960, component$, descr$, onhand,     ~
                            committed, buy, manuf, mkk$, type$
                REM PRINT AN ENTRY FOR ATOMS.
                    init(" ") print$()
                    call "CONVERT" (needed, 2.4, print$(1))
                    if avail = 0                                         ~
                       then print$(2) = " "                              ~
                       else call "CONVERT" (avail, 2.4, print$(2))
                    REM PRINT$(3), BUILD FROM COMPONENTS, STAYS BLANK.
                    if buy = 0                                           ~
                       then print$(4) = " "                              ~
                       else call "CONVERT" (buyy, 2.4, print$(4))
                    gosub L31210
                return

L31210:         REM ROUTINE THAT INDENTS, FORMATS ENTRIES, AND PRINTS.
                    prtpart$ = " "
                    put str(prtpart$, l% * 3 - 2, 30), using L31360,      ~
                        count%(l%), component$
                    gosub L31390 : gosub d_type
                    print using L31740, prtpart$, print$(1), type$, tdescr$

                    tagprinted% = 0
                    prtpart$ = " "
                    put str(prtpart$, l% * 3 - 2, 37), using L31370,      ~
                        descr$
                    gosub L31390
                    print using L31740, prtpart$, " ", " ", " "
                    tagprinted% = 0
                    return
L31360:                 %###. #########################
L31370:                 %     ################################

L31390:         REM PAGE CONTROL ROUTINE FOR THE ORDER ANALYSIS
                    select printer (134)
                    line% = line% + 1
                    if line% < 60 then return
                       if page% = 0 then L31470
                          if tagprinted% <> 0 then L31470
                             print using L31660
                             tagprinted% = 1
L31470:                page% = page% + 1
                       print page
                       call "DATE" addr("HD", hdrdate$)
                       print using L31620, page%
                       print
                       print
                       print using L31640 , hdrdescr$, hdrdate$
                       print using L31660
                       print using L31680
                       print using L31700
                       print using L31720
                       print using L31660
                       line% = 10%
                       return

L31620: %PAGE####  B I L L   O F   M A T E R I A L   U S E A G E   R E Q ~
        ~I R E M E N T   F O R   Q U A N T I T Y   S P E C I F I E D
L31640: %          ######################################################~
        ~############     #############################################
L31660: %+---------------------------------------------------------------~
        ~-----------------+----------+----------------------+
L31680: %!                                                               ~
        ~                 ! REQUIRED !                      !
L31700: %!                  P A R T   C O D E   /   D E S C R I P T I O N~
        ~                 ! FOR THIS !TYPE / DESCRIPTION    !
L31720: %!                                                               ~
        ~                 !  PART    !                      !
L31740: %!###############################################################~
        ~#################!##########! ### ################ !

            REM ROUTINE TO LOAD COMPONENT PART INFORMATION.
                deffn'10(part$)
L31790:         call "READ101" (#9, part$, f1%(9))
                     if f1%(9) = 0 then L31870      /* MAKE NEW WF ENTRY*/
              get #9, using L31960, part$, descr$, onhand, committed,     ~
                     buy, manuf, mkk$, type$

                avail = 0
                return

L31870:         REM IF NOT FOUND, CREATE NEW WORK FILE ENTRY FOR THIS P/N.
                    toh, buy, manuf, onhand, committed = 0
                    call "DESCRIBE" (#4, part$, descr$, 0%, f1%(4))
                    type$ = " "
                    if f1%(4) = 0% then L31910
                       get #4 using L31898, type$
L31898:                    FMT POS(180), CH(3)
L31910:             write #9, using L31960, part$, descr$, toh   , 0 , 0, ~
                               0, mkr$, type$

                    goto L31790

L31960:         FMT CH(25),              /* PART NUMBER KEY TO FILE    */~
                    CH(32),              /* PART DESCRIPTION           */~
                    PD(14,4),            /* QUANTITY ON HAND           */~
                    PD(14,4),            /* QUANTITY COMMITTED         */~
                    PD(14,4),            /* QUANTITY TO BUY            */~
                    PD(14,4),            /* QUANTITY TO MANUFACTURE    */~
                    CH(2),               /* BOM MARKER                 */~
                    CH(3)                /* TYPE                       */

L32080:         readkey$ = " "
                line% = 1000%

L32110:         call "PLOWNEXT" (#9, readkey$, 0%, f1%(9))
                     if f1%(9) = 0 then L32340      /* TOTAL REPORT     */
           get #9, using L32150, part$, descr$, onhand, committed, buy,   ~
                     manuf, mkk$, type$
L32150:    FMT CH(25), CH(32), 4 * PD(14,4), CH(2), CH(3)
                if mkk$ = "PH" then print$(5) = "PHANTOM   "
                if mkk$ = "PA" then print$(5) = "PHANT ASSM"
                if mkk$ = "OP" then print$(5) = "OPTION    "
                if mkk$ = "UU" then print$(5) = "USEUP PART"
                call "CONVERT" (onhand, 2.4, print$(1))
                call "CONVERT" (committed, 2.4, print$(2))
                call "CONVERT" (buy, 2.4, print$(3))
                call "CONVERT" (manuf, 2.4, print$(4))
                if onhand = 0 then print$(1) = " "
                if committed = 0 then print$(2) = " "
                if buy    = 0 then print$(3) = " "
                if manuf     = 0 then print$(4) = " "
                gosub L32830              /* PAGE CONTROL SUBROUTINE    */
                gosub d_type
                print using L33010, part$, descr$, print$(2), print$(5),  ~
                                   type$, tdescr$
                tagprinted% = 0
                init(" ") print$()
                goto L32110

L32340:         REM PRINT TAG LINE FOR REPORT, EXIT PROGRAM
                    if tagprinted% <> 0 then L32370
                    print using L33030
L32370:             print using L33120 : print using L33140
                    print using L33160 : print using L33180

           line% = 1000%
           for part% = 1% to 16%
           if part$(part%) = " " then goto   L32490
           gosub L32540
               call "DESCRIBE" (#4, part$(part%), descr$, 0%, f1%(4))
               if f1%(4) = 0% then L32460
                  get #4 using L32453, type$
L32453:               FMT POS(180), CH(3)
L32460:    convert part% to paa$, pic(####)
           gosub d_type
           print using L32680, paa$, part$(part%), descr$,                ~
                              quantity$(part%), type$, tdescr$
L32490:    next part%
           print using L32700
           goto L32750

                REM PAGE CONTROL SUBROUTINE FOR DEMAND REPORT.
L32540:             line% = line% + 1
                    if line% < 60 then return
                       page% = page% + 1
                       print page
                       print using L32660, page%
                       print
                       print using L32700
                       print using L32720
                       print using L32700
                       line% = 5
                       return

L32660: %PAGE####  O R D E R   O F   B O M   S T R U C T U R E S

L32680: % !   #### !#########################!###########################~
        ~#####!##########! ### ################ !
L32700: % +--------+-------------------------+---------------------------~
        ~-----+----------+----------------------+
L32720: % ! STRUCT !  P A R T   N E E D E D  !     D E S C R I P T I O N ~
        ~     ! QUANTITY !TYPE / DESCRIPTION    !

L32750:        anotherflag% = 1%
               call "FILEBGON" (#9)
               f29% = 1%
               close printer
               goto inputmode

L32830:         REM PAGE CONTROL SUBROUTINE FOR SUMMARY REPORT.
                    line% = line% + 1
                    if line% < 60 then return
                       page% = page% + 1
                       print page
                       print using L32980, page%
                       print
                       print
                       print using L33030
                       print using L33050
                       print using L33070
                       print using L33090
                       print using L33030
                       line% = 9
                       return
L32980: %PAGE####  S U M M A R I Z E D   B I L L   O F   M A T E R I A L ~
        ~ S

L33010: %!#########################!################################!####~
        ~######!          !##########! ### ################# !
L33030: %+-------------------------+--------------------------------+----~
        ~------+          +----------+-----------------------+
L33050: %!                         !                                ! TOT~
        ~AL    !          ! SPECIAL  !                       !
L33070: %!  P A R T   N U M B E R  !     D E S C R I P T I O N      ! SUM~
        ~MARY  !          ! PART USE !TYPE / DESCRIPTION     !
L33090: %!                         !                                ! BOM~
        ~ USE  !          !  NOTES   !                       !

L33120: %  COMPONENT PARTS OR ASSEMBLIES MAY BE USED DIFFERENTLY IN DIFFE~
        ~RENT BILL STRUCTURES.  THE 'SPECIAL PART USE NOTES'
L33140: %  INDICATE ANY NON-STANDARD USE ENCOUNTERED.  PARTS MARKED 'PHAN~
        ~TOM' OR 'USEUP PART' WERE SO SPECIFIED IN AT LEAST
L33160: %  ONE BOM.  THOSE MARKED 'OPTION' WERE SUMMARIZED BY THE OPTION ~
        ~MARKER SHOWN EVEN THOUGH THAT PART MAY BE A
L33180: %  GENERIC PART.  PLEASE USE PRODUCTION PLANNING TO PROPERLY ANAL~
        ~YZE OPTION PARTS.
L33200:     REM FORMATS HDRDESCR$, WHICH TELLS HOW MANY OF WHAT WE BUILDIN
                call "DESCRIBE" (#4, part$(part%), readkey$, 1%, f1%(4))
                     if f1%(4) = 0 then readkey$ = "(PART NOT ON FILE)"
           convert part% to demnum$, pic(###)
        hdrdescr$ = "BOM STRUCTURE "& str(demnum$,1,3) & "   "           ~
            & quantity$(part%)                                           ~
            & " PART "   & part$(part%) & " " & readkey$
                return

        d_type
            type% = 0% : tdescr$ = "UNDEFINED"
            convert type$ to type%, data goto L33390
            if type% = 0% then tdescr$ = "GENERIC"
            if type% > 0% and type% < 200% then tdescr$ = "UNPLANNED"
            if type% > 199% and type% < 490% then tdescr$ = "PURCHASED"
            if type% > 489% and type% < 500% then tdescr$ = "PURCH. TOOL"
            if type% > 499% and type% < 790% then tdescr$ = "MANUFACTURED"
            if type% > 789% and type% < 800% then tdescr$ = "MFG. TOOL"
            if type% > 799% and type% < 1000% then tdescr$ ="MANUFACTURED"
L33390:     return

        REM *************************************************************~
            *  A N S W E R   Q U E S T I O N S / I N P U T   P A R T S  *~
            *                                                           *~
            * ANSWER QUESTIONS AND INPUT PART NUMBER(S) TO PROCESS FOR  *~
            * THE ORDER ANALYSIS BIT.                                   *~
            *************************************************************

            deffn'203(fieldnr%)
                  init(hex(84)) fac$(), lfac$()
                  if fieldnr% <> 0% then L40110
                  init(hex(86)) lfac$(), str(fac$(), 1, 2%*maxpart%+1%)
L40110:           on fieldnr% gosub L40150,         /* PART NUMBER      */~
                                    L40180          /* QUANTITY TO MAKE */
                     goto L40220

L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L40180:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
               "SUMMARY BILL OF MATERIALS PROCESSOR - ENTER ALL OF THE CO~
        ~MPONENTS",                                                       ~
               at (01,75),                                               ~
                  "PAGE 1",                                              ~
               at (02,02),                                               ~
                  "DESIRED, THEN PF-16 TO PROCESS AND SUMMARIZE",        ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (06,02),                                               ~
                  "UP TO 16 PARTS CAN BE SUMMARIZED AT ONCE.  THE ENTIRE ~
        ~BILL WILL",                                                      ~
               at (07,02),                                               ~
                  "BE EXPLODED FOR EACH PART SHOWN BELOW",               ~
               at (10,09),                                               ~
                  "PART NUMBER",                                         ~
               at (10,29),                                               ~
                  "QUANTITY",                                            ~
               at (10,52),                                               ~
                  "PART NUMBER",                                         ~
               at (10,72),                                               ~
                  "QUANTITY",                                            ~
               at (12,02), fac(fac$( 1,1)), part$    ( 1)       , ch(25),~
               at (12,28), fac(fac$( 1,2)), quantity$( 1)       , ch(10),~
               at (12,45), fac(fac$( 2,1)), part$    ( 2)       , ch(25),~
               at (12,71), fac(fac$( 2,2)), quantity$( 2)       , ch(10),~
               at (13,02), fac(fac$( 3,1)), part$    ( 3)       , ch(25),~
               at (13,28), fac(fac$( 3,2)), quantity$( 3)       , ch(10),~
               at (13,45), fac(fac$( 4,1)), part$    ( 4)       , ch(25),~
               at (13,71), fac(fac$( 4,2)), quantity$( 4)       , ch(10),~
               at (14,02), fac(fac$( 5,1)), part$    ( 5)       , ch(25),~
               at (14,28), fac(fac$( 5,2)), quantity$( 5)       , ch(10),~
               at (14,45), fac(fac$( 6,1)), part$    ( 6)       , ch(25),~
               at (14,71), fac(fac$( 6,2)), quantity$( 6)       , ch(10),~
               at (15,02), fac(fac$( 7,1)), part$    ( 7)       , ch(25),~
               at (15,28), fac(fac$( 7,2)), quantity$( 7)       , ch(10),~
               at (15,45), fac(fac$( 8,1)), part$    ( 8)       , ch(25),~
               at (15,71), fac(fac$( 8,2)), quantity$( 8)       , ch(10),~
               at (16,02), fac(fac$( 9,1)), part$    ( 9)       , ch(25),~
               at (16,28), fac(fac$( 9,2)), quantity$( 9)       , ch(10),~
               at (16,45), fac(fac$(10,1)), part$    (10)       , ch(25),~
               at (16,71), fac(fac$(10,2)), quantity$(10)       , ch(10),~
               at (17,02), fac(fac$(11,1)), part$    (11)       , ch(25),~
               at (17,28), fac(fac$(11,2)), quantity$(11)       , ch(10),~
               at (17,45), fac(fac$(12,1)), part$    (12)       , ch(25),~
               at (17,71), fac(fac$(12,2)), quantity$(12)       , ch(10),~
               at (18,02), fac(fac$(13,1)), part$    (13)       , ch(25),~
               at (18,28), fac(fac$(13,2)), quantity$(13)       , ch(10),~
               at (18,45), fac(fac$(14,1)), part$    (14)       , ch(25),~
               at (18,71), fac(fac$(14,2)), quantity$(14)       , ch(10),~
               at (19,02), fac(fac$(15,1)), part$    (15)       , ch(25),~
               at (19,28), fac(fac$(15,2)), quantity$(15)       , ch(10),~
               at (19,45), fac(fac$(16,1)), part$    (16)       , ch(25),~
               at (19,71), fac(fac$(16,2)), quantity$(16)       , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65), fac(hex(8c)), pf16$,                   ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40960
                  call "MANUAL" ("BOMSUMRY")
                  goto L40220

L40960:        if keyhit% <> 15 then L41000
                  call "PRNTSCRN"
                  goto L40220

L41000:        REM GET CURSOR LOCATION FOR EDIT MODE COMPUTATIONS.
                   close ws
                   call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                   return

        REM *************************************************************~
            *      T E S T   T A B U L A R   I N F O R M A T I O N      *~
            *                                                           *~
            * TESTS TABULAR INFORMATION TO MAKE SURE THAT THE DATA IS   *~
            * OK.  ZAP QUANTITY IF BLANK PART NUMBER ENTERED.           *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120,         /* PART NUMBER      */~
                                    L50300          /* QUANTITY TO BUILD*/
                     return
L50120:     REM TEST DATA FOR PART NUMBER
                if part$(c%) <> " " then L50160
                   quantity$(c%) = " "
                   goto L50450
L50160:         call "DESCRIBE" (#4, part$(c%), infomsg$, 1%, f1%(4))
                if f1%(4) = 0 then L50280
                     gosub'22 (part$(c%), bom$)
                readkey$ = str(part$(c%),1,25) & str(bom$,1,3)
                call "PLOWNEXT" (#5, readkey$, 28%, f1%(5))
                     if f1%(5) = 0 then L50260
                if quantity$(c%) <> " " then return
                fieldnr% = 2%
                errormsg$ = hex(00)
                return
L50260:         errormsg$ = "Part Has No Components: " & part$(c%)
                return
L50280:         errormsg$ = "Part Number Not On File: " & part$(c%)
                return
L50300:     REM TEST DATA FOR QUANTITY TO BUILD
                if part$(c%) = " " then L50420
                convert quantity$(c%) to quantity, data goto L50370
                if quantity < 0 then L50390
                call "NUMSMASH" (quantity, 2, quantity$(c%))
                return

L50370:         errormsg$= "Illegal Entry For Quantity: " & quantity$(c%)
                   return
L50390:         errormsg$ = "Quantity Must Be Greater Than Zero: "       ~
                                         & quantity$(c%)
                    return
L50420:         errormsg$="Cannot enter Quantity if Part Number is Blank"
                    return

L50450: REM     CLEAN UP STACK
            if c% > maxpart% then return
            if c% = maxpart% then L50540
            if maxpart% < 2% then L50540
            for i% = c% to maxpart% - 1%
                part$    (i%) = part$    (i%+1%)
                quantity$(i%) = quantity$(i%+1%)
            next i%

L50540:     maxpart% = max(maxpart% - 1%, 0%)
            init (" ") part$(maxpart%+1%), quantity$(maxpart%+1%)
            return

L60000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD FROM DISK.             *~
            *************************************************************

            get    #5, using L60100,                                      ~
                       component$, quantity, xused, mkr$
            return

L60100:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* WHICH ALTERNATE BOM        */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                POS(89),                                                 ~
                CH(2)                    /* BOM MARKER                 */

        REM FIND THE CURRENTLY EFFECTIVE BILL STRUCTURE FOR THE PART
        REM PASSED IN

        deffn'22(part22$, bom$)
           bom$ = "001"
           call "READ100" (#24, str(part22$,1,25) & "1001", f1%(24))
                     if f1%(24) <> 1% then return

           get #24, using L60270, b$()
L60270:        FMT  XX(29),  490*CH(3)
               if b$(today%) <> " " then bom$ = b$(today%)
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
