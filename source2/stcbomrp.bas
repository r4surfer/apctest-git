        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   TTTTT   CCC   BBBB    OOO   M   M  RRRR   PPPP    *~
            *  S        T    C      B   B  O   O  MM MM  R   R  P   P   *~
            *   SSS     T    C      BBBB   O   O  M M M  RRRR   PPPP    *~
            *      S    T    C      B   B  O   O  M   M  R  R   P       *~
            *   SSS     T     CCC   BBBB    OOO   M   M  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCBOMRP - PRINTS PRICED BILLS OF MATERAILS.  UP TO 16    *~
            *            ASSEMBLY NUMBER MAY BE ENTERED.  BOM'S ARE     *~
            *            PRICED USING THE CURRENT COST SET.             *~
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
            * 07/22/87 ! ORIGINAL - REPLACES BOMSTAND             ! MJB *~
            * 10/26/87 ! No longer reverts to current cost set    ! HES *~
            * 11/10/87 ! Stop devide by zero in END_LEVEL logic   ! HES *~
            * 11/20/87 ! Extend totals on subassemblies           ! HES *~
            * 03/30/88 ! Total rewrite using clone of BOMMRKUP    ! HES *~
            * 03/29/88 ! Finalizing for release                   ! RJM *~
            * 06/02/88 ! Now skips RE & TL type components (30462)! RJM *~
            * 06/10/88 ! Bugs Fixed; Now close & reopens new cost ! RJM *~
            *          !      Sets if edited, Plows for parts in  !     *~
            *          !      STCHNY during selection.            !     *~
            * 05/05/89 ! Corrected Input editing of Part Numbers  ! MJB *~
            * 04/10/90 ! PRR 11607 Corrected adding bucket 5 twice! JDH *~
            * 06/05/91 ! PRRs 11634, 11607, 11609 Rounding problem! JIM *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 04/20/92 ! PRR 12403 Fixed when over 6 levels.      ! JDH *~
            *          ! PRR 12321 Fixed Desc returnd from GETCODE!     *~
            * 09/21/93 ! PRR 12924 Added PF14 to display report.  ! JDH *~
            *          ! Added support for new BOM Markers 'NC' & !     *~
            *          !   'NP' which do not add costs.           !     *~
            *          ! Corrected costs for Phantom Parts.       !     *~
            * 10/15/93 ! Added support to new BOM Markers 'BP'    ! JDH *~
            * 04/04/97 ! Change PUTPARM call for NT Compatibility ! LDJ *~
	    *          !   which has neg. costs, unlike std ByProd!     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            bom$3,                       /* WHICH BOM IS EFFECTIVE     */~
            bomid$(16)3,                 /* BOM ID's for multiple      */~
            company$60,                  /* COMPANY NAME               */~
            component$25,                /* COMPONENT PART NUMBER      */~
            cost(12),                    /* Standard Costs By Bucket   */~
            cost$(12)9,                  /* Standard Costs By Bucket   */~
            count%(16),                  /* INDENT INDEXES THIS LEVEL  */~
            cst_bom(12), cst_rte(12),    /* costs                      */~
            cst_msc(12),                 /* costs                      */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITS  */~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            descr$32,                    /* DESCRIPTION OF PART        */~
            editmessage$79,              /* Screen Edit Message        */~
            edttran$80,                  /* TRANSLATION STRING FOR EDIT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(16)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            hdr1$(5)9,                   /* report headers             */~
            hdr2$(5)9,                   /* report headers             */~
            hdrdescr$132,                /* BUILD XXX OF XXX MESSAGE   */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line #2             */~
            line10$79,                   /* Screen Line #10            */~
            maxlevels$1,                 /* MAXIMUM NUMBER OF LEVELS   */~
            mkr$2,                       /* BOM MARKER                 */~
            p$(16)31,                    /* NEXT BOM REC EACH LEVEL    */~
            part$25,                     /* PART TO DO THIS LEVEL      */~
            part$(16)25,                 /* PART NUMBERS FOR MULTIPLE  */~
            part22$25,                   /* PART NUMBERS FOR MULTIPLE  */~
            print$(6)10,                 /* 5 NUMBERS, PRINT FORMATTED */~
            prtpart$57,                  /* PART NUMBER/DESCR INDENTED */~
            ptype$3,                     /* PART TYPE                  */~
            r(16),                       /* QUANTITY REQUIRED TO HERE  */~
            readkey$100,                 /* KEY TO PROCESS IN READING  */~
            set$8,                       /* Cost Set ID                */~
            setsave$8,                   /* cost set ID                */~
            summary$3,                   /* PRINT SUMMARY REPORT?      */~
            time$8,                      /* TIME OF DAY, REPORT HEADER */~
            uom$4,                       /* PART UNIT OF MEASURE       */~
            usrid$3,                     /* USER ID, REPORT HEADER     */~
            bkid$(12)10,                 /* Cost Bucket ID's           */~
            cset$8,                      /* Current Cost Set ID        */~
            setdescr$30,                 /* Cost Set description       */~
            setid$4                      /* System Internal Set ID     */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con : mat f1% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * #10 ! STCHNY   ! STANDARD COST SET - INVENTORY STANDARDS  *~
            * #11 ! STCMAPNG ! STANDARD COST SET - MAPPINGS             *~
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

            select #10, "STCHNY",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 25

           select #11, "STCMAPNG" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 8

           select #34, "SYSFILE2" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 4, f1%( 4), f2%( 4), 0%, " ")
            call "OPENCHCK" (# 5, f1%( 5), f2%( 5), 0%, " ")
            call "OPENCHCK" (#34, f1%(34), f2%(34), 0%, " ")

            if f1%(4) = 1% and f1%(5) = 1% and f1%(34) = 1% then L09000
                call "ASKUSER" (keyhit%, "Missing File(s)",              ~
                                "Files could not be Found",              ~
                                " ",                                     ~
                                "Press RETURN to exit.")
                end

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES FIELDS NECESSARY FOR PROCESSING THE REPORT.   *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            time$ = " " : call "TIME" (time$)
            call "COMPNAME" (12%, company$, 0%)
            call "EXTRACT" addr ("ID", usrid$, 3%)

            REM SET UP STRINGS FOR EDIT TRANSLATION.
                init(hex(00)) str(edttran$,  1, 80)
                init(hex(01)) str(edttran$,  1, 27)
                init(hex(02)) str(edttran$, 44, 26)

            editmessage$ = "Position Cursor and press RETURN to edit data"
            str(line2$,62) = "STCBOMRP: " & str(cms2v$,1,8)
            line10$ = "Part Number              " & hex(8c)
            str(line10$,43) = hex(ac) & "Part Number              " &    ~
                hex(8c)

            buckets% = 2%
            call "STCSETID" (buckets%, #34, cset$, setid$, bkid$())
            if buckets% > 0% then L10000
                setid$ = " "


L10000: REM *************************************************************~
            *               I N P U T   Q U E S T I O N S               *~
            *                                                           *~
            * INPUTS THE QUESTIONS AND THE PART NUMBER INFORMATION      *~
            * AND EXITS WHEN A BLANK PART NUMBER IS ENTERED.            *~
            *************************************************************

        inputmode
            init(" ") summary$, maxlevels$, part$(), errormsg$, set$,    ~
                      infomsg$, inpmessage$, setdescr$, bomid$()
            edit% = 0%

            for fieldnr% = 1% to 2%
                gosub'161(fieldnr%)
                      if enabled% =  0% then L10220
L10160:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then       L65000
                      if keyhit% <>  0% then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
L10220:         next fieldnr%

L10240:     for screenline% = 1% to 16%
                c% = screenline%
                    fieldnr% = 1%
                    gosub'163(fieldnr%)
                          if enabled% = 0% then L10400
L10290:             gosub'203(fieldnr%)
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  = 16% then       L65000
                          if keyhit% <>  0% then       L10290
                    gosub'153(fieldnr%)
                          if c% > 1% and part$(c%) = " " then L10420
                          if errormsg$ <> " " then L10290
L10400:         next screenline%

L10420: REM *************************************************************~
            *                E D I T   R E S P O N S E S                *~
            *                                                           *~
            * EDITS BOTH THE HEADER AND LINE ITEMS RESPONSES FOR THE    *~
            * ORDER ANALYSIS PARAMETERS.                                *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, infomsg$

            edit% = 1%
L10530:     gosub'201(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% and part$(1) <> " " then L30000
                  if keyhit%  = 16% and part$(1) <> " " then L30000
                  if keyhit% <>  0% then       L10530
            if cursor%(1) > 11% then L10680          /* EDIT TABLE DATA  */
               fieldnr% = cursor%(1) - 6%
               if fieldnr% =  2% or fieldnr% = 1% then goto L10620
               goto L10530
L10620:           gosub'161(fieldnr%)
                        if enabled% = 0% then L10530
L10628:           gosub'201(fieldnr%)
                        if keyhit%  =  1% then gosub startover
                        if keyhit% <>  0% then       L10628
                  gosub'151(fieldnr%)
                        if errormsg$  <> " " then L10628
                  goto L10530
L10680:     REM EDIT TABULAR DATA FIELDS.
                if cursor%(1) < 12% or cursor%(1) > 19% then L10530
                     fieldnr% = val(str(edttran$, cursor%(2)))
                     c%, screenline% = 2% * (cursor%(1) - 12%) + 1%
                     if fieldnr% = 0% then L10530
                  REM SET SCREENLINE% FOR COLUMN 2
                          if fieldnr% = 1% then L10770
                                fieldnr% = 1%
                                c%, screenline% = screenline% + 1%

L10770:              gosub'163(fieldnr%)
                          if enabled% = 0% then L10530
L10777:              gosub'203(fieldnr%)
                          if keyhit%  =  1% then gosub startover
                          if keyhit% <>  0% then       L10777
                     gosub'153(fieldnr%)
                          if errormsg$ <> " " then L10777
            REM "Smash" the Part #s to compress out blank ones.
                     call "LINSMASH" (part$())
                      goto L10530

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   I N P U T      *~
            *                                                           *~
            * SETS DEFAULTS AND INPUTS QUESTIONS FOR LINEAR INPUT.      *~
            *************************************************************

            deffn'161(fieldnr%)
                  inpmessage$ = " "
                  on fieldnr% gosub L20150,         /* MAX # OF LEVELS  */~
                                    L20270          /* COST SET         */

                     return
L20150:     REM Default/Enable For Maximum Number Of Levels
                enabled% = 1%
                maxlevels$ = "9"
                inpmessage$ = "Report allows up to '9' levels of explosio~
        ~n."
                return

L20270:     REM Default/Enable for Cost Set To use
                inpmessage$ = "Select the Cost Set you wish to examine."
                if set$ = " " then set$ = cset$
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   P A R T S      *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR THE PART NUMBER SCREEN*~
            * A FIELD IS DISABLED IF THE PREVIOUS PART FIELD IS BLANK.  *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0%
                  inpmessage$ = " "
                  on fieldnr% gosub L21130          /* PART NUMBER      */

                     return
L21130:     REM DEFAULT/ENABLE FOR PART NUMBER
                enabled% = 1%
             inpmessage$ = "Enter the desired Part #, partial or '?'" &  ~
                           " to see List or Blank to End Input"
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
            *       P R I N T   P R I C E D   B I L L S                 *~
            *                                                           *~
            *                                                           *~
            * SAME TECHNIQUE AS IN BOMOA.  THE SYSTEM DOESN'T SEE ANY   *~
            * ON-HAND INVENTORY SO THE COMPLETE SUMMARY BILL IS PROC-   *~
            * SSED FOR EACH PART, THEN CUMULATED FOR ALL ASSEMBLIES     *~
            * SPECIFIED.                                                *~
            *************************************************************

            page% = 0
            call "SHOSTAT" ("Generating Costed Bill(s) of Materials")

            select printer (134)
            call "SETPRNT" ("BOM003", " ", 0%, 0%)
            if keyhit% = 14% then call "SET" addr("PM", "K")

            for part% = 1% to 16%
                if str(part$(part%),1,25) = " " then L30290
                   l% = 0%
                   line% = 1000%
                put #5, using L30190, part$(part%), " ","001"," ",        ~
                                   0, 1, 0, 0, " ", " "
L30190:            FMT CH(25), CH(25), CH(3),CH(3), 4*PD(14,4), CH(2),   ~
                       POS(99), CH(04)
                gosub L32510           /* FORMAT HDRDESCR$           */
                gosub'7(" ", 1%)      /* START WITH FAKE RECORD.    */
            REM Print Tag Line For This Part
                    if tagprinted% <> 0% then L30290
                       gosub L31220    /* PAGE CONTROL SUBROUTINE    */
                       if tagprinted% <> 0% then L30290
                          print using L32050
                          tagprinted% = 1%
L30290:     next part%

            time$ = " " : call "TIME" (time$)
            print skip(1)
            print using L32250, time$
            if keyhit% = 14% then call "GETPRTNM" addr(file$, lib$, vol$)
            close printer
            call "SETPRNT" ("BOM003", " ", 0%, 1%)
            if keyhit% <> 14% then inputmode

            REM Display printed file...
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                     "FILE    ", file$, 8%, "LIBRARY ", lib$, 8%,        ~
                "VOLUME  ", vol$, 6%, "ACCESS  ", "PRINT ", 6%, "@", ret%)
            call"LINK"addr("DISPLAY ","S"," "," ",0%," "," ",0%, "N", 0%,~
	                   ret%)
            call "SCRATCH" addr("F", file$, lib$, vol$, " ", " ", 0%)
            goto inputmode

            deffn'7(part$, r(l%+1%))
                  l% = l% + 1%
                  gosub'22 (part$, bom$)
                  p$(l%) = str(part$,,25) & str(bom$,,3)
                  count%(l%) = 0%        /* SEQUENCE NUMBER FOR PRINT  */
                  if l% = 1% then L30460  /* HANDLE FAKE REC FOR LEV 1  */

L30430:           call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                       if f1%(5) = 0% then L30630
                  if str(p$(l%),29,3) = "  0" then L30430
L30460:           gosub L55000            /* LOAD BOM RECORD & INFO.    */
                  if mkr$ = "RE" or mkr$ = "TL" then L30430
                  if mkr$ = "NC" or mkr$ = "NP" then L30430
                  if l% = 1% then needed = r(l%)  else                   ~
                       needed = quantity * xused * r(l%)

             REM Check To See If This Part Has Components Or Not
                  gosub'22 (component$, bom$)
                  readkey$ = str(component$,1,25) & str(bom$,1,3) & "  0"

                  call "READ100" (#5, readkey$, f1%(5))
                      if f1%(5) = 0% then component% = 0%                ~
                                     else component% = 1%
                  count%(l%) = count%(l%) + 1%
                  if component% = 0% or l% = maxlevels + 1%              ~
                     then gosub L30740              /* ATOM CASE        */~
                     else gosub L30670              /* (SUB)ASSEMBLY    */
                  goto L30430

L30630:           REM End Routine Gracefully.
                      l% = l% - 1%
                      return

L30670:     REM Routine To Cost Parts For Those That Are Manufactured
             gosub L30740          /* PRINT INDENTED ENTRY.      */

                REM Now Dig If Any To Build From Components.
                    gosub'7 (component$, needed)
                    return

L30740:     REM Print Da Datas...
             print$() = " "
             call "READ100" (#4, component$, f1%(4))
                  if f1%(4) = 1% then L30757
                       descr$ = "NOT ON FILE"
                       ptype$ = " "  :  uom$ = " "
                       goto L30760
L30757:           get #4, using L30758, descr$, uom$, ptype$
L30758:                FMT XX(25), CH(32), XX(16), CH(4), POS(180), CH(3)

L30760:      mat cst_bom = zer : mat cst_rte = zer
             mat cst_msc = zer : mat cost    = zer
             call "STCCOSTS" (component$, set$, #34, 3%, stdcost, cost(),~
                              cst_bom(), cst_rte(), cst_msc())

             stdcost = 0
             for k% = 1% to 12%
                if mkr$ <> "PH" then L30830
                    cost(k%) = cost(k%) - cst_rte(k%) - cst_msc(k%)
L30830:         stdcost = stdcost + cost(k%)
            next k%

             call "CONVERT" (needed*stdcost,2.4,str(print$(3),,9))
             if needed < 0 and mkr$ <> "BP" then print$(3%) = "NC ByProd"

             cost$() = " "
             x% = 9%
             for i% = 1% to 4%
                  tempn = round(needed * cost(i%), 4)
                  if tempn = 0 then L30930
                  if i% > 2% then x% = 8%
                  call "CONVERT" (tempn, 2.4, str(cost$(i%),,x%))
L30930:      next i%
             if buckets% <= 5% then L30940
                  for i% = 6% to buckets%
                       cost(5%) = cost(5%) + cost(i%)
                  next i%
L30940:      tempn = round(needed * cost(5%), 4)
             if tempn > 0 then                                           ~
                  call "CONVERT" (tempn, 2.4, str(cost$(5%),,8))
             call "CONVERT" (needed, 0.4, str(print$(2),,7))
             gosub L31040
             return

L31040:         REM Routine That Indents, Formats Entries, And Prints.
                     prtpart$ = " "
                     x% = l% * 3% - 2%
                     if x% + len(component$) > 52% then                  ~
                          x% = 53% - len(component$)
                     put str(prtpart$, x%,), using L31190,                ~
                          count%(l%), component$
                     gosub L31220                   /* PAGE CONTROL */
                     print using L32210, prtpart$, ptype$, uom$,          ~
                          print$(2), cost$(1), cost$(2), cost$(3),       ~
                          cost$(4), cost$(5), print$(3)
                     tagprinted% = 0%
                     prtpart$ = " "
                     x% = l% * 3% - 2%
                     if x% + len(descr$) > 52% then x% = 53% - len(descr$)
                     str(prtpart$, x% + 5%,) = descr$

                     line% = line% + 1%
                     print using L32210, prtpart$, " ", " ", " ", " ",    ~
                                        " ", " ", " ", " ", " "
                     tagprinted% = 0
                     return
L31190:                   %###. #########################
                          %     ################################

L31220:         REM Page Control Routine For The Cost Analysis

                     line% = line% + 1%
                     if line% < 60% then return
                     if page% = 0% then L31300
                          if tagprinted% <> 0% then L31300
                                print using L32050
                                tagprinted% = 1%
L31300:                   page% = page% + 1%
                     print page
                     print using L32000, date$, time$, company$
                     print using L32010, usrid$, page%
                     print skip(1)

                     print using L32030, hdrdescr$, set$, setdescr$
                     print using L32050
                     print using L32090, hdr1$(1%), hdr1$(2%), hdr1$(3%), ~
                                        hdr1$(4%), hdr1$(5%)
                     print using L32130, bkid$(1%), bkid$(2%), bkid$(3%), ~
                                        bkid$(4%), bkid$(5%)
                     print using L32170, hdr2$(1%), hdr2$(2%), hdr2$(3%), ~
                                        hdr2$(4%), hdr2$(5%)
                     print using L32050
                     line% = 9%
                     return


L32000: %RUN: ######## @ ########           #############################~
        ~###############################                      STCBOMRP:BOM~
        ~003

L32010: % BY: ###                  B I L L   O F   M A T E R I A L S   C ~
        ~O S T E D   A T   C U R R E N T   S T A N D A R D        PAGE:  #~
        ~###

L32030: %################################################################~
        ~################   COST SET: ######## ###########################~
        ~###
L32050:  %+--------------------------------------------------------+---+-~
        ~---+-------+---------+---------+--------+--------+--------+------~
        ~---+

L32090:  %!                                                        !   ! ~
        ~   ! TOTAL !######## !######## !########!########!########!  TOTA~
        ~L  !

L32130:  %!     P A R T   C O D E   /   D E S C R I P T I O N      !PRT!U~
        ~OM !  QTY  !#########!#########!########!########!########!STANDA~
        ~RD !

L32170:  %!                                                        !TYP! ~
        ~   ! REQ'RD!#########!#########!########!########!########!  COST~
        ~   !

L32210:  %!########################################################!###!#~
        ~###!#######!#########!#########!########!########!########!######~
        ~###!

L32250: %                                  ********** E N D   O F   R E P~
        ~ O R T  (@ ########) **********

L32510: REM Formats HDRDESCR$, which tell us what we asked for
                call "DESCRIBE" (#4, part$(part%), readkey$, 1%, f1%(4))
                hdrdescr$ = "Assembly # " & part$(part%) & " " &         ~
                            readkey$ & " Using BOM ID: " & bomid$(part%)

                return

        REM *************************************************************~
            *  A N S W E R   Q U E S T I O N S / I N P U T   P A R T S  *~
            *                                                           *~
            * ANSWER QUESTIONS AND INPUT PART NUMBER(S) TO PROCESS FOR  *~
            * THE ORDER ANALYSIS BIT.                                   *~
            *************************************************************

            deffn'201(fieldnr%)
                  if fieldnr% = 0% then pf16$ = "(16)Print Report"       ~
                                   else pf16$ = "(16)Exit Program"
                  if fieldnr% = 0% then pf14$ = "(14)View Report"        ~
                                   else pf14$ = " "
                  if fieldnr% > 0% and edit% = 1% then pf16$ = " "
                  if fieldnr% > 1% and edit% = 0% then pf16$ = " "
                  if edit% = 1% and fieldnr% = 0% then                   ~
                                inpmessage$ = editmessage$
                  init(hex(84)) lfac$(), fac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$(), fac$()
                  if pf16$ <> " " then lfac$(16%) = hex(84)

                  on fieldnr% gosub L40190,         /* MAXIMUM # LEVELS */~
                                    L40190          /* COST SET         */
                     goto L40420

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

            deffn'203(fieldnr%)
                  init(hex(84)) fac$(), lfac$()
                  pf14$ = " "
                  if fieldnr% > 0% and edit% = 1% then pf16$ = " "
                  if fieldnr% > 1% and edit% = 0% then pf16$ = " "
                  on fieldnr% gosub L40350          /* PART NUMBER      */

                     goto L40420

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%) = hex(80)
                      return
L40350:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%) = hex(82)
                      return

L40420:     accept                                                       ~
               at (01,02),                                               ~
               "Print Costed Bill of Materials",                         ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (07,02),                                               ~
                  "How Many BOM Levels Do You Want To Explode?(1 - 9)",  ~
               at (07,55), fac(lfac$(1)), maxlevels$             , ch(1),~
                                                                         ~
               at (08,02), "Cost Set to analyze",                        ~
               at (08,22), fac(lfac$(2)), set$                  , ch(08),~
               at (08,34), fac(hex(8c)),  setdescr$             , ch(30),~
                                                                         ~
               at (10,02), fac(hex(ac)),    line10$             , ch(79),~
               at (12,02), fac(fac$( 1)), part$    ( 1)         , ch(25),~
               at (12,45), fac(fac$( 2)), part$    ( 2)         , ch(25),~
               at (13,02), fac(fac$( 3)), part$    ( 3)         , ch(25),~
               at (13,45), fac(fac$( 4)), part$    ( 4)         , ch(25),~
               at (14,02), fac(fac$( 5)), part$    ( 5)         , ch(25),~
               at (14,45), fac(fac$( 6)), part$    ( 6)         , ch(25),~
               at (15,02), fac(fac$( 7)), part$    ( 7)         , ch(25),~
               at (15,45), fac(fac$( 8)), part$    ( 8)         , ch(25),~
               at (16,02), fac(fac$( 9)), part$    ( 9)         , ch(25),~
               at (16,45), fac(fac$(10)), part$    (10)         , ch(25),~
               at (17,02), fac(fac$(11)), part$    (11)         , ch(25),~
               at (17,45), fac(fac$(12)), part$    (12)         , ch(25),~
               at (18,02), fac(fac$(13)), part$    (13)         , ch(25),~
               at (18,45), fac(fac$(14)), part$    (14)         , ch(25),~
               at (19,02), fac(fac$(15)), part$    (15)         , ch(25),~
               at (19,45), fac(fac$(16)), part$    (16)         , ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,48), fac(hex(8c)),   pf14$                , ch(15),~
               at (24,65), fac(lfac$(16)), pf16$                , ch(16),~
                                                                         ~
               keys(hex(00010d0f100e)),                                  ~
               key (keyhit%)

               if keyhit% <> 13% then L40910
                  call "MANUAL" ("STCBOMRP")
                  goto L40420

L40910:        if keyhit% <> 15% then L40950
                  call "PRNTSCRN"
                  goto L40420

L40950:        REM GET CURSOR LOCATION FOR EDIT MODE COMPUTATIONS.
                   close ws
                   call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                   return

        REM *************************************************************~
            *    T E S T   D A T A   F O R   L I N E A R   I N P U T    *~
            *                                                           *~
            * TESTS DATA FOR LINEAR INPUT MODE.  CHECK THE PARAMETERS   *~
            * TO SEE THAT THEY ALL LINE UP.                             *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* MAX # OF LEVELS  */~
                                    L50280          /* COST SET         */


                     return

L50150: REM TEST DATA FOR MAXIMUM NUMBER OF LEVELS
            call "NUMTEST" (maxlevels$, 1, 9, errormsg$, 0.0, maxlevels)
            return

L50280: REM Test data for Cost Set to use
            plowkey$   = "STC.HDR." & set$
            setdescr$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#34, plowkey$, setdescr$, 8%, 0.30, onfile%)
            if onfile% = 1% then L50400
L50380:         errormsg$ = "Please select a valid Cost Set"
                return
L50400:     set$ = str(plowkey$,9)
            buckets% = 2%
            call "STCSETID" (buckets%, #34, set$, setid$, bkid$())
            if buckets% < 1% then L50380
            for i% = 10% to 11%
                call "GETUFBS1" addr(#i%, f2%(i%))
                if f2%(i%) = 1% then close #i%
            next i%
            call "STCFOPEN" (set$, "SXXXSX", #34, errormsg$, #10, #10,   ~
                             #10, #10, #11, #11)
            if errormsg$ <> " " then return
            hdr1$(), hdr2$() = " "
            for i% = 1% to buckets%
                if i% > 5% then L50570
                hdr1$(i%) = "STANDARD "
                hdr2$(i%) = "  COST   "
L50570:     next i%
            if buckets% < 6% then L50630
                  hdr1$(5%) = " SUM OF "
                  bkid$(5%) = " BUCKETS"
                  convert buckets% to x$, pic(##)
                  hdr2$(5%) = " 5 -> " & x$
L50630:     if edit% = 1% and set$ <> setsave$ then L50645
            setsave$ = set$
            return
L50645:         setsave$ = set$
                return clear             /* Changed Cost Set           */
                part$() = " "            /* Wipe out Part entered &    */
                goto L10240               /* Re-Enter parts.            */

        REM *************************************************************~
            *      T E S T   T A B U L A R   I N F O R M A T I O N      *~
            *                                                           *~
            * TESTS TABULAR INFORMATION TO MAKE SURE THAT THE DATA IS   *~
            * OK.  ZAP QUANTITY IF BLANK PART NUMBER ENTERED.           *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51300          /* PART NUMBER      */

                     return
L51300: REM TEST DATA FOR PART NUMBER
            if part$(c%) <> " " then L51315
            if part$( ) = " " then L51900
            if part$(c%) = " " then return
L51315:     if part$(c%) = "?" then part$(c%) = " "
            call "GETCODE" (#4, part$(c%), infomsg$, 0%, -0.003, f1%(4))
                if f1%(4) = 0% then  L51800
            str(infomsg$,33) = " "
            call "GETCODE" (#10, part$(c%), " ", 0%, -0.003, f1%(10))
                if f1%(10) = 0% then  L51850
            get #10 using L51370, bom$
L51370:         FMT POS(38), CH(3)
                if bom$ = "   " then L51950
            readkey$ = str(part$(c%),1,25) & str(bom$,1,3) & "  0"
            call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0% then L51750
                bomid$(c%) = bom$
                return

L51750:         errormsg$ = "Assy Number " & part$(c%) &                 ~
                            " Has No Components."
                return

L51800:         errormsg$ = "Assy Number " & part$(c%) &                 ~
                            " Not On Part Master File "
                return

L51850:         errormsg$ = "Assy Number " & part$(c%) &                 ~
                            " Not in Cost Set " &  set$
                return

L51900:         errormsg$ = "You must enter at least one Assembly Number"
                return

L51950:         errormsg$ = "No BOM Assigned to Assy " & part$(c%) &     ~
                            " in Cost Set " & set$
                return

L55000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD FROM DISK.             *~
            *************************************************************

            get #5, using L55110, component$, quantity, xused, mkr$

            return

L55110:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* WHICH ALTERNATE BOM        */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                POS(89),                                                 ~
                CH(2)                    /* BOM MARKER                 */



        REM FIND THE BOM ID FOR THIS PART IN THE COST SET

        deffn'22(part22$, bom$)

            call "READ100" (#10, str(part22$,1,25), f1%(10))
                if f1%(10) = 1% then L55290
                     bom$ = all(hex(00))
                     return
L55290:         get #10, using L55300, bom$
L55300:              FMT XX(37), CH(3)
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
